{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Module
  ( optimize
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Map ((!))

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Effects as Effects
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Expression as Expr
import qualified Optimize.Names as Names
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Main as E
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- OPTIMIZE


type Result i w a =
  Result.Result i w E.Error a


type Annotations =
  Map.Map Name.Name Can.Annotation


optimize :: Annotations -> Can.Module -> Result i [W.Warning] Opt.LocalGraph
optimize annotations (Can.Module home _ _ decls unions aliases _ effects) =
  addDecls home annotations decls $
    addEffects home effects $
      addUnions home unions $
        addAliases home aliases $
          Opt.LocalGraph Nothing Map.empty Map.empty



-- UNION


type Nodes =
  Map.Map Opt.Global Opt.Node


addUnions :: ModuleName.Canonical -> Map.Map Name.Name Can.Union -> Opt.LocalGraph -> Opt.LocalGraph
addUnions home unions (Opt.LocalGraph main nodes fields) =
  Opt.LocalGraph main (Map.foldr (addUnion home) nodes unions) fields


addUnion :: ModuleName.Canonical -> Can.Union -> Nodes -> Nodes
addUnion home (Can.Union _ ctors _ opts) nodes =
  List.foldl' (addCtorNode home opts) nodes ctors


addCtorNode :: ModuleName.Canonical -> Can.CtorOpts -> Nodes -> Can.Ctor -> Nodes
addCtorNode home opts nodes (Can.Ctor name index numArgs _) =
  let
    node =
      case opts of
        Can.Normal -> Opt.Ctor index numArgs
        Can.Unbox -> Opt.Box
        Can.Enum -> Opt.Enum index
  in
  Map.insert (Opt.Global home name) node nodes



-- ALIAS


addAliases :: ModuleName.Canonical -> Map.Map Name.Name Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAliases home aliases graph =
  Map.foldrWithKey (addAlias home) graph aliases


addAlias :: ModuleName.Canonical -> Name.Name -> Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAlias home name (Can.Alias _ tipe) graph@(Opt.LocalGraph main nodes fieldCounts) =
  case tipe of
    Can.TRecord fields Nothing ->
      let
        function =
          Opt.Function (map (\(name,_) -> (A.At A.zero name)) (Can.fieldsToList fields)) $
            Opt.Record A.zero $
              Map.mapKeys (A.At A.zero) $
                Map.mapWithKey (\field _ -> Opt.VarLocal A.zero field) fields

        node =
          Opt.Define A.zero function Set.empty
      in
      Opt.LocalGraph
        main
        (Map.insert (Opt.Global home name) node nodes)
        (Map.foldrWithKey addRecordCtorField fieldCounts fields)

    _ ->
      graph


addRecordCtorField :: Name.Name -> Can.FieldType -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addRecordCtorField name _ fields =
  Map.insertWith (+) name 1 fields



-- ADD EFFECTS


addEffects :: ModuleName.Canonical -> Can.Effects -> Opt.LocalGraph -> Opt.LocalGraph
addEffects home effects graph@(Opt.LocalGraph main nodes fields) =
  case effects of
    Can.NoEffects ->
      graph

    Can.Ports ports ->
      Map.foldrWithKey (addPort home) graph ports

    Can.Manager _ _ _ manager ->
      let
        fx = Opt.Global home "$fx$"
        cmd = Opt.Global home "command"
        sub = Opt.Global home "subscription"
        link = Opt.Link fx
        newNodes =
          case manager of
            Can.Cmd _ ->
              Map.insert cmd link $
              Map.insert fx (Opt.Manager Opt.Cmd) nodes

            Can.Sub _ ->
              Map.insert sub link $
              Map.insert fx (Opt.Manager Opt.Sub) nodes

            Can.Fx _ _ ->
              Map.insert cmd link $
              Map.insert sub link $
              Map.insert fx (Opt.Manager Opt.Fx) nodes
      in
      Opt.LocalGraph main newNodes fields


addPort :: ModuleName.Canonical -> Name.Name -> Can.Port -> Opt.LocalGraph -> Opt.LocalGraph
addPort home name port_ graph =
  case port_ of
    Can.Incoming _ payloadType _ ->
      let
        (deps, fields, decoder) = Names.run (Port.toDecoder payloadType)
        node = Opt.PortIncoming decoder deps
      in
      addToGraph (Opt.Global home name) node fields graph

    Can.Outgoing _ payloadType _ ->
      let
        (deps, fields, encoder) = Names.run (Port.toEncoder payloadType)
        node = Opt.PortOutgoing encoder deps
      in
      addToGraph (Opt.Global home name) node fields graph



-- HELPER


addToGraph :: Opt.Global -> Opt.Node -> Map.Map Name.Name Int -> Opt.LocalGraph -> Opt.LocalGraph
addToGraph name node fields (Opt.LocalGraph main nodes fieldCounts) =
  Opt.LocalGraph
    main
    (Map.insert name node nodes)
    (Map.unionWith (+) fields fieldCounts)



-- ADD DECLS


addDecls :: ModuleName.Canonical -> Annotations -> Can.Decls -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
addDecls home annotations decls graph =
  case decls of
    Can.Declare def subDecls ->
      addDecls home annotations subDecls =<< addDef home annotations def graph

    Can.DeclareRec d ds subDecls ->
      let defs = d:ds in
      case findMain defs of
        Nothing ->
          addDecls home annotations subDecls (addRecDefs home defs graph)

        Just region ->
          Result.throw $ E.BadCycle region (defToName d) (map defToName ds)

    Can.SaveTheEnvironment ->
      Result.ok graph


findMain :: [Can.Def] -> Maybe A.Region
findMain defs =
  case defs of
    [] ->
      Nothing

    def:rest ->
      case def of
        Can.Def (A.At region name) _ _ ->
          if name == Name._main then Just region else findMain rest

        Can.TypedDef (A.At region name) _ _ _ _ ->
          if name == Name._main then Just region else findMain rest


defToName :: Can.Def -> Name.Name
defToName def =
  case def of
    Can.Def (A.At _ name) _ _          -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name



-- ADD DEFS


addDef :: ModuleName.Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
addDef home annotations def graph =
  case def of
    Can.Def (A.At region name) args body ->
      do  let (Can.Forall _ tipe) = annotations ! name
          Result.warn $ W.MissingTypeAnnotation region name tipe
          addDefHelp region annotations home name args body graph

    Can.TypedDef (A.At region name) _ typedArgs body _ ->
      addDefHelp region annotations home name (map fst typedArgs) body graph


addDefHelp :: A.Region -> Annotations -> ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> Opt.LocalGraph -> Result i w Opt.LocalGraph
addDefHelp region annotations home name args body graph@(Opt.LocalGraph _ nodes fieldCounts) =
  if name /= Name._main then
    Result.ok (addDefNode home region name args body Set.empty graph)
  else
    let
      (Can.Forall _ tipe) = annotations ! name

      addMain (deps, fields, main) =
        addDefNode home region name args body deps $
          Opt.LocalGraph (Just main) nodes (Map.unionWith (+) fields fieldCounts)
    in
    case Type.deepDealias tipe of
      Can.TType hm nm [_] | hm == ModuleName.virtualDom && nm == Name.node ->
          Result.ok $ addMain $ Names.run $
            Names.registerKernel Name.virtualDom Opt.Static

      Can.TType hm nm [flags, _, message] | hm == ModuleName.platform && nm == Name.program ->
          case Effects.checkPayload flags of
            Right () ->
              Result.ok $ addMain $ Names.run $
                Opt.Dynamic message <$> Port.toFlagsDecoder flags

            Left (subType, invalidPayload) ->
              Result.throw (E.BadFlags region subType invalidPayload)

      _ ->
          Result.throw (E.BadType region tipe)


addDefNode :: ModuleName.Canonical -> A.Region -> Name.Name -> [Can.Pattern] -> Can.Expr -> Set.Set Opt.Global -> Opt.LocalGraph -> Opt.LocalGraph
addDefNode home region name args body mainDeps graph =
  let
    (deps, fields, def) =
      Names.run $
        case args of
          [] ->
            Expr.optimize Set.empty body

          _ ->
            do  (argNames, destructors) <- Expr.destructArgs args
                obody <- Expr.optimize Set.empty body
                pure $ Opt.Function argNames $
                  foldr Opt.Destruct obody destructors
  in
  addToGraph (Opt.Global home name) (Opt.Define region def (Set.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


data State =
  State
    { _values :: [(Name.Name, Opt.Expr)]
    , _functions :: [Opt.Def]
    }


addRecDefs :: ModuleName.Canonical -> [Can.Def] -> Opt.LocalGraph -> Opt.LocalGraph
addRecDefs home defs (Opt.LocalGraph main nodes fieldCounts) =
  let
    names = reverse (map toName defs)
    cycleName = Opt.Global home (Name.fromManyNames names)
    cycle = foldr addValueName Set.empty defs
    links = foldr (addLink home (Opt.Link cycleName)) Map.empty defs

    (deps, fields, State values funcs) =
      Names.run $
        foldM (addRecDef cycle) (State [] []) defs
  in
  Opt.LocalGraph
    main
    (Map.insert cycleName (Opt.Cycle names values funcs deps) (Map.union links nodes))
    (Map.unionWith (+) fields fieldCounts)


toName :: Can.Def -> Name.Name
toName def =
  case def of
    Can.Def      (A.At _ name) _ _     -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name


addValueName :: Can.Def -> Set.Set Name.Name -> Set.Set Name.Name
addValueName def names =
  case def of
    Can.Def      (A.At _ name)   args _   -> if null args then Set.insert name names else names
    Can.TypedDef (A.At _ name) _ args _ _ -> if null args then Set.insert name names else names


addLink :: ModuleName.Canonical -> Opt.Node -> Can.Def -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
addLink home link def links =
  case def of
    Can.Def (A.At _ name) _ _ ->
      Map.insert (Opt.Global home name) link links

    Can.TypedDef (A.At _ name) _ _ _ _ ->
      Map.insert (Opt.Global home name) link links



-- ADD RECURSIVE DEFS


addRecDef :: Set.Set Name.Name -> State -> Can.Def -> Names.Tracker State
addRecDef cycle state def =
  case def of
    Can.Def (A.At region name) args body ->
      addRecDefHelp cycle region state name args body

    Can.TypedDef (A.At region name) _ args body _ ->
      addRecDefHelp cycle region state name (map fst args) body


addRecDefHelp :: Set.Set Name.Name -> A.Region -> State -> Name.Name -> [Can.Pattern] -> Can.Expr -> Names.Tracker State
addRecDefHelp cycle region (State values funcs) name args body =
  case args of
    [] ->
      do  obody <- Expr.optimize cycle body
          pure $ State ((name, obody) : values) funcs

    _:_ ->
      do  odef <- Expr.optimizePotentialTailCall cycle region name args body
          pure $ State values (odef : funcs)
