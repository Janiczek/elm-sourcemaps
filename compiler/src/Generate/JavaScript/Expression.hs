{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Expression
  ( generate
  , generateCtor
  , generateField
  , generateTailDef
  , generateMain
  , Code
  , codeToExpr
  , codeToStmtList
  )
  where


import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Version as V
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Json.Encode as Encode
import Json.Encode ((==>))
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A



-- EXPRESSIONS


generateJsExpr :: Mode.Mode -> ModuleName.Canonical -> Opt.Expr -> JS.Expr
generateJsExpr mode parentModule expression =
  codeToExpr (generate mode parentModule expression)


generate :: Mode.Mode -> ModuleName.Canonical -> Opt.Expr -> Code
generate mode parentModule expression =
  case expression of
    Opt.Bool (A.Region start _) bool ->
      JsExpr $ JS.TrackedBool parentModule start bool

    Opt.Chr (A.Region start _) char ->
      JsExpr $
        case mode of
          Mode.Dev _ ->
            JS.Call toChar [JS.TrackedString parentModule start (Utf8.toBuilder char)]

          Mode.Prod _ ->
            JS.TrackedString parentModule start (Utf8.toBuilder char)

    Opt.Str (A.Region start _) string ->
      JsExpr $ JS.TrackedString parentModule start (Utf8.toBuilder string)

    Opt.Int (A.Region start _) int ->
      JsExpr $ JS.TrackedInt parentModule start int

    Opt.Float (A.Region start _) float ->
      JsExpr $ JS.TrackedFloat parentModule start (Utf8.toBuilder float)

    Opt.VarLocal (A.Region startPos _) name ->
      JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromLocalHumanReadable name) (JsName.fromLocal name)

    Opt.VarGlobal (A.Region startPos _) (Opt.Global home name) ->
      JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)

    Opt.VarEnum (A.Region startPos _) (Opt.Global home name) index ->
      case mode of
        Mode.Dev _ ->
          JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)

        Mode.Prod _ ->
          JsExpr $ JS.Int (Index.toMachine index)

    Opt.VarBox (A.Region startPos _) (Opt.Global home name) ->
      JsExpr $
        case mode of
          Mode.Dev _ -> JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
          Mode.Prod _ -> JS.Ref $ JsName.fromGlobal ModuleName.basics Name.identity

    Opt.VarCycle (A.Region startPos _) home name ->
      JsExpr $ JS.Call (JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromCycle home name)) []

    Opt.VarDebug region name home unhandledValueName ->
      JsExpr $ generateDebug name home region unhandledValueName

    Opt.VarKernel (A.Region startPos _) home name ->
      JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromKernel home name) (JsName.fromKernel home name)

    Opt.List region entries ->
      case entries of
        [] ->
          JsExpr $ JS.Ref (JsName.fromKernel Name.list "Nil")

        _ ->
          let generatedEntries = map (generateJsExpr mode parentModule) entries
           in JsExpr $
                if region == A.zero
                  then JS.Array generatedEntries
                  else JS.TrackedArray parentModule region generatedEntries

    Opt.Function args body ->
      let argNames = map (\(A.At region name) -> A.At region (JsName.fromLocal name)) args
       in generateTrackedFunction parentModule argNames (generate mode parentModule body)

    Opt.Call (A.Region startPos _) func args ->
      JsExpr $ generateCall mode parentModule startPos func args

    Opt.TailCall name args ->
      JsBlock $ generateTailCall mode parentModule name args

    Opt.If branches final ->
      generateIf mode parentModule branches final

    Opt.Let def body ->
      JsBlock $
        generateDef mode parentModule def : codeToStmtList (generate mode parentModule body)

    Opt.Destruct (Opt.Destructor name path) body ->
      let
        pathDef = JS.Var (JsName.fromLocal name) (generatePath mode path)
      in
      JsBlock $ pathDef : codeToStmtList (generate mode parentModule body)

    Opt.Case label root decider jumps ->
      JsBlock $ generateCase mode parentModule label root decider jumps

    Opt.Accessor _ field ->
      JsExpr $ JS.Function Nothing [JsName.dollar]
        [ JS.Return $
            JS.Access (JS.Ref JsName.dollar) (generateField mode field)
        ]

    Opt.Access record (A.Region startPos _) field ->
      JsExpr $ JS.TrackedAccess (generateJsExpr mode parentModule record) parentModule startPos (generateField mode field)

    Opt.Update region record fields ->
      JsExpr $
        JS.Call (JS.Ref (JsName.fromKernel Name.utils "update"))
          [ generateJsExpr mode parentModule record
          , generateRecord mode parentModule region fields
          ]

    Opt.Record region fields ->
      JsExpr $ generateRecord mode parentModule region fields

    Opt.Unit ->
      case mode of
        Mode.Dev _ ->
          JsExpr $ JS.Ref (JsName.fromKernel Name.utils "Tuple0")

        Mode.Prod _ ->
          JsExpr $ JS.Int 0

    Opt.Tuple a b maybeC ->
      JsExpr $
        case maybeC of
          Nothing ->
            JS.Call (JS.Ref (JsName.fromKernel Name.utils "Tuple2"))
              [ generateJsExpr mode parentModule a
              , generateJsExpr mode parentModule b
              ]

          Just c ->
            JS.Call (JS.Ref (JsName.fromKernel Name.utils "Tuple3"))
              [ generateJsExpr mode parentModule a
              , generateJsExpr mode parentModule b
              , generateJsExpr mode parentModule c
              ]

    Opt.Shader src attributes uniforms ->
      let
        toTranlation field =
          ( JsName.fromLocal field
          , JS.String (JsName.toBuilder (generateField mode field))
          )

        toTranslationObject fields =
          JS.Object (map toTranlation (Set.toList fields))
      in
      JsExpr $ JS.Object $
        [ ( JsName.fromLocal "src", JS.String (Shader.toJsStringBuilder src) )
        , ( JsName.fromLocal "attributes", toTranslationObject attributes )
        , ( JsName.fromLocal "uniforms", toTranslationObject uniforms )
        ]



-- CODE CHUNKS


data Code
    = JsExpr JS.Expr
    | JsBlock [JS.Stmt]


codeToExpr :: Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr expr ->
      expr

    JsBlock [ JS.Return expr ] ->
      expr

    JsBlock stmts ->
      JS.Call (JS.Function Nothing [] stmts) []


codeToStmtList :: Code -> [JS.Stmt]
codeToStmtList code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
        stmts

    JsExpr expr ->
        [ JS.Return expr ]

    JsBlock stmts ->
        stmts


codeToStmt :: Code -> JS.Stmt
codeToStmt code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
        JS.Block stmts

    JsExpr expr ->
        JS.Return expr

    JsBlock [stmt] ->
        stmt

    JsBlock stmts ->
        JS.Block stmts



-- CHARS


{-# NOINLINE toChar #-}
toChar :: JS.Expr
toChar =
  JS.Ref (JsName.fromKernel Name.utils "chr")



-- CTOR


generateCtor :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor mode (Opt.Global home name) index arity =
  let
    argNames =
      Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

    ctorTag =
      case mode of
        Mode.Dev _ -> JS.String (Name.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)
  in
  generateFunction argNames $ JsExpr $ JS.Object $
    (JsName.dollar, ctorTag) : map (\n -> (n, JS.Ref n)) argNames


ctorToInt :: ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Int
ctorToInt home name index =
  if home == ModuleName.dict && name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin" then
    0 - Index.toHuman index
  else
    Index.toMachine index



-- RECORDS


generateRecord :: Mode.Mode -> ModuleName.Canonical -> A.Region -> Map.Map (A.Located Name.Name) Opt.Expr -> JS.Expr
generateRecord mode parentModule region fields =
  let toPair (A.At fieldRegion field, value) =
        (A.At fieldRegion $ generateField mode field, generateJsExpr mode parentModule value)
   in JS.TrackedObject parentModule region (map toPair (Map.toList fields))


generateField :: Mode.Mode -> Name.Name -> JsName.Name
generateField mode name =
  case mode of
    Mode.Dev _ ->
      JsName.fromLocal name

    Mode.Prod fields ->
      fields ! name




-- DEBUG


generateDebug :: Name.Name -> ModuleName.Canonical -> A.Region -> Maybe Name.Name -> JS.Expr
generateDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= "todo" then
    JS.Ref (JsName.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        JS.Call (JS.Ref (JsName.fromKernel Name.debug "todo")) $
          [ JS.String (Name.toBuilder home)
          , regionToJsExpr region
          ]

      Just valueName ->
        JS.Call (JS.Ref (JsName.fromKernel Name.debug "todoCase")) $
          [ JS.String (Name.toBuilder home)
          , regionToJsExpr region
          , JS.Ref (JsName.fromLocal valueName)
          ]


regionToJsExpr :: A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
  JS.Object
    [ ( JsName.fromLocal "start", positionToJsExpr start )
    , ( JsName.fromLocal "end", positionToJsExpr end )
    ]


positionToJsExpr :: A.Position -> JS.Expr
positionToJsExpr (A.Position line column) =
  JS.Object
    [ ( JsName.fromLocal "line", JS.Int (fromIntegral line) )
    , ( JsName.fromLocal "column", JS.Int (fromIntegral column) )
    ]



-- FUNCTION


generateFunction :: [JsName.Name] -> Code -> Code
generateFunction args body =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call helper
          [ JS.Function Nothing args $
              codeToStmtList body
          ]

    Nothing ->
      let
        addArg arg code =
          JsExpr $ JS.Function Nothing [arg] $
            codeToStmtList code
      in
      foldr addArg body args


generateTrackedFunction :: ModuleName.Canonical -> [A.Located JsName.Name] -> Code -> Code
generateTrackedFunction parentModule args body =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call
          helper
          [ JS.TrackedFunction parentModule args $
              codeToStmtList body
          ]
    Nothing ->
      case args of
        [_] ->
          JsExpr $
            JS.TrackedFunction parentModule args $
              codeToStmtList body
        _ ->
          let addArg arg code =
                JsExpr $
                  JS.Function Nothing [arg] $
                    codeToStmtList code
           in foldr addArg body (map A.toValue args)

{-# NOINLINE funcHelpers #-}
funcHelpers :: IntMap.IntMap JS.Expr
funcHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeF n))) [2..9]



-- CALLS


generateCall :: Mode.Mode -> ModuleName.Canonical -> A.Position -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCall mode parentModule pos func args =
  case func of
    Opt.VarGlobal _ global@(Opt.Global (ModuleName.Canonical pkg _) _) | pkg == Pkg.core ->
      generateCoreCall mode parentModule pos global args

    Opt.VarBox _ _ ->
      case mode of
        Mode.Dev _ ->
          generateCallHelp mode parentModule pos func args

        Mode.Prod _ ->
          case args of
            [arg] ->
              generateJsExpr mode parentModule arg

            _ ->
              generateCallHelp mode parentModule pos func args

    _ ->
      generateCallHelp mode parentModule pos func args


generateCallHelp :: Mode.Mode -> ModuleName.Canonical -> A.Position -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCallHelp mode parentModule pos func args =
  generateNormalCall
    parentModule
    pos
    (generateJsExpr mode parentModule func)
    (map (generateJsExpr mode parentModule) args)


generateGlobalCall :: ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateGlobalCall parentModule pos@(A.Position line col) home name args =
  let ref =
        if line == 0 && col == 0
          then JS.Ref (JsName.fromGlobal home name)
          else JS.TrackedRef parentModule pos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
   in generateNormalCall parentModule pos ref args


generateNormalCall :: ModuleName.Canonical -> A.Position -> JS.Expr -> [JS.Expr] -> JS.Expr
generateNormalCall parentModule pos func args =
  case IntMap.lookup (length args) callHelpers of
    Just helper ->
      JS.TrackedNormalCall parentModule pos helper func args

    Nothing ->
      List.foldl' (\f a -> JS.Call f [a]) func args


{-# NOINLINE callHelpers #-}
callHelpers :: IntMap.IntMap JS.Expr
callHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeA n))) [2..9]



-- CORE CALLS


generateCoreCall :: Mode.Mode -> ModuleName.Canonical -> A.Position -> Opt.Global -> [Opt.Expr] -> JS.Expr
generateCoreCall mode parentModule pos (Opt.Global home@(ModuleName.Canonical _ moduleName) name) args =
  if moduleName == Name.basics then
    generateBasicsCall mode parentModule pos home name args

  else if moduleName == Name.bitwise then
    generateBitwiseCall parentModule pos home name (map (generateJsExpr mode parentModule) args)

  else if moduleName == Name.tuple then
    generateTupleCall parentModule pos home name (map (generateJsExpr mode parentModule) args)

  else if moduleName == Name.jsArray then
    generateJsArrayCall parentModule pos home name (map (generateJsExpr mode parentModule) args)

  else
    generateGlobalCall parentModule pos home name (map (generateJsExpr mode parentModule) args)


generateTupleCall :: ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateTupleCall parentModule pos home name args =
  case args of
    [value] ->
      case name of
        "first"  -> JS.Access value (JsName.fromLocal "a")
        "second" -> JS.Access value (JsName.fromLocal "b")
        _ -> generateGlobalCall parentModule pos home name args

    _ ->
      generateGlobalCall parentModule pos home name args


generateJsArrayCall :: ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateJsArrayCall parentModule pos home name args =
  case args of
    [entry]        | name == "singleton" -> JS.Array [entry]
    [index, array] | name == "unsafeGet" -> JS.Index array index
    _ -> generateGlobalCall parentModule pos home name args


generateBitwiseCall :: ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateBitwiseCall parentModule pos home name args =
  case args of
    [arg] ->
      case name of
        "complement" -> JS.Prefix JS.PrefixComplement arg
        _ -> generateGlobalCall parentModule pos home name args

    [left,right] ->
      case name of
        "and"            -> JS.Infix JS.OpBitwiseAnd left right
        "or"             -> JS.Infix JS.OpBitwiseOr  left right
        "xor"            -> JS.Infix JS.OpBitwiseXor left right
        "shiftLeftBy"    -> JS.Infix JS.OpLShift     right left
        "shiftRightBy"   -> JS.Infix JS.OpSpRShift   right left
        "shiftRightZfBy" -> JS.Infix JS.OpZfRShift   right left
        _ -> generateGlobalCall parentModule pos home name args

    _ ->
      generateGlobalCall parentModule pos home name args


generateBasicsCall :: Mode.Mode -> ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [Opt.Expr] -> JS.Expr
generateBasicsCall mode parentModule pos home name args =
  case args of
    [elmArg] ->
      let arg = generateJsExpr mode parentModule elmArg in
      case name of
        "not"      -> JS.Prefix JS.PrefixNot arg
        "negate"   -> JS.Prefix JS.PrefixNegate arg
        "toFloat"  -> arg
        "truncate" -> JS.Infix JS.OpBitwiseOr arg (JS.Int 0)
        _ -> generateGlobalCall parentModule pos home name [arg]

    [elmLeft, elmRight] ->
      case name of
        -- NOTE: removed "composeL" and "composeR" because of this issue:
        -- https://github.com/elm/compiler/issues/1722
        "append"   -> append mode parentModule elmLeft elmRight
        "apL"      -> generateJsExpr mode parentModule $ apply elmLeft elmRight
        "apR"      -> generateJsExpr mode parentModule $ apply elmRight elmLeft
        _ ->
          let
            left = generateJsExpr mode parentModule elmLeft
            right = generateJsExpr mode parentModule elmRight
          in
          case name of
            "add"  -> JS.Infix JS.OpAdd left right
            "sub"  -> JS.Infix JS.OpSub left right
            "mul"  -> JS.Infix JS.OpMul left right
            "fdiv" -> JS.Infix JS.OpDiv left right
            "idiv" -> JS.Infix JS.OpBitwiseOr (JS.Infix JS.OpDiv left right) (JS.Int 0)
            "eq"   -> equal left right
            "neq"  -> notEqual left right
            "lt"   -> cmp JS.OpLt JS.OpLt   0  left right
            "gt"   -> cmp JS.OpGt JS.OpGt   0  left right
            "le"   -> cmp JS.OpLe JS.OpLt   1  left right
            "ge"   -> cmp JS.OpGe JS.OpGt (-1) left right
            "or"   -> JS.Infix JS.OpOr  left right
            "and"  -> JS.Infix JS.OpAnd left right
            "xor"  -> JS.Infix JS.OpNe  left right
            "remainderBy" -> JS.Infix JS.OpMod right left
            _ -> generateGlobalCall parentModule pos home name [left, right]

    _ ->
      generateGlobalCall parentModule pos home name (map (generateJsExpr mode parentModule) args)


equal :: JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right then
    strictEq left right
  else
    JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]


notEqual :: JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right then
    strictNEq left right
  else
    JS.Prefix JS.PrefixNot $
      JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]


cmp :: JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
cmp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right then
    JS.Infix idealOp left right
  else
    JS.Infix backupOp
      (JS.Call (JS.Ref (JsName.fromKernel Name.utils "cmp")) [left, right])
      (JS.Int backupInt)


isLiteral :: JS.Expr -> Bool
isLiteral expr =
  case expr of
    JS.String _ ->
      True

    JS.Float _ ->
      True

    JS.Int _ ->
      True

    JS.Bool _ ->
      True

    _ ->
      False


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor region field ->
      Opt.Access value region field

    Opt.Call region f args ->
      Opt.Call region f (args ++ [value])

    _ ->
      Opt.Call (Maybe.fromMaybe A.zero (exprRegion func)) func [value]

exprRegion :: Opt.Expr -> Maybe A.Region
exprRegion expr =
  case expr of
    Opt.Bool region _ -> Just region
    Opt.Chr region _ -> Just region
    Opt.Str region _ -> Just region
    Opt.Int region _ -> Just region
    Opt.Float region _ -> Just region
    Opt.VarLocal region _ -> Just region
    Opt.VarGlobal region _ -> Just region
    Opt.VarEnum region _ _ -> Just region
    Opt.VarBox region _ -> Just region
    Opt.VarCycle region _ _ -> Just region
    Opt.VarDebug region _ _ _ -> Just region
    Opt.VarKernel region _ _ -> Just region
    Opt.List region _ -> Just region
    Opt.Function _ _ -> Nothing
    Opt.Call region _ _ -> Just region
    Opt.TailCall _ _ -> Nothing
    Opt.If _ _ -> Nothing
    Opt.Let _ _ -> Nothing
    Opt.Destruct _ _ -> Nothing
    Opt.Case _ _ _ _ -> Nothing
    Opt.Accessor region _ -> Just region
    Opt.Access _ region _ -> Just region
    Opt.Update region _ _ -> Just region
    Opt.Record region _ -> Just region


append :: Mode.Mode -> ModuleName.Canonical -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode parentModule left right =
  let seqs = generateJsExpr mode parentModule left : toSeqs mode parentModule right
  in
  if any isStringLiteral seqs then
    foldr1 (JS.Infix JS.OpAdd) seqs
  else
    foldr1 jsAppend seqs


jsAppend :: JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
  JS.Call (JS.Ref (JsName.fromKernel Name.utils "ap")) [a, b]


toSeqs :: Mode.Mode -> ModuleName.Canonical -> Opt.Expr -> [JS.Expr]
toSeqs mode parentModule expr =
  case expr of
    Opt.Call _ (Opt.VarGlobal _ (Opt.Global home "append")) [left, right]
      | home == ModuleName.basics ->
          generateJsExpr mode parentModule left : toSeqs mode parentModule right

    _ ->
      [generateJsExpr mode parentModule expr]


isStringLiteral :: JS.Expr -> Bool
isStringLiteral expr =
  case expr of
    JS.String _ ->
      True

    _ ->
      False



-- SIMPLIFY INFIX OPERATORS


strictEq :: JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
  case left of
    JS.Int 0 ->
      JS.Prefix JS.PrefixNot right

    JS.Bool bool ->
      if bool then right else JS.Prefix JS.PrefixNot right

    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixNot left

        JS.Bool bool ->
          if bool then left else JS.Prefix JS.PrefixNot left

        _ ->
          JS.Infix JS.OpEq left right


strictNEq :: JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.Int 0 ->
      JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot right)

    JS.Bool bool ->
      if bool then JS.Prefix JS.PrefixNot right else right

    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot left)

        JS.Bool bool ->
          if bool then JS.Prefix JS.PrefixNot left else left

        _ ->
          JS.Infix JS.OpNe left right



-- TAIL CALL


-- TODO check if JS minifiers collapse unnecessary temporary variables
--
generateTailCall :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> [(Name.Name, Opt.Expr)] -> [JS.Stmt]
generateTailCall mode parentModule name args =
  let
    toTempVars (argName, arg) =
      (JsName.makeTemp argName, generateJsExpr mode parentModule arg)

    toRealVars (argName, _) =
      JS.ExprStmt $
        JS.Assign (JS.LRef (JsName.fromLocal argName)) (JS.Ref (JsName.makeTemp argName))
  in
  JS.Vars (map toTempVars args)
  : map toRealVars args
  ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


generateDef :: Mode.Mode -> ModuleName.Canonical -> Opt.Def -> JS.Stmt
generateDef mode parentModule def =
  case def of
    Opt.Def (A.Region start _) name body ->
      JS.TrackedVar parentModule start (JsName.fromLocal name) (JsName.fromLocal name) $
        generateJsExpr mode parentModule body

    Opt.TailDef (A.Region start _) name argNames body ->
      JS.TrackedVar parentModule start (JsName.fromLocal name) (JsName.fromLocal name) $
        codeToExpr (generateTailDef mode parentModule name argNames body)


generateTailDef :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> [A.Located Name.Name] -> Opt.Expr -> Code
generateTailDef mode parentModule name argNames body =
  generateTrackedFunction parentModule (map (\(A.At region argName) -> A.At region (JsName.fromLocal argName)) argNames) $
    JsBlock
    [ JS.Labelled (JsName.fromLocal name) $
        JS.While (JS.Bool True) $
          codeToStmt $ generate mode parentModule body
    ]



-- PATHS


generatePath :: Mode.Mode -> Opt.Path -> JS.Expr
generatePath mode path =
  case path of
    Opt.Index index subPath ->
      JS.Access (generatePath mode subPath) (JsName.fromIndex index)

    Opt.Root name ->
      JS.Ref (JsName.fromLocal name)

    Opt.Field field subPath ->
      JS.Access (generatePath mode subPath) (generateField mode field)

    Opt.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (generatePath mode subPath) (JsName.fromIndex Index.first)

        Mode.Prod _ ->
          generatePath mode subPath



-- GENERATE IFS


generateIf :: Mode.Mode -> ModuleName.Canonical -> [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Code
generateIf mode parentModule givenBranches givenFinal =
  let
    (branches, final) =
      crushIfs givenBranches givenFinal

    convertBranch (condition, expr) =
      ( generateJsExpr mode parentModule condition
      , generate mode parentModule expr
      )

    branchExprs = map convertBranch branches
    finalCode = generate mode parentModule final
  in
  if isBlock finalCode || any (isBlock . snd) branchExprs then
    JsBlock [ foldr addStmtIf (codeToStmt finalCode) branchExprs ]
  else
    JsExpr $ foldr addExprIf (codeToExpr finalCode) branchExprs


addExprIf :: (JS.Expr, Code) -> JS.Expr -> JS.Expr
addExprIf (condition, branch) final =
  JS.If condition (codeToExpr branch) final


addStmtIf :: (JS.Expr, Code) -> JS.Stmt -> JS.Stmt
addStmtIf (condition, branch) final =
  JS.IfStmt condition (codeToStmt branch) final


isBlock :: Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr _ -> False


crushIfs :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfs branches final =
  crushIfsHelp [] branches final


crushIfsHelp
    :: [(Opt.Expr, Opt.Expr)]
    -> [(Opt.Expr, Opt.Expr)]
    -> Opt.Expr
    -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches final =
  case unvisitedBranches of
    [] ->
        case final of
          Opt.If subBranches subFinal ->
              crushIfsHelp visitedBranches subBranches subFinal

          _ ->
              (reverse visitedBranches, final)

    visiting : unvisited ->
        crushIfsHelp (visiting : visitedBranches) unvisited final



-- CASE EXPRESSIONS


generateCase :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> [JS.Stmt]
generateCase mode parentModule label root decider jumps =
  foldr (goto mode parentModule label) (generateDecider mode parentModule label root decider) jumps


goto :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> (Int, Opt.Expr) -> [JS.Stmt] -> [JS.Stmt]
goto mode parentModule label (index, branch) stmts =
  let
    labeledDeciderStmt =
      JS.Labelled
        (JsName.makeLabel label index)
        (JS.While (JS.Bool True) (JS.Block stmts))
  in
  labeledDeciderStmt : codeToStmtList (generate mode parentModule branch)


generateDecider :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> [JS.Stmt]
generateDecider mode parentModule label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      codeToStmtList (generate mode parentModule branch)

    Opt.Leaf (Opt.Jump index) ->
      [ JS.Break (Just (JsName.makeLabel label index)) ]

    Opt.Chain testChain success failure ->
      [ JS.IfStmt
          (List.foldl1' (JS.Infix JS.OpAnd) (map (generateIfTest mode root) testChain))
          (JS.Block $ generateDecider mode parentModule label root success)
          (JS.Block $ generateDecider mode parentModule label root failure)
      ]

    Opt.FanOut path edges fallback ->
      [ JS.Switch
          (generateCaseTest mode root path (fst (head edges)))
          ( foldr
              (\edge cases -> generateCaseBranch mode parentModule label root edge : cases)
              [JS.Default (generateDecider mode parentModule label root fallback)]
              edges
          )
      ]


generateIfTest :: Mode.Mode -> Name.Name -> (DT.Path, DT.Test) -> JS.Expr
generateIfTest mode root (path, test) =
  let
    value = pathToJsExpr mode root path
  in
  case test of
    DT.IsCtor home name index _ opts ->
      let
        tag =
          case mode of
            Mode.Dev _ -> JS.Access value JsName.dollar
            Mode.Prod _ ->
              case opts of
                Can.Normal -> JS.Access value JsName.dollar
                Can.Enum   -> value
                Can.Unbox  -> value
      in
      strictEq tag $
        case mode of
          Mode.Dev _ -> JS.String (Name.toBuilder name)
          Mode.Prod _ -> JS.Int (ctorToInt home name index)

    DT.IsBool True ->
      value

    DT.IsBool False ->
      JS.Prefix JS.PrefixNot value

    DT.IsInt int ->
      strictEq value (JS.Int int)

    DT.IsChr char ->
      strictEq (JS.String (Utf8.toBuilder char)) $
        case mode of
          Mode.Dev _ -> JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []
          Mode.Prod _ -> value

    DT.IsStr string ->
      strictEq value (JS.String (Utf8.toBuilder string))

    DT.IsCons ->
      JS.Access value (JsName.fromLocal "b")

    DT.IsNil ->
      JS.Prefix JS.PrefixNot $
        JS.Access value (JsName.fromLocal "b")

    DT.IsTuple ->
      error "COMPILER BUG - there should never be tests on a tuple"



generateCaseBranch :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> Name.Name -> (DT.Test, Opt.Decider Opt.Choice) -> JS.Case
generateCaseBranch mode parentModule label root (test, subTree) =
  JS.Case
    (generateCaseValue mode test)
    (generateDecider mode parentModule label root subTree)


generateCaseValue :: Mode.Mode -> DT.Test -> JS.Expr
generateCaseValue mode test =
  case test of
    DT.IsCtor home name index _ _ ->
      case mode of
        Mode.Dev _ -> JS.String (Name.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)

    DT.IsInt int ->
      JS.Int int

    DT.IsChr char ->
      JS.String (Utf8.toBuilder char)

    DT.IsStr string ->
      JS.String (Utf8.toBuilder string)

    DT.IsBool _ ->
      error "COMPILER BUG - there should never be three tests on a boolean"

    DT.IsCons ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsNil ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsTuple ->
      error "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest :: Mode.Mode -> Name.Name -> DT.Path -> DT.Test -> JS.Expr
generateCaseTest mode root path exampleTest =
  let
    value = pathToJsExpr mode root path
  in
  case exampleTest of
    DT.IsCtor home name _ _ opts ->
      if name == Name.bool && home == ModuleName.basics then
        value
      else
        case mode of
          Mode.Dev _ ->
            JS.Access value JsName.dollar

          Mode.Prod _ ->
            case opts of
              Can.Normal ->
                JS.Access value JsName.dollar

              Can.Enum ->
                value

              Can.Unbox ->
                value

    DT.IsInt _ ->
      value

    DT.IsStr _ ->
      value

    DT.IsChr _ ->
      case mode of
        Mode.Dev _ ->
          JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []

        Mode.Prod _ ->
          value

    DT.IsBool _ ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsCons ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsNil ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsTuple ->
      error "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr :: Mode.Mode -> Name.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
  case path of
    DT.Index index subPath ->
      JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex index)

    DT.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex Index.first)

        Mode.Prod _ ->
          pathToJsExpr mode root subPath

    DT.Empty ->
      JS.Ref (JsName.fromLocal root)



-- GENERATE MAIN


generateMain :: Mode.Mode -> ModuleName.Canonical -> Opt.Main -> JS.Expr
generateMain mode home main =
  case main of
    Opt.Static ->
      JS.Ref (JsName.fromKernel Name.virtualDom "init")
        # JS.Ref (JsName.fromGlobal home "main")
        # JS.Int 0
        # JS.Int 0

    Opt.Dynamic msgType decoder ->
      JS.Ref (JsName.fromGlobal home "main")
        # generateJsExpr mode home decoder
        # toDebugMetadata mode msgType


(#) :: JS.Expr -> JS.Expr -> JS.Expr
(#) func arg =
  JS.Call func [arg]


toDebugMetadata :: Mode.Mode -> Can.Type -> JS.Expr
toDebugMetadata mode msgType =
  case mode of
    Mode.Prod _ ->
      JS.Int 0

    Mode.Dev Nothing ->
      JS.Int 0

    Mode.Dev (Just interfaces) ->
      JS.Json $ Encode.object $
        [ "versions" ==> Encode.object [ "elm" ==> V.encode V.compiler ]
        , "types"    ==> Type.encodeMetadata (Extract.fromMsg interfaces msgType)
        ]
