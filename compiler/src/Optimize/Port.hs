{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Port
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )
  where


import Prelude hiding (maybe, null)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Type as Type
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Names as Names
import qualified Reporting.Annotation as A



-- ENCODE


toEncoder :: Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    Can.TAlias _ _ args alias ->
      toEncoder (Type.dealias args alias)

    Can.TLambda _ _ ->
      error "toEncoder: function"

    Can.TVar _ ->
      error "toEncoder: type variable"

    Can.TUnit ->
      Opt.Function [A.At A.zero Name.dollar] <$> encode "null"

    Can.TTuple a b c ->
      encodeTuple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == Name.float  -> encode "float"
          | name == Name.int    -> encode "int"
          | name == Name.bool   -> encode "bool"
          | name == Name.string -> encode "string"
          | name == Name.value  -> Names.registerGlobal A.zero ModuleName.basics Name.identity

        [arg]
          | name == Name.maybe -> encodeMaybe arg
          | name == Name.list  -> encodeList arg
          | name == Name.array -> encodeArray arg

        _ ->
          error "toEncoder: bad custom type"

    Can.TRecord _ (Just _) ->
      error "toEncoder: bad record"

    Can.TRecord fields Nothing ->
      let
        encodeField (name, Can.FieldType _ fieldType) =
          do  encoder <- toEncoder fieldType
              let value = Opt.Call A.zero encoder [Opt.Access (Opt.VarLocal A.zero Name.dollar) A.zero name]
              return $ Opt.Tuple (Opt.Str A.zero (Name.toElmString name)) value Nothing
      in
      do  object <- encode "object"
          keyValuePairs <- traverse encodeField (Map.toList fields)
          Names.registerFieldDict fields $
            Opt.Function [A.At A.zero Name.dollar] (Opt.Call A.zero object [Opt.List A.zero keyValuePairs])



-- ENCODE HELPERS


encodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do  null <- encode "null"
      encoder <- toEncoder tipe
      destruct <- Names.registerGlobal A.zero ModuleName.maybe "destruct"
      return $
        Opt.Function [A.At A.zero Name.dollar] $
          Opt.Call A.zero destruct [ null, encoder, Opt.VarLocal A.zero Name.dollar ]


encodeList :: Can.Type -> Names.Tracker Opt.Expr
encodeList tipe =
  do  list <- encode "list"
      encoder <- toEncoder tipe
      return $ Opt.Call A.zero list [ encoder ]


encodeArray :: Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  do  array <- encode "array"
      encoder <- toEncoder tipe
      return $ Opt.Call A.zero array [ encoder ]


encodeTuple :: Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
encodeTuple a b maybeC =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root Name.dollar))) body

    encodeArg arg tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.Call A.zero encoder [ Opt.VarLocal A.zero arg ]
  in
  do  list <- encode "list"
      identity <- Names.registerGlobal A.zero ModuleName.basics Name.identity
      arg1 <- encodeArg "a" a
      arg2 <- encodeArg "b" b

      case maybeC of
        Nothing ->
          return $ Opt.Function [A.At A.zero Name.dollar] $
            let_ "a" Index.first $
            let_ "b" Index.second $
              Opt.Call A.zero list
                [ identity
                , Opt.List A.zero
                    [ arg1, arg2 ]
                ]

        Just c ->
          do  arg3 <- encodeArg "c" c
              return $ Opt.Function [A.At A.zero Name.dollar] $
                let_ "a" Index.first $
                let_ "b" Index.second $
                let_ "c" Index.third $
                  Opt.Call A.zero list
                    [ identity
                    , Opt.List A.zero
                        [ arg1, arg2, arg3 ]
                    ]



-- FLAGS DECODER


toFlagsDecoder :: Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    Can.TUnit ->
      do  succeed <- decode "succeed"
          return $ Opt.Call A.zero succeed [ Opt.Unit ]

    _ ->
      toDecoder tipe



-- DECODE


toDecoder :: Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    Can.TLambda _ _ ->
      error "functions should not be allowed through input ports"

    Can.TVar _ ->
      error "type variables should not be allowed through input ports"

    Can.TAlias _ _ args alias ->
      toDecoder (Type.dealias args alias)

    Can.TUnit ->
      decodeTuple0

    Can.TTuple a b c ->
      decodeTuple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == Name.float  -> decode "float"
          | name == Name.int    -> decode "int"
          | name == Name.bool   -> decode "bool"
          | name == Name.string -> decode "string"
          | name == Name.value  -> decode "value"

        [arg]
          | name == Name.maybe -> decodeMaybe arg
          | name == Name.list  -> decodeList arg
          | name == Name.array -> decodeArray arg

        _ ->
          error "toDecoder: bad type"

    Can.TRecord _ (Just _) ->
      error "toDecoder: bad record"

    Can.TRecord fields Nothing ->
      decodeRecord fields



-- DECODE MAYBE


decodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do  nothing <- Names.registerGlobal A.zero ModuleName.maybe "Nothing"
      just    <- Names.registerGlobal A.zero ModuleName.maybe "Just"

      oneOf <- decode "oneOf"
      null  <- decode "null"
      map_  <- decode "map"

      subDecoder <- toDecoder tipe

      return $
        Opt.Call A.zero oneOf
          [ Opt.List A.zero
              [ Opt.Call A.zero null [ nothing ]
              , Opt.Call A.zero map_ [ just, subDecoder ]
              ]
          ]


-- DECODE LIST


decodeList :: Can.Type -> Names.Tracker Opt.Expr
decodeList tipe =
  do  list <- decode "list"
      decoder <- toDecoder tipe
      return $ Opt.Call A.zero list [ decoder ]



-- DECODE ARRAY


decodeArray :: Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  do  array <- decode "array"
      decoder <- toDecoder tipe
      return $ Opt.Call A.zero array [ decoder ]



-- DECODE TUPLES


decodeTuple0 :: Names.Tracker Opt.Expr
decodeTuple0 =
  do  null <- decode "null"
      return (Opt.Call A.zero null [ Opt.Unit ])


decodeTuple :: Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
decodeTuple a b maybeC =
  do  succeed <- decode "succeed"
      case maybeC of
        Nothing ->
          let tuple = Opt.Tuple (toLocal 0) (toLocal 1) Nothing in
          indexAndThen 0 a =<<
            indexAndThen 1 b (Opt.Call A.zero succeed [tuple])

        Just c ->
          let tuple = Opt.Tuple (toLocal 0) (toLocal 1) (Just (toLocal 2)) in
          indexAndThen 0 a =<<
            indexAndThen 1 b =<<
              indexAndThen 2 c (Opt.Call A.zero succeed [tuple])


toLocal :: Int -> Opt.Expr
toLocal index =
  Opt.VarLocal A.zero (Name.fromVarIndex index)


indexAndThen :: Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
  do  andThen <- decode "andThen"
      index <- decode "index"
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call A.zero andThen
          [ Opt.Function [A.At A.zero (Name.fromVarIndex i)] decoder
          , Opt.Call A.zero index [ Opt.Int A.zero i, typeDecoder ]
          ]



-- DECODE RECORDS


decodeRecord :: Map.Map Name.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
  let
    toFieldExpr name _ =
      Opt.VarLocal A.zero name

    record =
      Opt.Record A.zero (Map.mapKeys (A.At A.zero) (Map.mapWithKey toFieldExpr fields))
  in
    do  succeed <- decode "succeed"
        foldM fieldAndThen (Opt.Call A.zero succeed [record]) =<<
          Names.registerFieldDict fields (Map.toList fields)


fieldAndThen :: Opt.Expr -> (Name.Name, Can.FieldType) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, Can.FieldType _ tipe) =
  do  andThen <- decode "andThen"
      field <- decode "field"
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call A.zero andThen
          [ Opt.Function [A.At A.zero key] decoder
          , Opt.Call A.zero field [ Opt.Str A.zero (Name.toElmString key), typeDecoder ]
          ]



-- GLOBALS HELPERS


encode :: Name.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal A.zero ModuleName.jsonEncode name


decode :: Name.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal A.zero ModuleName.jsonDecode name
