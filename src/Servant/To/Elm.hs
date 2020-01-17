{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.To.Elm where

import Protolude hiding (Type, functionName, moduleName)

import qualified Bound
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import Servant.API ((:<|>), (:>))
import qualified Servant.API as Servant
import qualified Servant.API.Modifiers as Servant

import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pattern as Pattern
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type
import Language.Haskell.To.Elm

-- | Generate an Elm function for making a request to a Servant endpoint.
elmEndpointDefinition
  :: Expression Void -- ^ The URL base of the endpoint
  -> Name.Module -- ^ The module that the function should be generated into
  -> Endpoint -- ^ A description of the endpoint
  -> Definition
elmEndpointDefinition urlBase moduleName endpoint =
  Definition.Constant
    (Name.Qualified moduleName functionName)
    elmTypeSig
    (panic "expression not closed" <$> lambdaArgs argNames elmLambdaBody)
  where
    functionName =
      case _functionName endpoint of
        [] ->
          ""

        firstPart:rest ->
          firstPart <> foldMap capitalise rest

    capitalise s =
      case Text.uncons s of
        Nothing ->
          ""

        Just (c, s') ->
          Text.cons (Char.toUpper c) s'

    elmTypeSig :: Type Void
    elmTypeSig =
      Type.funs
        (concat
          [ [ _encodedType arg
            | (_, arg, _) <- _headers endpoint
            ]
          , [ _encodedType arg
            | Capture _ (_, arg) <- numberedPathSegments
            ]
          , [ case type_ of
                Required ->
                  vacuous $ _encodedType arg

                Optional ->
                  vacuous $ _encodedType arg

                Flag ->
                  vacuous $ _encodedType arg

                List ->
                  vacuous $ Type.App "List.List" $ _encodedType arg
            | (_, type_, arg) <- _queryString $ _url endpoint
            ]
          , [ _encodedType body
            | Just (_, body) <- [_body endpoint]
            ]
          ]
        )
        elmReturnType

    elmReturnType =
      let
        type_ =
          maybe "Basics.()" _decodedType $ _returnType endpoint
      in
      Type.App
        "Cmd.Cmd"
        (Type.apps
          "Result.Result"
          [Type.tuple "Http.Error" (Type.App "Maybe.Maybe" $ Type.Record [("metadata", "Http.Metadata"), ("body", "String.String")]), type_]
        )

    elmReturnDecoder =
      case _returnType endpoint of
        Nothing ->
          panic "elmRequest: No return type" -- TODO?

        Just ret ->
          vacuous $ _decoder ret

    numberedPathSegments =
      go 0 $ _path $ _url endpoint
      where
        go !i segments =
          case segments of
            [] ->
              []

            Static p:segments' ->
              Static p : go i segments'

            Capture str arg:segments' ->
              Capture str (i, arg) : go (i + 1) segments'

    argNames =
      concat
      [ [ headerArgName i
        | (i, _) <- zip [0..] $ _headers endpoint
        ]
      , [ capturedArgName i
        | Capture _ (i, _) <- numberedPathSegments
        ]
      , [ paramArgName i
        | (i, _) <- zip [0..] $ _queryString $ _url endpoint
        ]
      , [ bodyArgName
        | Just _ <- [_body endpoint]
        ]
      ]

    lambdaArgs :: [Text] -> Expression Text -> Expression Text
    lambdaArgs args rhs =
      case args of
        [] ->
          rhs

        arg:args' ->
          Expression.Lam $ Bound.abstract1 arg $ lambdaArgs args' rhs

    elmLambdaBody :: Expression Text
    elmLambdaBody =
      Expression.App
        "Http.request"
        (Expression.Record
          [ ("method", Expression.String $ toS $ _method endpoint)
          , ("headers", elmHeaders)
          , ("url", elmUrl)
          , ("body", elmBody)
          , ("expect", elmExpect)
          , ("timeout", "Maybe.Nothing")
          , ("tracker", "Maybe.Nothing")
          ]
        )

    elmParams =
      [ case type_ of
        Required ->
          Expression.List
            [Expression.String (name <> "=") Expression.++ encode (pure $ paramArgName i)]

        Optional ->
          Expression.apps
            "Maybe.Extra.unwrap"
            [ Expression.List []
            , "List.singleton" Expression.<< Expression.App "Basics.++" (Expression.String $ name <> "=")
            , encode $ pure $ paramArgName i
            ]

        Flag ->
          Expression.If
            (pure $ paramArgName i)
            (Expression.List [Expression.String name])
            (Expression.List [])

        List ->
          Expression.apps
            "List.map"
            [ Expression.App "Basics.++" (Expression.String (name <> "[]=")) Expression.<< encoder
            , pure $ paramArgName i
            ]
      | (i, (name, type_, arg)) <- zip [0..] $ _queryString $ _url endpoint
      , let
          encoder =
            vacuous $ _encoder arg

          encode =
            Expression.App encoder
      ]

    elmUrl =
      case elmParams of
        [] ->
          withoutParams

        [elmParams'] ->
          withParams elmParams'

        _ ->
          withParams (Expression.App "List.concat" $ Expression.List elmParams)

      where
        withoutParams =
          Expression.apps
            "String.join"
            [ Expression.String "/"
            , Expression.List $ vacuous urlBase : fmap elmPathSegment numberedPathSegments
            ]

        withParams params =
          withoutParams Expression.++
            Expression.Case params
              [ (Pattern.List [], Bound.toScope $ Expression.String "")
              , ( Pattern.Var 0
                , Bound.toScope $ Expression.String "?" Expression.++ Expression.apps "String.join" [Expression.String "&", pure $ Bound.B 0]
                )
              ]


    elmHeaders =
      let
        headerDecoder i name arg  =
          Expression.apps
            "Http.header"
            [ Expression.String name
            , Expression.App
              (vacuous $ _encoder arg)
              (pure $ headerArgName i)
            ]

        optionalHeaderDecoder i name arg =
          Expression.apps
            "Maybe.map"
            [ Expression.App
              "Http.header"
              (Expression.String name)
            , Expression.App
              (vacuous $ _encoder arg)
              (pure $ headerArgName i)
            ]
      in
      case _headers endpoint of
        [] ->
          Expression.List []

        _
          | all (\(_, _, required) -> required) (_headers endpoint) ->
          Expression.List
            [ headerDecoder i name arg
            | (i, (name, arg, _)) <- zip [0..] $ _headers endpoint
            ]

        _ ->
          Expression.apps "List.filterMap"
          [ "Basics.identity"
          , Expression.List
              [ if required then
                  Expression.App "Maybe.Just" $ headerDecoder i name arg

                else
                  optionalHeaderDecoder i name arg
              | (i, (name, arg, required)) <- zip [0..] $ _headers endpoint
              ]
          ]


    elmBody =
      case _body endpoint of
        Nothing ->
          "Http.emptyBody"

        Just (bodyType, body) ->
          Expression.App
            (vacuous bodyType)
            (Expression.App (vacuous $ _encoder body) $ pure bodyArgName)

    elmExpect =
      Expression.apps
        "Http.expectStringResponse"
        [ "Basics.identity"
        , Expression.Lam $ Bound.toScope $
            Expression.Case (pure $ Bound.B ())
            [ ( Pattern.Con "Http.BadUrl_" [Pattern.Var 0]
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple (Expression.App "Http.BadUrl" $ pure (Bound.B 0)) "Maybe.Nothing"
              )
            , ( Pattern.Con "Http.Timeout_" []
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple "Http.Timeout" "Maybe.Nothing"
              )
            , ( Pattern.Con "Http.NetworkError_" []
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple "Http.NetworkError" "Maybe.Nothing"
              )
            , ( Pattern.Con "Http.BadStatus_" [Pattern.Var 0, Pattern.Var 1]
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple
                  (Expression.App "Http.BadStatus" (Expression.App (Expression.Proj "statusCode") $ pure $ Bound.B 0))
                  (Expression.App "Maybe.Just" $ Expression.Record [("metadata", pure $ Bound.B 0), ("body", pure $ Bound.B 1)])
              )
            , ( Pattern.Con "Http.GoodStatus_" [Pattern.Var 0, Pattern.Var 1]
              , Bound.toScope $
                if fmap _decodedType (_returnType endpoint) == Just (elmType @Servant.NoContent) then
                  Expression.If (Expression.apps ("Basics.==") [pure $ Bound.B 1, Expression.String ""])
                    (Expression.App "Result.Ok" "NoContent.NoContent")
                    (Expression.App "Result.Err" $
                      Expression.tuple
                        (Expression.App "Http.BadBody" $ Expression.String "Expected the response body to be empty")
                        (Expression.App "Maybe.Just" $ Expression.Record [("metadata", pure $ Bound.B 0), ("body", pure $ Bound.B 1)])
                    )

                else
                  Expression.apps "Result.mapError"
                    [ Expression.Lam $ Bound.toScope $
                      Expression.tuple
                        (Expression.App "Http.BadBody" $
                          Expression.App "Json.Decode.errorToString" $
                          pure $ Bound.B ()
                        )
                        (Expression.App "Maybe.Just" $ Expression.Record [("metadata", pure $ Bound.F $ Bound.B 0), ("body", pure $ Bound.F $ Bound.B 1)])
                    , Expression.apps "Json.Decode.decodeString" [elmReturnDecoder, pure $ Bound.B 1]
                    ]
              )
            ]
        ]

    elmPathSegment pathSegment =
      case pathSegment of
        Static s ->
          Expression.String s

        Capture _ (i, arg) ->
          Expression.App
            (vacuous $ _encoder arg)
            (pure $ capturedArgName i)

    bodyArgName :: Text
    bodyArgName =
      "body"

    headerArgName :: Int -> Text
    headerArgName i =
      "header" <> show i

    capturedArgName :: Int -> Text
    capturedArgName i =
      "capture" <> show i

    paramArgName :: Int -> Text
    paramArgName i =
      "param" <> show i

-------------------------------------------------------------------------------
-- * Endpoints

-- | @'HasElmEndpoints' api@ means that the Servant API @api@ can be converted
-- to a list of 'Endpoint's, which contains the information we need to generate
-- an Elm client library for the API.
class HasElmEndpoints api where
  elmEndpoints' :: Endpoint -> [Endpoint]

-- | Convert an API to a list of Elm endpoint descriptors, 'Endpoint'.
--
-- Usage: @'elmEndpoints' \@MyAPI@
elmEndpoints :: forall api. HasElmEndpoints api => [Endpoint]
elmEndpoints = elmEndpoints' @api Endpoint
  { _url = URL
    { _path = []
    , _queryString = []
    }
  , _method = "GET"
  , _headers = []
  , _body = Nothing
  , _returnType = Nothing
  , _functionName = []
  }

-- | Contains the information we need about an endpoint to generate an Elm
-- definition that calls it.
data Endpoint = Endpoint
  { _url :: URL
  , _method :: HTTP.Method
  , _headers :: [(Text, Encoder, Bool)]
  , _body :: Maybe (Expression Void, Encoder)
  , _returnType :: Maybe Decoder
  , _functionName :: [Text]
  }

data PathSegment e
  = Static Text
  | Capture Text e
  deriving (Show)

data QueryParamType
  = Required
  | Optional
  | Flag
  | List
  deriving (Show)

data URL = URL
  { _path :: [PathSegment Encoder]
  , _queryString :: [(Text, QueryParamType, Encoder)]
  }
  deriving (Show)

data Encoder = Encoder { _encoder :: Expression Void, _encodedType :: Type Void }
  deriving (Show)
data Decoder = Decoder { _decoder :: Expression Void, _decodedType :: Type Void }
  deriving (Show)

makeEncoder :: forall value a. HasElmEncoder value a => Encoder
makeEncoder = Encoder (elmEncoder @value @a) (elmType @a)

makeDecoder :: forall value a. HasElmDecoder value a => Decoder
makeDecoder = Decoder (elmDecoder @value @a) (elmType @a)

instance HasElmEndpoints Servant.EmptyAPI where
  elmEndpoints' _ = []

instance (HasElmEndpoints a, HasElmEndpoints b) => HasElmEndpoints (a :<|> b) where
  elmEndpoints' prefix =
    elmEndpoints' @a prefix <> elmEndpoints' @b prefix

instance (KnownSymbol symbol, HasElmEncoder Text a, HasElmEndpoints api)
  => HasElmEndpoints (Servant.Capture' mods symbol a :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _url = (_url prefix)
          { _path = _path (_url prefix) <> [Capture str $ makeEncoder @Text @a]
          }
        , _functionName = _functionName prefix <> ["by", str]
        }
      where
        str =
          toS $ symbolVal $ Proxy @symbol

instance (KnownSymbol symbol, HasElmEncoder Text a, HasElmEndpoints api)
  => HasElmEndpoints (Servant.CaptureAll symbol a :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _url = (_url prefix)
          { _path = _path (_url prefix) <> [Capture str $ makeEncoder @Text @a]
          }
        , _functionName = _functionName prefix <> ["by", str]
        }
      where
        str =
          toS $ symbolVal $ Proxy @symbol

instance (Servant.ReflectMethod method, HasElmDecoder Aeson.Value a, list ~ '[Servant.JSON])
  => HasElmEndpoints (Servant.Verb method status list a) where
    elmEndpoints' prefix =
      [ prefix
        { _method = method
        , _returnType = Just $ makeDecoder @Aeson.Value @a
        , _functionName = Text.toLower (toS method) : _functionName prefix
        }
      ]
      where
        method =
          Servant.reflectMethod $ Proxy @method

instance
  ( Servant.SBoolI (Servant.FoldRequired mods)
  , KnownSymbol symbol
  , HasElmEncoder (Servant.RequiredArgument mods Text) (Servant.RequiredArgument mods a)
  , HasElmEndpoints api
  ) => HasElmEndpoints (Servant.Header' mods symbol a :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _headers = _headers prefix <>
          [ ( toS $ symbolVal $ Proxy @symbol
            , makeEncoder @(Servant.RequiredArgument mods Text) @(Servant.RequiredArgument mods a)
            , case Servant.sbool @(Servant.FoldRequired mods) of
                Servant.STrue ->
                  True

                Servant.SFalse ->
                  False
            )
          ]
        }

instance
  ( Servant.SBoolI (Servant.FoldRequired mods)
  , KnownSymbol symbol
  , HasElmEncoder (Servant.RequiredArgument mods Text) (Servant.RequiredArgument mods a)
  , HasElmEndpoints api
  ) => HasElmEndpoints (Servant.QueryParam' mods symbol a :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _url = (_url prefix)
          { _queryString =
            _queryString (_url prefix) <>
            [ ( toS $ symbolVal $ Proxy @symbol
              , case Servant.sbool @(Servant.FoldRequired mods) of
                  Servant.STrue ->
                    Required

                  Servant.SFalse ->
                    Optional
              , makeEncoder @(Servant.RequiredArgument mods Text) @(Servant.RequiredArgument mods a)
              )
            ]
          }
        }

instance (KnownSymbol symbol, HasElmEncoder Text a, HasElmEndpoints api)
  => HasElmEndpoints (Servant.QueryParams symbol a :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _url = (_url prefix)
          { _queryString =
            _queryString (_url prefix) <>
            [ ( toS $ symbolVal $ Proxy @symbol
              , List
              , makeEncoder @Text @a
              )
            ]
          }
        }

instance (KnownSymbol symbol, HasElmEndpoints api)
  => HasElmEndpoints (Servant.QueryFlag symbol :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _url = (_url prefix)
          { _queryString =
            _queryString (_url prefix) <>
            [ ( toS $ symbolVal $ Proxy @symbol
              , Flag
              , Encoder "Basics.identity" "Basics.Bool"
              )
            ]
          }
        }

instance (HasElmEncoder Aeson.Value a, HasElmEndpoints api, list ~ '[Servant.JSON])
  => HasElmEndpoints (Servant.ReqBody' mods list a :> api) where
    elmEndpoints' prefix =
      elmEndpoints' @api prefix
        { _body = Just ("Http.jsonBody", makeEncoder @Aeson.Value @a)
        }

instance (KnownSymbol path, HasElmEndpoints api) => HasElmEndpoints (path :> api) where
  elmEndpoints' prefix =
    elmEndpoints' @api prefix
      { _url = (_url prefix)
        { _path = _path (_url prefix) <> [Static path]
        }
      , _functionName = _functionName prefix <> [path]
      }
    where
      path =
        toS $ symbolVal $ Proxy @path

instance HasElmEndpoints api => HasElmEndpoints (Servant.RemoteHost :> api) where
  elmEndpoints' = elmEndpoints' @api

instance HasElmEndpoints api => HasElmEndpoints (Servant.IsSecure :> api) where
  elmEndpoints' = elmEndpoints' @api

instance HasElmEndpoints api => HasElmEndpoints (Servant.Vault :> api) where
  elmEndpoints' = elmEndpoints' @api

instance HasElmEndpoints api => HasElmEndpoints (Servant.WithNamedContext name context api) where
  elmEndpoints' = elmEndpoints' @api

instance HasElmEndpoints api => HasElmEndpoints (Servant.HttpVersion :> api) where
  elmEndpoints' = elmEndpoints' @api

instance HasElmEndpoints api => HasElmEndpoints (Servant.Summary summary :> api) where
  elmEndpoints' = elmEndpoints' @api

instance HasElmEndpoints api => HasElmEndpoints (Servant.Description description :> api) where
  elmEndpoints' = elmEndpoints' @api

-------------------------------------------------------------------------------
-- Orphans

instance HasElmType Servant.NoContent where
  elmDefinition =
    Just $ Definition.Type "NoContent.NoContent" [("NoContent", [])]

instance HasElmDecoder Aeson.Value Servant.NoContent where
  elmDecoder =
    Expression.App "Json.Decode.succeed" "NoContent.NoContent"
