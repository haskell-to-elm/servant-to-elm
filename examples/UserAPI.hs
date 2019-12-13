{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module UserAPI where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Generics.SOP as SOP
import Servant.API

import qualified Language.Elm.Pretty as Pretty
import qualified Servant.Foreign.Bidirectional as Servant
import Language.Haskell.To.Elm
import Servant.Elm.Bidirectional

data User = User
  { name :: Text
  , age :: Int
  } deriving
    ( Generic
    , Aeson.ToJSON
    , SOP.Generic
    , SOP.HasDatatypeInfo
    , HasElmDecoder Aeson.Value
    , HasElmEncoder Aeson.Value
    , HasElmType
    )

instance HasElmDefinition User where
  elmDefinition =
    deriveElmTypeDefinition @User defaultOptions "Api.User.User"

instance HasElmDecoderDefinition Aeson.Value User where
  elmDecoderDefinition =
    deriveElmJSONDecoder @User defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoderDefinition Aeson.Value User where
  elmEncoderDefinition =
    deriveElmJSONEncoder @User defaultOptions Aeson.defaultOptions "Api.User.encoder"

type UserAPI
  = "user" :> Get '[JSON] User
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

main :: IO ()
main = do
  let
    endpoints :: [Servant.Request ElmEncoder ElmDecoder]
    endpoints =
      Servant.listFromAPI
        (Proxy :: Proxy Elm)
        (Proxy :: Proxy ElmEncoder)
        (Proxy :: Proxy ElmDecoder)
        (Proxy :: Proxy UserAPI)

    definitions =
      map (elmRequest "Config.urlBase" ["Api"]) endpoints
      <> jsonDefinitions @User

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
