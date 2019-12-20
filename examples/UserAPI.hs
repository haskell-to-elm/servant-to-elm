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
import Language.Haskell.To.Elm
import Servant.To.Elm

data User = User
  { name :: Text
  , age :: Int
  } deriving (Generic, Aeson.ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType User where
  elmDefinition =
    Just $ deriveElmTypeDefinition @User defaultOptions "Api.User.User"

instance HasElmDecoder Aeson.Value User where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @User defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoder Aeson.Value User where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @User defaultOptions Aeson.defaultOptions "Api.User.encoder"

type UserAPI
  = "user" :> Get '[JSON] User
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

main :: IO ()
main = do
  let
    definitions =
      map (elmEndpointDefinition "Config.urlBase" ["Api"]) (elmEndpoints @UserAPI)
      <> jsonDefinitions @User

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
