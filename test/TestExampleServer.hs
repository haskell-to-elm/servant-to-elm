{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
module TestExampleServer where

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as Pretty
import GHC.Generics
import qualified Generics.SOP as SOP
import qualified Language.Elm.Definition as Elm (Definition)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Servant.API
import Servant.To.Elm

import Language.Haskell.To.Elm

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
  :<|> "user" :> ReqBody '[JSON] User :> PostNoContent '[JSON] NoContent

definitionModules :: HashMap [Text] (Pretty.Doc ann)
definitionModules =
  Pretty.modules (Simplification.simplifyDefinition <$> definitions)
  where
    definitions :: [Elm.Definition]
    definitions =
      map (elmEndpointDefinition "Config.urlBase" ["Api"]) (elmEndpoints @UserAPI)
        <> jsonDefinitions @User

requestInfoModules :: HashMap [Text] (Pretty.Doc ann)
requestInfoModules =
  Pretty.modules (Simplification.simplifyDefinition <$> definitions)
  where
    definitions :: [Elm.Definition]
    definitions =
      map (elmEndpointRequestInfo ["Api"]) (elmEndpoints @UserAPI)
        <> jsonDefinitions @User
