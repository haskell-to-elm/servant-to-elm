{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString as BS
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Prettyprint.Doc as Pretty
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process (callCommand)
import Test.Hspec

import qualified TestExampleServer

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  describe "servant-to-elm" $ do
    it "elmEndpointDefinition" $
      withTempDir (testElmClient TestExampleServer.definitionModules)
    it "elmEndpointRequestInfo" $
      withTempDir (testElmClient TestExampleServer.requestInfoModules)
  where
    withTempDir =
      withSystemTempDirectory
        "servant-to-elm" -- temp dir name template

testElmClient :: HashMap [Text] (Pretty.Doc ann) -> FilePath -> IO ()
testElmClient elmModules tempDir = do
  let srcDir = tempDir </> "src"
  createDirectory srcDir
  createDirectory (srcDir </> "Api")

  for_ (List.sortOn fst (HashMap.toList elmModules)) $ \(moduleName, content) ->
    writeElmModule srcDir moduleName content

  copyFile ("test" </> "elm.json") (tempDir </> "elm.json")

  copyFile ("test" </> "Config.elm") (srcDir </> "Config.elm")

  compileGeneratedElm
  where
    compileGeneratedElm :: IO ()
    compileGeneratedElm =
      withCurrentDirectory tempDir $
        callCommand "elm make src/**.elm"

writeElmModule :: FilePath -> [Text] -> Pretty.Doc ann -> IO ()
writeElmModule srcDir moduleName content =
  BS.writeFile
    path
    (encodeUtf8 (Text.pack (show content)))
  where
    path :: FilePath
    path =
      foldl' (</>) mempty (srcDir : fmap Text.unpack moduleName)
        <> ".elm"
