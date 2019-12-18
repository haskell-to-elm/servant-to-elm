# servant-elm-bidirectional

This is a library for generating Elm client libraries from Servant API
definitions.

See [haskell-to-elm](https://github.com/folq/haskell-to-elm) for background
information and a more elaborate motivation.

## Basic usage

Given a Servant API like the following

```haskell
type UserAPI
  = "user" :> Get '[JSON] User
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] NoContent
```

we can generate the Elm code for making requests against it as follows:

```haskell
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
```

Running `main` prints:

```elm
module Api exposing (..)

import Api.User
import Config
import Http
import Json.Decode
import NoContent


getUser : Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) Api.User.User)
getUser =
    Http.request { method = "GET"
    , headers = []
    , url = String.join "/" [Config.urlBase, "user"]
    , body = Http.emptyBody
    , expect = Http.expectStringResponse identity (\a -> case a of
        Http.BadUrl_ b ->
            Err (Http.BadUrl b , Nothing)

        Http.Timeout_ ->
            Err (Http.Timeout , Nothing)

        Http.NetworkError_ ->
            Err (Http.NetworkError , Nothing)

        Http.BadStatus_ b c ->
            Err (Http.BadStatus b.statusCode , Just { metadata = b, body = c })

        Http.GoodStatus_ b c ->
            Result.mapError (\d -> (Http.BadBody (Json.Decode.errorToString d) , Just { metadata = b
            , body = c })) (Json.Decode.decodeString Api.User.decoder c))
    , timeout = Nothing
    , tracker = Nothing }


postUser : Api.User.User -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) NoContent.NoContent)
postUser a =
    Http.request { method = "POST"
    , headers = []
    , url = String.join "/" [Config.urlBase, "user"]
    , body = Http.jsonBody (Api.User.encoder a)
    , expect = Http.expectStringResponse identity (\b -> case b of
        Http.BadUrl_ c ->
            Err (Http.BadUrl c , Nothing)

        Http.Timeout_ ->
            Err (Http.Timeout , Nothing)

        Http.NetworkError_ ->
            Err (Http.NetworkError , Nothing)

        Http.BadStatus_ c d ->
            Err (Http.BadStatus c.statusCode , Just { metadata = c, body = d })

        Http.GoodStatus_ c d ->
            if d == "" then
                Ok NoContent.NoContent

            else
                Err (Http.BadBody "Expected the response body to be empty" , Just { metadata = c
                , body = d }))
    , timeout = Nothing
    , tracker = Nothing }
module Api.User exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias User =
    { name : String, age : Int }


encoder : User -> Json.Encode.Value
encoder a =
    Json.Encode.object [ ("name" , Json.Encode.string a.name)
    , ("age" , Json.Encode.int a.age) ]


decoder : Json.Decode.Decoder User
decoder =
    Json.Decode.succeed User |>
    Json.Decode.Pipeline.required "name" Json.Decode.string |>
    Json.Decode.Pipeline.required "age" Json.Decode.int
```

In an actual project we would be writing the code to disk instead of printing it.

See [this file](examples/UserAPI.hs) for the full code with imports.

## Related projects

Libraries that use or are used by servant-elm-bidirectional:
- [haskell-to-elm](https://github.com/folq/haskell-to-elm) generates Elm types and JSON encoders and decoders from Haskell types.
- [elm-syntax](https://github.com/folq/elm-syntax) defines Haskell ASTs for Elm's syntax, and lets us pretty-print it.
- [haskell-to-elm-test](https://github.com/folq/haskell-to-elm-test) does end-to-end testing of this library.

Others:
- [servant-elm](http://hackage.haskell.org/package/servant-elm)
