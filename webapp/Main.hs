{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404, status405)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet
import Text.Lucius
import Data.Text (strip, pack, unpack)
import Data.Text.Lazy.Encoding
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import qualified Data.Map as Map

import Interpreter
 
main =
  do putStrLn $ "Listening on port " ++ show port
     run port app
       where port = 3000

app :: Application
app req respond = dispatch >>= respond
  where
    dispatch =
      case pathInfo req of
        [] -> index req
        ["res", "main.css"] -> return mainCss
        ["res", "bg.png"] -> return backgroundImage
        x -> return $ notFound (show x)

htmlCT = ("Content-Type", "text/html")

index :: Request -> IO Response
index req =
  case requestMethod req of
    "GET" -> return $ indexForm EmptyRequest
    "POST" -> indexResponse req
    m -> return $ invalidMethod (show m)

indexForm :: InterpreterRequest -> Response
indexForm InterpreterRequest
  {
    context = c@Context { evalStepLimit = stepLimit },
    input = inputLines
  } =
  let output = evalLines c (filter (not . (== ""))
                            (map (unpack . strip . pack) inputLines)) in
    responseBuilder status200 [htmlCT] $
    renderHtmlBuilder $(shamletFile "webapp/templates/index.hamlet")
indexForm EmptyRequest = indexForm emptyRequest

indexResponse :: Request -> IO Response
indexResponse req =
  do (params, _) <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
     let params' = Map.fromList params in
       return $ indexForm $ parseInterpreterRequest params'

notFound :: String -> Response
notFound x = responseBuilder status404 [htmlCT] $
  renderHtmlBuilder $(shamletFile "webapp/templates/404.hamlet")

invalidMethod :: String -> Response
invalidMethod x = responseBuilder status405 [htmlCT] $
  renderHtmlBuilder $(shamletFile "webapp/templates/405.hamlet")

cssCT = ("Content-Type", "text/css")

mainCss :: Response
mainCss = responseLBS status200 [cssCT] $
  encodeUtf8 $ renderCss ($(luciusFile "webapp/templates/main.css") renderRes)

pngCT = ("Content-Type", "image/png")

backgroundImage :: Response
backgroundImage = responseLBS status200 [pngCT] $
  fromStrict $(embedFile "webapp/templates/bg.png")

parseInterpreterRequest :: Map.Map ByteString ByteString -> InterpreterRequest
parseInterpreterRequest req =
  (InterpreterRequest
   (webContext
    {
      evalStepLimit =
        max 1 $
        min maxEvalStepLimit (
        (fst . head . reads)
          (maybe
           (show $ evalStepLimit webContext) toString $ Map.lookup "stepLimit" req)),
      strategy = chooseStrategy $ maybe "default" toString $ Map.lookup "strategy" req
    })
   (lines $ maybe "" toString $ Map.lookup "src" req))

instance ToMarkup InterpreterResponse where
  toMarkup (InterpreterResponse (Left error)) =
    $(shamletFile "webapp/templates/error.hamlet")
  toMarkup (InterpreterResponse (Right trace)) =
    let latex = render latexRendererSpec trace in
      $(shamletFile "webapp/templates/success.hamlet")

data Resource = BackgroundImage

renderRes :: Render Resource
renderRes BackgroundImage _ = "/res/bg.png"
