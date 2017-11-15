  -- import Jarvis
--
--  gameLoop :: Game -> IO()
--  gameLoop game = do
--    col <- getLine
--    let column = read col :: Int
--    let newGame = addPawn game column
--    putStrLn $displayGame newGame
--    if gameComplete $pawns newGame
--       then putStrLn ((show $player newGame) ++ " won!")
--       else gameLoop newGame
--
{-# LANGUAGE OverloadedStrings #-} -- we're using Text below
{-# LANGUAGE QuasiQuotes #-}
module Main where
  import Game
  import Yesod
  import Text.Hamlet (HtmlUrl, hamlet)
  import Data.Text (Text)
  import Text.Blaze.Html.Renderer.String (renderHtml)

  data MyRoute = Home | Time | Stylesheet

  render :: MyRoute -> [(Text, Text)] -> Text
  render Home _ = "/home"
  render Time _ = "/time"
  render Stylesheet _ = "/style.css"

  template :: Text -> HtmlUrl MyRoute
  template title = [hamlet|
  $doctype 5
  <html>
      <head>
          <title>#{title}
          <link rel=stylesheet href=@{Stylesheet}>
      <body>
          <h1>#{title}
  |]

  data HelloWorld = HelloWorld

  mkYesod "HelloWorld" [parseRoutes|
  / HomeR GET
  |]

  instance Yesod HelloWorld
  getHomeR :: Handler Html
  getHomeR = defaultLayout [whamlet|#{getPage}|]
  getPage = renderHtml $ template "My Title" render
  main = warp 3000 HelloWorld
