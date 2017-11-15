 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE OverloadedStrings     #-}
 {-# LANGUAGE QuasiQuotes           #-}
 {-# LANGUAGE TemplateHaskell       #-}
 {-# LANGUAGE TypeFamilies          #-}
 import           Control.Applicative ((<$>), (<*>))
 import           Data.Text           (Text)
 import           Data.Time           (Day)
 import           Yesod
 import           Yesod.Form.Jquery
 import           Puissance4.Game


 data App = App

 mkYesod "App" [parseRoutes|
 / HomeR GET
 /person PersonR POST
 |]

 instance Yesod App

 -- Tells our application to use the standard English messages.
 -- If you want i18n, then you can supply a translating function instead.
 instance RenderMessage App FormMessage where
     renderMessage _ _ = defaultFormMessage

 -- And tell us where to find the jQuery libraries. We'll just use the defaults,
 -- which point to the Google CDN.
 instance YesodJquery App

 -- The datatype we wish to receive from the form
 data Column = Column { age :: Int}
   deriving Show

 columnForm :: Html -> MForm Handler (FormResult Column, Widget)
 columnForm = renderDivs $ Column
     <$> areq intField "Age" Nothing
 -- The GET handler displays the form
 getHomeR :: Handler Html
 getHomeR = defaultLayout $ do
     setTitle "Puissance 4"
     toWidget [lucius| h1 { color: green; } |]
     addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
     toWidget [julius|
      var x = 7;
      var y = 6;
      var game = #{toJSON $addPawn (addPawn initGame 3) 4};
      $(function() { //on load
          printBoard(x, y, game);
      });

      function printBoard(i_x, i_y, game) {
          var maxRow = parseInt(i_y);
          var maxCol = parseInt(i_x);
          var pawns = game['pawns'];
          $("#board").append("<table oncontextmenu=\"return false\">");

          function findPiece(y, x, pieces){
             for(var i = 0 ; i < pieces.length ; i++){
               if(pieces[i][1] == x && pieces[i][2] == y){
                 return pieces[i][0]['pawn'][0];
               }
             }
             return '_';
          }
           for(var row = maxRow-1; row >= 0 ; row--) {
                  $("#board").append("<tr>");
                   for(var col = 0; col < maxCol ; col++) {
                       $("#board").append("<td> |"+ findPiece(row, col, pawns) +"| </td>");
                 }
                 $("#board").append("</tr>");
            }
             $("#board").append("</table>");
     } |]
     toWidgetHead
         [hamlet|
         $doctype 5
         <head>
           <title>TODO supply a title
           <meta charset="UTF-8">
           <meta name="viewport" content="width=device-width, initial-scale=1.0">
           <link rel='stylesheet' type='text/css' href='css/board.css' >
           <script type="text/javascript" src="jquery-2.1.1.min.js">
           <script type="text/javascript" src="Scripts/board.js">
         <body>
             <p> <center><h3><font size="20" color="black"> Board Game
             <div>
             <div id="board">
                 <div class="cell">
           |]
      -- Generate the form to be displayed

 postPersonR :: Handler Html
 postPersonR = do
     ((result, widget), enctype) <- runFormPost columnForm
     case result of
         FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
         _ -> defaultLayout
             [whamlet|
                 <p>Invalid input, let's try again.
                 <form method=post action=@{PersonR} enctype=#{enctype}>
                     ^{widget}
                     <button>Submit
             |]

 main :: IO ()
 main = warp 3000 App

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE TypeFamilies      #-}
--
-- module Main where
-- import           Control.Applicative ((<$>), (<*>))
-- import           Data.Text (Text)
-- import           Yesod
-- import           Yesod.Form.Jquery
--
-- data App = App
--
-- mkYesod "App" [parseRoutes|
-- / HomeR GET
-- /column IntR POST|]
--
-- instance Yesod App
-- instance RenderMessage App FormMessage where
--     renderMessage _ _ = defaultFormMessage
-- instance YesodJquery App
--
-- columnForm :: Html -> MForm Handler (FormResult Int, Widget)
-- columnForm = renderDivs $ Int
--   <$> areq intField "Int" Nothing
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
-- postIntR :: Handler Html
-- postIntR = do
--     ((result, widget), enctype) <- runFormPost columnForm
--     case result of
--         FormSuccess column -> defaultLayout [whamlet|<p>#{show column}|]
--         _ -> defaultLayout
--             [whamlet|
--                 <p>Invalid input, let's try again.
--                 <form method=post action=@{IntR} enctype=#{enctype}>
--                     ^{widget}
--                     <button>Submit
--             |]
--
-- main :: IO ()
-- main = warp 3000 App
--
