{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Control.Applicative ((<$>), optional)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat)
import Data.Text.Lazy (unpack)
import Data.ByteString.UTF8 (fromString)
import Happstack.Lite
--import Happstack.Server.FileServe.BuildingBlocks
import Happstack.Server.Monads (require)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, onclick)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Database.PostgreSQL.Simple

trim = dropWhileEnd isSpace . dropWhile isSpace

main :: IO ()
main = do
  putStrLn "starting server"
  connectionString <- readFile "connection_string.txt"
  conn <- connectPostgreSQL . fromString . trim $ connectionString
  xs <- query_ conn "select * from animals"
  forM_ xs $ \(name,spec) ->
    putStrLn $ name ++ ": " ++ spec
  serve Nothing $ myApp conn

myApp :: Connection -> ServerPart Response
myApp conn = msum
  [ dir "echo"   $ echo
  , dir "form"   $ formPage conn
  , dir "delete" $ deletePage conn
  , dir "react-app" $ serveFile (guessContentTypeM mimeTypes) "frontend/public/index.html"
  , homePage conn
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      css
      H.title . toHtml $ title
    H.body $ do
      H.table $ do
        H.tr $ do
          H.td $ a ! href "/" $ "home"
          H.td $ a ! href "/form" $ "create animal"
          H.td $ a ! href "/react-app" $ "react app"
      body

insert_animal :: Connection -> String -> String -> IO (Maybe ())
insert_animal conn name species = do
  execute conn "insert into animals (name, species) values (?, ?)"
               (name, species)
  return $ Just ()

delete_animal :: Connection -> String -> String -> IO (Maybe ())
delete_animal conn name spec = do
  execute conn "delete from animals where name = ? and species = ?"
                (name, spec)
  return $ Just ()

query_animals :: Connection -> IO (Maybe [(String, String)])
query_animals conn = do
  animals <- query_ conn "select * from animals"
  return . Just $ animals

formPage :: Connection -> ServerPart Response
formPage conn = msum [ viewForm, processForm conn ]
  where
    viewForm :: ServerPart Response
    viewForm = do
        method GET
        ok $ template "create animal" $
          form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
            label ! A.for "name" $ "name"
            input ! type_ "text" ! A.id "name" ! name "name"
            label ! A.for "species" $ "species"
            input ! type_ "text" ! A.id "species" ! name "species"
            input ! type_ "submit" ! value "Create"

    processForm :: Connection -> ServerPart Response
    processForm conn = do
        method POST
        name <- lookText "name"
        species <- lookText "species"
        require (insert_animal conn (unpack name) (unpack species)) $ \_ -> return ()
        ok $ template "Success!" $ do
          H.p "Created new animal:"
          H.p . toHtml $ unpack name ++ ", " ++ unpack species

homePage :: Connection -> ServerPart Response
homePage conn = 
  require (query_animals conn) $ \animals ->
    ok $ template "home page" $ do
      H.table ! A.id "animal-table" $ do
        H.thead $ do
          H.th ! A.colspan "3" $ "Animals"
          H.tr $ do
            H.th $ "name"
            H.th $ "species"
            H.th $ ""
        H.tbody $ do
          forM_ animals $ \(name,spec) ->
            H.tr $ do
              H.td . toHtml $ name
              H.td . toHtml $ spec
              H.td $ a ! href (H.stringValue $ "/delete?name=" ++ name ++ "&spec=" ++ spec)
                       ! onclick (H.stringValue $ "return confirm('really delete " ++ name ++ "?')") $ "delete"

deletePage :: Connection -> ServerPart Response
deletePage conn = do
  name <- lookText "name"
  spec <- lookText "spec"
  require (delete_animal conn (unpack name) (unpack spec)) return
  ok $ template "delete" $ do
    H.p . H.string $ "Deleted " ++ unpack name ++ " the " ++ unpack spec
  
echo :: ServerPart Response
echo =
  path $ \(msg :: String) ->
    ok $ template "echo" $ do
      p $ "echo says: " >> toHtml msg
      p "Change the url to echo something else."

css :: Html
css =
 let s = Data.Text.concat
      [ "body { color: #555; padding: 0; margin: 0; margin-left: 1em;}"
      , "ul { list-style-type: none; }"
      , "ol { list-style-type: none; }"
      , "h1 { font-size: 1.5em; color: #555; margin: 0; }"
      , ".author { color: #aaa; }"
      , ".date { color: #aaa; }"
      , ".tags { color: #aaa; }"
      , ".post { border-bottom: 1px dotted #aaa; margin-top: 1em; }"
      , ".bdy  { color: #555; margin-top: 1em; }"
      , ".post-footer { margin-top: 1em; margin-bottom: 1em; }"
      , "label { display: inline-block; width: 3em; }"
      , "#menu { margin: 0; padding: 0; margin-left: -1em;"
      ,         "border-bottom: 1px solid #aaa; }"
      , "#menu li { display: inline; margin-left: 1em; }"
      , "#menu form { display: inline; margin-left: 1em; }"
      , "#animal-table, #animal-table th, #animal-table td { border: 1px solid #aaa; }"
      , "td { min-width: 100px; }"
      ]
  in H.style ! A.type_ "text/css" $ H.toHtml s
