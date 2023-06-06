{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Map (fromList)
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

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

main :: IO ()
main = do
  putStrLn "starting server"
  connectionString <- readFile "../connection_string.txt"
  conn <- connectPostgreSQL . fromString . trim $ connectionString
  xs <- query_ conn "select * from animals"
  forM_ xs $ \(name,spec) ->
    putStrLn $ name ++ ": " ++ spec
  serve Nothing $ myApp conn

myApp :: Connection -> ServerPart Response
myApp conn = msum
  [ dir "echo"      $ echo
  , dir "math"      $ math
  , dir "form"      $ formPage conn
  , dir "delete"    $ deletePage conn
  , dir "static"    $ serveDirectory EnableBrowsing [] "../static"
  , dir "thing"     $ path $ \s -> ok $ template "thing" $ H.h1 (H.string s)
  , homePage conn
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! href "static/css/main.css"
      H.title . toHtml $ title
    H.body $ do
      H.table ! A.id "nav-bar" $ do
        H.tr $ do
          H.td $ a ! href "/"      $ "home"
          H.td $ a ! href "/form"  $ "create animal"
          H.td $ a ! href "/math"  $ "math"
      H.div ! A.id "body-div" $ do
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
            H.div $ do
              label ! A.for "name" $ "name"
              input ! type_ "text" ! A.id "name" ! name "name"
            H.div $ do
              label ! A.for "species" $ "species"
              input ! type_ "text" ! A.id "species" ! name "species"
            H.div $ do
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

math :: ServerPart Response
math = do
  ok $ template "math" $ do
    H.div $ "Here is a diagram"
    H.div $ do
      H.img ! A.class_ "image" ! A.src "static/img/figure_1_5_b.png" ! A.alt "geometric diagram"

