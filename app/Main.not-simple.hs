
module Main where

import Control.Monad    (msum)
import Data.Char        (toLower)
import Happstack.Server ( FromReqURI(..), simpleHTTP, nullConf
                        , seeOther, path, ok, dir
                        )

data Subject = World | Haskell
sayHello :: Subject -> String
sayHello World   = "Hello, World!"
sayHello Haskell = "Hello, Haskell!"

instance FromReqURI Subject where
  fromReqURI sub =
    case map toLower sub of
      "haskell" -> Just Haskell
      "world"   -> Just World
      _         -> Nothing

main :: IO ()
main = do
  putStrLn "running server"
  simpleHTTP nullConf $
    msum [ dir "hello" $ path $ \subject -> ok $ sayHello subject
         , seeOther "/hello/Haskell" "/hello/Haskell"
         ]

