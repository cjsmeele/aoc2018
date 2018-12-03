module Common (module Common
              ,module Polyglot
              ,(&&&)) where

import Control.Arrow
import Polyglot
import System.Environment

withParsed p name f
  = do args <- getArgs
       let name' = case args of
                     [x] -> x
                     _   -> "input/" ++ name
       parsed <- parse p <$> readFile name'
       case parsed of
         Just xs -> print (f xs)
         Nothing -> error "could not parse input"
