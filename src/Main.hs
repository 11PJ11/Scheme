module Main where

import           Scheme
import           System.Environment

main:: IO ()
main = getArgs >>= print . eval . readExpr . head
    -- do
    --   (expr:_) <- getArgs
    --   print $ eval $ readExpr $ head expr
