module Lib
    ( someFunc
    ) where

import Text.Parsec
import Ltl.Parser
import Ltl.Checker
import Ltl.SpuriousClauses
import Ltl.HtmlPresentation
import Control.Monad
import System.Environment



someFunc :: IO ()
someFunc = do 
    (formula:_) <- getArgs
    case parse ltlParser "" formula of
        Left e -> print e 
        Right e -> do
            spurious <- getSpuriousClauses e
            putStrLn $ prepareHtmlOutput e spurious 
        --Right e -> print $ extractPossibilities e
        --Right e -> print =<< smvIsTautology e
