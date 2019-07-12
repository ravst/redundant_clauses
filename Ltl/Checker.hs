module Ltl.Checker where

import Data.HashSet
import Ltl.Types
import System.IO.Temp
import System.IO
import System.Process
import Control.Monad
import Data.List (isInfixOf)
import Config

getVariables :: LTLFormula -> HashSet String 
getVariables (LBoolConst _ _)  = empty
getVariables (LProposition _ p) = singleton p
getVariables (LUnOp _ _ f) = getVariables f
getVariables (LBinOp _ _ a b) = union (getVariables a) (getVariables b)

smvOpening :: String -> String
smvOpening name = "MODULE " ++ name

smvVars :: [String] -> [String]
smvVars names = "VAR" : [name ++ ": boolean;" | name <- names]

smvFormula :: LTLFormula -> String
smvFormula f = "LTLSPEC " ++ (show f)

smvFile :: LTLFormula -> String
smvFile formula = unlines $ concat [
    [smvOpening "main"],
    smvVars $ toList $ getVariables formula,
    [smvFormula formula]] 


runSMV :: LTLFormula -> IO String
runSMV formula = withTempFile temporaryDir "model.smv" $ \file_name -> \file_handle -> do
    hPutStr file_handle $ smvFile formula
    hClose file_handle
    readCreateProcess ((proc smvBinary [file_name]) {std_err=NoStream}) ""

getSMVResult :: String -> Bool
getSMVResult output = isInfixOf "is true" output

smvIsTautology :: LTLFormula -> IO Bool
smvIsTautology formula = getSMVResult <$> runSMV formula

checkEquiv :: LTLFormula -> LTLFormula -> IO Bool
checkEquiv f g = smvIsTautology (LBinOp LEquiv emptyPos f g)
