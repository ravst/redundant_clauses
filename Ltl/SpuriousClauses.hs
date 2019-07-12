module Ltl.SpuriousClauses where

import Ltl.Types
import Ltl.Checker
import Control.Monad

data ClausePart = CRight | CLeft
    deriving (Show, Eq)

data SkippedClauseInfo = SkippedClauseInfo {
    parentClause :: LTLFormula,
    clausePart :: ClausePart}
    deriving (Show, Eq)

extractPossibilities :: LTLFormula -> [(SkippedClauseInfo, LTLFormula)]
extractPossibilities f@(LBoolConst _ _) = []
extractPossibilities f@(LProposition _ _) = []
extractPossibilities (LUnOp op pos subf) = (\x -> LUnOp op pos <$> x) <$> extractPossibilities subf
extractPossibilities f@(LBinOp op pos suba subb) = concat [
    (\subb -> LBinOp op pos suba <$> subb) <$> extractPossibilities subb,
    (\suba -> flip (LBinOp op pos) subb <$> suba) <$> extractPossibilities suba,
    [(SkippedClauseInfo f CLeft, subb)],
    [(SkippedClauseInfo f CRight, suba)]]

getSpuriousClauses :: LTLFormula -> IO [(SkippedClauseInfo, LTLFormula)]
getSpuriousClauses formula = spuriousClauses
  where
    isSpurious (_, withSkipped) = checkEquiv formula withSkipped 
    spuriousClauses = filterM isSpurious $ extractPossibilities formula




