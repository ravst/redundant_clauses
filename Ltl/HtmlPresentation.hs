module Ltl.HtmlPresentation where

import Ltl.Types
import Ltl.SpuriousClauses

inRed :: String -> String
inRed x = "<font color='red'>" ++ x ++ "</font>"

getSpuriousClauseInContext :: SkippedClauseInfo -> LTLFormula -> String
getSpuriousClauseInContext _ (LBoolConst True _) = "TRUE"
getSpuriousClauseInContext _ (LBoolConst False _) = "FALSE"
getSpuriousClauseInContext _ (LProposition _ name) = name
getSpuriousClauseInContext info (LUnOp op _ f) =
    show op ++ " " ++ inParens (getSpuriousClauseInContext info f)
getSpuriousClauseInContext info f@(LBinOp op _ a b)
    | f /= parentClause info = inParens (getSpuriousClauseInContext info a) ++ " " ++ show op ++ " " ++ inParens (getSpuriousClauseInContext info b)
    | CRight == clausePart info =  inParens (getSpuriousClauseInContext info a) ++ " " ++ show op ++ " " ++ (inParens . inRed) (getSpuriousClauseInContext info b)
    | CLeft == clausePart info =  (inParens . inRed) (getSpuriousClauseInContext info a) ++ " " ++ show op ++ " " ++ inParens (getSpuriousClauseInContext info b)

makeSpuriousClauseRow :: LTLFormula -> (SkippedClauseInfo, LTLFormula) -> String
makeSpuriousClauseRow formula (skippedInfo, equiv) = unwords [
    "<tr><td>",
    getSpuriousClauseInContext skippedInfo formula,
    "</td><td>",
    show equiv,
    "</td></tr>"]

tableHeaderRow :: String
tableHeaderRow = unwords [
    "<tr><th>",
    "Redundant clause (marked in red)",
    "</th><th>",
    "Formula without the redundant clause",
    "</th></tr>"]

displaySpuriousClauses :: LTLFormula -> [(SkippedClauseInfo, LTLFormula)] -> String
displaySpuriousClauses formula infos = 
    unlines $ ["<table class=\"table\">", tableHeaderRow] ++ 
              [makeSpuriousClauseRow formula info | info <- infos] ++
              ["</table>"]

data AlertType = AlertSuccess | AlertWarning

instance Show AlertType where
  show AlertSuccess = "alert alert-success"
  show AlertWarning = "alert alert-warning"

asAlert :: AlertType -> String -> String
asAlert alertType content = unlines[
    unwords ["<div class=\"", show alertType, "\">"],
    content,
    "</div>"]



prepareHtmlOutput :: LTLFormula -> [(SkippedClauseInfo, LTLFormula)] -> String
prepareHtmlOutput f [] = asAlert AlertSuccess ("Formula <b>" ++ (show f) ++ "</b> does not contain any redundant clauses")
prepareHtmlOutput f l = asAlert AlertWarning ("Formula <b>" ++ (show f) ++ "</b> contains the following redundant clauses:") ++ displaySpuriousClauses f l
    

