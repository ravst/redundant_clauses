module Ltl.Types where

import Text.Parsec (SourcePos)
import Text.ParserCombinators.Parsec.Pos (newPos)

type VarId = String

data LTLBinOp = 
    LOr |
    LAnd |
    LXor |
    LNxor |
    LImpl |
    LEquiv |
    LUntil |
    LRelease
    deriving Eq

instance Show LTLBinOp where
    show LOr = "|"
    show LAnd = "&"
    show LXor = "xor"
    show LNxor = "nxor"
    show LImpl = "->"
    show LEquiv = "<->"
    show LUntil = "U"
    show LRelease = "V"

data LTLUnOp = 
    LNot |
    LNext |
    LAlways |
    LEventually
    deriving Eq

instance Show LTLUnOp where
    show LNot = "!"
    show LNext = "X"
    show LAlways = "G"
    show LEventually = "F"

data LTLFormula = 
    LBoolConst Bool SourcePos |
    LProposition SourcePos String|
    LUnOp LTLUnOp SourcePos LTLFormula |
    LBinOp LTLBinOp SourcePos LTLFormula LTLFormula 
    deriving Eq

inParens :: String -> String
inParens x = "(" ++ x ++ ")"

instance Show LTLFormula where
    show (LBoolConst True _) = "TRUE"
    show (LBoolConst False _) = "FALSE"
    show (LProposition _ s) = s
    show (LUnOp op _ f) = show op ++ " " ++ inParens (show f)  
    show (LBinOp op _ a b) = inParens (show a) ++ show op ++ inParens (show b)

emptyPos :: SourcePos
emptyPos = newPos "???" (-1) (-1)
