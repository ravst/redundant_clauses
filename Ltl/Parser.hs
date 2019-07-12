module Ltl.Parser where

import Ltl.Types
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
   emptyDef { Token.commentStart    = "(-"
            , Token.commentEnd      = "-)"
            , Token.commentLine     = "--"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = ["TRUE", "FALSE", "X", "G", "U", "V", "F", "X", "G"]
            , Token.reservedOpNames = ["!", "|", "&", "xor", "nxor", 
                                       "<->", "->", "U", "V", "F",
                                       "X", "G"]
            }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

--reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

ltlParser :: Parser LTLFormula
ltlParser = whiteSpace >> parseExpression

parseExpression :: Parser LTLFormula
parseExpression = buildExpressionParser operators terms

operators :: [[Operator Char st LTLFormula]]
operators = [
                -- Propositional Unary operator
                [Prefix (reservedOp "!" *> (LUnOp LNot <$> getPosition))],

                -- LTL Unary opretators
                [Prefix (reservedOp "G" *> (LUnOp LAlways     <$> getPosition))],
                [Prefix (reservedOp "F" *> (LUnOp LEventually <$> getPosition))],
                [Prefix (reservedOp "X" *> (LUnOp LNext       <$> getPosition))],

                -- Propositional Binary operators
                [Infix (reservedOp "&"    *> (LBinOp LAnd   <$> getPosition)) AssocLeft],
                [Infix (reservedOp "|"    *> (LBinOp LOr    <$> getPosition)) AssocLeft],
                [Infix (reservedOp "xor"  *> (LBinOp LXor   <$> getPosition)) AssocLeft,
                 Infix (reservedOp "nxor" *> (LBinOp LNxor  <$> getPosition)) AssocLeft],
                [Infix (reservedOp "->"   *> (LBinOp LImpl  <$> getPosition)) AssocNone],
                [Infix (reservedOp "<->"  *> (LBinOp LEquiv <$> getPosition)) AssocNone],
                
                --LTL Binary oprators:
                [Infix (reservedOp "U" *> (LBinOp LUntil   <$> getPosition)) AssocLeft,
                 Infix (reservedOp "V" *> (LBinOp LRelease <$> getPosition)) AssocLeft]
                
            ]

terms :: Parser LTLFormula
terms = parens parseExpression <|>
        LProposition <$> getPosition <*> identifier <|>
        reserved "TRUE" *> (LBoolConst True <$> getPosition) <|>
        reserved "FALSE" *> (LBoolConst False <$> getPosition) <?>
        "atomic formula"  
        
