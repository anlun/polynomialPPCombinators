--import Core
import Text.Printf

import Text.Parsec
import Text.Parsec.Expr

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "'''"
           , Token.commentEnd      = "'''"
           , Token.commenLine      = "#"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "def"
                                     , "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"                                   
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ] 
           }
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt
