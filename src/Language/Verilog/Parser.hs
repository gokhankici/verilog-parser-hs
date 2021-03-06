module Language.Verilog.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr  as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef { Tok.commentStart    = "/*"
                          , Tok.commentEnd      = "*/"
                          , Tok.commentLine     = "//"
                          , Tok.nestedComments  = True
                          , Tok.identStart      = letter
                          , Tok.identLetter     = alphaNum <|> oneOf "_'"
                          , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
                          , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                          , Tok.reservedNames   = []
                          , Tok.reservedOpNames = []
                          , Tok.caseSensitive   = True
                          }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)
