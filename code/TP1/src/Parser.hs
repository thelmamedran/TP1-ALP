module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop

addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)

intterm :: Parser (Exp Int)
intterm = chainl1 intatom prodop

prodop :: Parser (Exp Int -> Exp Int -> Exp Int)
prodop = (reservedOp lis "*" >> return Times)
  <|> (reservedOp lis "/" >> return Div)

intatom :: Parser (Exp Int)
intatom = parens lis intexp
          <|> try (do n <- natural lis
                      return (Const (fromInteger n)))
          <|> try (do v <- identifier lis
                      reservedOp lis "++"
                      return (VarInc v))
          <|> try (do v <- identifier lis
                      reservedOp lis "--"
                      return (VarDec v))
          <|> do v <- identifier lis
                 return (Var v)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
