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
                        , "++"
                        , "--"
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
intterm = chainl1 unary prodop

unary :: Parser (Exp Int)
unary = (reservedOp lis "-" >> UMinus <$> unary)
    <|> intatom
    
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
boolexp = chainl1 boolterm orop

orop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orop = reservedOp lis "||" >> return Or

boolterm :: Parser (Exp Bool)
boolterm = chainl1 boolfactor andop

andop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andop = reservedOp lis "&&" >> return And

boolfactor :: Parser (Exp Bool)
boolfactor = (do reservedOp lis "!"
                 b <- boolfactor
                 return (Not b))
         <|> boolatom

boolatom :: Parser (Exp Bool)
boolatom = (reserved lis "true" >> return BTrue)
       <|> (reserved lis "false" >> return BFalse)
       <|> parens lis boolexp
       <|> boolrel

boolrel :: Parser (Exp Bool)
boolrel = try (do e1 <- intexp
                  op <- relop
                  e2 <- intexp
                  return (op e1 e2))

relop :: Parser (Exp Int -> Exp Int -> Exp Bool)
relop = (reservedOp lis "==" >> return Eq)
    <|> (reservedOp lis "!=" >> return NEq)
    <|> (reservedOp lis "<" >> return Lt)
    <|> (reservedOp lis ">" >> return Gt)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 commterm seqop

seqop :: Parser (Comm -> Comm -> Comm)
seqop = reservedOp lis ";" >> return Seq

commterm :: Parser Comm
commterm = skipcmd
       <|> ifcmd
       <|> repeatcmd
       <|> assigncmd

skipcmd :: Parser Comm
skipcmd = reserved lis "skip" >> return Skip

ifcmd :: Parser Comm
ifcmd = do reserved lis "if"
           b <- boolexp
           c1 <- braces lis comm
           (do reserved lis "else"
               c2 <- braces lis comm
               return (IfThenElse b c1 c2))
            <|> return (IfThen b c1)

repeatcmd :: Parser Comm
repeatcmd = do reserved lis "repeat"
               c <- braces lis comm
               reserved lis "until"
               b <- boolexp
               return (RepeatUntil c b)

assigncmd :: Parser Comm 
assigncmd = do x <- identifier lis 
               reservedOp lis "="
               e <- intexp
               return (Let x e)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
