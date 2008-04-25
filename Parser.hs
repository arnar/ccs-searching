module Parser where

import CCS

import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

ldef = emptyDef { P.commentStart = "/*"
                , P.commentEnd = "*/"
                , P.commentLine = "//"
                }

lexer = P.makeTokenParser ldef
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
symbol = P.symbol lexer
braces = P.braces lexer
commaSep1 = P.commaSep1 lexer
semiSep1 = P.semiSep1 lexer
semi = P.semi lexer
parens = P.parens lexer

signalName :: Parser Signal
signalName = do c <- lower
                cs <- many (alphaNum <|> char '_')
                return (c:cs)
             <?> "an action name"

processName :: Parser ProcessName
processName = do c <- upper
                 cs <- many (alphaNum <|> char '_')
                 return (c:cs)
              <?> "a process name"

action :: Parser Action
action = do s <- lexeme signalName
            return (Input s)
         <|>
         do symbol "^"
            s <- lexeme signalName
            return (Output s)

simpleProcess :: Parser Process
simpleProcess = do char '0'
                   return nullProcess
                <|>
                do pn <- lexeme processName
                   return (Name pn)
                <|>
                parens process

renaming :: Parser Renaming
renaming = do rs <- between (symbol "[") (symbol "]") (commaSep1 rename)
              return $ Map.fromList rs
              where
                rename :: Parser (Signal, Signal)
                rename = do s1 <- lexeme signalName
                            symbol "/"
                            s2 <- lexeme signalName
                            return (s2,s1)

restriction :: Parser Restriction
restriction = do symbol "\\"
                 oneAction <|> manyActions
                 where
                   manyActions = braces (do ss <- commaSep1 action
                                            return $ Set.fromList ss)
                   oneAction = do a <- action
                                  return $ Set.singleton a

process_adapter :: Parser (Process -> Process)
process_adapter = do restr <- restriction
                     return (\p -> Restrict p restr)
                  <|>
                  do ren <- renaming
                     return (\p -> Rename p ren)

adaptedProcess :: Parser Process
adaptedProcess = do p <- simpleProcess
                    adapters <- many process_adapter
                    return $ foldr (flip (.)) id adapters p

prefixedProcess :: Parser Process
prefixedProcess = do a <- action
                     symbol "."
                     p <- prefixedProcess
                     return (Prefix a p)
                  <|> adaptedProcess

parallelProcess :: Parser Process
parallelProcess = do whiteSpace
                     chainl1 (lexeme prefixedProcess) pipe
                     where
                       pipe = do { symbol "|" ; return Parallel }

choiceProcess :: Parser Process
choiceProcess = do ps <- sepBy1 parallelProcess (symbol "+")
                   if null $ tail ps
                      then return $ head ps
                      else return $ Choice ps

process :: Parser Process
process = choiceProcess


-- Parser for HML formulas

hmlAtom :: Parser HMLFormula
hmlAtom = do { symbol "tt" ; return HMLTrue } 
      <|> do { symbol "ff" ; return HMLFalse } 
      <|> parens hmlFormula

hmlModalOperator :: Parser (HMLFormula -> HMLFormula)
hmlModalOperator = do a <- between (symbol "[") (symbol "]") action
                      return (\f -> HMLBox a f)
                   <|>
                   do a <- between (symbol "<") (symbol ">") action
                      return (\f -> HMLDiamond a f)

hmlPrefixed :: Parser HMLFormula
hmlPrefixed = do ops <- many hmlModalOperator
                 f <- hmlAtom
                 return $ foldr (flip (.)) id (reverse ops) f

hmlAnd :: Parser HMLFormula
hmlAnd = do chainl1 hmlPrefixed combine
            where
              combine = do { symbol "&" ; return HMLAnd }

hmlOr :: Parser HMLFormula
hmlOr = do chainl1 hmlAnd combine
           where
             combine = do { symbol "|" ; return HMLOr }

hmlFormula :: Parser HMLFormula
hmlFormula = hmlOr

testp :: String -> IO ()
testp input
      = case (parse process "" input) of
          Left err -> do putStr "parse error at "
                         print err
          Right p -> putStrLn $ showCCS p