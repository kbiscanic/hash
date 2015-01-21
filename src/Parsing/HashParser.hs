-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.

module Parsing.HashParser where

import           Control.Applicative  (Applicative, (*>), (<$), (<$>), (<*),
                                       (<*>), (<|>))
import           Control.Monad        (void)
import           Data.Char            (digitToInt)
import           Text.Parsec          (ParseError, Parsec, alphaNum, char, eof,
                                       letter, many, many1, noneOf, oneOf,
                                       optionMaybe, parse, try)
import           Text.Parsec.Char     (digit)
import           Text.Parsec.String   (Parser)

import           Language.Expressions

parseAll :: String -> Either String [TLExpr]
parseAll s = case parse (many parseTLExpr <* eof) "" ((unlines . map uncomment . lines) s) of
    Left err -> Left $ show err
    Right xs -> Right xs

parseTLExpr :: Parser TLExpr
parseTLExpr = try parseTLCmd

parseTLCmd :: Parser TLExpr
parseTLCmd = TLCmd <$> (try parseAssign <|> parseCmd)

parseCmd :: Parser Cmd
parseCmd = Cmd <$> parseExprNoQ <*> (whitespace *> many parseExpr) <*> optionMaybe parseIn <*> optionMaybe parseOut <*> rmWs parseAppend

parseAssign :: Parser Cmd
parseAssign = Assign <$> parseVar <*> (sym '=' *> whitespace *> parseExpr)

parseExpr :: Parser Expr
parseExpr = try parseVar <|> parseStr

parseVar :: Parser Expr
parseVar = do
  var <- parseVar'
  return $ Var $ "$" ++ var

parseVar' :: Parser String
parseVar' = rmWs $ sym '$' *> many1 (alphaNum <|> char '_')

parseExprNoQ :: Parser Expr
parseExprNoQ =  parseVar <|> parseStrNoQ

parseIn :: Parser Expr
parseIn = sym '<' *> whitespace *> parseExpr

parseOut :: Parser Expr
parseOut = sym '>' *> whitespace *> parseExpr

parseAppend :: Parser Bool
parseAppend = (sym '+' *> return True) <|> return False

parseStr :: Parser Expr
parseStr = Str <$> parseStr'

parseStrNoQ :: Parser Expr
parseStrNoQ = Str <$> parseStrNoQ'

parseStr' :: Parser String
parseStr' = rmWs $ concat <$> (sym '"' *> many charStr <* sym '"')

parseStrNoQ' :: Parser String
parseStrNoQ' = many1 alphaNum

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

rmWs :: Parser a -> Parser a
rmWs p = p <* whitespace

sym :: Char -> Parser ()
sym c = void $ char c

charStr :: Parser String
charStr = return <$> nonEscape <|> escape

charStrNoQ :: Parser String
charStrNoQ = return <$> nonEscapeNoQ

escape :: Parser String
escape = (\x y -> [x,y]) <$> char '\\' <*> oneOf "\\\"0nrvtbf"

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

nonEscapeNoQ :: Parser Char
nonEscapeNoQ = noneOf "\\\"\0\n\r\v\t\b\f "

uncomment :: String -> String
uncomment = takeWhile ('#' /=)
