module Lexer where
import Data.Char

type Token = String

-- lexer breaks input into a sequence of small chunks
-- eg (identifiers, numbers, symbols etc)
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
  | isSpace x = lexer xs
  | isDigit  x = numtoken : lexer restnum
  | isLetter x = vartoken : lexer restvar
     where
     numtoken = x : takeWhile isDigit xs
     restnum = dropWhile isDigit xs
     vartoken = x : takeWhile isIdChar xs
     restvar = dropWhile isIdChar xs
lexer (a : b : s)
   | elementvon op binOp = op : lexer s
     where op = [a,b]
lexer (x:xs) = [x] : lexer xs


isIdChar :: Char -> Bool
isIdChar c = isLetter c || isDigit c || (c == '_')

binOp :: [String]
binOp = ["=="]

keywords :: [String]
keywords = ["let", "in" , "if" , "else", "then"] 

elementvon x (c:cs) 
   | x == c  = True
   |otherwise = elementvon x cs
elementvon _ []  = False
