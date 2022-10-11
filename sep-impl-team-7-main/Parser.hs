module Parser where
import Data.Char
import Lexer
import Syntax
import Control.Applicative

--a function of the input Tokens, returning a list of possible outcomes! A --single outcome is the remaining input and the parsed value.
newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])]}

-- takes a list of tokens and return a program
parse :: [Token] -> Program
parse eingabe = (take_first_parse . runParser pProgram) eingabe


--runParser :: Parser a -> [Token] -> [(a,[Token])]
--runParser (Parser f) eingabe  = f eingabe

-- Returns successfuly parsed
take_first_parse :: [(p, [Token])] -> p
take_first_parse ((prog, []):_) = prog
take_first_parse ((prog, [";"]):_) = prog
take_first_parse(_:others) = take_first_parse others
take_first_parse  _   = error ("syntax Error")

instance Functor Parser where
-- fmap :: (a -> b) -< f a -> f b
--fmap f p == f <$> p
  fmap f p = Parser $ \toks -> [(f v , toks2) | 
                                (v,toks2) <- runParser p toks]

instance Applicative Parser where
-- pure :: a -> fa
  pure a = Parser $ \toks -> [(a, toks)]

  f <*> p = Parser $ \toks -> [(fn v , toks3) | 
                                (fn ,toks2) <- runParser f toks , 
                                (v,toks3) <- runParser p toks2 ]


{-
combines two parsers, say p1 and p2. First it uses p1 to parse the input, and then it uses p2 to parse the same input; it returns all the successful parses returned by either p1 or p2
-}
instance Alternative Parser where
-- <|> combine function : combines results of two parsers
  p1 <|> p2 = Parser $ \toks -> runParser p1 toks ++ runParser p2 toks



{-
it takes a parser, p, and returns a new parser which recognises zero or more occurrences of whatever p recognises. it must either see one or more occurrences, or zero occurrences: // some
-}
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p <|> pure []


pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = Parser $ \toks -> [(a:rest , toks2 ) | 
                                  (a,toks1) <- runParser p toks , 
                                  (rest,toks2) <- runParser (pZeroOrMore p) toks1
                                  ]
--to look for one or more occurrences of a symbol, separated by some other symbol ";".
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = Parser $ \toks -> [(a:b , toks2) |
                                              (a,toks1) <- runParser p1 toks ,
                                              (b,toks2) <- runParser (pZeroOrMore ((\_ a -> a) <$> p2 <*> p1 )) toks1
                                            ]

{-
satisfy takes a function which tells whether or not the string inside the token has the desired property, and returns a parser which recognises a token with the property
-}
satisfy  :: (String -> Bool) -> Parser String
satisfy  pred = Parser $ \t -> case t of
  (tok:toks) | pred tok -> [(tok, toks)]
  _                          -> []


pLit :: String -> Parser String
pLit s = satisfy  (== s)

pWahrheitswert :: Parser String
pWahrheitswert = satisfy $ \v -> v == "True" || v == "False"  || v == "true" || v == "false" 

pVar :: Parser String
pVar = satisfy  $ \v -> v `notElem` keywords && isVar v && isNotBool v
  where
    isVar []     = False
    isVar (c:cs) = isAlpha c && all isIdChar cs
    isNotBool x = x /= "True" && x /= "False" && x /= "false" && x /= "true"

pNum :: Parser Integer
pNum = read <$> (satisfy  (all isDigit))

pParens :: Parser Expr
pParens =  (\_ v _ -> v) <$> (pLit "(") <*> pExpr <*> (pLit ")")

pProgram :: Parser Program
pProgram = pOneOrMoreWithSep pDef (pLit (";"))

pDef :: Parser Definition
pDef = (\name variable _ expr -> (name , variable , expr)) <$> pVar <*> (pZeroOrMore pVar) <*>  (pLit "=") <*> pExpr 

pExpr :: Parser Expr
pExpr =  pExpr1 <|> pELet <|> pIf
  where
    pELet = (\_ lokal _ expr -> ELet lokal expr) <$> (pLit "let") <*> (pOneOrMoreWithSep pLokal (pLit";")) <*> (pLit "in") <*> pExpr
      where
        pLokal =  (\var _ e _-> (var, e)) <$> pVar <*> (pLit "=") <*> pExpr <*> (pZeroOrMore (pLit ";"))


    pIf = (\_ e1 _ e2 _ e3 -> EIf e1 e2 e3) <$> (pLit "if") <*> (pExpr) <*> (pLit "then") <*> (pExpr) <*> (pLit "else") <*> (pExpr)


pExpr1 :: Parser Expr
pExpr1 =  pExpr2 <|> pOr
  where
    pOr = (\e _ e2 -> EBin (BOp Or) e e2) <$> pExpr2 <*> (pLit "|") <*> pExpr2


pExpr2 :: Parser Expr
pExpr2 = pExpr3A <|> pAnd
  where
    pAnd = (\e _ e2 -> EBin (BOp And) e e2) <$> pExpr3 <*> (pLit "&") <*> pExpr3


pExpr3A :: Parser Expr
pExpr3A = pNot <|> pExpr3  where
  pNot = (\e2 e -> EUn (Not No) e) <$> (pLit"not") <*> pEWahr where
    pEWahr = fmap Ewahrheitswert pWahrheitswert


pExpr3 :: Parser Expr
pExpr3 =  pExpr4 <|> pComparisonOperator where
  pComparisonOperator = pLT <|> pEQ  where 
    pLT =  (\e _ e2 -> EBin (ROp RLT) e e2) <$> pExpr4 <*> (pLit "<") <*> pExpr4
    pEQ =  (\e _ e2 -> EBin (ROp REQ) e e2) <$> pExpr4 <*> (pLit "==") <*> pExpr4


pExpr4 :: Parser Expr
pExpr4 =pExpr5 <|> pRestExpression4 where
  pRestExpression4 = pPlus <|> pMinus where
    pPlus =  (\e _ e2 -> EBin (NOp Plus) e e2) <$> pExpr5 <*> (pLit "+") <*> pExpr4
    pMinus =  (\e _ e2 -> EBin (NOp Minus) e e2) <$> pExpr5 <*> (pLit "-") <*> pExpr5


pExpr5 :: Parser Expr
pExpr5 =  pStrich <|> pExpr6 where
  pStrich = (\e2 e ->  EUn (Strich Stri) e ) <$> (pLit "-") <*> pExpr6


pExpr6 :: Parser Expr
pExpr6 = pExpr7 <|> pRestExpression6 where
  pRestExpression6 = pTimes <|> pDiv where
    pTimes =  (\e _ e2 -> EBin (NOp Times) e e2) <$> pExpr7 <*> (pLit "*") <*> pExpr6
    pDiv =  (\e _ e2 -> EBin (NOp Div) e e2) <$> pExpr7 <*> (pLit "/") <*> pExpr7


pExpr7 :: Parser Expr 
pExpr7 = pEAp <|> pAtomicExpr  where
  pAtomicExpr = pEVar <|> pENum <|> pEWahr <|>  pParens 
  pEVar = fmap  EVar pVar

  pENum = fmap ENum pNum

  pEWahr = fmap Ewahrheitswert pWahrheitswert

  pEAp = fmap apChain (pOneOrMore pAtomicExpr)
    where
      apChain xs = foldl1 EAp xs     
      apChain _        = error "Error not implemented"
