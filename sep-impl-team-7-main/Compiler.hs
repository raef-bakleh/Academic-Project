module Compiler where
import Syntax
import Parser

data Befehle = Reset | Pushfun String | Call | Unwind | 
               Operator String | Pushparam Integer | Slide Integer |
               Return | Update Integer | Pushval BoolOrInt |
               Makeapp | Alloc | SlideLet Integer | Halt deriving Show

data BoolOrInt = Bool Bool | Integer Integer deriving (Eq, Show)

type Code = [Befehle] 

--Eingabe, die der Compiler vom Parser bekommt, bestehend aus dem Namen der
--übersetzten Funktion, einer Liste ihrer formalen Parameter und dem der Syntax
--entsprechenden geparsten Ausdruck (rechten Seite) der Funktionsdefinition
type Ausdruck = (String, [String], Expr) 

--wird am Ende an den Emulator übergeben, erstes Element ist der Code, der F-Code
--in MF-Code übersetzt und das zweite besteht aus den Namen der übersetzten 
--Funktionen, deren Stelligkeit und der Adresse des ersten Befehls ihrer 
--Übersetzung in Code
type Ausgabe = (Code, [(String, Integer, Integer)]) 

initCode :: Code
initCode = [Reset, Pushfun "main", Call, Halt] 

first :: [a] -> a
first [] = error "Liste leer"
first (x:xs) = x

rest :: [a] -> [a]
rest [] = []
rest (x:xs) = xs

plusplusTupel :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
plusplusTupel (x1,y1) (x2,y2) = (x1 ++ x2, y1 ++ y2)

--initCode und operatorCode sind immer gleich
compile :: [Ausdruck] -> Ausgabe
compile [] = ([],[])
compile eingabe = (initCode, []) `plusplusTupel`
                  (operatorCode, ausgabeOperator) `plusplusTupel`
                  compileHelp eingabe ([],[]) 121 

--i dient dem Hochzählen der Codeadressen der Übersetzungen der Funktions-
--definitionen, die erste nach den Operatoren ist 121
compileHelp :: [Ausdruck] -> Ausgabe -> Integer -> Ausgabe
compileHelp [] _ _ = ([],[])
compileHelp ((fname,args,expr):xs) (code, ausgabetripel) i = 
             (code ++ uebDef expr args,
             ausgabetripel ++ [(fname, toInteger $ length args, i)]) 
             `plusplusTupel` compileHelp xs (code, ausgabetripel) (i + getDrittes(code, ausgabetripel) + (toInteger $ length $ code ++ uebDef expr args))

getDrittes :: Ausgabe -> Integer
getDrittes (_, []) = 0
getDrittes (_, (_,_,dr):rest) = dr

--Uebersetzung eines definierenden Ausdrucks einer Funktionsdefinition 
uebDef :: Expr -> [String] -> Code 
uebDef expr xs = uebKons expr (pos xs 1) ++ 
                 [Update $ toInteger(length xs),
                 Slide $ toInteger(1 + length xs),
                 Unwind, Call, Return]

--lokale Umgebung
pos :: [String] -> Integer -> [(String, Integer)]
pos [] _ = []
pos (x:xs) n = [(x, n)] ++ pos xs (n+1)

--sucht anhand eines Strings in der lokalen Umgebung nach der jeweiligen Adresse
--und gibt den Integer-Wert zurück
posLookup :: [(String, Integer)] -> String -> Integer
posLookup [] _ = -1
posLookup ((x, n): xs) str = if x == str then n else posLookup xs str

--erhoeht alle Integerwerte der lokalen Umgebung um einen gewuenschten Wert
posPlus :: [(String, Integer)] -> Integer -> [(String, Integer)]
posPlus [] _ = []
posPlus ((str, int):xs) i = [(str, int + i)] ++ posPlus xs i

--posLet nach Skript
posLet :: (String, Integer) -> [(String, Integer)] -> [(String, Integer)]
posLet (str, i) [] = [(str,i)]
posLet (str1, i1) ((str2, i2):xs) = if str1 == str2
                                    then [(str1, i1)] ++ posLet (str1, i1) xs
                                    else [(str2, i2)] ++ posLet (str1, i1) xs

fstList :: [(String, a)] -> [String]
fstList [] = []
fstList (x:xs) = [fst x] ++ fstList xs

--Uebersetzungsschema für alle von unserer Syntax definierten Ausdruecke, bekommt 
--als Eingabe einen Ausdruck und eine Liste von dessen lokaler Umgebung
uebKons :: Expr -> [(String, Integer)] -> Code
uebKons (ENum a) xs = [Pushval (Integer (getNum (ENum a)))] --Integer?
uebKons (Ewahrheitswert b) xs = [Pushval (Bool (getBool (Ewahrheitswert b)))]
uebKons (EVar str) xs = if str `elem` fstList xs 
                        then [Pushparam (posLookup xs str)] 
                        else [Pushfun str]   
uebKons (EAp (expr1) (expr2)) xs = uebKons expr2 xs ++ 
                                   uebKons expr1 (posPlus xs 1) ++
                                   [Makeapp]
uebKons (EUn unOp (expr)) xs = uebKons expr xs ++
                               [Pushfun (umwandleUnOp unOp), Makeapp]
uebKons (EBin binOp (expr1) (expr2)) xs = uebKons expr2 xs ++
                                          uebKons expr1 (posPlus xs 1) ++
                                          [Pushfun (umwandleBinOp(binOp)), Makeapp, Makeapp]
uebKons (EIf (expr1) (expr2) (expr3)) xs = uebKons expr3 xs ++
                                           uebKons expr2 (posPlus xs 1) ++
                                           uebKons expr1 (posPlus xs 2) ++
                                           [Pushfun "if", Makeapp,
                                           Makeapp, Makeapp]
uebKons (ELet defs (expr)) xs = uebKonsLet (ELet defs (expr)) xs 1 ++ [SlideLet(toInteger(length defs))]

--Uebersetzungsschema für einen Let-Ausdruck, bekommt als Eingabe einen Ausdruck, eine
--Liste seiner lokalen Umgebung und einen Integer, der dazu genutzt wird, die lokale 
--Umgebung im rekursiven Fall um 1(oder theoretisch auch einen anderen Wert) zu erhöhen
uebKonsLet :: Expr -> [(String, Integer)] -> Integer -> Code
uebKonsLet (ELet lokDef expr) xs i = if leereListe(lokDef) 
                                     then uebKons expr xs
                                     else uebKons (snd(first lokDef)) xs ++
                                     [Alloc, Makeapp] ++
                                     uebKonsLet (ELet (rest(lokDef)) expr) (posLet (fst(first lokDef), (-1)) (posPlus xs i)) i

leereListe :: [a] -> Bool
leereListe [] = True
leereListe _ = False

getNum :: Expr -> Integer
getNum (ENum a) = a

getBool :: Expr -> Bool
getBool (Ewahrheitswert "True") = True
getBool (Ewahrheitswert "true") = True
getBool (Ewahrheitswert "False") = False
getBool (Ewahrheitswert "false") = False

umwandleUnOp :: UnOp -> String
umwandleUnOp (Not No) = "not"
umwandleUnOp (Strich Stri) = "negate"

umwandleBinOp :: BinOp -> String
umwandleBinOp (NOp Plus) = "+"
umwandleBinOp (NOp Minus) = "-"
umwandleBinOp (NOp Times) = "*"
umwandleBinOp (NOp Div) = "/"
umwandleBinOp (BOp And) = "&"
umwandleBinOp (BOp Or) = "|"
umwandleBinOp (ROp RLT) = "<"
umwandleBinOp (ROp REQ) = "=="

ausgabeOperator :: [(String, Integer, Integer)]
ausgabeOperator = [("true", 0, 4), ("false", 0, 10),
                  ("not", 1, 16), ("negate", 1, 23),
                  ("|", 2, 30), ("&", 2, 40), ("+", 2, 50), ("-", 2, 60),
                  ("*", 2, 70), ("/", 2, 80), ("==", 2, 90), ("<", 2, 100),
                  ("if", 3, 110)]

operatorCode :: Code
operatorCode = codeTrueFalse ++ codeUnOp ++ codeBinOp ++ codeIf 

codeTrueFalse :: Code 
codeTrueFalse = [Pushval (Bool True), Update 0,
                Slide 1, Unwind, Call, Return]
                ++ [Pushval (Bool False), Update 0,
                Slide 1, Unwind, Call, Return]

codeUnOp :: Code
codeUnOp = [Pushparam 1, Unwind, Call, Operator "Not", Update 1, Slide 2, Return]
           ++ [Pushparam 1, Unwind, Call, Operator "Negate", Update 1, Slide 2, Return]

codeBinOp :: Code
codeBinOp = codeBinOpHelp (BOp Or) 
            ++ codeBinOpHelp (BOp And)
            ++ codeBinOpHelp (NOp Plus)
            ++ codeBinOpHelp (NOp Minus)
            ++ codeBinOpHelp (NOp Times)
            ++ codeBinOpHelp (NOp Div)
            ++ codeBinOpHelp (ROp REQ)
            ++ codeBinOpHelp (ROp RLT)

codeBinOpHelp :: BinOp -> Code
codeBinOpHelp binOp = [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call]
                      ++ [Operator (umwandleBinOp binOp)]
                      ++ [Update 2, Slide 3, Return]

codeIf :: Code
codeIf = [Pushparam 1, Unwind, Call, Pushparam 3, Pushparam 5, 
         Operator "If", Update 3, Slide 4, Unwind, Call, Return]
