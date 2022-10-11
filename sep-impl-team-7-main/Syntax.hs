module Syntax where

data Expr = EVar String           
            | ENum Integer               
            | EAp (Expr) (Expr)  
            | ELet                  
                [LokalDefinition]        
                (Expr)
            | EIf (Expr) (Expr) (Expr)             
            | Ewahrheitswert String 
            | EBin BinOp (Expr) (Expr)            
            | EUn UnOp (Expr)
            deriving (Show)

type Name = String

type Variable = Name

-- Definition ::= Variable {Variable} "=" Ausdruck .
type Definition = (Variable, [Variable], Expr)

type Program  = [Definition]     

-- Lokaldefinition ::= Variable "=" Ausdruck .
type LokalDefinition = (Name,Expr)

type LokalDefinitionen = [LokalDefinition]

data BinOp = NOp NumOp | BOp BoolOp | ROp RelOp deriving (Show)
 
data UnOp = Not Negation | Strich Negation deriving (Show)

data Negation = No | Stri deriving (Show)

data NumOp = Plus | Minus | Times | Div deriving (Show)

data BoolOp = And | Or deriving (Show)

data RelOp = REQ |  RLT deriving (Show)
