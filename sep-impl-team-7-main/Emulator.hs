module Emulator where
import Lexer
import Syntax
import Parser    
import Compiler

type ProgramCounter = Integer

data Stack = S String Integer deriving (Show)
data Heap = H Integer Wurzel deriving (Show)

data Wurzel = DEF String Integer Integer | VAL BoolOrInt | APP Integer Integer | IND Integer | UNINITIALIZED deriving(Show)

type HeapList = [Heap]
type StackList = [Stack] 
type Status = (ProgramCounter, HeapList, StackList)

--the input is from compiler, namely the global environment, which is placed at the beginning of the heap list.
initialHeap :: [(String, Integer, Integer)] -> Integer -> HeapList
initialHeap [] _ = []
initialHeap ((s,n,i) : xs) p = [H p (DEF s n i)] ++ initialHeap xs (p + 1)

getWurzelBoolOrInt :: Wurzel -> BoolOrInt
getWurzelBoolOrInt (VAL v) = v
getWurzelBoolOrInt (DEF _ _ _) = error "not val, is a DEF"
getWurzelBoolOrInt (APP _ _) = error "not val, is a APP"
getWurzelBoolOrInt (IND i) = error "not val, is a IND"
getWurzelBoolOrInt (UNINITIALIZED) = error "not val, is a UNINITIALIZED"

getStackValue :: Stack -> Integer
getStackValue (S "H" i) = i
getStackValue (S "C" i) = i

--take the second to last element out of the stack list
secondToLastStack :: StackList -> Stack
secondToLastStack [] = error "empty list"
secondToLastStack (x:xs) = if length xs > 1 then secondToLastStack xs else x

--t = -1 but we don't have to consider because there is no -1 of index in list.
reset :: Status -> Status
reset (p, h, t) = (p + 1, h, t)

--T = T + 1; Stack[T] = adress(f)
pushFun :: String -> Status -> [(String, Integer)] -> Status
pushFun s (p, h, t) m = (p + 1, h, t ++ [(S "H" (matching s m))])

--T = T + 1; Stack[T] = new(VAL, t, w)
-- PushVal (Bool True)
pushValue :: BoolOrInt -> Status -> Status
pushValue v (p, h, t) = (p + 1, h ++ [(H (toInteger(length h)) (VAL v))], t ++ [(S "H" (toInteger(length h)))])

--stack[T - 1] := new(APP,stack[T],stack[T-1]);T := T - 1
makeApp :: Status -> Status
makeApp (p, h, t) = let lh = (toInteger(length h))
                    in (p + 1, h ++ [(H lh (APP (getStackValue(last t)) (getStackValue(secondToLastStack t))))], init(init t) ++ [(S "H" lh)])

--return the stack list from the beginning to nth last.
returnBeforeNStack :: Integer -> StackList -> StackList
returnBeforeNStack n [] = []
returnBeforeNStack n (x:xs) | ((length xs + 1) == fromInteger(n)) = []
                            | otherwise = let list = returnBeforeNStack n xs in [x] ++ list

--clear n stack below the top two stacks.
slide :: Integer -> Status -> Status
slide i (p, h, t) = (p + 1, h, (returnBeforeNStack (i + 2) t) ++ [(secondToLastStack t), (last t)])

--return the root of nth Heap in the heap list.
getWurzelHeap :: Integer -> HeapList -> Wurzel
getWurzelHeap i [] = error "not found"
getWurzelHeap i ((H a w):xs) = if i == a then w else getWurzelHeap i xs

unwind :: Status -> Status
unwind (p, h, t) = case getWurzelHeap (getStackValue(last t)) h of
                    IND a -> (p, h, (init t) ++ [(S "H" a)])
                    APP a1 a2 -> (p, h, t ++ [(S "H" a1)])
                    VAL v -> (p + 1, h, t)
                    DEF f n d -> (p + 1, h, t)

--jump to the code adress which is stored in the DEF cell.
call :: Status -> Status
call (p, h, t) = case getWurzelHeap (getStackValue(last t)) h of
                  VAL v -> (p + 1, h, t)
                  DEF f n d -> (d, h, t ++ [(S "C" (p + 1))])

--replace the nth heap of the heap list with the new heap.
replaceNthHeapList :: Integer -> Heap -> HeapList -> HeapList
replaceNthHeapList _ _ [] = error "not found"
replaceNthHeapList i nh (x:xs) | i == 0 = nh:xs
                               | otherwise = x:replaceNthHeapList (i - 1) nh xs
                 
--getStackValue(t!!((length t) - 1 - n - 2)) the adress of heap that need to be updated.
--change the root of a heap to a IND root.                  
update :: Integer -> Status -> Status
update n (p, h, t) = let hp = (getStackValue(t!!((length t) - 1 - fromInteger(n) - 2)))
                     in (p + 1, replaceNthHeapList hp (H hp (IND (getStackValue(last t)))) h, t)

--jump to the adress that stores in the second last stack.
returnStack :: Status -> Status
returnStack (p, h, t) = (getStackValue(secondToLastStack t), h, init(init t) ++ [(last t)])

getIntegerValue :: BoolOrInt -> Integer
getIntegerValue (Integer i) = i
getIntegerValue (Bool _) = error "not integer"

getBoolValue :: BoolOrInt -> Bool
getBoolValue (Bool b) = b
getBoolValue (Integer _) = error "not bool"

checkSameValue :: BoolOrInt -> BoolOrInt -> Bool 
checkSameValue (Bool ib) (Bool jb) = if ib == jb then True else False
checkSameValue (Integer ii) (Integer ji) = if ii == ji then True else False
checkSameValue _ _ = error "not same type"

operator :: String -> Status -> Status
operator str (p, h, t) = let lastv = getWurzelBoolOrInt (getWurzelHeap (getStackValue(last t)) h)
                             sndlastv = getWurzelBoolOrInt (getWurzelHeap (getStackValue(secondToLastStack t)) h)
                             newpositionheap = (toInteger(length h))
                             truewurzel = (VAL (Bool True))
                             falsewurzel = (VAL (Bool False))
                             unarynewstack = (init t) ++ [(S "H" newpositionheap)]
                             binarynewstack = init(init t) ++ [(S "H" newpositionheap)]
                          in case str of
                               "Negate" -> (p + 1, h ++ [(H newpositionheap (VAL (Integer (-(getIntegerValue lastv)))))], unarynewstack)
                               "+" -> (p + 1, h ++ [(H newpositionheap (VAL (Integer ((getIntegerValue sndlastv) + (getIntegerValue lastv)))))], binarynewstack)
                               "*" -> (p + 1, h ++ [(H newpositionheap (VAL (Integer ((getIntegerValue sndlastv) * (getIntegerValue lastv)))))], binarynewstack)
                               "-" -> (p + 1, h ++ [(H newpositionheap (VAL (Integer ((getIntegerValue sndlastv) - (getIntegerValue lastv)))))], binarynewstack)
                               "/" -> (p + 1, h ++ [(H newpositionheap (VAL (Integer (toInteger ((getIntegerValue sndlastv) `div` (getIntegerValue lastv))))))], binarynewstack)
                               "If" -> if (getBoolValue (getWurzelBoolOrInt (getWurzelHeap (getStackValue(t!!((length t) - 1 - 2))) h))) == True
                                       then (p + 1, h, init(init(init t)) ++ [(S "H" (getStackValue(secondToLastStack t)))])
                                       else (p + 1, h, init(init(init t)) ++ [(S "H" (getStackValue(last t)))])
                               "Not" -> (p + 1, h ++ [(H newpositionheap (VAL (Bool (not (getBoolValue lastv)))))], unarynewstack)
                               "&" -> (p + 1, h ++ [(H newpositionheap (VAL (Bool ((getBoolValue sndlastv) && (getBoolValue lastv)))))], binarynewstack)
                               "|" -> (p + 1, h ++ [(H newpositionheap (VAL (Bool ((getBoolValue sndlastv) || (getBoolValue lastv)))))], binarynewstack)
                               "<" -> (p + 1, h ++ [(H newpositionheap (VAL (Bool ((getIntegerValue sndlastv) < (getIntegerValue lastv)))))], binarynewstack)
                               "==" -> if (checkSameValue lastv sndlastv)
                                       then (p + 1, h ++ [(H newpositionheap truewurzel)], binarynewstack)
                                       else (p + 1, h ++ [(H newpositionheap falsewurzel)], binarynewstack)

--return the second adress of a app.
getAppValue :: Wurzel -> Integer
getAppValue (APP a1 a2) = a2
getAppValue (DEF _ _ _) = error "not app"
getAppValue (VAL _) = error "not app"
getAppValue (IND _) = error "not app"
getAppValue (UNINITIALIZED) = error "not app"

--store the second adress of the app in the next stack.
pushParameter :: Integer -> Status -> Status
pushParameter i (p, h, t) = (p + 1, h, t ++ [(S "H" (getAppValue (getWurzelHeap (getStackValue(t!!((length t) - fromInteger(i) - 2))) h)))])


alloc :: Status -> Status
alloc (p, h, t) = (p + 1, h ++ [(H (toInteger(length h)) (UNINITIALIZED))], t ++ [(S "H" (toInteger(length h)))])

--clear n stack below the top stacks.
slideLet :: Integer -> Status -> Status
slideLet i (p, h, t) = (p + 1, h, (returnBeforeNStack (i + 1) t) ++ [(last t)])

--the last step in the MF program.
halt :: Status -> Status
halt (p, h, t) = (-1, h ,t)

getLastWurzelHeap :: Heap -> Wurzel
getLastWurzelHeap (H _ w) = w

getHeapList :: Status -> HeapList
getHeapList (p, h, t) = h

getStackList :: Status -> StackList
getStackList (p, h, t) = t

--arrange the initial heap to a list of tupels, in which matches the name of a function with the position of the heaplist.
arrangeInitialHeap :: HeapList -> [(String, Integer)]
arrangeInitialHeap [] = []
arrangeInitialHeap ((H i (DEF s n a)):xs) = [(s,i)] ++ arrangeInitialHeap xs

--return the position of the heap list, when the name matches the corresponding tupel.
matching :: String -> [(String, Integer)] -> Integer
matching s [] = error "there is no match"
matching s ((str,i):xs) = if s == str then i else matching s xs

--the input is from compiler, namely the codes for MF. The output is a tupel with the final status and the total of steps.
emulateTotal :: Ausgabe -> (BoolOrInt, Integer)
emulateTotal a = let initialheaplist = (initialHeap (snd(a)) 0)
                     finalheaplist = getHeapList finalstatus
                     finalstatus = fst(emulate (fst(a)) matches (0, initialheaplist, []))
                     matches = arrangeInitialHeap initialheaplist
                     stepstotal = snd(emulate (fst(a)) matches (0, initialheaplist, []))
                 in ((getWurzelBoolOrInt(getWurzelHeap (getStackValue(last (getStackList finalstatus))) finalheaplist)), stepstotal)


getpc :: Status -> Integer
getpc (p, h, t) = p
 
emulate :: Code -> [(String, Integer)] -> Status -> (Status, Integer)
emulate code m (-1, h, t) = ((-1, h, t), 0)
emulate code m status = let (s, step) = emulate code m (emulateHelp (code !! fromInteger(getpc status)) m status) in (s, step + 1)

--when the corresponding command comes, change the status.
emulateHelp :: Befehle -> [(String, Integer)] -> Status -> Status
emulateHelp Reset m status = reset status  
emulateHelp (Pushfun strf) m status = pushFun strf status m
emulateHelp (Pushparam ip) m status = pushParameter ip status
emulateHelp Makeapp m status = makeApp status
emulateHelp (Slide is) m status = slide is status
emulateHelp Call m status = call status 
emulateHelp Return m status = returnStack status 
emulateHelp Unwind m status = unwind status
emulateHelp (Pushval boi) m status = pushValue boi status
emulateHelp (Operator stro) m status = operator stro status
emulateHelp (Update iu) m status = update iu status
emulateHelp Alloc m status = alloc status
emulateHelp (SlideLet ist) m status = slideLet ist status
emulateHelp Halt m status = halt status 

--run = emulateTotal . compile . parse . lexer

run :: String -> String 
run str = "The result is " ++ show(result) ++ ", a total of " ++ show(steps) ++ " steps."
          where result = fst((emulateTotal . compile . parse . lexer) str)
                steps = snd((emulateTotal . compile . parse . lexer) str)
