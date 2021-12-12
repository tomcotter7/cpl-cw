module ExecuteTAM where

import TAM
import Data.List
import Data.Maybe
import StateMonad
import System.IO

data TAMValue = TAMValue {
    ts :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack
} deriving (Eq, Show)

type LName = String

type TAMSt a = StateIO TAMValue a

tsPop :: TAMSt MTInt
tsPop = do t <- stIOGet
           let s = tsStack t
           stIOUpdate (t {tsStack = tail (s)})
           return (head s)

tsPlace :: Int -> Int -> TAMSt ()
tsPlace a v = do t <- stIOGet
                 let s = tsStack t
                 stIOUpdate (t {tsStack = reverse (replaceAtIndex a v (reverse s))})
               

tsPush :: Int -> TAMSt ()
tsPush n = do t <- stIOGet
              stIOUpdate (t {tsStack = n : tsStack t})

lCounter :: LName -> TAMSt ()
lCounter l = do t <- stIOGet
                stIOUpdate (t {tsCounter = (fromJust $ elemIndex (Label l) (ts t))})

continueT :: TAMSt ()
continueT = do t <- stIOGet
               stIOUpdate (t {tsCounter = tsCounter t + 1})

exec :: TAMInst -> TAMSt ()
exec HALT = return ()
exec (STORE a) = do v <- tsPop
                    tsPlace a v
                    continueT
exec PUTINT = do v <- tsPop
                 lift (putStrLn ("output > " ++ (show v)))
                 continueT
exec GETINT = do lift (putStr ("input : ")) 
                 lift (hFlush stdout)
                 i <- lift (getLine)
                 tsPush (read i)
                 continueT
exec (LOADL i) = do tsPush i
                    continueT
exec (LOAD a) = do t <- stIOGet
                   let s = reverse (tsStack t)
                   tsPush (s !! a)
                   continueT
exec (JUMPIFZ l) = do v <- tsPop
                      case v of 
                        0 -> do lCounter l
                                continueT
                        _ -> continueT
exec (JUMP l) = do lCounter l
                   continueT 
exec (Label l) = continueT
exec NOT = execSi NOT
exec NEG = execSi NEG 
exec ADD = execBi ADD
exec SUB = execBi SUB
exec MUL = execBi MUL
exec DIV = execBi DIV
exec AND = execBi AND
exec OR  = execBi OR
exec LSS = execBi LSS
exec GTR = execBi GTR
exec EQL = execBi EQL
                


execBi :: TAMInst -> TAMSt ()
execBi ti = do x <- tsPop
               y <- tsPop
               let r = stringToBOperator ti x y
               tsPush r
               continueT

execSi :: TAMInst -> TAMSt ()
execSi ti = do v <- tsPop
               tsPush (stringToSOperator ti v)
               continueT

stringToBOperator :: TAMInst -> MTInt -> MTInt -> MTInt
stringToBOperator ADD x y = y + x
stringToBOperator SUB x y = y - x
stringToBOperator MUL x y = y * x
stringToBOperator DIV x y = y `div` x
stringToBOperator AND x y = y `intAND` x
stringToBOperator OR x y = y `intOR` x
stringToBOperator LSS x y = y `intLSS` x
stringToBOperator GTR x y = y `intGTR` x
stringToBOperator EQL x y = y `intEQL` x

stringToSOperator :: TAMInst -> MTInt -> MTInt
stringToSOperator NEG v = -v
stringToSOperaetor NOT v = intNOT v




replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

nextInst :: TAMSt TAMInst
nextInst = do t <- stIOGet
              let insts = ts t
              let counter = tsCounter t 
              return (insts !! counter)

stackT :: TAMSt Stack
stackT = do t <- stIOGet
            return (tsStack t)


execT :: TAMSt Stack
execT = do inst <- nextInst
           exec inst
           if inst == HALT then stackT
                           else execT

initTS :: [TAMInst] -> TAMValue
initTS is = TAMValue {ts = is, tsCounter = 0, tsStack = []}

execTAM' :: [TAMInst] -> IO Stack
execTAM' tam = do (stk,_) <- appIO execT (initTS tam) 
                  return stk
