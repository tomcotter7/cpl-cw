{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus
        
  extended by Thomas Cotter

TAM Virtual Machine

Supports: MiniTriangle Language

-}

module TAM where

import Data.List (intercalate)

type MTInt = Int -- TAM Integer type (values in stack)
type A = Int
type L = String

-- Instructions of the Virtual Machine

data TAMInst
  = LOADL MTInt   -- push Integer into the stack
  -- Arithmetic operations
  | ADD           -- adds two top values in the stack
  | SUB           -- subtract second element of stack from top
  | MUL           -- multiplies top values in the stack
  | DIV           -- divides the second value by the top (integer division)
  | NEG           -- negates the top of the stack
  -- Boolean operations
  | AND           -- Boolean conjunction (non-zero values are True)
  | OR            -- Boolean disjunction
  | NOT           -- Boolean negation
  -- Relational operations
  | LSS           -- order operation <
  | GTR           -- order operation >
  | EQL           -- equality operator
  | LOAD A        -- reads the content of stack address a and pushes the value to the top of the stack
  | STORE A       -- pops top of stack and write the value to stack location a
  | JUMPIFZ L     -- pops top of stack, if 0 jump to L
  | JUMP L        -- jump to L
  | Label L       -- identifies the position of a label
  | GETINT        -- reads integer from terminal and pushes it to top of stack
  | PUTINT        -- pops top of stack and prints to terminal
  | HALT
  deriving (Eq,Show)

type Stack = [MTInt]

emptyStack :: Stack
emptyStack = []

-- Correspondence between Booleans and integers
boolInt :: Bool -> MTInt
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: MTInt -> Bool
intBool x = x/=0

-- Convenient composition operators

-- Pre-composing with a 2-argument function
infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)


-- Implementation of boolean operations on Integers, always return 0 or 1

intAND :: MTInt -> MTInt -> MTInt
intAND = boolInt .< (&&) <. intBool

intOR :: MTInt -> MTInt -> MTInt
intOR = boolInt .< (||) <. intBool

intNOT :: MTInt -> MTInt
intNOT = boolInt . not . intBool

-- Relational operations, return 0 (False) or 1 (True)

intLSS :: MTInt -> MTInt -> MTInt
intLSS = boolInt .< (<)

intGTR :: MTInt -> MTInt -> MTInt
intGTR = boolInt .< (>)

intEQL :: MTInt -> MTInt -> MTInt
intEQL = boolInt .< (==)

-- writing out a TAM program
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++ show inst ++ "\n") ""

-- parsing a TAM program
parseTAM :: String -> [TAMInst]
parseTAM = pTAM . words where
  pTAM ("LOADL":x:src) = LOADL (read x) : pTAM src
  pTAM ("ADD":src) = ADD : pTAM src
  pTAM ("SUB":src) = SUB : pTAM src
  pTAM ("MUL":src) = MUL : pTAM src
  pTAM ("DIV":src) = DIV : pTAM src
  pTAM ("NEG":src) = NEG : pTAM src
  pTAM ("AND":src) = AND : pTAM src
  pTAM ("OR" :src) = OR  : pTAM src
  pTAM ("NOT":src) = NOT : pTAM src
  pTAM ("LSS":src) = LSS : pTAM src
  pTAM ("GTR":src) = GTR : pTAM src
  pTAM ("EQL":src) = EQL : pTAM src
  pTAM ("LOAD":x:src) = LOAD (read x) : pTAM src
  pTAM ("STORE":x:src) = STORE (read x) : pTAM src
  pTAM ("JUMPIFZ":l:src) = JUMPIFZ l : pTAM src
  pTAM ("JUMP":l:src) = JUMP l : pTAM src
  pTAM ("Label":l:src) = Label l : pTAM src
  pTAM ("GETINT":src) = GETINT : pTAM src
  pTAM ("PUTINT":src) = PUTINT : pTAM src
  pTAM ("HALT":src) = HALT : pTAM src 
  pTAM _ = []
