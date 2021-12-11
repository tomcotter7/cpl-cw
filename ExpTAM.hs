{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

Generation of TAM code from the AST of an Arithmetic expression
Extension with Boolean, Relational, Conditional Expressions

-}

module ExpTAM where

import ExpParser
import TAM
import StateMonad
import Data.List

type LabelName = String
type VariableIdentifier = String
type StackAddress = Int
type VarEnv = [(VariableIdentifier,StackAddress)]


astCode :: VarEnv -> AST -> [TAMInst]
astCode ve (LitInteger x) = [LOADL x]
-- Relational operators that don't have TAM instructions
astCode ve (BinOp LeqOp t1 t2) =
  astCode ve (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
astCode ve (BinOp GeqOp t1 t2) =
  astCode ve (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
astCode ve (BinOp NeqOp t1 t2) =
  astCode ve (UnOp NegBool (BinOp EqOp t1 t2))
-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
astCode ve (Conditional b t1 t2) =
  astCode ve (BinOp Addition
             (BinOp Multiplication (UnOp NegBool (UnOp NegBool b)) t1)
             (BinOp Multiplication (UnOp NegBool b) t2))
-- General cases
astCode ve (BinOp op t1 t2) = (astCode ve t1) ++ (astCode ve t2) ++ [binOpTAM op]
astCode ve (UnOp op t) = (astCode ve t) ++ [unOpTAM op]
-- Program AST
astCode ve (LetIn d c) = tsd ++ tsc ++ [HALT]
                      where
                        (ve', tsd) = declsCode d
                        tsc = commCode c ve'
astCode ve (Var i) = [LOAD (getAddress i ve)]

binOpTAM :: BinOperator -> TAMInst
binOpTAM Addition       = ADD
binOpTAM Subtraction    = SUB
binOpTAM Multiplication = MUL
binOpTAM Division       = DIV
binOpTAM Conjunction    = AND
binOpTAM Disjunction    = OR
binOpTAM LssOp          = LSS
binOpTAM GtrOp          = GTR
binOpTAM EqOp           = EQL

unOpTAM :: UnOperator -> TAMInst
unOpTAM Negation = NEG
unOpTAM NegBool  = NOT


fresh :: ST Int LabelName
fresh = do n <- stGet
           stUpdate (n+1)
           return ('#' : (show n))

             
findAddress :: String -> VarEnv -> Maybe StackAddress
findAddress s ve = do (s,a) <- find (\(x,_) -> x == s) ve
                      return a

getAddress :: String -> VarEnv -> StackAddress
getAddress s ve = case findAddress s ve of
                    Just a -> a
                    Nothing -> error ("Variable does not exist" ++ s)

declTAM :: Declaration -> ST (VarEnv,StackAddress) [TAMInst]
declTAM (Decl v) = do (ve,a) <- stGet
                      stUpdate ((v,a):ve,a+1)
                      return [LOADL 0]
declTAM (Init v n) = do (ve,a) <- stGet
                        stUpdate ((v,a):ve,a+1)
                        return (astCode ve n)

-- multiple declarations
declsTAM :: Declarations -> ST (VarEnv,StackAddress) [TAMInst]
declsTAM [] = return []
declsTAM (d:ds) = do t <- declTAM d
                     ts <- declsTAM ds
                     return (t ++ ts)


declsCode :: Declarations -> (VarEnv,[TAMInst])
declsCode d = let (ts,(ve,a)) = app (declsTAM d) ([],0) in (ve,ts)

commCode :: Command -> VarEnv -> [TAMInst]
commCode c ve = let (tsc,_) = app (commandTAM c ve) 1 in tsc


commandTAM :: Command -> VarEnv -> ST Int [TAMInst]
commandTAM (Assignment i e) ve = return (te ++ [STORE a])
                                 where
                                    te = astCode ve e
                                    a = getAddress i ve
commandTAM (IfThenElse e c1 c2) ve = do l1 <- fresh
                                        l2 <- fresh     
                                        tc1 <- commandTAM c1 ve
                                        tc2 <- commandTAM c2 ve
                                        let te = astCode ve e
                                        return (te ++ [JUMPIFZ l1] ++ tc1 ++ [JUMP l2, Label l1] ++ tc2 ++ [Label l2])
commandTAM (WhileDo e c) ve = do l1 <- fresh
                                 l2 <- fresh
                                 tc <- commandTAM c ve 
                                 let te = astCode ve e
                                 return ([Label l2] ++ te ++ [JUMPIFZ l1] ++ tc ++ [JUMP l2, Label l1])
commandTAM (GetInt i) ve = return ([GETINT, STORE a])
                           where
                               a = getAddress i ve
commandTAM (PrintInt e) ve = return (te ++ [PUTINT])
                             where
                               te = astCode ve e
commandTAM (BeginEnd cs) ve = commandsTAM cs ve


commandsTAM :: Commands -> VarEnv -> ST Int [TAMInst]
commandsTAM [] _ = return []
commandsTAM (c:cs) ve = do t <- commandTAM c ve
                           ts <- commandsTAM cs ve
                           return (t ++ ts)
