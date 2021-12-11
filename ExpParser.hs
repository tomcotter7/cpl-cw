{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

Parser by Venanzio Capretta (based on Henrik Nilsson 2006-2013).
Parser for Arithmetic expressions, generating an Abstract Syntax
Tree from an expression (a string).
Uses functional parsers.

-}

module ExpParser where

import FunParser
import Control.Applicative

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show,Enum)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

data AST = LitInteger Int
         | Var Identifier
         | BinOp BinOperator AST AST
         | UnOp  UnOperator AST
         | Conditional AST AST AST
         | LetIn Declarations Command
  deriving (Eq,Show)

type Identifier = String
type Commands = [Command]
type Declarations = [Declaration]

data Command = Assignment Identifier AST
               | IfThenElse AST Command Command
               | WhileDo AST Command
               | GetInt Identifier
               | PrintInt AST
               | BeginEnd Commands
    deriving (Eq,Show)

data Declaration = Decl Identifier | Init Identifier AST
    deriving (Eq,Show)


-- Parse the top string: error if parsers fail or input not consumed

astParse :: String -> AST
astParse src = case parse prog src of
  [(t,"")] -> t
  _ -> case parse expr src of
        [(t2,"")] -> t2
        _ -> error "Parsing Error"

expParse :: String -> AST
expParse src = case parse expr src of
  [(t,"")] -> t
  _ -> error "Parsing error"

progParse :: String -> AST
progParse src = case parse prog src of
  [(t,"")] -> t
  _ -> error "Parsing error"
{-
Grammar for program

program ::= let declarations in command
declaration = var identifier | var identifier := exp
declarations = declaration | declaration ; declaration
command ::= identifier := exp
            | if exp then command else command
            | while exp do command
            | getint (identifier)
            | printint (exp)
            | begin commands end
commands = command | command ; commands
 -}


prog :: Parser AST
prog = do symbol "let"
          d <- decls
          symbol "in"
          c <- cmd
          return (LetIn d c)
       <|> empty

decls :: Parser Declarations
decls = do ds <- many middecl
           d <- decl
           return (ds ++ [d])


middecl :: Parser Declaration
middecl = do d <- decl
             symbol ";"
             return (d)

decl :: Parser Declaration
decl = do symbol "var"
          i <- identifier
          (do symbol ":="
              e <- expr
              return (Init i e))
           <|> return (Decl i)

semicmd :: Parser Command
semicmd = do c <- cmd
             symbol ";"
             return c

cmds :: Parser Commands
cmds = do c1 <- cmd
          (do symbol ";"
              cs <- many semicmd
              c2 <- cmd
              return ([c1] ++ cs ++ [c2])
           <|> 
           return [c1])

cmd :: Parser Command
cmd = do ifs <- ifstate
         return (ifs)
      <|> (do w <- whiledo
              return (w))
      <|> (do gi <- getint
              return (gi))
      <|> (do pi <- printint
              return (pi))
      <|> (do be <- beginend
              return (be))
      <|> (do as <- assign
              return (as))


assign :: Parser Command
assign = do i <- identifier
            symbol ":="
            e <- expr
            return (Assignment i e) 

getint :: Parser Command
getint = do symbol "getint" 
            i <- parens identifier
            return (GetInt i)

printint :: Parser Command
printint =  do symbol "printint" 
               e <- parens expr
               return (PrintInt e)

beginend :: Parser Command
beginend = do symbol "begin"
              cs <- cmds
              symbol "end"
              return (BeginEnd cs) 

whiledo :: Parser Command
whiledo = do symbol "while"
             e <- expr
             symbol "do"
             c <- cmd
             return (WhileDo e c)

ifstate :: Parser Command
ifstate = do symbol "if"
             e <- expr
             symbol "then"
             c1 <- cmd
             symbol "else"
             c2 <- cmd
             return (IfThenElse e c1 c2)
               

{-
Grammar for expressions (either arith or bool):
We allow arith expressions to occur as bool expressions in parentheses
Conditionals must be in parentheses, except at the top level

  exp ::= bexp | bexp ? bexp : bexp

  bexp ::= cexp | cexp || bexp
  cexp ::= bterm | bterm && cexp
  bterm ::= aexp | aexp `op` aexp
             where `op` is one of <,<=,>,>=,==,!=

  aexp ::= mexp | mexp + aexp | mexp - aexp
  mexp ::= aterm | aterm * mexp | aterm / mexp
  aterm ::= intLit | - aterm | ! aterm | ( exp ) | var identifier

-}

expr :: Parser AST
expr = do b <- bexp
          (do symbol "?"
              e0 <- bexp
              symbol ":"
              e1 <- bexp
              return (Conditional b e0 e1)
           <|>
           return b)


bexp :: Parser AST
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser AST
cexp = do e0 <- bterm
          (do symbol "&&"
              e1 <- cexp
              return (BinOp Conjunction e0 e1)
           <|>
           return e0)

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop = choice [ symbol "<=" >> return LeqOp
               , symbol "<"  >> return LssOp
               , symbol ">=" >> return GeqOp
               , symbol ">"  >> return GtrOp
               , symbol "==" >> return EqOp
               , symbol "!=" >> return NeqOp
               ]

bterm :: Parser AST
bterm = do e0 <- aexp
           (do op <- relop
               e1 <- aexp
               return (BinOp op e0 e1)
            <|>
            return e0) 


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- For left-associativity, we use an auxiliary function aexp'
--    that keeps a functional accumulator

aexp :: Parser AST
aexp = aexp' id

aexp' :: (AST -> AST) -> Parser AST
aexp' f = do e0 <- mexp
             (do op <- addminus
                 aexp' (BinOp op (f e0))
              <|>
              return (f e0))

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser AST
mexp = mexp' id

mexp' :: (AST -> AST) -> Parser AST
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser AST
aterm = (natural >>= return . LitInteger)
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> (do i <- identifier
                return (Var i))
        <|> parens expr
        
