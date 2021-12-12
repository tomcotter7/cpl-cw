{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

  extended by Thomas Cotter

Compiles MiniTriangle Programs into TAM programs

-}

module ExpCompiler where

import TAM
import ExpParser
import ExpTAM


-- Compiling Programs to TAM

compProgram :: String -> [TAMInst]
compProgram s = astCode [] (astParse s)

-- reading from a file

compileTAM :: String -> String
compileTAM = writeTAM . compProgram

