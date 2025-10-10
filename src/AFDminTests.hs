---------------------------------------------------------------------------
-- Pruebas básicas del módulo AFDmin
---------------------------------------------------------------------------

import AFD
import AFDmin

-- Definimos un AFD con estados redundantes
afdEjemplo :: AFD
{--
afdEjemplo = AFD
  { estadosD = ["q0", "q1", "q2", "q3"]
  , alfabetoD = ['a', 'b']
  , transicionesD =
      [ ("q0",'a',"q1"), ("q0",'b',"q2")
      , ("q1",'a',"q3"), ("q1",'b',"q3")
      , ("q2",'a',"q3"), ("q2",'b',"q3")
      , ("q3",'a',"q3"), ("q3",'b',"q3")
      ]
  , inicialD = "q0"
  , finalD = ["q1", "q2"]
  }

afdEjemplo = AFD
  { estadosD = ["q0", "q1", "q2", "q3", "q4", "q5", "q6"]
  , alfabetoD = ['a', 'b']
  , transicionesD =
      [ ("q0",'a',"q1"), ("q0",'b',"q2")
      , ("q1",'a',"q3"), ("q1",'b',"q4")
      , ("q2",'a',"q4"), ("q2",'b',"q3")
      , ("q3",'a',"q5"), ("q3",'b',"q5")
      , ("q4",'a',"q5"), ("q4",'b',"q5")
      , ("q5",'a',"q5"), ("q5",'b',"q5")
      ]
  , inicialD = "q0"
  , finalD = ["q1", "q2", "q5"]
  }
--}
afdEjemplo = AFD
  { estadosD = ["q0", "q1", "q2", "q3", "q4", "q5", "q6", "q7"]
  , alfabetoD = ['a', 'b']
  , transicionesD =
      [ ("q1",'a',"q2"), ("q1",'b',"q3")
      , ("q2",'a',"q4"), ("q2",'b',"q5")
      , ("q3",'a',"q6"), ("q3",'b',"q7")
      , ("q4",'a',"q4"), ("q4",'b',"q5")
      , ("q5",'a',"q6"), ("q5",'b',"q7")
      , ("q6",'a',"q4"), ("q6",'b',"q5")
      , ("q7",'a',"q6"), ("q7",'b',"q7")
      ]
  , inicialD = "q1"
  , finalD = ["q2", "q6"]
  }

-- Prueba: eliminar estados inalcanzables
testUnreachables :: IO ()
testUnreachables = print $ removeUnreachable afdEjemplo

-- Prueba: refinamiento de particiones
testGroups :: IO ()
testGroups = print $ groupEquivalents afdEjemplo

-- Prueba: construcción completa del AFD mínimo
testAFDmin :: IO ()
testAFDmin = print $ getAFDmin afdEjemplo

-- Ejecutar todas
main :: IO ()
main = do
  putStrLn "---- Eliminación de inalcanzables ----"
  testUnreachables
  putStrLn "\n---- Grupos equivalentes ----"
  testGroups
  putStrLn "\n---- AFD mínimo ----"
  testAFDmin
