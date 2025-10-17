-- | Proyecto 1: Constructor de un analizador lexico
-- | Archivo de pruebas.
-- | Equipo: Los Discípulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
module Main where

import Regex
import AFD
import AFDmin

import Test.QuickCheck -- Para hacer pruebas con aleatorieidad
import Control.Exception (try, evaluate, SomeException) -- Para manejar las excepciones esperadas en las pruebas
import Control.Monad (replicateM) -- 
import Data.Char (chr) -- Actualmente usado para convertir enteros a caracteres


-- ------------------------------------------------------------------------------
-- =============================================================================
--                                 Pruebas Regex
-- =============================================================================
-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- Generador de Regex con formato válido,
-- Genera Strings de tamaño n
-- ------------------------------------------------------------------------------
genValidRegex :: Int -> Gen String
-- Caso base, Genera un String de una sola letra mayuscula 
genValidRegex 0 = (:[]) <$> elements ['A'..'Z']
--Caso recursivo, genera alguno de los casos validos de regex y
-- minimizamos n hasta que llegue a 0
genValidRegex n = oneof
  [ -- E+E
    do r1 <- genValidRegex (n `div` 2)
       r2 <- genValidRegex (n `div` 2)
       return (r1 ++ "+" ++ r2),
    -- (E+E)
    do r1 <- genValidRegex (n `div` 2)
       r2 <- genValidRegex (n `div` 2)
       return ("(" ++ r1 ++ "+" ++ r2 ++ ")"),
    -- String de una sola letra, generará casos de
    -- concatenacion como: 'A'E, 'A'(E)
    (:[]) <$> elements ['A'..'Z'], 
    -- E*
    do r <- genValidRegex (n `div` 2)
       if last r `elem` ['*','+','(']  -- evitamos casos "**", "+*", "(*"
          then return r
          else return (r ++ "*")
  ]


-- ------------------------------------------------------------------------------
-- Generador de Regex intencionalmente inválidas
-- Genera Strings de tamaño n
-- ------------------------------------------------------------------------------
genInvalidRegex :: Int -> Gen String
-- Caso Base. Genera una cadena con los simbolos reservados de las regex 
genInvalidRegex 0 = oneof [return "*", return "+", return ")", return "("]
--Caso recursivo, genera alguno de los casos invalidos de regex y
-- minimizamos n hasta que llegue a 0
genInvalidRegex n = oneof
  [ -- Cadena que termina con un simbolo reservado
    do r <- genValidRegex (n `div` 2); return (r ++ "*+"),
    -- Cadena que inicia con un simbolo reservado
    do r <- genValidRegex (n `div` 2); return ("+" ++ r),
    -- Union de cadenas invalidas
    do r1 <- genInvalidRegex (n `div` 2)
       r2 <- genInvalidRegex (n `div` 2)
       return (r1 ++ r2),
    -- Parentesis extra al inicio
    do r <- genInvalidRegex (n `div` 2); return ("(" ++ r),
    -- Parentesis extra al final
    do r <- genInvalidRegex (n `div` 2); return (r ++ ")")
  ]


-- ------------------------------------------------------------------------------
-- Generacion Aleatoria de cadenas válidas e inválidas para Regex
-- Se asigna un valor booleano al azar y:
-- Si el booleano resulta True, se genera una Regex válida de longitud 6 
-- Si el booleano resulta False, se genera una Regex invalida de longitud 6
-- ------------------------------------------------------------------------------
genRegexString :: Gen (String, Bool)
genRegexString = do
  b <- arbitrary -- Bool asignado al azar
  str <- if b then genValidRegex 5 else genInvalidRegex 5
  return (str, b)


-- ------------------------------------------------------------------------------
-- Generacion de pruebas para getRegex usando QuickCheck
-- ------------------------------------------------------------------------------
pruebasgetRegex :: Property
pruebasgetRegex = forAll genRegexString $ \(s, shouldBeValid) ->
    ioProperty $ do
      -- Evalúa la cadena con getRegex; si lanza excepción, se considera inválida
      str <- try (evaluate $ getRegex s) :: IO (Either SomeException Regex)
      return $ case str of
                 Right _ -> shouldBeValid   -- no falló, debió ser válida
                 Left _  -> not shouldBeValid  -- falló, debió ser inválida

-- Regex que representa el lenguaje IMP
imp = "([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)+((0)+(['1'..'9']['0'..'9']*)+(-['1'..'9']['0'..'9']*))+((\\+)+(-)+(\\*)+(/))+((<)+(>)+(=))+((:=))+((if)+(then)+(else)+(while)+(do))+((;)+(\\+))"

imp2 = "([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)"
        ++ "+((0)+(['1'..'9']['0'..'9']*)+(-['1'..'9']['0'..'9']*))"
        ++ "+((\\+)+(-)+(\\*)+(/))"
        ++ "+((<)+(>)+(=))"
        ++ "+((:=))"
        ++ "+((if)+(then)+(else)+(while)+(do))"
        ++ "+((;)+(\\+))"


-- ------------------------------------------------------------------------------
-- =============================================================================
--                                Pruebas AFDmin
-- =============================================================================
-- ------------------------------------------------------------------------------
afdEjemplo1 = AFD
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

afdEjemplo2 = AFD
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

afdEjemplo3 = AFD
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
testUnreachables :: AFD -> IO ()
testUnreachables afd = print $ removeUnreachable afd

testUnreachables2 = testUnreachables afdEjemplo2
testUnreachables4 = testUnreachables (getAFD "(0)+(ZD*)+(-ZD*)")

-- Prueba: refinamiento de particiones
testGroups :: Int -> AFD -> IO ()
testGroups n afd = print $ groupEquivalentsDesde n afd

testGroups1 = testGroups 0 afdEjemplo1
testGroups2 = testGroups 0 afdEjemplo2
testGroups3 = testGroups 0 afdEjemplo3
testGroups4 = testGroups 0 (getAFD "(0)+(ZD*)+(-ZD*)")

-- Prueba: construcción completa del AFD mínimo
testAFDmin :: Int -> AFD -> IO ()
testAFDmin n afd = print $ minimizaAFDDesde n afd

testAFDmin1 = testAFDmin 0 afdEjemplo1
testAFDmin2 = testAFDmin 0 afdEjemplo2
testAFDmin3 = testAFDmin 0 afdEjemplo3
testAFDmin4 = testAFDmin 0 (getAFD "(0)+(ZD*)+(-ZD*)")

-- ------------------------------------------------------------------------------
-- Ejecucion de Pruebas
-- ------------------------------------------------------------------------------
main :: IO ()
main = do
  -- REGEX --
  putStrLn "---- Pruebas Expresiones Regulares ----"
  quickCheckWith stdArgs { maxSuccess = 1000 } pruebasgetRegex

  -- AFDmin --
  putStrLn "\n---- Eliminación de inalcanzables ----"
  testUnreachables4
  putStrLn "\n---- Grupos equivalentes ----"
  testGroups4
  putStrLn "\n---- AFD mínimo ----"
  testAFDmin4
