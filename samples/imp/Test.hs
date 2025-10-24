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
import AFNEp
import AFN
import AFD
import AFDmin
import MDD
import LectorArchivo

import Test.QuickCheck -- Para hacer pruebas con aleatorieidad
import Control.Exception (try, evaluate, SomeException) -- Para manejar las excepciones esperadas en las pruebas
import Data.Char (chr) -- Actualmente usado para convertir enteros a caracteres

import Text.Read (readMaybe)

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


-- ------------------------------------------------------------------------------
-- =============================================================================
--                              Pruebas AFNEp
-- =============================================================================
-- ------------------------------------------------------------------------------

-- Automáta que reconoce la expresión (0+1)*1
respAFNEp1 = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"],
        alfabeto = "01",
        transiciones = [("q0",Just '0',["q1"]),
                        ("q2",Just '1',["q3"]),
                        ("q4",Nothing,["q0","q2"]),
                        ("q1",Nothing,["q5"]),
                        ("q3",Nothing,["q5"]),
                        ("q6",Nothing,["q4","q7"]),
                        ("q5",Nothing,["q4","q7"]),
                        ("q8",Just '1',["q9"]),
                        ("q7",Nothing,["q8"])],
        inicial = "q6",
        final = "q9"}

-- Construcción del AFNEp  para (0+1)*1
testAFNEp1 = getAFNEp "(0+1)*1"

-- Comprobacion de que la respuesta esperada es igual que la generada
testEq_AFNEp = respAFNEp1 == testAFNEp1


-- ------------------------------------------------------------------------------
-- =============================================================================
--                                Pruebas AFN
-- =============================================================================
-- ------------------------------------------------------------------------------

-- Automata que reconoce la expresion "(0+1)*1"
respAFN1 = AFN 
  { estadosN = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"], 
    alfabetoN = "01", 
    transicionesN = 
      [ ("q0",'0',["q0","q1","q2","q4","q5","q7","q8"]),
        ("q1",'0',["q0","q1","q2","q4","q5","q7","q8"]),
        ("q1",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
        ("q2",'1',["q0","q2","q3","q4","q5","q7","q8"]),
        ("q3",'0',["q0","q1","q2","q4","q5","q7","q8"]),
        ("q3",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
        ("q4",'0',["q0","q1","q2","q4","q5","q7","q8"]),
        ("q4",'1',["q0","q2","q3","q4","q5","q7","q8"]),
        ("q5",'0',["q0","q1","q2","q4","q5","q7","q8"]),
        ("q5",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
        ("q6",'0',["q0","q1","q2","q4","q5","q7","q8"]),
        ("q6",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
        ("q7",'1',["q9"]),("q8",'1',["q9"])],
    inicialN = "q6", 
    finalN = "q9"}

-- Construccion del AFN para (0+1)*1
testAFN1 = afnEp_to_AFN testAFNEp1

-- Comprobacion de que la respuesta esperada es igual que la generada
testEq_AFN = respAFN1 == testAFN1

-- ------------------------------------------------------------------------------
-- =============================================================================
--                                Pruebas AFD
-- =============================================================================
-- ------------------------------------------------------------------------------

-- Prueba : Construccion completa del AFD
testAFD :: AFN -> IO ()
testAFD afn = print $ afn_to_AFD afn 

testAFD1 = testAFD testAFN1
testAFD2 = testAFD (getAFN "(0)+(ZD*)+(-ZD*)")
testAFD3 = testAFD (getAFN "(0)+(['1'..'9']['0'..'9']*)+(-['1'..'9']['0'..'9']*)") -- Version extendida del testAFD2


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
-- =============================================================================
--                                Pruebas IMP
-- =============================================================================
-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- Expresiones regulares que representan las categorías léxicas
-- del lenguaje IMP.
-- Los conjuntos utilizados en el lenguaje son:
--       A = {a,b,c,...,z,A,B,C,..,Z}
--       D = {0,1,2,...,9}
--       Z = {1,2,...,9}
-- ------------------------------------------------------------------------------
-- enteros = 0 | ZD* | (-ZD*) 
entero = "(0 + ['1'..'9']['0'..'9']* + (-['1'..'9']['0'..'9']*))"
-- id = A(A*)D*
identificador = "([['a'..'z'] ++ ['A'..'Z']]([['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)"
-- op = + | - | *
operador = "((\\+)+(-)+(\\*))"
-- opbool = = | <= | not | and
opbool = "(= + <= + not + and)"
-- asignacion = (:=)
asig = "(:=)"
-- palabras reservadas = true | false | skip | if | then | else | while | do
reservadas = "(true + false + skip + if + then + else + while + do)"
-- delimitador = ;
delimitador = "(;)"

-- ------------------------------------------------------------------------------
-- Cadena que especifica la etiqueta de cada una de las categorías léxicas
-- que pertenecen al lenguaje IMP.
-- Utilizamos el separador # para notificar donde inicia el identificador
-- de la etiqueta y donde termina.
-- ------------------------------------------------------------------------------
-- enteros = 0 | ZD* | (-ZD*) 
entero_etiq = entero ++ "#entero#"
-- id = A(A*)D*
identificador_etiq = identificador ++ "#identificador#"
-- op = + | - | *
operador_etiq = operador ++ "#operador#"
-- opbool = = | <= | not | and
opbool_etiq = opbool ++ "#opBool#"
-- asignacion = (:=)
asig_etiq = asig ++ "#asig#"
-- palabras reservadas = true | false | skip | if | then | else | while | do
reservadas_etiq = reservadas ++ "#palabraReservada#"
-- delimitadores = ;
delimitador_etiq = delimitador ++ "#delimitador#"



-- ------------------------------------------------------------------------------
-- Expresion regular que representa todo el lenguaje IMP incluyendo la etiqueta
-- de las categorías léxicas que lo conforman.
-- ------------------------------------------------------------------------------
imp = entero_etiq ++
      " + " ++ identificador_etiq ++
      " + "  ++ operador_etiq  ++ 
      " + " ++ opbool_etiq ++
      " + "  ++ asig_etiq ++
      " + "  ++ reservadas_etiq ++
      " + "  ++ delimitador_etiq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de expresiones regulares de categorías léxicas del
-- lenguaje IMP
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
testER :: String -> IO ()
testER s = print $ getRegex s 


-- id = A(A*)D*
testID = testER identificador_etiq
-- enteros = 0 | ZD* | (-ZD*) 
testNUM = testER entero_etiq
-- op = + | - | *
testOP = testER operador_etiq
-- opbool = = | <= | not | and
testOPB = testER opbool_etiq
-- asignacion = (:=)
testASIG = testER asig_etiq
-- palabras reservadas = true | false | skip | if | then | else | while | do
testRES = testER reservadas_etiq
-- delimitadores = ;
testDEL = testER delimitador_etiq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Resumen de las respuestas que se generan en las pruebas testER.
-- Cada respuesta se guarda en formato Regex para realizar las pruebas de AFNEp
-- para cada categoría léxica de IMP sin que dependa de la recursión de resultados
-- anteriores. El objetivo de guardar los resultados es evitar tiempos de espera 
-- demasiado largos para pruebas que ya se ha comprobado que son correctos.
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La funcion unionChars se encarga de obtener la union de los simbolos
-- que pertenecen a una lista de caracteres. Se utilizara para mayor
-- claridad en la respuesta de las pruebas.
-- ---------------------------------------------------------------------
unionChars :: [Char] -> Regex
unionChars s = foldr1 Union (map Symbol s)

-- ---------------------------------------------------------------------
-- La funcion concatChars se encarga de obtener la concatenacion de los
-- simbolos en una lista de caracteres. Se utilizara para mayor
-- claridad en la respuesta de las pruebas.
-- ---------------------------------------------------------------------
concatChars :: [Char] -> Regex
concatChars []  = error "Conjunto vacío no soportado."
concatChars [c] = Symbol c
concatChars (c:cs) = foldl Concat (Symbol c) (map Symbol cs)

-- ---------------------------------------------------------------------
-- La función concatRegex se encarga de descomponer una expresión regular
-- de tipo Concat en una lista de subexpresiones mientras éstas
-- subexpresiones sean del mismo tipo.
-- Si la expresión no es una concatenación, devuelve una lista que 
-- contiene la expresión de tipo diferente a Concat. Se utilizara para
-- reestructurar el formato de Regex obtenida por la función 'getRegex'
-- de manera sencilla en las pruebas.
-- ---------------------------------------------------------------------
concatRegex :: Regex -> [Regex]
concatRegex (Concat r1 r2) = concatRegex r1 ++ (concatRegex r2)
concatRegex r = [r]

-- ---------------------------------------------------------------------
-- La función concatAll se encarga de reconstruir una única expresión
-- regular a partir de una lista de expresiones Regex que genera la
-- función concatRegex, concatenándolas en el orden adecuado para
-- mantener el formato de Regex que genera la función getRegex.
-- La función tambien puede ser utilizada para combinar
-- varias subexpresiones en una sola concatenación.
-- ---------------------------------------------------------------------
concatAll :: [Regex] -> Regex
concatAll = foldl1 Concat

-- ---------------------------------------------------------------------
-- Constantes de expresiones regulares en categorías léxicas de IMP
-- Los conjuntos utilizados en el lenguaje son:
--       A = {a,b,c,...,z,A,B,C,..,Z}
--       D = {0,1,2,...,9}
--       Z = {1,2,...,9}
-- ---------------------------------------------------------------------
abecedario :: Regex
abecedario = unionChars (['a'..'z']++['A'..'Z'])

cero_nueve :: Regex
cero_nueve =  unionChars ['0'..'9']

uno_nueve :: Regex
uno_nueve = unionChars ['1'..'9']


-- ---------------------------------------------------------------------
-- Expresiones regulares por categoría léxica
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- id = A(A*)D*
-- ---------------------------------------------------------------------
resp_terminal_testID = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Union (Symbol 'z') (Union (Symbol 'A') (Union (Symbol 'B') (Union (Symbol 'C') (Union (Symbol 'D') (Union (Symbol 'E') (Union (Symbol 'F') (Union (Symbol 'G') (Union (Symbol 'H') (Union (Symbol 'I') (Union (Symbol 'J') (Union (Symbol 'K') (Union (Symbol 'L') (Union (Symbol 'M') (Union (Symbol 'N') (Union (Symbol 'O') (Union (Symbol 'P') (Union (Symbol 'Q') (Union (Symbol 'R') (Union (Symbol 'S') (Union (Symbol 'T') (Union (Symbol 'U') (Union (Symbol 'V') (Union (Symbol 'W') (Union (Symbol 'X') (Union (Symbol 'Y') (Symbol 'Z')))))))))))))))))))))))))))))))))))))))))))))))))))) (Star (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Union (Symbol 'z') (Union (Symbol 'A') (Union (Symbol 'B') (Union (Symbol 'C') (Union (Symbol 'D') (Union (Symbol 'E') (Union (Symbol 'F') (Union (Symbol 'G') (Union (Symbol 'H') (Union (Symbol 'I') (Union (Symbol 'J') (Union (Symbol 'K') (Union (Symbol 'L') (Union (Symbol 'M') (Union (Symbol 'N') (Union (Symbol 'O') (Union (Symbol 'P') (Union (Symbol 'Q') (Union (Symbol 'R') (Union (Symbol 'S') (Union (Symbol 'T') (Union (Symbol 'U') (Union (Symbol 'V') (Union (Symbol 'W') (Union (Symbol 'X') (Union (Symbol 'Y') (Symbol 'Z')))))))))))))))))))))))))))))))))))))))))))))))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))))) (Symbol '#')) (Symbol 'i')) (Symbol 'd')) (Symbol 'e')) (Symbol 'n')) (Symbol 't')) (Symbol 'i')) (Symbol 'f')) (Symbol 'i')) (Symbol 'c')) (Symbol 'a')) (Symbol 'd')) (Symbol 'o')) (Symbol 'r')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta en terminal: [A][A]*[D]*#identificador#
-- Donde:
--       [A] = {a,b,c,...,z,A,B,C,...,Z}
--       [D] = {0,1,2,...,9}
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- [A][A]*
letras = Concat abecedario (Star abecedario)
-- [A][A]*[D]*
formatID = Concat letras (Star cero_nueve)
-- etiqueta para identificadores
etiqID = concatChars "#identificador#"
-- [A][A]*[D]*#identificador#
id_etiq = concatAll (concatRegex formatID ++  concatRegex etiqID)
-- ---------------------------------------------------------------------
-- [A][A]*[D]*#identificador# 
respTestID :: Regex
respTestID = id_etiq
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- enteros = 0 | ZD* | (-ZD*) 
-- ---------------------------------------------------------------------
resp_terminal_testNUM = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '0') (Concat (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9'))))))))) (Union (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9'))))))))))) (Concat (Concat (Symbol '-') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9'))))))))))))))) (Symbol '#')) (Symbol 'e')) (Symbol 'n')) (Symbol 't')) (Symbol 'e')) (Symbol 'r')) (Symbol 'o')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta en terminal: (0 + [Z][D]* + (-[Z][D]*))*#entero#
-- Donde:
--       [Z] = {1,2,...,9}
--       [D] = {0,1,2,...,9}
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- - [Z]
negativo = Concat (Symbol '-') uno_nueve
-- [D]*
digitos = Star cero_nueve
-- (-[Z][D]*)
negativos = Concat negativo digitos
-- [D]* + (-[Z][D]*)
digitosOneg = Union digitos negativos
-- [Z][D]* + (-[Z][D]*)
positivoOneg = (Concat uno_nueve digitosOneg)
-- 0 + [Z][D]* + (-[Z][D]*)
enterosRegex = Union (Symbol '0') positivoOneg
-- etiqueta para enteros
etiqNUM = concatChars "#entero#"
-- (0 + [Z][D]* + (-[Z][D]*))#entero#
num_etiq = concatAll (concatRegex enterosRegex ++  concatRegex etiqNUM)
-- ---------------------------------------------------------------------
-- (0 + [Z][D]* + (-[Z][D]*))#entero#
resptestNUM :: Regex
resptestNUM = num_etiq
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- op = + | - | *
-- ---------------------------------------------------------------------
resp_terminal_testOP = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '+') (Union (Symbol '-') (Symbol '*'))) (Symbol '#')) (Symbol 'o')) (Symbol 'p')) (Symbol 'e')) (Symbol 'r')) (Symbol 'a')) (Symbol 'd')) (Symbol 'o')) (Symbol 'r')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: ((+) + (-) + (*))#operador#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- ((+) + (-) + (*))
operadorRegex = Union (Symbol '+') (Union (Symbol '-') (Symbol '*'))
-- etiqueta para operador
etiqOP = concatChars "#operador#"
-- ((+) + (-) + (*))#operador#
op_etiq = concatAll (concatRegex operadorRegex ++  concatRegex etiqOP)
-- ---------------------------------------------------------------------
-- ((+) + (-) + (*))#operador#
respTestOP :: Regex
respTestOP = op_etiq
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- opbool = = | <= | not | and
-- ---------------------------------------------------------------------
resp_terminal_testOPB = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '=') (Union (Concat (Symbol '<') (Symbol '=')) (Union (Concat (Concat (Symbol 'n') (Symbol 'o')) (Symbol 't')) (Concat (Concat (Symbol 'a') (Symbol 'n')) (Symbol 'd'))))) (Symbol '#')) (Symbol 'o')) (Symbol 'p')) (Symbol 'B')) (Symbol 'o')) (Symbol 'o')) (Symbol 'l')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (=) + (<=) + (not) + (and)
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- and
funAND = concatChars "and"
-- not
funNOT = concatChars "not"
-- ((not) + (and))
funBool = Union funNOT funAND
-- (<=) + ((not) + (and))
menorqueOfunBool = Union (Concat (Symbol '<') (Symbol '=')) funBool 
-- = + ((<=) + (not) + (and))
opBoolRegex = Union (Symbol '=') menorqueOfunBool
-- etiqueta para operador
etiqOPB = concatChars "#opBool#"
-- ((+) + (-) + (*))#operador#
opb_etiq = concatAll (concatRegex opBoolRegex ++  concatRegex etiqOPB)
-- ---------------------------------------------------------------------
-- ((+) + (-) + (*))#operador#
-- ---------------------------------------------------------------------
respTestOPB :: Regex
respTestOPB = opb_etiq
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- asignacion = (:=)
-- ---------------------------------------------------------------------
resp_terminal_testASIG = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Symbol ':') (Symbol '=')) (Symbol '#')) (Symbol 'a')) (Symbol 's')) (Symbol 'i')) (Symbol 'g')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta (:=)#asig#
-- Interpretación en tipo Regex: :=#asig#
-- ---------------------------------------------------------------------
respTestASIG :: Regex
respTestASIG = concatChars ":=#asig#"
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- palabras reservadas = true | false | skip | if | then | else | while | do
-- ---------------------------------------------------------------------
resp_terminal_testRES = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'r')) (Symbol 'u')) (Symbol 'e')) (Union (Concat (Concat (Concat (Concat (Symbol 'f') (Symbol 'a')) (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Union (Concat (Concat (Concat (Symbol 's') (Symbol 'k')) (Symbol 'i')) (Symbol 'p')) (Union (Concat (Symbol 'i') (Symbol 'f')) (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n')) (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Union (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e')) (Concat (Symbol 'd') (Symbol 'o'))))))))) (Symbol '#')) (Symbol 'p')) (Symbol 'a')) (Symbol 'l')) (Symbol 'a')) (Symbol 'b')) (Symbol 'r')) (Symbol 'a')) (Symbol 'R')) (Symbol 'e')) (Symbol 's')) (Symbol 'e')) (Symbol 'r')) (Symbol 'v')) (Symbol 'a')) (Symbol 'd')) (Symbol 'a')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (true + false + skip + if + then + else + while + do)#palabraReservada#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex: (true + false + skip + if + then + else + while + do)
-- while + do
whileYdo = Union (concatChars "while") (concatChars "do")
-- else + while + do
elseYotras = Union (concatChars "else") whileYdo
-- then + else + while + do
thenYotras = Union (concatChars "then") elseYotras
-- if + then + else + while + do
ifYotras = Union (concatChars "if") thenYotras
-- skip + if + then + else + while + do
skipYotras = Union (concatChars "skip") ifYotras
-- false + skip + if + then + else + while + do
falseYotras = Union  (concatChars "false") skipYotras
-- (true + false + skip + if + then + else + while + do)
reservadasRegex = Union (concatChars "true") falseYotras
-- etiqueta para palabras reservadas
etiqRES = concatChars "#palabraReservada#"
-- (true + false + skip + if + then + else + while + do)#palabraReservada#
palabraReservada_etiq = concatAll (concatRegex reservadasRegex ++  concatRegex etiqRES)
-- ---------------------------------------------------------------------
-- (true + false + skip + if + then + else + while + do)#palabraReservada#
respTestRES :: Regex
respTestRES = palabraReservada_etiq
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- delimitadores = ;
-- ---------------------------------------------------------------------
resp_terminal_testDEL = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Symbol ';') (Symbol '#')) (Symbol 'd')) (Symbol 'e')) (Symbol 'l')) (Symbol 'i')) (Symbol 'm')) (Symbol 'i')) (Symbol 't')) (Symbol 'a')) (Symbol 'd')) (Symbol 'o')) (Symbol 'r')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (;)#delimitador#
-- Interpretación en tipo Regex: ;#delimitador#
-- ---------------------------------------------------------------------
resptestDEL :: Regex
resptestDEL = concatChars ";#delimitador#"
-- ---------------------------------------------------------------------


-- Comprobación de que la respuesta en terminal genera la misma Regex
-- que la interpretación de tipo Regex que fue asociada
testRegexEq :: String -> Regex -> Bool
testRegexEq str regex = (read str :: Regex) == regex


-- Prueba para Regex de las categorias lexicas de IMP
testIMPRegexEq :: [Bool]
testIMPRegexEq =
  [ 
    -- indentificadores
    testRegexEq resp_terminal_testID respTestID,
    -- enteros
    testRegexEq resp_terminal_testNUM  resptestNUM,
    -- operadores
    testRegexEq resp_terminal_testOP respTestOP,
    -- opbool
    testRegexEq resp_terminal_testOPB respTestOPB,
    -- asignaciones
    testRegexEq resp_terminal_testASIG respTestASIG,
    -- palabras reservadas
    testRegexEq resp_terminal_testRES respTestRES,
    -- Delimitadores
    testRegexEq resp_terminal_testDEL resptestDEL
  ]


-- Comprobación de que todas las Regex equivalen a la impresion en terminal
regex_Eq :: Bool
regex_Eq = and testIMPRegexEq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFNEp para expresiones regulares del lenguaje IMP
-- utiliza la función definida en AFNEp: expr_to_AFNEp :: Regex -> AFNEp
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D*
testAFNEp_ID = expr_to_AFNEp respTestID
-- enteros = 0 | ZD* | (-ZD*) 
testAFNEP_NUM = expr_to_AFNEp resptestNUM
-- op = + | - | *
testAFNEp_OP = expr_to_AFNEp respTestOP
-- opbool = = | <= | not | and
testAFNEp_OPB = expr_to_AFNEp respTestOPB
-- asignacion = (:=)
testAFNEp_ASIG = expr_to_AFNEp respTestASIG
-- palabras reservadas = true | false | skip | if | then | else | while | do
testAFNEp_RES = expr_to_AFNEp respTestRES
-- delimitadores = ;
testAFNEp_ESP = expr_to_AFNEp resptestDEL


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFN con base a AFNEp generados en el lenguaje IMP
-- utiliza la función definida en AFN: afnEp_to_AFN :: AFNEp -> AFN
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D*
testAFN_ID = afnEp_to_AFN testAFNEp_ID
-- enteros = 0 | ZD* | (-ZD*) 
testAFN_Z = afnEp_to_AFN testAFNEP_NUM
-- op = + | - | *
testAFN_OP = afnEp_to_AFN testAFNEp_OP
-- opbool = = | <= | not | and
testAFN_OPB = afnEp_to_AFN testAFNEp_OPB
-- asignacion = (:=)
testAFN_ASIG = afnEp_to_AFN testAFNEp_ASIG
-- palabras reservadas = true | false | skip | if | then | else | while | do
testAFN_RES = afnEp_to_AFN testAFNEp_RES
-- delimitadores = ;
testAFN_DEL = afnEp_to_AFN testAFNEp_ESP


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFD con base a AFN generados en el lenguaje IMP
-- utiliza la función definida en AFD: afn_to_AFD :: AFN -> AFD
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D*
testAFD_ID = afn_to_AFD testAFN_ID
-- enteros = 0 | ZD* | (-ZD*) 
testAFD_NUM = afn_to_AFD testAFN_Z
-- op = + | - | *
testAFD_OP = afn_to_AFD testAFN_OP
-- opbool = = | <= | not | and
testAFD_OPB = afn_to_AFD testAFN_OPB
-- asignacion = (:=)
testAFD_ASIG = afn_to_AFD testAFN_ASIG
-- palabras reservadas = true | false | skip | if | then | else | while | do
testAFD_RES = afn_to_AFD testAFN_RES
-- delimitadores = ;
testAFD_DEL = afn_to_AFD testAFN_DEL


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFDmin con base a AFD generados en el lenguaje IMP
-- utiliza la función definida en AFDmin: minimizaAFD :: AFD -> AFD
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D*
testAFDmin_ID = minimizaAFD testAFD_ID
-- enteros = 0 | ZD* | (-ZD*) 
testAFDmin_NUM = minimizaAFD testAFD_NUM
-- op = + | - | *
testAFDmin_OP = minimizaAFD testAFD_OP
-- opbool = = | <= | not | and
testAFDmin_OPB = minimizaAFD testAFD_OPB
-- asignacion = (:=)
testAFDmin_ASIG = minimizaAFD testAFD_ASIG
-- palabras reservadas = true | false | skip | if | then | else | while | do
testAFDmin_RES = minimizaAFD testAFD_RES
-- delimitadores = ;
testAFDmin_DEL = minimizaAFD testAFD_DEL


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Resumen de las respuestas que se generan en las pruebas testAFDmin.
-- Cada respuesta se guarda en formato AFD para comparar que todo el proceso:
--            Regex -> AFNEp -> AFN -> AFD -> AFDmin
-- Se haya ejecutado correctamente. 
-- Si el resultado final de minimización es el esperado, entonces los resultados
-- de los que depende también deben serlo.
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D*
respTestAFDmin_ID = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
                    "q13","q14","q15","q16","q17"],
        alfabetoD = "#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
        transicionesD = [("q1",'A',"q2"),("q1",'B',"q2"),("q1",'C',"q2"),("q1",'D',"q2"),
                         ("q1",'E',"q2"),("q1",'F',"q2"),("q1",'G',"q2"),("q1",'H',"q2"),
                         ("q1",'I',"q2"),("q1",'J',"q2"),("q1",'K',"q2"),("q1",'L',"q2"),
                         ("q1",'M',"q2"),("q1",'N',"q2"),("q1",'O',"q2"),("q1",'P',"q2"),
                         ("q1",'Q',"q2"),("q1",'R',"q2"),("q1",'S',"q2"),("q1",'T',"q2"),
                         ("q1",'U',"q2"),("q1",'V',"q2"),("q1",'W',"q2"),("q1",'X',"q2"),
                         ("q1",'Y',"q2"),("q1",'Z',"q2"),("q1",'a',"q2"),("q1",'b',"q2"),
                         ("q1",'c',"q2"),("q1",'d',"q2"),("q1",'e',"q2"),("q1",'f',"q2"),
                         ("q1",'g',"q2"),("q1",'h',"q2"),("q1",'i',"q2"),("q1",'j',"q2"),
                         ("q1",'k',"q2"),("q1",'l',"q2"),("q1",'m',"q2"),("q1",'n',"q2"),
                         ("q1",'o',"q2"),("q1",'p',"q2"),("q1",'q',"q2"),("q1",'r',"q2"),
                         ("q1",'s',"q2"),("q1",'t',"q2"),("q1",'u',"q2"),("q1",'v',"q2"),
                         ("q1",'w',"q2"),("q1",'x',"q2"),("q1",'y',"q2"),("q1",'z',"q2"),
                         ("q10",'i',"q3"),("q11",'f',"q9"),("q12",'c',"q13"),("q13",'a',"q4"),
                         ("q14",'o',"q15"),("q15",'r',"q16"),("q16",'#',"q0"),("q17",'#',"q10"),
                         ("q17",'0',"q17"),("q17",'1',"q17"),("q17",'2',"q17"),("q17",'3',"q17"),
                         ("q17",'4',"q17"),("q17",'5',"q17"),("q17",'6',"q17"),("q17",'7',"q17"),
                         ("q17",'8',"q17"),("q17",'9',"q17"),("q2",'#',"q10"),("q2",'0',"q17"),
                         ("q2",'1',"q17"),("q2",'2',"q17"),("q2",'3',"q17"),("q2",'4',"q17"),
                         ("q2",'5',"q17"),("q2",'6',"q17"),("q2",'7',"q17"),("q2",'8',"q17"),
                         ("q2",'9',"q17"),("q2",'A',"q2"),("q2",'B',"q2"),("q2",'C',"q2"),
                         ("q2",'D',"q2"),("q2",'E',"q2"),("q2",'F',"q2"),("q2",'G',"q2"),
                         ("q2",'H',"q2"),("q2",'I',"q2"),("q2",'J',"q2"),("q2",'K',"q2"),
                         ("q2",'L',"q2"),("q2",'M',"q2"),("q2",'N',"q2"),("q2",'O',"q2"),
                         ("q2",'P',"q2"),("q2",'Q',"q2"),("q2",'R',"q2"),("q2",'S',"q2"),
                         ("q2",'T',"q2"),("q2",'U',"q2"),("q2",'V',"q2"),("q2",'W',"q2"),
                         ("q2",'X',"q2"),("q2",'Y',"q2"),("q2",'Z',"q2"),("q2",'a',"q2"),
                         ("q2",'b',"q2"),("q2",'c',"q2"),("q2",'d',"q2"),("q2",'e',"q2"),
                         ("q2",'f',"q2"),("q2",'g',"q2"),("q2",'h',"q2"),("q2",'i',"q2"),
                         ("q2",'j',"q2"),("q2",'k',"q2"),("q2",'l',"q2"),("q2",'m',"q2"),
                         ("q2",'n',"q2"),("q2",'o',"q2"),("q2",'p',"q2"),("q2",'q',"q2"),
                         ("q2",'r',"q2"),("q2",'s',"q2"),("q2",'t',"q2"),("q2",'u',"q2"),
                         ("q2",'v',"q2"),("q2",'w',"q2"),("q2",'x',"q2"),("q2",'y',"q2"),
                         ("q2",'z',"q2"),("q3",'d',"q5"),("q4",'d',"q14"),("q5",'e',"q6"),
                         ("q6",'n',"q7"),("q7",'t',"q8"),("q8",'i',"q11"),("q9",'i',"q12")],
        inicialD = "q1",
        finalD = ["q0"]
}

-- enteros = 0 | ZD* | (-ZD*) 
respTestAFDmin_NUM = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12"], 
        alfabetoD = "#-0123456789enort", 
        transicionesD = [("q1",'0',"q2"),("q1",'1',"q3"),("q1",'2',"q3"),("q1",'3',"q3"),
                         ("q1",'4',"q3"),("q1",'5',"q3"),("q1",'6',"q3"),("q1",'7',"q3"),
                         ("q1",'8',"q3"),("q1",'9',"q3"),("q10",'r',"q11"),("q11",'o',"q12"),
                         ("q12",'#',"q0"),("q2",'#',"q4"),("q3",'#',"q4"),("q3",'-',"q6"),
                         ("q3",'0',"q7"),("q3",'1',"q7"),("q3",'2',"q7"),("q3",'3',"q7"),
                         ("q3",'4',"q7"),("q3",'5',"q7"),("q3",'6',"q7"),("q3",'7',"q7"),
                         ("q3",'8',"q7"),("q3",'9',"q7"),("q4",'e',"q8"),("q5",'e',"q10"),
                         ("q6",'1',"q7"),("q6",'2',"q7"),("q6",'3',"q7"),("q6",'4',"q7"),
                         ("q6",'5',"q7"),("q6",'6',"q7"),("q6",'7',"q7"),("q6",'8',"q7"),
                         ("q6",'9',"q7"),("q7",'#',"q4"),("q7",'0',"q7"),("q7",'1',"q7"),
                         ("q7",'2',"q7"),("q7",'3',"q7"),("q7",'4',"q7"),("q7",'5',"q7"),
                         ("q7",'6',"q7"),("q7",'7',"q7"),("q7",'8',"q7"),("q7",'9',"q7"),
                         ("q8",'n',"q9"),("q9",'t',"q5")],
        inicialD = "q1",
        finalD = ["q0"]
}

-- op = + | - | *
respTestAFDmin_OP = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11"],
        alfabetoD = "#*+-adeopr",
        transicionesD = [("q1",'*',"q2"),("q1",'+',"q2"),("q1",'-',"q2"),("q10",'a',"q11"),
                          ("q11",'d',"q3"),("q2",'#',"q4"),("q3",'o',"q5"),("q4",'o',"q8"),
                          ("q5",'r',"q7"),("q6",'r',"q10"),("q7",'#',"q0"),("q8",'p',"q9"),
                          ("q9",'e',"q6")],
        inicialD = "q1",
        finalD = ["q0"]
}

-- opbool = = | <= | not | and
respTestAFDmin_OPB = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14"],
        alfabetoD = "#<=Badlnopt",
        transicionesD = [("q1",'<',"q2"),("q1",'=',"q10"),("q1",'a',"q11"),("q1",'n',"q5"),
                         ("q10",'#',"q6"),("q11",'n',"q9"),("q12",'t',"q10"),("q13",'p',"q14"),
                         ("q14",'B',"q3"),("q2",'=',"q10"),("q3",'o',"q4"),("q4",'o',"q7"),
                         ("q5",'o',"q12"),("q6",'o',"q13"),("q7",'l',"q8"),("q8",'#',"q0"),
                         ("q9",'d',"q10")],
        inicialD = "q1", 
        finalD = ["q0"]
}

-- asignacion = (:=)
respTestAFDmin_ASIG = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8"], 
        alfabetoD = "#:=agis", 
        transicionesD = [("q1",':',"q2"),("q2",'=',"q3"),("q3",'#',"q4"),("q4",'a',"q5"),
                         ("q5",'s',"q6"),("q6",'i',"q7"),("q7",'g',"q8"),("q8",'#',"q0")],
        inicialD = "q1",
        finalD = ["q0"]
}

-- palabras reservadas = true | false | skip | if | then | else | while | do
respTestAFDmin_RES = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
                    "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
                    "q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35"],
        alfabetoD = "#Rabdefhiklnoprstuvw",
        transicionesD = [("q1",'d',"q2"),("q1",'e',"q17"),("q1",'f',"q11"),("q1",'i',"q32"),
                         ("q1",'s',"q33"),("q1",'t',"q34"),("q1",'w',"q35"),("q10",'a',"q16"),
                         ("q11",'a',"q17"),("q12",'a',"q19"),("q13",'a',"q22"),("q14",'a',"q26"),
                         ("q15",'a',"q27"),("q16",'l',"q12"),("q17",'l',"q24"),("q18",'l',"q3"),
                         ("q19",'b',"q20"),("q2",'o',"q7"),("q20",'r',"q13"),("q21",'r',"q25"),
                         ("q22",'R',"q4"),("q23",'s',"q5"),("q24",'s',"q3"),("q25",'v',"q14"),
                         ("q26",'d',"q15"),("q27",'#',"q0"),("q28",'u',"q3"),("q29",'n',"q7"),
                         ("q3",'e',"q7"),("q30",'i',"q9"),("q31",'i',"q18"),("q32",'f',"q7"),
                         ("q33",'k',"q30"),("q34",'h',"q6"),("q34",'r',"q28"),("q35",'h',"q31"),
                         ("q4",'e',"q23"),("q5",'e',"q21"),("q6",'e',"q29"),("q7",'#',"q8"),
                         ("q8",'p',"q10"),("q9",'p',"q7")], 
        inicialD = "q1",
        finalD = ["q0"]
}

-- delimitadores = ;
respTestAFDmin_DEL = AFD {
        estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14"],
        alfabetoD = "#;adeilmort",
        transicionesD = [("q1",';',"q2"),("q10",'i',"q12"),("q11",'i',"q13"),("q12",'m',"q11"),
                         ("q13",'t',"q14"),("q14",'a',"q3"),("q2",'#',"q4"),("q3",'d',"q5"),
                         ("q4",'d',"q8"),("q5",'o',"q6"),("q6",'r',"q7"),("q7",'#',"q0"),
                         ("q8",'e',"q9"),("q9",'l',"q10")], 
        inicialD = "q1",
        finalD = ["q0"]
}

-- ------------------------------------------------------------------------------
-- Función para la comprobación de que la respuesta generada coincide con
-- el resultado esperado.
-- ------------------------------------------------------------------------------
testAFDminEq :: AFD -> AFD -> Bool
testAFDminEq afd1 afd2 = afd1 == afd2

-- ------------------------------------------------------------------------------
-- Prueba para AFDmin de las categorias lexicas de IMP
-- ------------------------------------------------------------------------------
testIMP_AFDminEq :: [Bool]
testIMP_AFDminEq =
  [ 
    -- indentificadores
    testAFDminEq testAFDmin_ID respTestAFDmin_ID,
    -- enteros
    testAFDminEq testAFDmin_NUM  respTestAFDmin_NUM,
    -- operadores
    testAFDminEq testAFDmin_OP respTestAFDmin_OP,
    -- opbool
    testAFDminEq testAFDmin_OPB respTestAFDmin_OPB,
    -- asignaciones
    testAFDminEq testAFDmin_ASIG respTestAFDmin_ASIG,
    -- palabras reservadas
    testAFDminEq testAFDmin_RES respTestAFDmin_RES,
    -- Delimitadores
    testAFDminEq testAFDmin_DEL respTestAFDmin_DEL
  ]


-- ------------------------------------------------------------------------------
-- Comprobación de que la respuesta generada coincide con el resultado esperado.
-- ------------------------------------------------------------------------------
afd_Eq :: Bool
afd_Eq = and testIMP_AFDminEq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Ejecucion de  todas las pruebas
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
main :: IO ()
main = do
  -- REGEX --
  putStrLn "\n---- Pruebas Expresiones Regulares Aleatorias ----"
  quickCheckWith stdArgs { maxSuccess = 1000 } pruebasgetRegex
  
  -- AFNEp --
  putStrLn "---- Prueba AFNEp ----"
  print testAFNEp1
  putStrLn "\n---- Coincidencia de automata generado con automata esperado ----"
  print testEq_AFNEp

  -- AFN --
  putStrLn "\n---- Prueba AFNEp ----"
  print testAFN1
  putStrLn "\n---- Coincidencia de automata generado con automata esperado ----"
  print testEq_AFN

  -- AFD --
  putStrLn "\n---- Prueba AFD ----"
  testAFD2

  -- AFDmin --
  putStrLn "\n---- Eliminación de inalcanzables ----"
  testUnreachables4
  putStrLn "\n---- Grupos equivalentes ----"
  testGroups4
  putStrLn "\n---- AFD mínimo ----"
  testAFDmin4
  
  
  -- REGEX IMP --
  putStrLn "\n---- Expresiones Regulares IMP ----"
  putStrLn "\n---- Enteros ----"
  testNUM
  putStrLn "\n---- Operadores ----"
  testOP
  putStrLn "\n---- Operadores Booleanos ----"
  testOPB
  putStrLn "\n---- Asignacion ----"
  testASIG
  putStrLn "\n---- Palabras Reservadas ----"
  testRES
  putStrLn "\n---- Delimitadores ----"
  testDEL
  putStrLn "\n---- Identificadores ----"
  testID
  
  putStrLn "\n---- Coincidencia de REGEX en terminal con Regex esperadas en la solución ----"
  print regex_Eq
  
  {--
  -- AFNEp IMP --
  putStrLn "\n---- AFNEp para categorías léxicas de IMP ----"
  putStrLn "\n---- Identificadores ----"
  print testAFNEp_ID
  putStrLn "\n---- Enteros ----"
  print testAFNEP_NUM
  putStrLn "\n---- Operadores ----"
  print testAFNEp_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFNEp_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFNEp_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFNEp_RES
  putStrLn "\n---- Delimitadores ----"
  print testAFNEp_ESP


  -- AFN IMP --
  putStrLn "\n---- AFN para categorías léxicas de IMP ----"
  putStrLn "\n---- Identificadores ----"
  print testAFN_ID
  putStrLn "\n---- Enteros ----"
  print testAFN_Z
  putStrLn "\n---- Operadores ----"
  print testAFN_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFN_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFN_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFN_RES
  putStrLn "\n---- Delimitadores ----"
  print testAFN_DEL

  
  -- AFD IMP --
  putStrLn "\n---- AFD para categorías léxicas de IMP ----"
  putStrLn "\n---- Identificadores ----"
  print testAFD_ID
  putStrLn "\n---- Enteros ----"
  print testAFD_NUM
  putStrLn "\n---- Operadores ----"
  print testAFD_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFD_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFD_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFD_RES
  putStrLn "\n---- Delimitadores ----"
  print testAFD_DEL

--}
  -- AFDmin IMP --
  putStrLn "\n---- AFD minimizado para categorías léxicas de IMP ----"
  putStrLn "\n---- Enteros ----"
  print testAFDmin_NUM
  putStrLn "\n---- Operadores ----"
  print testAFDmin_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFDmin_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFDmin_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFDmin_RES
  putStrLn "\n---- Delimitadores ----"
  print testAFDmin_DEL
  putStrLn "\n---- Identificadores ----"
  print testAFDmin_ID

  putStrLn "\n---- Coincidencia de AFDmin generado con AFDmin esperado ----"
  print afd_Eq


  (cadena, tokens) <- procesarArchivo "../specs/IMP.md"

  let lenguajeR = getRegex cadena
  let mdd = getMDD cadena tokens

  putStrLn "El lenguaje regular resultante es:"
  print lenguajeR

  putStrLn "\nLa mdd resultante es:"
  print mdd

  let 
    cadena1 = "var1:=8;if+72"
    resultado1 = procesarTokens cadena1 mdd
    cadena2 = "skip;:=="
    resultado2 = procesarTokens cadena2 mdd
    cadena3 = "176+24-12+var27"
    resultado3 = procesarTokens cadena3 mdd
    cadena4 = "while:=not;var3andop"
    resultado4 = procesarTokens cadena3 mdd

  putStrLn "\nSe probara la cadena:" 
  print cadena1
  putStrLn "\nEl resultado fue:" 
  print resultado1

  putStrLn "\nSe probara la cadena:" 
  print cadena2
  putStrLn "\nEl resultado fue:" 
  print resultado2

  putStrLn "\nSe probara la cadena:" 
  print cadena3
  putStrLn "\nEl resultado fue:" 
  print resultado3

  putStrLn "\nSe probara la cadena:" 
  print cadena4
  putStrLn "\nEl resultado fue:" 
  print resultado4