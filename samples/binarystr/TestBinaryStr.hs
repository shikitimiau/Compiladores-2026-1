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
import Data.Char (chr) -- Actualmente usado para convertir nat a caracteres

import Text.Read (readMaybe)

archivoBinaryStr :: FilePath
archivoBinaryStr = "specs/BinaryStr.md"

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
--                             Pruebas BinaryStr
-- =============================================================================
-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- Expresiones regulares que representan las categorías léxicas
-- del lenguaje BinaryStr.
-- Los conjuntos utilizados en el lenguaje son:
--       A = {a,b,c,...,z,A,B,C,..,Z}
--       D = {0,1,2,...,9}
--       Z = {1,2,...,9}
--       B = {0,1}
-- ------------------------------------------------------------------------------
-- palabras reservadas = true | false | if | then | else | while
reservadas = "(true + false + if + then + else + while)"
-- id = A(A*)
identificador = "([['A'..'Z'] ++ ['a'..'z']]([['A'..'Z'] ++ ['a'..'z']]*))"
-- asignacion = (=)
asig = "(=)"
-- binario = B(B*)
binario = "['0','1']['0','1']*"
-- nat = 0 | ZD*
nat = "(0 + ['1'..'9']['0'..'9']*)"
-- funBool = eq? | prefix? | suffix? | subStr?
funBool = "(eq? + prefix? + suffix? + subStr?)"
-- opbool = < | > | == | >= | <=
opbool = "(< + > + == + >= + <=)"
-- funStr concat | norm | rev
funStr = "concat + norm + rev"
-- opStr & + ! + | + ||
opStr = "(& + ! + | + ||)"
-- funArith count1 | count0 | length | natValue
funArith = "count1 + count0 + length + natValue"
-- opArith = + | - | * | /
opArith = "((\\+)+(-)+(\\*)+(/))"


-- ------------------------------------------------------------------------------
-- Cadena que especifica la etiqueta de cada una de las categorías léxicas
-- que pertenecen al lenguaje BinaryStr.
-- Utilizamos el separador # para notificar donde inicia el identificador
-- de la etiqueta y donde termina.
-- ------------------------------------------------------------------------------
-- palabras reservadas = true | false | if | then | else | while
reservadas_etiq = reservadas ++ "#reserved#"
-- id = A(A*)
identificador_etiq = identificador ++ "#id#"
-- asignacion = (=)
asig_etiq = asig ++ "#asig#"
-- binario = B(B*)
binario_etiq = binario ++ "#binary#"
-- nat = 0 | ZD*
nat_etiq = nat ++ "#nat#"
-- funBool = eq? | prefix? | suffix? | subStr?
funBool_etiq = funBool ++ "#funBool#"
-- opbool = < | > | == | >= | <=
opbool_etiq = opbool ++ "#opbool#"
-- funStr concat | norm | rev
funStr_etiq = funStr ++ "#funStr#"
-- opStr & + ! + | + ||
opStr_etiq = opStr ++ "#opStr#"
-- funArith count1 | count0 | length | natValue
funArith_etiq = funArith ++ "#funArith#"
-- opArith = + | - | * | /
opArith_etiq = opArith ++ "#opArith#"



-- ------------------------------------------------------------------------------
-- Expresion regular que representa todo el lenguaje BinaryStr incluyendo la etiqueta
-- de las categorías léxicas que lo conforman.
-- ------------------------------------------------------------------------------
binaryStr = reservadas_etiq ++
      " + " ++ identificador_etiq ++
      " + "  ++ asig_etiq  ++ 
      " + " ++ binario_etiq ++
      " + "  ++ nat_etiq ++
      " + "  ++ funBool_etiq ++
      " + "  ++ opbool_etiq ++
      " + "  ++ funStr_etiq ++
      " + "  ++ opStr_etiq ++
      " + "  ++ funArith_etiq ++
      " + "  ++ opArith_etiq

-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de expresiones regulares de categorías léxicas del
-- lenguaje BinaryStr
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
testER :: String -> IO ()
testER s = print $ getRegex s 

-- palabras reservadas = true | false | if | then | else | while
testRES = testER reservadas_etiq
-- id = A(A*)
testID = testER identificador_etiq
-- asignacion = (=)
testASIG = testER asig_etiq
-- binario = B(B*)
testBIN = testER binario_etiq
-- nat = 0 | ZD*
testNAT = testER nat_etiq
-- funBool = eq? | prefix? | suffix? | subStr?
testFUNB = testER funBool_etiq
-- opbool = < | > | == | >= | <=
testOPB = testER opbool_etiq
-- funStr = concat | norm | rev
testFUNS = testER funStr_etiq
-- opStr = & | + | ! + | + ||
testOPS = testER opStr_etiq
-- funArith = count1 | count0 | length | natValue
testFUNA = testER funArith_etiq
-- opArith = + | - | * | /
testOPA = testER opArith_etiq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Resumen de las respuestas que se generan en las pruebas testER.
-- Cada respuesta se guarda en formato Regex para realizar las pruebas de AFNEp
-- para cada categoría léxica de BinaryStr sin que dependa de la recursión de resultados
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
-- Constantes de expresiones regulares en categorías léxicas de BinaryStr
-- Los conjuntos utilizados en el lenguaje son:
--       A = {a,b,c,...,z,A,B,C,..,Z}
--       D = {0,1,2,...,9}
--       Z = {1,2,...,9}
--       B = {0,1}
-- ---------------------------------------------------------------------
abecedario :: Regex
abecedario = unionChars (['A'..'Z']++['a'..'z'])

cero_nueve :: Regex
cero_nueve =  unionChars ['0'..'9']

uno_nueve :: Regex
uno_nueve = unionChars ['1'..'9']

binary ::Regex
binary = unionChars ['0','1']

-- ---------------------------------------------------------------------
-- Expresiones regulares por categoría léxica
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- palabras reservadas = true | false | if | then | else | while
-- ---------------------------------------------------------------------
resp_terminal_testRES = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'r')) (Symbol 'u')) (Symbol 'e')) (Union (Concat (Concat (Concat (Concat (Symbol 'f') (Symbol 'a')) (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Union (Concat (Symbol 'i') (Symbol 'f')) (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n')) (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e'))))))) (Symbol '#')) (Symbol 'r')) (Symbol 'e')) (Symbol 's')) (Symbol 'e')) (Symbol 'r')) (Symbol 'v')) (Symbol 'e')) (Symbol 'd')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (true + false + if + then + else + while)#reserved#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- else + while
elseYwhile = Union (concatChars "else") (concatChars "while")
-- then + else + while
thenYotras = Union (concatChars "then") elseYwhile
-- if + then + else + while
ifYotras = Union (concatChars "if") thenYotras
-- false + if + then + else + while
falseYotras = Union (concatChars "false") ifYotras
-- true + false + if + then + else + while
reservadasRegex = Union (concatChars "true") falseYotras
-- etiqueta para palabras reservadas
etiqRES = concatChars "#reserved#"
-- (true + false + if + then + else + while)#reserved#
reservadas_etiq1 = concatAll (concatRegex reservadasRegex ++ concatRegex etiqRES)
-- ---------------------------------------------------------------------
-- (true + false + if + then + else + while)#reserved#
respTestRES :: Regex
respTestRES = reservadas_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- id = A(A*)
-- ---------------------------------------------------------------------
resp_terminal_testID = "Concat (Concat (Concat (Concat (Concat (Union (Symbol 'A') (Union (Symbol 'B') (Union (Symbol 'C') (Union (Symbol 'D') (Union (Symbol 'E') (Union (Symbol 'F') (Union (Symbol 'G') (Union (Symbol 'H') (Union (Symbol 'I') (Union (Symbol 'J') (Union (Symbol 'K') (Union (Symbol 'L') (Union (Symbol 'M') (Union (Symbol 'N') (Union (Symbol 'O') (Union (Symbol 'P') (Union (Symbol 'Q') (Union (Symbol 'R') (Union (Symbol 'S') (Union (Symbol 'T') (Union (Symbol 'U') (Union (Symbol 'V') (Union (Symbol 'W') (Union (Symbol 'X') (Union (Symbol 'Y') (Union (Symbol 'Z') (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))))))))))))))))))))))))))))) (Star (Union (Symbol 'A') (Union (Symbol 'B') (Union (Symbol 'C') (Union (Symbol 'D') (Union (Symbol 'E') (Union (Symbol 'F') (Union (Symbol 'G') (Union (Symbol 'H') (Union (Symbol 'I') (Union (Symbol 'J') (Union (Symbol 'K') (Union (Symbol 'L') (Union (Symbol 'M') (Union (Symbol 'N') (Union (Symbol 'O') (Union (Symbol 'P') (Union (Symbol 'Q') (Union (Symbol 'R') (Union (Symbol 'S') (Union (Symbol 'T') (Union (Symbol 'U') (Union (Symbol 'V') (Union (Symbol 'W') (Union (Symbol 'X') (Union (Symbol 'Y') (Union (Symbol 'Z') (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))))))))))))))))))))))))))))))) (Symbol '#')) (Symbol 'i')) (Symbol 'd')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: [A][A]*#id#
-- Donde:
--       [A] = {a,b,c,...,z,A,B,C,...,Z}
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- [A][A]*
idRegex = Concat abecedario (Star abecedario)
-- etiqueta para identificadores
etiqID = concatChars "#id#"
-- [A][A]*#id#
identificador_etiq1 = concatAll (concatRegex idRegex ++ concatRegex etiqID)
-- ---------------------------------------------------------------------
-- [A][A]*#id#
respTestID :: Regex
respTestID = identificador_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- asignacion = (=)
-- ---------------------------------------------------------------------
resp_terminal_testASIG = "Concat (Concat (Concat (Concat (Concat (Concat (Symbol '=') (Symbol '#')) (Symbol 'a')) (Symbol 's')) (Symbol 'i')) (Symbol 'g')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (=)#asig#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- (=)#asig#
respTestASIG :: Regex
respTestASIG = concatChars "=#asig#"
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- binario = B(B*)
-- ---------------------------------------------------------------------
resp_terminal_testBIN = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '0') (Symbol '1')) (Star (Union (Symbol '0') (Symbol '1')))) (Symbol '#')) (Symbol 'b')) (Symbol 'i')) (Symbol 'n')) (Symbol 'a')) (Symbol 'r')) (Symbol 'y')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: [B][B]*#binary#
-- Donde:
--       [B] = {0,1}
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- [B][B]*
binarioRegex = Concat binary (Star binary)
-- etiqueta para binarios
etiqBIN = concatChars "#binary#"
-- [B][B]*#binary#
binario_etiq1 = concatAll (concatRegex binarioRegex ++ concatRegex etiqBIN)
-- ---------------------------------------------------------------------
-- [B][B]*#binary#
respTestBIN :: Regex
respTestBIN = binario_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- nat = 0 | ZD*
-- ---------------------------------------------------------------------
resp_terminal_testNAT = "Concat (Concat (Concat (Concat (Concat (Union (Symbol '0') (Concat (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9'))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9'))))))))))))) (Symbol '#')) (Symbol 'n')) (Symbol 'a')) (Symbol 't')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (0 + [Z][D]*)#nat#
-- Donde:
--       [Z] = {1,2,...,9}
--       [D] = {0,1,2,...,9}
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- [Z][D]*
zd = Concat uno_nueve (Star cero_nueve)
-- (0 + [Z][D]*)
natRegex = Union (Symbol '0') zd
-- etiqueta para nat
etiqNAT = concatChars "#nat#"
-- (0 + [Z][D]*)#nat#
nat_etiq1 = concatAll (concatRegex natRegex ++ concatRegex etiqNAT)
-- ---------------------------------------------------------------------
-- (0 + [Z][D]*)#nat#
respTestNAT :: Regex
respTestNAT = nat_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- funBool = eq? | prefix? | suffix? | subStr?
-- ---------------------------------------------------------------------
resp_terminal_testFUNB = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Concat (Concat (Symbol 'e') (Symbol 'q')) (Symbol '?')) (Union (Concat (Concat (Concat (Concat (Concat (Concat (Symbol 'p') (Symbol 'r')) (Symbol 'e')) (Symbol 'f')) (Symbol 'i')) (Symbol 'x')) (Symbol '?')) (Union (Concat (Concat (Concat (Concat (Concat (Concat (Symbol 's') (Symbol 'u')) (Symbol 'f')) (Symbol 'f')) (Symbol 'i')) (Symbol 'x')) (Symbol '?')) (Concat (Concat (Concat (Concat (Concat (Concat (Symbol 's') (Symbol 'u')) (Symbol 'b')) (Symbol 'S')) (Symbol 't')) (Symbol 'r')) (Symbol '?'))))) (Symbol '#')) (Symbol 'f')) (Symbol 'u')) (Symbol 'n')) (Symbol 'B')) (Symbol 'o')) (Symbol 'o')) (Symbol 'l')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (eq? + prefix? + suffix? + subStr?)#funBool#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- suffix? + subStr?
sufixYsubStr = Union (concatChars "suffix?") (concatChars "subStr?")
-- prefix? + suffix? + subStr?
prefixYotras = Union (concatChars "prefix?") sufixYsubStr
-- eq? + prefix? + suffix? + subStr?
funBoolRegex = Union (concatChars "eq?") prefixYotras
-- etiqueta para funBool
etiqFUNB = concatChars "#funBool#"
-- (eq? + prefix? + suffix? + subStr?)#funBool#
funBool_etiq1 = concatAll (concatRegex funBoolRegex ++ concatRegex etiqFUNB)
-- ---------------------------------------------------------------------
-- (eq? + prefix? + suffix? + subStr?)#funBool#
respTestFUNB :: Regex
respTestFUNB = funBool_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- opbool = < | > | == | >= | <=
-- ---------------------------------------------------------------------
resp_terminal_testOPB = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '<') (Union (Symbol '>') (Union (Concat (Symbol '=') (Symbol '=')) (Union (Concat (Symbol '>') (Symbol '=')) (Concat (Symbol '<') (Symbol '=')))))) (Symbol '#')) (Symbol 'o')) (Symbol 'p')) (Symbol 'b')) (Symbol 'o')) (Symbol 'o')) (Symbol 'l')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (< + > + == + >= + <=)#opbool#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- >= + <=
geqYleq = Union (concatChars ">=") (concatChars "<=")
-- == + >= + <=
compYotras = Union (concatChars "==") geqYleq
-- > + == + >= + <=
mayorYotras = Union (Symbol '>') compYotras
-- < + > + == + >= + <=
opBoolRegex = Union (Symbol '<') mayorYotras
-- etiqueta para opbool
etiqOPB = concatChars "#opbool#"
-- (< + > + == + >= + <=)#opbool#
opbool_etiq1 = concatAll (concatRegex opBoolRegex ++ concatRegex etiqOPB)
-- ---------------------------------------------------------------------
-- (< + > + == + >= + <=)#opbool#
respTestOPB :: Regex
respTestOPB = opbool_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- funStr = concat | norm | rev
-- ---------------------------------------------------------------------
resp_terminal_testFUNS = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Concat (Concat (Concat (Concat (Concat (Symbol 'c') (Symbol 'o')) (Symbol 'n')) (Symbol 'c')) (Symbol 'a')) (Symbol 't')) (Union (Concat (Concat (Concat (Symbol 'n') (Symbol 'o')) (Symbol 'r')) (Symbol 'm')) (Concat (Concat (Symbol 'r') (Symbol 'e')) (Symbol 'v')))) (Symbol '#')) (Symbol 'f')) (Symbol 'u')) (Symbol 'n')) (Symbol 'S')) (Symbol 't')) (Symbol 'r')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (concat + norm + rev)#funStr#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
funStrRegex = Union (concatChars "concat") (Union (concatChars "norm") (concatChars "rev"))
-- etiqueta para funStr
etiqFUNS = concatChars "#funStr#"
-- (concat + norm + rev)#funStr#
funStr_etiq1 = concatAll (concatRegex funStrRegex ++ concatRegex etiqFUNS)
-- ---------------------------------------------------------------------
-- (concat + norm + rev)#funStr#
respTestFUNS :: Regex
respTestFUNS = funStr_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- opStr = & + ! + | + ||
-- ---------------------------------------------------------------------
resp_terminal_testOPS = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '&') (Union (Symbol '!') (Union (Symbol '|') (Concat (Symbol '|') (Symbol '|'))))) (Symbol '#')) (Symbol 'o')) (Symbol 'p')) (Symbol 'S')) (Symbol 't')) (Symbol 'r')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (& + ! + | + ||)#opStr#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- | + ||
orYxor = Union (Symbol '|') (concatChars "||")
-- ! + | + ||
notYotros = Union (Symbol '!') orYxor
-- & + ! + | + ||
opStrRegex = Union (Symbol '&') notYotros
-- etiqueta para opStr
etiqOPS = concatChars "#opStr#"
-- (& + + + ! + + ||)#opStr#
opStr_etiq1 = concatAll (concatRegex opStrRegex ++ concatRegex etiqOPS)
-- ---------------------------------------------------------------------
-- (& + + + ! + + ||)#opStr#
respTestOPS :: Regex
respTestOPS = opStr_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- funArith = count1 | count0 | length | natValue
-- ---------------------------------------------------------------------
resp_terminal_testFUNA = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Concat (Concat (Concat (Concat (Concat (Symbol 'c') (Symbol 'o')) (Symbol 'u')) (Symbol 'n')) (Symbol 't')) (Symbol '1')) (Union (Concat (Concat (Concat (Concat (Concat (Symbol 'c') (Symbol 'o')) (Symbol 'u')) (Symbol 'n')) (Symbol 't')) (Symbol '0')) (Union (Concat (Concat (Concat (Concat (Concat (Symbol 'l') (Symbol 'e')) (Symbol 'n')) (Symbol 'g')) (Symbol 't')) (Symbol 'h')) (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Symbol 'n') (Symbol 'a')) (Symbol 't')) (Symbol 'V')) (Symbol 'a')) (Symbol 'l')) (Symbol 'u')) (Symbol 'e'))))) (Symbol '#')) (Symbol 'f')) (Symbol 'u')) (Symbol 'n')) (Symbol 'A')) (Symbol 'r')) (Symbol 'i')) (Symbol 't')) (Symbol 'h')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (count1 + count0 + length + natValue)#funArith#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- length + natValue
longYnatValue = Union (concatChars "length") (concatChars "natValue")
-- count0 + length + natValue
c0yotros = Union (concatChars "count0") longYnatValue
-- count1 + count0 + length + natValue
funArithRegex = Union (concatChars "count1") c0yotros
etiqFUNA = concatChars "#funArith#"
-- (count1 + count0 + length + natValue)#funArith#
funArith_etiq1 = concatAll (concatRegex funArithRegex ++ concatRegex etiqFUNA)
-- ---------------------------------------------------------------------
-- (count1 + count0 + length + natValue)#funArith#
respTestFUNA :: Regex
respTestFUNA = funArith_etiq1
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- opArith = + | - | * | /
-- ---------------------------------------------------------------------
resp_terminal_testOPA = "Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Concat (Union (Symbol '+') (Union (Symbol '-') (Union (Symbol '*') (Symbol '/')))) (Symbol '#')) (Symbol 'o')) (Symbol 'p')) (Symbol 'A')) (Symbol 'r')) (Symbol 'i')) (Symbol 't')) (Symbol 'h')) (Symbol '#')"
-- ---------------------------------------------------------------------
-- Resumen de la respuesta: (+ + - + * + /)#opArith#
-- ---------------------------------------------------------------------
-- Interpretación en tipo Regex:
-- * | /
multYdiv = Union (Symbol '*') (Symbol '/')
-- - | * | /
restYotros = Union (Symbol '-') multYdiv
-- + | - | * | /
opArithRegex = Union (Symbol '+') restYotros
-- etiqueta para opArith
etiqOPA = concatChars "#opArith#"
-- (+ + - + * + /)#opArith#
opArith_etiq1 = concatAll (concatRegex opArithRegex ++ concatRegex etiqOPA)
-- ---------------------------------------------------------------------
-- (+ + - + * + /)#opArith#
respTestOPA :: Regex
respTestOPA = opArith_etiq1
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Comprobación de que la respuesta en terminal genera la misma Regex
-- que la interpretación de tipo Regex que fue asociada
-- ---------------------------------------------------------------------
testRegexEq :: String -> Regex -> Bool
testRegexEq str regex = (read str :: Regex) == regex

-- ---------------------------------------------------------------------
-- Prueba para Regex de las categorias lexicas de BinaryStr
-- ---------------------------------------------------------------------
testBinaryStrRegexEq :: [Bool]
testBinaryStrRegexEq =
  [ 
    -- palabras reservadas
    testRegexEq resp_terminal_testRES respTestRES,
    -- identificadores
    testRegexEq resp_terminal_testID respTestID,
    -- asignaciones
    testRegexEq resp_terminal_testASIG respTestASIG,
    -- binarios
    testRegexEq resp_terminal_testBIN respTestBIN,
    -- naturales
    testRegexEq resp_terminal_testNAT respTestNAT,
    -- funciones booleanas
    testRegexEq resp_terminal_testFUNB respTestFUNB,
    -- operadores booleanos
    testRegexEq resp_terminal_testOPB respTestOPB,
    -- funciones sobre cadenas
    testRegexEq resp_terminal_testFUNS respTestFUNS,
    -- operadores sobre cadenas
    testRegexEq resp_terminal_testOPS respTestOPS,
    -- funciones aritméticas
    testRegexEq resp_terminal_testFUNA respTestFUNA,
    -- operadores aritméticos
    testRegexEq resp_terminal_testOPA respTestOPA
  ]

-- ---------------------------------------------------------------------
-- Comprobación de que todas las Regex equivalen a la BinaryStrresion 
-- en terminal
-- ---------------------------------------------------------------------
regex_Eq :: Bool
regex_Eq = and testBinaryStrRegexEq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFNEp para expresiones regulares del lenguaje BinaryStr
-- utiliza la función definida en AFNEp: expr_to_AFNEp :: Regex -> AFNEp
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- palabras reservadas = true | false | if | then | else | while
testAFNEp_RES = expr_to_AFNEp respTestRES
-- id = A(A*)
testAFNEp_ID = expr_to_AFNEp respTestID
-- asignacion = (=)
testAFNEp_ASIG = expr_to_AFNEp respTestASIG
-- binario = B(B*)
testAFNEp_BIN = expr_to_AFNEp respTestBIN
-- nat = 0 | ZD*
testAFNEp_NAT = expr_to_AFNEp respTestNAT
-- funBool = eq? | prefix? | suffix? | subStr?
testAFNEp_FUNB = expr_to_AFNEp respTestFUNB
-- opbool = < | > | == | >= | <=
testAFNEp_OPB = expr_to_AFNEp respTestOPB
-- funStr = concat | norm | rev
testAFNEp_FUNS = expr_to_AFNEp respTestFUNS
-- opStr = & + ! + | + ||
testAFNEp_OPS = expr_to_AFNEp respTestOPS
-- funArith = count1 | count0 | length | natValue
testAFNEp_FUNA = expr_to_AFNEp respTestFUNA
-- opArith = + | - | * | /
testAFNEp_OPA = expr_to_AFNEp respTestOPA


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFN con base a AFNEp generados en el lenguaje BinaryStr
-- utiliza la función definida en AFN: afnEp_to_AFN :: AFNEp -> AFN
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- palabras reservadas = true | false | if | then | else | while
testAFN_RES = afnEp_to_AFN testAFNEp_RES
-- id = A(A*)
testAFN_ID = afnEp_to_AFN testAFNEp_ID
-- asignacion = (=)
testAFN_ASIG = afnEp_to_AFN testAFNEp_ASIG
-- binario = B(B*)
testAFN_BIN = afnEp_to_AFN testAFNEp_BIN
-- nat = 0 | ZD*
testAFN_NAT = afnEp_to_AFN testAFNEp_NAT
-- funBool = eq? | prefix? | suffix? | subStr?
testAFN_FUNB = afnEp_to_AFN testAFNEp_FUNB
-- opbool = < | > | == | >= | <=
testAFN_OPB = afnEp_to_AFN testAFNEp_OPB
-- funStr = concat | norm | rev
testAFN_FUNS = afnEp_to_AFN testAFNEp_FUNS
-- opStr = & + ! + ||
testAFN_OPS = afnEp_to_AFN testAFNEp_OPS
-- funArith = count1 | count0 | length | natValue
testAFN_FUNA = afnEp_to_AFN testAFNEp_FUNA
-- opArith = + | - | * | /
testAFN_OPA = afnEp_to_AFN testAFNEp_OPA


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFD con base a AFN generados en el lenguaje BinaryStr
-- utiliza la función definida en AFD: afn_to_AFD :: AFN -> AFD
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- palabras reservadas = true | false | if | then | else | while
testAFD_RES = afn_to_AFD testAFN_RES
-- id = A(A*)
testAFD_ID = afn_to_AFD testAFN_ID
-- asignacion = (=)
testAFD_ASIG = afn_to_AFD testAFN_ASIG
-- binario = B(B*)
testAFD_BIN = afn_to_AFD testAFN_BIN
-- nat = 0 | ZD*
testAFD_NAT = afn_to_AFD testAFN_NAT
-- funBool = eq? | prefix? | suffix? | subStr?
testAFD_FUNB = afn_to_AFD testAFN_FUNB
-- opbool = < | > | == | >= | <=
testAFD_OPB = afn_to_AFD testAFN_OPB
-- funStr = concat | norm | rev
testAFD_FUNS = afn_to_AFD testAFN_FUNS
-- opStr = & + ! + | + ||
testAFD_OPS = afn_to_AFD testAFN_OPS
-- funArith = count1 | count0 | length | natValue
testAFD_FUNA = afn_to_AFD testAFN_FUNA
-- opArith = + | - | * | /
testAFD_OPA = afn_to_AFD testAFN_OPA


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFDmin con base a AFD generados en el lenguaje BinaryStr
-- utiliza la función definida en AFDmin: minimizaAFD :: AFD -> AFD
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- palabras reservadas = true | false | if | then | else | while
testAFDmin_RES = minimizaAFD testAFD_RES
-- id = A(A*)
testAFDmin_ID = minimizaAFD testAFD_ID
-- asignacion = (=)
testAFDmin_ASIG = minimizaAFD testAFD_ASIG
-- binario = B(B*)
testAFDmin_BIN = minimizaAFD testAFD_BIN
-- nat = 0 | ZD*
testAFDmin_NAT = minimizaAFD testAFD_NAT
-- funBool = eq? | prefix? | suffix? | subStr?
testAFDmin_FUNB = minimizaAFD testAFD_FUNB
-- opbool = < | > | == | >= | <=
testAFDmin_OPB = minimizaAFD testAFD_OPB
-- funStr = concat | norm | rev
testAFDmin_FUNS = minimizaAFD testAFD_FUNS
-- opStr = & + ! + | + ||
testAFDmin_OPS = minimizaAFD testAFD_OPS
-- funArith = count1 | count0 | length | natValue
testAFDmin_FUNA = minimizaAFD testAFD_FUNA
-- opArith = + | - | * | /
testAFDmin_OPA = minimizaAFD testAFD_OPA


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

-- palabras reservadas = true | false | if | then | else | while
respTestAFDmin_RES = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23"], 
  alfabetoD = "#adefhilnrstuvw",
  transicionesD = [("q1",'e',"q2"),("q1",'f',"q16"),("q1",'i',"q20"),("q1",'t',"q21"),
                   ("q1",'w',"q22"),("q10",'e',"q18"),("q11",'s',"q7"),("q12",'s',"q10"),
                   ("q13",'v',"q8"),("q14",'d',"q15"),("q15",'#',"q0"),("q16",'a',"q2"),
                   ("q17",'u',"q10"),("q18",'#',"q4"),("q19",'n',"q18"),("q2",'l',"q12"),
                   ("q20",'f',"q18"),("q21",'h',"q9"),("q21",'r',"q17"),("q22",'h',"q23"),
                   ("q23",'i',"q3"),("q3",'l',"q10"),("q4",'r',"q6"),("q5",'r',"q13"),
                   ("q6",'e',"q11"),("q7",'e',"q5"),("q8",'e',"q14"),("q9",'e',"q19")],
  inicialD = "q1",
  finalD = ["q0"]
}

-- id = A(A*)
respTestAFDmin_ID = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5"],
  alfabetoD = "#ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
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
                   ("q2",'#',"q5"),("q2",'A',"q2"),("q2",'B',"q2"),("q2",'C',"q2"),
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
                   ("q2",'z',"q2"),("q3",'d',"q4"),("q4",'#',"q0"),("q5",'i',"q3")],
  inicialD = "q1",
  finalD = ["q0"]
}

-- asignacion = (=)
respTestAFDmin_ASIG = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7"],
  alfabetoD = "#=agis",
  transicionesD = [("q1",'=',"q2"),("q2",'#',"q3"),("q3",'a',"q4"),("q4",'s',"q5"),
                   ("q5",'i',"q6"),("q6",'g',"q7"),("q7",'#',"q0")],
  inicialD = "q1",
  finalD = ["q0"]
}

-- binario = B(B*)
respTestAFDmin_BIN = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"],
  alfabetoD = "#01abinry",
  transicionesD = [("q1",'0',"q2"),("q1",'1',"q2"),("q2",'#',"q5"),("q2",'0',"q2"),
                   ("q2",'1',"q2"),("q3",'y',"q4"),("q4",'#',"q0"),("q5",'b',"q6"),
                   ("q6",'i',"q7"),("q7",'n',"q8"),("q8",'a',"q9"),("q9",'r',"q3")],
  inicialD = "q1",
  finalD = ["q0"]
}

-- nat = 0 | ZD*
respTestAFDmin_NAT = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7"],
  alfabetoD = "#0123456789ant",
  transicionesD = [("q1",'0',"q2"),("q1",'1',"q3"),("q1",'2',"q3"),("q1",'3',"q3"),
                   ("q1",'4',"q3"),("q1",'5',"q3"),("q1",'6',"q3"),("q1",'7',"q3"),
                   ("q1",'8',"q3"),("q1",'9',"q3"),("q2",'#',"q4"),("q3",'#',"q4"),
                   ("q3",'0',"q3"),("q3",'1',"q3"),("q3",'2',"q3"),("q3",'3',"q3"),
                   ("q3",'4',"q3"),("q3",'5',"q3"),("q3",'6',"q3"),("q3",'7',"q3"),
                   ("q3",'8',"q3"),("q3",'9',"q3"),("q4",'n',"q5"),("q5",'a',"q6"),
                   ("q6",'t',"q7"),("q7",'#',"q0")],
  inicialD = "q1",
  finalD = ["q0"]
}

-- funBool = eq? | prefix? | suffix? | subStr?
respTestAFDmin_FUNB = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22"], 
  alfabetoD = "#?BSbefilnopqrstux",
  transicionesD = [("q1",'e',"q2"),("q1",'p',"q14"),("q1",'s',"q7"),("q10",'o',"q11"),
                   ("q11",'o',"q12"),("q12",'l',"q13"),("q13",'#',"q0"),("q14",'r',"q18"),
                   ("q15",'r',"q17"),("q16",'t',"q15"),("q17",'?',"q3"),("q18",'e',"q5"),
                   ("q19",'i',"q20"),("q2",'q',"q17"),("q20",'x',"q17"),("q21",'b',"q22"),
                   ("q21",'f',"q5"),("q22",'S',"q16"),("q3",'#',"q4"),("q4",'f',"q6"),
                   ("q5",'f',"q19"),("q6",'u',"q8"),("q7",'u',"q21"),("q8",'n',"q9"),
                   ("q9",'B',"q10")], 
  inicialD = "q1", 
  finalD = ["q0"]
}

-- opbool = < | > | == | >= | <=
respTestAFDmin_OPB = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11"],
  alfabetoD = "#<=>blop",
  transicionesD = [("q1",'<',"q2"),("q1",'=',"q6"),("q1",'>',"q2"),("q10",'p',"q11"),
                   ("q11",'b',"q8"),("q2",'#',"q7"),("q2",'=',"q5"),("q3",'l',"q4"),
                   ("q4",'#',"q0"),("q5",'#',"q7"),("q6",'=',"q5"),("q7",'o',"q10"),
                   ("q8",'o',"q9"),("q9",'o',"q3")], 
  inicialD = "q1",
  finalD = ["q0"]
}

-- funStr = concat | norm | rev
respTestAFDmin_FUNS = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19"],
  alfabetoD = "#Sacefmnortuv",
  transicionesD = [("q1",'c',"q2"),("q1",'n',"q3"),("q1",'r',"q15"),("q10",'#',"q17"),
                   ("q11",'n',"q13"),("q12",'n',"q19"),("q13",'c',"q14"),("q14",'a',"q5"),
                   ("q15",'e',"q16"),("q16",'v',"q10"),("q17",'f',"q18"),("q18",'u',"q12"),
                   ("q19",'S',"q4"),("q2",'o',"q11"),("q3",'o',"q7"),("q4",'t',"q6"),
                   ("q5",'t',"q10"),("q6",'r',"q8"),("q7",'r',"q9"),("q8",'#',"q0"),
                   ("q9",'m',"q10")], 
  inicialD = "q1", 
  finalD = ["q0"]
}

-- opStr = & | + | ! + | + ||
respTestAFDmin_OPS = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"], 
  alfabetoD = "!#&Soprt|", 
  transicionesD = [("q1",'!',"q2"),("q1",'&',"q2"),("q1",'|',"q4"),("q2",'#',"q5"),
                   ("q3",'#',"q0"),("q4",'#',"q5"),("q4",'|',"q2"),("q5",'o',"q6"),
                   ("q6",'p',"q7"),("q7",'S',"q8"),("q8",'t',"q9"),("q9",'r',"q3")],
  inicialD = "q1",
  finalD = ["q0"]
}

-- funArith = count1 | count0 | length | natValue
respTestAFDmin_FUNA = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
              "q24","q25","q26","q27","q28"], 
  alfabetoD = "#01AVacefghilnortu",
  transicionesD = [("q1",'c',"q2"),("q1",'l',"q21"),("q1",'n',"q25"),("q10",'n',"q16"),
                   ("q11",'A',"q12"),("q12",'r',"q13"),("q13",'i',"q14"),("q14",'t',"q18"),
                   ("q15",'t',"q19"),("q16",'t',"q24"),("q17",'t',"q27"),("q18",'h',"q20"),
                   ("q19",'h',"q3"),("q2",'o',"q6"),("q20",'#',"q0"),("q21",'e',"q9"),
                   ("q22",'e',"q3"),("q23",'g',"q15"),("q24",'0',"q3"),("q24",'1',"q3"),
                   ("q25",'a',"q17"),("q26",'a',"q28"),("q27",'V',"q26"),("q28",'l',"q7"),
                   ("q3",'#',"q4"),("q4",'f',"q5"),("q5",'u',"q8"),("q6",'u',"q10"),
                   ("q7",'u',"q22"),("q8",'n',"q11"),("q9",'n',"q23")],
    inicialD = "q1",
    finalD = ["q0"]
}

-- opArith = + | - | * | /
respTestAFDmin_OPA = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10"], 
  alfabetoD = "#*+-/Ahioprt", 
  transicionesD = [("q1",'*',"q2"),("q1",'+',"q2"),("q1",'-',"q2"),("q1",'/',"q2"),
                   ("q10",'i',"q3"),("q2",'#',"q6"),("q3",'t',"q4"),("q4",'h',"q5"),
                   ("q5",'#',"q0"),("q6",'o',"q7"),("q7",'p',"q8"),("q8",'A',"q9"),
                   ("q9",'r',"q10")], 
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
-- Prueba para AFDmin de las categorias lexicas de BinaryStr
-- ------------------------------------------------------------------------------
testBinaryStr_AFDminEq :: [Bool]
testBinaryStr_AFDminEq =
  [ 
    -- palabras reservadas
    testAFDminEq testAFDmin_RES respTestAFDmin_RES,
    -- identificadores
    testAFDminEq testAFDmin_ID respTestAFDmin_ID,
    -- asignaciones
    testAFDminEq testAFDmin_ASIG respTestAFDmin_ASIG,
    -- binario
    testAFDminEq testAFDmin_BIN respTestAFDmin_BIN,
    -- nat
    testAFDminEq testAFDmin_NAT respTestAFDmin_NAT,
    -- funciones booleanas
    testAFDminEq testAFDmin_FUNB respTestAFDmin_FUNB,
    -- operadores booleanos
    testAFDminEq testAFDmin_OPB respTestAFDmin_OPB,
    -- funciones de cadenas
    testAFDminEq testAFDmin_FUNS respTestAFDmin_FUNS,
    -- operadores de cadenas
    testAFDminEq testAFDmin_OPS respTestAFDmin_OPS,
    -- funciones aritméticas
    testAFDminEq testAFDmin_FUNA respTestAFDmin_FUNA,
    -- operadores aritméticos
    testAFDminEq testAFDmin_OPA respTestAFDmin_OPA
  ]


-- ------------------------------------------------------------------------------
-- Comprobación de que la respuesta generada coincide con el resultado esperado.
-- ------------------------------------------------------------------------------
afd_Eq :: Bool
afd_Eq = and testBinaryStr_AFDminEq


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
  
  
  -- REGEX BinaryStr --
  putStrLn "\n---- Expresiones Regulares BinaryStr ----"
  putStrLn "\n---- Palabras Reservadas ----"
  testRES
  putStrLn "\n---- Asignacion ----"
  testASIG
  putStrLn "\n---- Binario ----"
  testBIN
  putStrLn "\n---- Nat ----"
  testNAT
  putStrLn "\n---- Funciones Booleanas ----"
  testFUNB
  putStrLn "\n---- Operadores Booleanos ----"
  testOPB
  putStrLn "\n---- Funciones de Cadenas ----"
  testFUNS
  putStrLn "\n---- Operadores de Cadenas ----"
  testOPS
  putStrLn "\n---- Funciones Aritméticas ----"
  testFUNA
  putStrLn "\n---- Operadores Aritméticos ----"
  testOPA
  putStrLn "\n---- Identificadores ----"
  testID
  
  putStrLn "\n---- Coincidencia de REGEX en terminal con Regex esperadas en la solución ----"
  print regex_Eq
  
  -- AFDmin BinaryStr --
  putStrLn "\n---- AFD minimizado para categorías léxicas de BinaryStr ----"
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFDmin_RES
  putStrLn "\n---- Asignacion ----"
  print testAFDmin_ASIG
  putStrLn "\n---- Binario ----"
  print testAFDmin_BIN
  putStrLn "\n---- Nat ----"
  print testAFDmin_NAT
  putStrLn "\n---- Funciones Booleanas ----"
  print testAFDmin_FUNB
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFDmin_OPB
  putStrLn "\n---- Funciones de Cadenas ----"
  print testAFDmin_FUNS
  putStrLn "\n---- Operadores de Cadenas ----"
  print testAFDmin_OPS
  putStrLn "\n---- Funciones Aritméticas ----"
  print testAFDmin_FUNA
  putStrLn "\n---- Operadores Aritméticos ----"
  print testAFDmin_OPA
  putStrLn "\n---- Identificadores ----"
  print testAFDmin_ID

  putStrLn "\n---- Coincidencia de AFDmin generado con AFDmin esperado ----"
  print afd_Eq

  (cadena, tokens) <- procesarArchivo archivoBinaryStr

  let lenguajeR = getRegex cadena
  let mdd = getMDD cadena tokens

  putStrLn "El lenguaje regular resultante es:"
  print lenguajeR

  putStrLn "\nLa MDD resultante es:"
  print mdd

  let 
    cadena1 = "while01110&101010=9"
    resultado1 = procesarTokens cadena1 mdd
    cadena2 = "natValue=13/04/2023"
    resultado2 = procesarTokens cadena2 mdd
    cadena3 = "eq?noes=110<=111*"
    resultado3 = procesarTokens cadena3 mdd
    cadena4 = "while==norm=rev!"
    resultado4 = procesarTokens cadena4 mdd

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
