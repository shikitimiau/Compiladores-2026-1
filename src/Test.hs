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

import Test.QuickCheck -- Para hacer pruebas con aleatorieidad
import Control.Exception (try, evaluate, SomeException) -- Para manejar las excepciones esperadas en las pruebas
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
testAFD3 :: IO ()
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
-- Regex que representa el lenguaje IMP sin espacios
-- ------------------------------------------------------------------------------
imp = "([c|c<-['a'..'z']++['A'..'Z']]([c|c<-['a'..'z']++['A'..'Z']]*)['0'..'9']*)+((0)+(['1'..'9']['0'..'9']*)+(-['1'..'9']['0'..'9']*))+((\\+)+(-)+(\\*)+(/))+((<)+(>)+(=))+((:=))+((if)+(then)+(else)+(while)+(do))+((;)+(\\+))"


-- ------------------------------------------------------------------------------
-- Regex que representa el lenguaje IMP con espacios
-- ------------------------------------------------------------------------------
imp2 = "([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)"
        ++ "+ ((0) + (['1'..'9']['0'..'9']*) + (-['1'..'9']['0'..'9']*))"
        ++ "+ ((\\+) + (-) + (\\*) + (/))"
        ++ "+ ((<) + (>) + (=))"
        ++ "+ ((:=))"
        ++ "+ ((if) + (then) + (else) + (while) + (do))"
        ++ "+ ((;) + (\\+))"


-- ------------------------------------------------------------------------------
-- Expresiones regulares que representan las categorías léxicas
-- del lenguaje IMP con espacios
-- ------------------------------------------------------------------------------
-- id = A(A*)D*
identificadores = "([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)"
-- enteros = 0 + ZD* + (-ZD*)
enteros = "((0) + (['1'..'9']['0'..'9']*) + (-['1'..'9']['0'..'9']*))"
-- op = (+)+(-)+(*)+(/)
operadores = "((\\+) + (-) + (\\*) + (/))"
-- opbool = (<) + (>) + (=)
opbool = "((<) + (>) + (=))"
-- asignacion = :=
asig = "((:=))"
-- palabras reservadas = if + then  +  else + while + do
reservadas = "((if) + (then) + (else) + (while) + (do))"
-- simbolos especiales = ; + +
especiales = "((;) + (\\+))"

-- ------------------------------------------------------------------------------
-- Regex de IMP con terminación de etiqueta #E y espacios
-- --------------------
-- | Categoría Léxica |
-- |------------------|
-- | identificadores  |
-- | enteros          |
-- | operadores       |
-- | opbool           |
-- | asignacion       |
-- | reservadas       |
-- | especiales       |
-- --------------------
-- ------------------------------------------------------------------------------
imp3 = "([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)#IDENTIFICADOR#"
        ++ " + ((0) + (['1'..'9']['0'..'9']*) + (-['1'..'9']['0'..'9']*))#ENTERO#"
        ++ " + ((\\+) + (-) + (\\*) + (/))#OPERADOR#"
        ++ " + ((<) + (>) + (=))#OPBOOL#"
        ++ " + ((:=))#ASIG#"
        ++ " + ((if) + (then) + (else) + (while) + (do))#RESERVADA#"
        ++ " + ((;) + (\\+))#ESPECIAL#"



-- ------------------------------------------------------------------------------
-- Expresiones regulares que representan las categorías léxicas
-- del lenguaje IMP con espacios y etiquetas #E
-- -------------------------------
-- | Etiqueta | Categoría Léxica |
-- |----------|------------------|
-- |     I    | identificadores  |
-- |     Z    | enteros          |
-- |     O    | operadores       |
-- |     B    | opbool           |
-- |     A    | asignacion       |
-- |     R    | reservadas       |
-- |     E    | especiales       |
-- -------------------------------
-- ------------------------------------------------------------------------------
-- id = A(A*)D* ------- etiqueta I
idE = "(([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)#I)"
-- enteros = 0 + ZD* + (-ZD*) ------- etiqueta Z
intE = "(((0) + (['1'..'9']['0'..'9']*) + (-['1'..'9']['0'..'9']*))#Z)"
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
opE = "(((\\+) + (-) + (\\*) + (/))#O)"
-- opbool = (<) + (>) + (=) ------- etiqueta B
opboolE = "(((<) + (>) + (=))#B)"
-- asignacion = := ------- etiqueta A
asigE = "(((:=))#A)"
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
reservadasE = "(((if) + (then) + (else) + (while) + (do))#R)"
-- simbolos especiales = ; + + ------- etiqueta E
especialesE = "(((;) + (\\+))#E)"



-- ------------------------------------------------------------------------------
-- Cadenas para generar Regex con conflicto por usar alfabetos cuya intersección
-- es distinta al vacío: 
-- conflictoSuma: operadores + especiales
-- conflictoABC: identificadores + reservadas
-- conflictoIg: opBool + asig
-- ------------------------------------------------------------------------------
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
conflictoSuma = opE ++ "+" ++ especialesE
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
conflictoABC = idE ++ "+" ++ reservadasE
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
conflictoIg = opboolE ++ "+" ++ asigE

-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- > "#(([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)#I)#+#(((if) + (then) + (else) + (while) + (do))#R)#"
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
conflictoABCseccionado = "#" ++ idE ++ "#+#" ++ reservadasE ++ "#"



-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de expresiones regulares de categorías léxicas del
-- lenguaje IMP
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
testERIMP :: String -> IO ()
testERIMP s = print $ getRegex s 


-- id = A(A*)D* ------- etiqueta I
testID = testERIMP idE
-- enteros = 0 + ZD* + (-ZD*) ------- etiqueta Z
testZ = testERIMP intE
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
testOP = testERIMP opE
-- opbool = (<) + (>) + (=) ------- etiqueta B
testOPB = testERIMP opboolE
-- asignacion = := ------- etiqueta A
testASIG = testERIMP asigE
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testRES = testERIMP reservadasE
-- simbolos especiales = ; + + ------- etiqueta E
testESP = testERIMP especialesE


-- ------------------------------------------------------------------------------
-- Regex que generan conflictos por alfabetos con intersección no vacía 
-- conflictoSuma: operadores + especiales
-- conflictoABC: identificadores + reservadas
-- conflictoIg: opBool + asig
-- ------------------------------------------------------------------------------
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
testConflictoSuma = testERIMP conflictoSuma
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testConflictoABC = testERIMP conflictoABC
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
testConflictoIg = testERIMP conflictoIg


-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- > "#(([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)#I)#+#(((if) + (then) + (else) + (while) + (do))#R)#"
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testconflictoABCseccionado = testERIMP conflictoABCseccionado

-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Resumen de las respuestas que se generan en las pruebas testERIMP.
-- Cada respuesta se guarda en formato Regex para realizar las pruebas de AFNEp
-- para cada categoría léxica de IMP sin que dependa de la recursión de resultados
-- anteriores. El objetivo de guardar los resultados es evitar tiempos de espera 
-- demasiado largos para pruebas que ya se ha comprobado que son correctos.
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Constantes de expresiones regulares en categorías léxicas de IMP
-- ---------------------------------------------------------------------
abecedario :: Regex
abecedario = foldr1 Union (map Symbol ['a'..'z'])

cero_nueve :: Regex
cero_nueve = foldr1 Union (map Symbol ['0'..'9'])

uno_nueve :: Regex
uno_nueve = foldr1 Union (map Symbol ['1'..'9'])


-- ---------------------------------------------------------------------
-- Expresiones regulares por categoría léxica
-- ---------------------------------------------------------------------

-- Identificadores: A(A*)D* ------- etiqueta I
resp_terminal_testID = "Concat (Concat (Concat (Concat (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))) (Star (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))))) (Symbol '#')) (Symbol 'I')"
{--
-- Resumen de la respuesta en terminal.
-- Usamos la notación:
--       [A] = {a,b,c,...,z}
--       [D] = {0,1,2,...,9}
--
--[A][A]*[D]*#I
Concat 
      -- [A][A]*[D]*#
      (Concat 
              -- [A][A]*[D]*
             (Concat
                    -- [A][A]*
                    (Concat (A) (Star (A)))
                    -- [D]*
                    (Star (D))
              )
              -- #
              (Symbol '#')
        )
-- I
(Symbol 'I')
--}
-- Interpretación en tipo Regex:
respTestID :: Regex
respTestID =
        -- [A][A]*[D]*#I
        Concat
                -- [A][A]*[D]*#
                (Concat 
                        -- [A][A]*[D]*
                        (Concat
                                -- [A][A]*
                                (Concat abecedario (Star abecedario))
                                -- [D]*
                                (Star cero_nueve)
                        )
                        -- #
                        (Symbol '#')
                )
                -- I
                (Symbol 'I')


-- Enteros: 0 + ZD* + (-ZD*) ------- etiqueta Z
resp_terminal_testZ = "Concat (Concat (Union (Symbol '0') (Union (Concat (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9'))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))))) (Concat (Concat (Symbol '-') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))))))) (Symbol '#')) (Symbol 'Z')"
{--
-- Resumen de la respuesta.
-- Usamos la notación:
--       [Z] = {1,2,3,...,9}
--       [D] = {0,1,2,...,9}
--
-- ( 0 + [Z][D]* + (-[Z][D]*))#Z
  Concat 
        -- ( 0 + [Z][D]* + (-[Z][D]*))#
        (Concat
                -- 0 + [Z][D]* + (-[Z][D]*)
                (Union (Symbol '0')
                        -- [Z][D]* + (-[Z][D]*)
                       (Union (Concat (Z) (Star (D)))
                              -- (-[Z][D]*)
                              (Concat 
                                    -- - [Z]
                                    (Concat (Symbol '-') (Z))
                                    -- [D]* 
                                    (Star (D))
                              )
                        )
                )
                -- #
                (Symbol '#')
        )
        -- Z
        (Symbol 'Z')
--}
-- Interpretación en tipo Regex:
respTestZ :: Regex
respTestZ =
-- ( 0 + [Z][D]* + (-[Z][D]*))#Z
  Concat
        -- ( 0 + [Z][D]* + (-[Z][D]*))#
        (Concat
                -- 0 + [Z][D]* + (-[Z][D]*)
                (Union (Symbol '0')
                        -- [Z][D]* + (-[Z][D]*)
                       (Union (Concat uno_nueve (Star cero_nueve))
                              -- (-[Z][D]*)
                              (Concat 
                                    -- - [Z]
                                    (Concat (Symbol '-') uno_nueve)
                                    -- [D]* 
                                    (Star cero_nueve)
                              )
                        )
                )
                -- #
                (Symbol '#')
        )
        -- Z
        (Symbol 'Z')


-- Operadores: (+)+(-)+(*)+(/) ------- etiqueta O
resp_terminal_testOP = "Concat (Concat (Union (Symbol '+') (Union (Symbol '-') (Union (Symbol '*') (Symbol '/')))) (Symbol '#')) (Symbol 'O')"
{--
-- Resumen de la respuesta:
-- ((+) + (-) + (*) + (/))#O
Concat
      -- ((+) + (-) + (*) + (/))#
      (Concat 
              -- (+) + (-) + (*) + (/)
              (Union (Symbol '+')
                      -- (-) + (*) + (/)
                     (Union (Symbol '-')
                            -- (*)+(/)
                            (Union (Symbol '*') (Symbol '/'))
                      )
              )
              -- #
              (Symbol '#'))
      -- O
      (Symbol 'O')
--}
-- Interpretación en tipo Regex:
respTestOP :: Regex
respTestOP = 
        -- ((+) + (-) + (*) + (/))#O
        Concat
                -- ((+) + (-) + (*) + (/))#
                (Concat 
                        -- (+) + (-) + (*) + (/)
                        (Union (Symbol '+')
                                -- (-) + (*) + (/)
                                (Union (Symbol '-')
                                        -- (*)+(/)
                                        (Union (Symbol '*') (Symbol '/'))
                                )
                        )
                        -- #
                        (Symbol '#')
                )
                -- O
                (Symbol 'O')

-- opbool: (<) + (>) + (=) ------- etiqueta B
resp_terminal_testOPB = "Concat (Concat (Union (Symbol '<') (Union (Symbol '>') (Symbol '='))) (Symbol '#')) (Symbol 'B')"
{--
-- Resumen de la respuesta:
-- ((<)+(>)+(=))#B
Concat 
      -- ((<)+(>)+(=))#
      (Concat 
              -- (<)+(>)+(=)
              (Union 
                    -- <
                    (Symbol '<')
                    -- (>)+(=)
                    (Union (Symbol '>') (Symbol '='))
              )
              -- #
              (Symbol '#')
      )
      -- B
      (Symbol 'B')
--}
-- Interpretación en tipo Regex:
respTestOPB :: Regex
respTestOPB =
        -- ((<)+(>)+(=))#B
        Concat 
                -- ((<)+(>)+(=))#
                (Concat 
                        -- (<)+(>)+(=)
                        (Union
                                -- <
                                (Symbol '<')
                                -- (>)+(=)
                                (Union (Symbol '>') (Symbol '='))
                        )
                        -- #
                        (Symbol '#')
                )
                -- B
                (Symbol 'B')


-- Asignacion: (:=) ------- etiqueta A
resp_terminal_testASIG = "Concat (Concat (Concat (Symbol ':') (Symbol '=')) (Symbol '#')) (Symbol 'A')"
{--
-- Resumen de la respuesta:
-- :=#A
Concat
      -- :=#
      (Concat
              -- :=
              (Concat (Symbol ':') (Symbol '='))
              -- #
              (Symbol '#')
      )
      -- A
      (Symbol 'A')
--}
-- Interpretación en tipo Regex:
respTestASIG :: Regex
respTestASIG =
        -- :=#A
        Concat
                -- :=#
                (Concat
                        -- :=
                        (Concat (Symbol ':') (Symbol '='))
                        -- #
                        (Symbol '#')
                )
                -- A
                (Symbol 'A')
                


-- Palabras reservadas: (if + then  + else + while + do) ------- etiqueta R
resp_terminal_testRES = "Concat (Concat (Union (Concat (Symbol 'i') (Symbol 'f')) (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n')) (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Union (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e')) (Concat (Symbol 'd') (Symbol 'o')))))) (Symbol '#')) (Symbol 'R')"
{--
-- Resumen de la respuesta:
-- (if + then + else + while + do)#R
Concat
      -- (if + then + else + while + do)#
      (Concat 
              -- (if + then + else + while + do)
              (Union 
                      -- if + (then + else + while + do)
                      (Concat (Symbol 'i') (Symbol 'f'))
                      -- then + (else + while + do)
                      (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n'))
                             -- else + (while + do)
                             (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e'))
                                    -- while + (do)
                                    (Union (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e'))
                                           -- do
                                           (Concat (Symbol 'd') (Symbol 'o'))
                                    )
                              )
                      )
              ) 
              -- #
              (Symbol '#')
      )
      -- R
      (Symbol 'R')
--}
-- Interpretación en tipo Regex:
respTestRES :: Regex
respTestRES = 
        -- (if + then + else + while + do)#R
        Concat
                -- (if + then + else + while + do)#
                (Concat
                        -- if + (then + else + while + do)
                        (Union 
                                -- if
                                (Concat (Symbol 'i') (Symbol 'f'))
                                -- then + (else + while + do)
                                (Union
                                        (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n'))
                                        -- else + (while + do)
                                        (Union
                                                (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e'))
                                                -- while + (do)
                                                (Union
                                                        -- while
                                                        (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e'))
                                                        -- do
                                                        (Concat (Symbol 'd') (Symbol 'o'))
                                                )
                                        )
                                )
                        )
                        -- #
                        (Symbol '#')
                )
                -- R
                (Symbol 'R')


-- Simbolos especiales: (; + +) ------- etiqueta E
resp_terminal_testESP = "Concat (Concat (Union (Symbol ';') (Symbol '+')) (Symbol '#')) (Symbol 'E')"
{--
-- Resumen de la respuesta:
-- (; + +)#E
Concat
      -- (; + +)#
      (Concat
            -- ; + +
            (Union (Symbol ';') (Symbol '+'))
            -- #
            (Symbol '#')
      )
      -- E
      (Symbol 'E')
--}
-- Interpretación en tipo Regex:
respTestESP :: Regex
respTestESP = 
        -- (; + +)#E
        Concat
                -- (; + +)#
                (Concat
                        -- ; + +
                        (Union (Symbol ';') (Symbol '+'))
                        -- #
                        (Symbol '#')
                )
                -- E
                (Symbol 'E')


-- conflictoSuma: operadores + especiales
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
resp_terminal_testConflictoSuma = "Union (Concat (Concat (Union (Symbol '+') (Union (Symbol '-') (Union (Symbol '*') (Symbol '/')))) (Symbol '#')) (Symbol 'O')) (Concat (Concat (Union (Symbol ';') (Symbol '+')) (Symbol '#')) (Symbol 'E'))"
respTestConflictoSuma :: Regex
respTestConflictoSuma = Union respTestOP respTestESP


-- conflictoABC: identificadores + reservadas
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
resp_terminal_testConflictoABC = "Union (Concat (Concat (Concat (Concat (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))) (Star (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))))) (Symbol '#')) (Symbol 'I')) (Concat (Concat (Union (Concat (Symbol 'i') (Symbol 'f')) (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n')) (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Union (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e')) (Concat (Symbol 'd') (Symbol 'o')))))) (Symbol '#')) (Symbol 'R'))" 
respTestConflictoABC :: Regex
respTestConflictoABC = Union respTestID respTestRES


-- conflictoIg: opBool + asig
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
resp_terminal_testConflictoIg = "Union (Concat (Concat (Union (Symbol '<') (Union (Symbol '>') (Symbol '='))) (Symbol '#')) (Symbol 'B')) (Concat (Concat (Concat (Symbol ':') (Symbol '=')) (Symbol '#')) (Symbol 'A'))"
respTestConflictoIg :: Regex
respTestConflictoIg = Union respTestOPB respTestASIG


-- conflictoABCseccionado: identificadores + reservadas
-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- > "#(([c | c <- ['a'..'z'] ++ ['A'..'Z']]([c | c <- ['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*)#I)#+#(((if) + (then) + (else) + (while) + (do))#R)#"
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
resp_terminal_testConflictoABCseccionado = "Union (Concat (Concat (Symbol '#') (Concat (Concat (Concat (Concat (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))) (Star (Union (Symbol 'a') (Union (Symbol 'b') (Union (Symbol 'c') (Union (Symbol 'd') (Union (Symbol 'e') (Union (Symbol 'f') (Union (Symbol 'g') (Union (Symbol 'h') (Union (Symbol 'i') (Union (Symbol 'j') (Union (Symbol 'k') (Union (Symbol 'l') (Union (Symbol 'm') (Union (Symbol 'n') (Union (Symbol 'o') (Union (Symbol 'p') (Union (Symbol 'q') (Union (Symbol 'r') (Union (Symbol 's') (Union (Symbol 't') (Union (Symbol 'u') (Union (Symbol 'v') (Union (Symbol 'w') (Union (Symbol 'x') (Union (Symbol 'y') (Symbol 'z')))))))))))))))))))))))))))) (Star (Union (Symbol '0') (Union (Symbol '1') (Union (Symbol '2') (Union (Symbol '3') (Union (Symbol '4') (Union (Symbol '5') (Union (Symbol '6') (Union (Symbol '7') (Union (Symbol '8') (Symbol '9')))))))))))) (Symbol '#')) (Symbol 'I'))) (Symbol '#')) (Concat (Concat (Symbol '#') (Concat (Concat (Union (Concat (Symbol 'i') (Symbol 'f')) (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n')) (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e')) (Union (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e')) (Concat (Symbol 'd') (Symbol 'o')))))) (Symbol '#')) (Symbol 'R'))) (Symbol '#'))"
{--
-- Resumen de la respuesta en terminal.
-- Usamos la notación:
--       [A] = {a,b,c,...,z}
--       [D] = {0,1,2,...,9}
--
-- (#([A]([A]*)[D]*)#I#) + (#(if (then + (else (while + (do)))))#R#)
Union 
        -- #([A]([A]*)[D]*)#I#
        (Concat
                -- #([A]([A]*)[D]*)#I
                (Concat 
                        -- #
                        (Symbol '#')
                        -- ([A]([A]*)[D]*)#I
                        (Concat
                                -- ([A]([A]*)[D]*)#
                                (Concat
                                        -- [A]([A]*)[D]*
                                        (Concat
                                                -- [A]([A]*)
                                                (Concat [A] (Star [A]))
                                                -- [D]*
                                                (Star [D])
                                        )
                                        -- #
                                        (Symbol '#')
                                )
                                -- I
                                (Symbol 'I')
                        )
                )
                -- #
                (Symbol '#')
        )
        -- #(if (then + (else (while + (do)))))#R#
        (Concat
                -- #(if (then + (else (while + (do)))))#R
                (Concat
                        (Symbol '#')
                        -- (if (then + (else (while + (do)))))#R
                        (Concat
                                -- (if (then + (else (while + (do)))))#
                                (Concat
                                        -- if (then + (else (while + (do))))
                                        (Union (Concat (Symbol 'i') (Symbol 'f'))
                                                -- then + (else (while + (do)))
                                                (Union (Concat (Concat (Concat (Symbol 't') (Symbol 'h')) (Symbol 'e')) (Symbol 'n'))
                                                        -- else + (while + (do))
                                                        (Union (Concat (Concat (Concat (Symbol 'e') (Symbol 'l')) (Symbol 's')) (Symbol 'e'))
                                                                -- while + (do)
                                                                (Union (Concat (Concat (Concat (Concat (Symbol 'w') (Symbol 'h')) (Symbol 'i')) (Symbol 'l')) (Symbol 'e'))
                                                                        -- do
                                                                        (Concat (Symbol 'd') (Symbol 'o'))
                                                                )
                                                        )
                                                )
                                        )
                                        (Symbol '#')
                                )
                                (Symbol 'R')
                        )
                )
                (Symbol '#')
        )
--}
-- Interpretación en tipo Regex:
respTestConflictoABCseccionado :: Regex
respTestConflictoABCseccionado = 
        -- (#[A][A]*[D]*#I#) + (#(if + then + else + while + do)#R#)
        Union 
                -- #[A][A]*[D]*#I#
                (Concat
                        -- #[A][A]*[D]*#I
                        (Concat (Symbol '#') respTestID)
                        -- #
                        (Symbol '#')
                )
                -- #(if + then + else + while + do)#R#
                (Concat
                        -- #(if + then + else + while + do)#R
                        (Concat (Symbol '#') respTestRES)
                        (Symbol '#')
                )



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
    testRegexEq resp_terminal_testZ  respTestZ,
    -- operadores
    testRegexEq resp_terminal_testOP respTestOP,
    -- opbool
    testRegexEq resp_terminal_testOPB respTestOPB,
    -- asignaciones
    testRegexEq resp_terminal_testASIG respTestASIG,
    -- palabras reservadas
    testRegexEq resp_terminal_testRES respTestRES,
    -- caracteres especiales
    testRegexEq resp_terminal_testESP respTestESP,
    -- conflictoSuma: operadores + especiales
    testRegexEq resp_terminal_testConflictoSuma respTestConflictoSuma,
    -- conflictoABC: identificadores + reservadas
    testRegexEq resp_terminal_testConflictoABC respTestConflictoABC,
    -- conflictoIg:  opBool + asig
    testRegexEq resp_terminal_testConflictoIg respTestConflictoIg,
    -- conflictoABCseccionado
    -- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
    testRegexEq resp_terminal_testConflictoABCseccionado respTestConflictoABCseccionado
  ]


-- Comprobación de que todas las Regex equivalen a la impresion en terminal
regexIMP_Eq :: Bool
regexIMP_Eq = and testIMPRegexEq



-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFNEp para expresiones regulares del lenguaje IMP
-- utiliza la función definida en AFNEp: expr_to_AFNEp :: Regex -> AFNEp
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D* ------- etiqueta I
testAFNEp_ID = expr_to_AFNEp respTestID
-- enteros = 0 + ZD* + (-ZD*) ------- etiqueta Z
testAFNEp_Z = expr_to_AFNEp respTestZ
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
testAFNEp_OP = expr_to_AFNEp respTestOP
-- opbool = (<) + (>) + (=) ------- etiqueta B
testAFNEp_OPB = expr_to_AFNEp respTestOPB
-- asignacion = := ------- etiqueta A
testAFNEp_ASIG = expr_to_AFNEp respTestASIG
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFNEp_RES = expr_to_AFNEp respTestRES
-- simbolos especiales = ; + + ------- etiqueta E
testAFNEp_ESP = expr_to_AFNEp respTestESP

-- conflictoSuma: operadores + especiales
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
testAFNEp_conflictoSuma = expr_to_AFNEp respTestConflictoSuma

-- conflictoABC: identificadores + reservadas
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFNEp_conflictoABC = expr_to_AFNEp respTestConflictoABC

-- conflictoIg: opBool + asig
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
testAFNEp_conflictoIg = expr_to_AFNEp respTestConflictoIg


-- conflictoABCseccionado: identificadores + reservadas
-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFNEp_conflictoABCseccionado = expr_to_AFNEp respTestConflictoABCseccionado

-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Respuestas esperadas de las pruebas testIMP_AFNEp.
-- Cada respuesta usa el tipo AFNEp para realizar las pruebas de AFN de cada una
-- de las categorías léxicas de IMP sin que los resultados dependan de la
-- recursion previa. El objetivo de guardar los resultados es evitar tiempos 
-- de espera muy largos para pruebas que ya se ha comprobado que son correctas.
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- AFNEp por categoría léxica
-- ---------------------------------------------------------------------

-- Identificadores: A(A*)D* ------- etiqueta I
idAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23","q24",
                   "q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36","q37","q38","q39","q40","q41","q42","q43","q44","q45","q46","q47",
                   "q48","q49","q50","q51","q52","q53","q54","q55","q56","q57","q58","q59","q60","q61","q62","q63","q64","q65","q66","q67","q68","q69","q70",
                   "q71","q72","q73","q74","q75","q76","q77","q78","q79","q80","q81","q82","q83","q84","q85","q86","q87","q88","q89","q90","q91","q92","q93",
                   "q94","q95","q96","q97","q98","q99","q100","q101","q102","q103","q104","q105","q106","q107","q108","q109","q110","q111","q112","q113","q114",
                   "q115","q116","q117","q118","q119","q120","q121","q122","q123","q124","q125","q126","q127","q128","q129","q130","q131","q132","q133","q134",
                   "q135","q136","q137","q138","q139","q140","q141","q142","q143","q144","q145","q146","q147","q148","q149","q150","q151","q152","q153","q154",
                   "q155","q156","q157","q158","q159","q160","q161","q162","q163","q164","q165","q166","q167","q168","q169","q170","q171","q172","q173","q174",
                   "q175","q176","q177","q178","q179","q180","q181","q182","q183","q184","q185","q186","q187","q188","q189","q190","q191","q192","q193","q194",
                   "q195","q196","q197","q198","q199","q200","q201","q202","q203","q204","q205","q206","q207","q208","q209","q210","q211","q212","q213","q214",
                   "q215","q216","q217","q218","q219","q220","q221","q222","q223","q224","q225","q226","q227","q228","q229","q230","q231","q232","q233","q234",
                   "q235","q236","q237","q238","q239","q240","q241","q242","q243","q244","q245","q246","q247","q248","q249"],
        alfabeto = "#0123456789Iabcdefghijklmnopqrstuvwxyz",
        transiciones = [("q0",Just 'a',["q1"]),("q2",Just 'b',["q3"]),("q4",Just 'c',["q5"]),("q6",Just 'd',["q7"]),("q8",Just 'e',["q9"]),("q10",Just 'f',["q11"]),
                        ("q12",Just 'g',["q13"]),("q14",Just 'h',["q15"]),("q16",Just 'i',["q17"]),("q18",Just 'j',["q19"]),("q20",Just 'k',["q21"]),
                        ("q22",Just 'l',["q23"]),("q24",Just 'm',["q25"]),("q26",Just 'n',["q27"]),("q28",Just 'o',["q29"]),("q30",Just 'p',["q31"]),
                        ("q32",Just 'q',["q33"]),("q34",Just 'r',["q35"]),("q36",Just 's',["q37"]),("q38",Just 't',["q39"]),("q40",Just 'u',["q41"]),
                        ("q42",Just 'v',["q43"]),("q44",Just 'w',["q45"]),("q46",Just 'x',["q47"]),("q48",Just 'y',["q49"]),("q50",Just 'z',["q51"]),
                        ("q52",Nothing,["q48","q50"]),("q49",Nothing,["q53"]),("q51",Nothing,["q53"]),("q54",Nothing,["q46","q52"]),("q47",Nothing,["q55"]),
                        ("q53",Nothing,["q55"]),("q56",Nothing,["q44","q54"]),("q45",Nothing,["q57"]),("q55",Nothing,["q57"]),("q58",Nothing,["q42","q56"]),
                        ("q43",Nothing,["q59"]),("q57",Nothing,["q59"]),("q60",Nothing,["q40","q58"]),("q41",Nothing,["q61"]),("q59",Nothing,["q61"]),
                        ("q62",Nothing,["q38","q60"]),("q39",Nothing,["q63"]),("q61",Nothing,["q63"]),("q64",Nothing,["q36","q62"]),("q37",Nothing,["q65"]),
                        ("q63",Nothing,["q65"]),("q66",Nothing,["q34","q64"]),("q35",Nothing,["q67"]),("q65",Nothing,["q67"]),("q68",Nothing,["q32","q66"]),
                        ("q33",Nothing,["q69"]),("q67",Nothing,["q69"]),("q70",Nothing,["q30","q68"]),("q31",Nothing,["q71"]),("q69",Nothing,["q71"]),
                        ("q72",Nothing,["q28","q70"]),("q29",Nothing,["q73"]),("q71",Nothing,["q73"]),("q74",Nothing,["q26","q72"]),("q27",Nothing,["q75"]),
                        ("q73",Nothing,["q75"]),("q76",Nothing,["q24","q74"]),("q25",Nothing,["q77"]), ("q75",Nothing,["q77"]),("q78",Nothing,["q22","q76"]),
                        ("q23",Nothing,["q79"]),("q77",Nothing,["q79"]),("q80",Nothing,["q20","q78"]),("q21",Nothing,["q81"]),("q79",Nothing,["q81"]),
                        ("q82",Nothing,["q18","q80"]),("q19",Nothing,["q83"]),("q81",Nothing,["q83"]),("q84",Nothing,["q16","q82"]),("q17",Nothing,["q85"]),
                        ("q83",Nothing,["q85"]),("q86",Nothing,["q14","q84"]),("q15",Nothing,["q87"]),("q85",Nothing,["q87"]),("q88",Nothing,["q12","q86"]),
                        ("q13",Nothing,["q89"]),("q87",Nothing,["q89"]),("q90",Nothing,["q10","q88"]),("q11",Nothing,["q91"]),("q89",Nothing,["q91"]),
                        ("q92",Nothing,["q8","q90"]),("q9",Nothing,["q93"]),("q91",Nothing,["q93"]),("q94",Nothing,["q6","q92"]),("q7",Nothing,["q95"]),
                        ("q93",Nothing,["q95"]),("q96",Nothing,["q4","q94"]),("q5",Nothing,["q97"]),("q95",Nothing,["q97"]),("q98",Nothing,["q2","q96"]),
                        ("q3",Nothing,["q99"]),("q97",Nothing,["q99"]),("q100",Nothing,["q0","q98"]),("q1",Nothing,["q101"]),("q99",Nothing,["q101"]),
                        ("q102",Just 'a',["q103"]),("q104",Just 'b',["q105"]),("q106",Just 'c',["q107"]),("q108",Just 'd',["q109"]),("q110",Just 'e',["q111"]),
                        ("q112",Just 'f',["q113"]),("q114",Just 'g',["q115"]),("q116",Just 'h',["q117"]),("q118",Just 'i',["q119"]),("q120",Just 'j',["q121"]),
                        ("q122",Just 'k',["q123"]),("q124",Just 'l',["q125"]),("q126",Just 'm',["q127"]),("q128",Just 'n',["q129"]),("q130",Just 'o',["q131"]),
                        ("q132",Just 'p',["q133"]),("q134",Just 'q',["q135"]),("q136",Just 'r',["q137"]),("q138",Just 's',["q139"]),("q140",Just 't',["q141"]),
                        ("q142",Just 'u',["q143"]),("q144",Just 'v',["q145"]),("q146",Just 'w',["q147"]),("q148",Just 'x',["q149"]),("q150",Just 'y',["q151"]),
                        ("q152",Just 'z',["q153"]),("q154",Nothing,["q150","q152"]),("q151",Nothing,["q155"]),("q153",Nothing,["q155"]),
                        ("q156",Nothing,["q148","q154"]),("q149",Nothing,["q157"]),("q155",Nothing,["q157"]),("q158",Nothing,["q146","q156"]),
                        ("q147",Nothing,["q159"]),("q157",Nothing,["q159"]),("q160",Nothing,["q144","q158"]),("q145",Nothing,["q161"]),("q159",Nothing,["q161"]),
                        ("q162",Nothing,["q142","q160"]),("q143",Nothing,["q163"]),("q161",Nothing,["q163"]),("q164",Nothing,["q140","q162"]),
                        ("q141",Nothing,["q165"]),("q163",Nothing,["q165"]),("q166",Nothing,["q138","q164"]),("q139",Nothing,["q167"]),("q165",Nothing,["q167"]),
                        ("q168",Nothing,["q136","q166"]),("q137",Nothing,["q169"]),("q167",Nothing,["q169"]),("q170",Nothing,["q134","q168"]),
                        ("q135",Nothing,["q171"]),("q169",Nothing,["q171"]),("q172",Nothing,["q132","q170"]),("q133",Nothing,["q173"]),("q171",Nothing,["q173"]),
                        ("q174",Nothing,["q130","q172"]),("q131",Nothing,["q175"]),("q173",Nothing,["q175"]),("q176",Nothing,["q128","q174"]),
                        ("q129",Nothing,["q177"]),("q175",Nothing,["q177"]),("q178",Nothing,["q126","q176"]),("q127",Nothing,["q179"]),("q177",Nothing,["q179"]),
                        ("q180",Nothing,["q124","q178"]),("q125",Nothing,["q181"]),("q179",Nothing,["q181"]),("q182",Nothing,["q122","q180"]),
                        ("q123",Nothing,["q183"]),("q181",Nothing,["q183"]),("q184",Nothing,["q120","q182"]),("q121",Nothing,["q185"]),("q183",Nothing,["q185"]),
                        ("q186",Nothing,["q118","q184"]),("q119",Nothing,["q187"]),("q185",Nothing,["q187"]),("q188",Nothing,["q116","q186"]),
                        ("q117",Nothing,["q189"]),("q187",Nothing,["q189"]),("q190",Nothing,["q114","q188"]),("q115",Nothing,["q191"]),("q189",Nothing,["q191"]),
                        ("q192",Nothing,["q112","q190"]),("q113",Nothing,["q193"]),("q191",Nothing,["q193"]),("q194",Nothing,["q110","q192"]),
                        ("q111",Nothing,["q195"]),("q193",Nothing,["q195"]),("q196",Nothing,["q108","q194"]),("q109",Nothing,["q197"]),("q195",Nothing,["q197"]),
                        ("q198",Nothing,["q106","q196"]),("q107",Nothing,["q199"]),("q197",Nothing,["q199"]),("q200",Nothing,["q104","q198"]),
                        ("q105",Nothing,["q201"]),("q199",Nothing,["q201"]),("q202",Nothing,["q102","q200"]),("q103",Nothing,["q203"]),("q201",Nothing,["q203"]),
                        ("q204",Nothing,["q202","q205"]),("q203",Nothing,["q202","q205"]),("q101",Nothing,["q204"]),("q206",Just '0',["q207"]),
                        ("q208",Just '1',["q209"]),("q210",Just '2',["q211"]),("q212",Just '3',["q213"]),("q214",Just '4',["q215"]),("q216",Just '5',["q217"]),
                        ("q218",Just '6',["q219"]),("q220",Just '7',["q221"]),("q222",Just '8',["q223"]),("q224",Just '9',["q225"]),("q226",Nothing,["q222","q224"]),
                        ("q223",Nothing,["q227"]),("q225",Nothing,["q227"]),("q228",Nothing,["q220","q226"]),("q221",Nothing,["q229"]),("q227",Nothing,["q229"]),
                        ("q230",Nothing,["q218","q228"]),("q219",Nothing,["q231"]),("q229",Nothing,["q231"]),("q232",Nothing,["q216","q230"]),("q217",Nothing,["q233"]),
                        ("q231",Nothing,["q233"]),("q234",Nothing,["q214","q232"]),("q215",Nothing,["q235"]),("q233",Nothing,["q235"]),("q236",Nothing,["q212","q234"]),
                        ("q213",Nothing,["q237"]),("q235",Nothing,["q237"]),("q238",Nothing,["q210","q236"]),("q211",Nothing,["q239"]),("q237",Nothing,["q239"]),
                        ("q240",Nothing,["q208","q238"]),("q209",Nothing,["q241"]),("q239",Nothing,["q241"]),("q242",Nothing,["q206","q240"]),("q207",Nothing,["q243"]),
                        ("q241",Nothing,["q243"]),("q244",Nothing,["q242","q245"]),("q243",Nothing,["q242","q245"]),("q205",Nothing,["q244"]),("q246",Just '#',["q247"]),
                        ("q245",Nothing,["q246"]),("q248",Just 'I',["q249"]),("q247",Nothing,["q248"])],
        inicial = "q100",
        final = "q249"}


-- Enteros: 0 + ZD* + (-ZD*) ------- etiqueta Z
intAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23","q24",
                   "q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36","q37","q38","q39","q40","q41","q42","q43","q44","q45","q46","q47",
                   "q48","q49","q50","q51","q52","q53","q54","q55","q56","q57","q58","q59","q60","q61","q62","q63","q64","q65","q66","q67","q68","q69","q70",
                   "q71","q72","q73","q74","q75","q76","q77","q78","q79","q80","q81","q82","q83","q84","q85","q86","q87","q88","q89","q90","q91","q92","q93",
                   "q94","q95","q96","q97","q98","q99","q100","q101","q102","q103","q104","q105","q106","q107","q108","q109","q110","q111","q112","q113","q114",
                   "q115","q116","q117","q118","q119","q120","q121","q122","q123","q124","q125","q126","q127","q128","q129","q130","q131","q132","q133","q134",
                   "q135","q136","q137","q138","q139","q140","q141","q142","q143","q144","q145","q146","q147","q148","q149","q150","q151","q152","q153","q154",
                   "q155","q156","q157","q158","q159"],
        alfabeto = "#-0123456789Z",
        transiciones = [("q0",Just '0',["q1"]),("q2",Just '1',["q3"]),("q4",Just '2',["q5"]),("q6",Just '3',["q7"]),("q8",Just '4',["q9"]),("q10",Just '5',["q11"]),
                        ("q12",Just '6',["q13"]),("q14",Just '7',["q15"]),("q16",Just '8',["q17"]),("q18",Just '9',["q19"]),("q20",Nothing,["q16","q18"]),
                        ("q17",Nothing,["q21"]),("q19",Nothing,["q21"]),("q22",Nothing,["q14","q20"]),("q15",Nothing,["q23"]),("q21",Nothing,["q23"]),
                        ("q24",Nothing,["q12","q22"]),("q13",Nothing,["q25"]),("q23",Nothing,["q25"]),("q26",Nothing,["q10","q24"]),("q11",Nothing,["q27"]),
                        ("q25",Nothing,["q27"]),("q28",Nothing,["q8","q26"]),("q9",Nothing,["q29"]),("q27",Nothing,["q29"]),("q30",Nothing,["q6","q28"]),
                        ("q7",Nothing,["q31"]),("q29",Nothing,["q31"]),("q32",Nothing,["q4","q30"]),("q5",Nothing,["q33"]),("q31",Nothing,["q33"]),
                        ("q34",Nothing,["q2","q32"]),("q3",Nothing,["q35"]),("q33",Nothing,["q35"]),("q36",Just '0',["q37"]),("q38",Just '1',["q39"]),
                        ("q40",Just '2',["q41"]),("q42",Just '3',["q43"]),("q44",Just '4',["q45"]),("q46",Just '5',["q47"]),("q48",Just '6',["q49"]),
                        ("q50",Just '7',["q51"]),("q52",Just '8',["q53"]),("q54",Just '9',["q55"]),("q56",Nothing,["q52","q54"]),("q53",Nothing,["q57"]),
                        ("q55",Nothing,["q57"]),("q58",Nothing,["q50","q56"]),("q51",Nothing,["q59"]),("q57",Nothing,["q59"]),("q60",Nothing,["q48","q58"]),
                        ("q49",Nothing,["q61"]),("q59",Nothing,["q61"]),("q62",Nothing,["q46","q60"]),("q47",Nothing,["q63"]),("q61",Nothing,["q63"]),
                        ("q64",Nothing,["q44","q62"]),("q45",Nothing,["q65"]),("q63",Nothing,["q65"]),("q66",Nothing,["q42","q64"]),("q43",Nothing,["q67"]),
                        ("q65",Nothing,["q67"]),("q68",Nothing,["q40","q66"]),("q41",Nothing,["q69"]),("q67",Nothing,["q69"]),("q70",Nothing,["q38","q68"]),
                        ("q39",Nothing,["q71"]),("q69",Nothing,["q71"]),("q72",Nothing,["q36","q70"]),("q37",Nothing,["q73"]),("q71",Nothing,["q73"]),
                        ("q74",Nothing,["q72","q75"]),("q73",Nothing,["q72","q75"]),("q35",Nothing,["q74"]),("q76",Just '-',["q77"]),("q78",Just '1',["q79"]),
                        ("q80",Just '2',["q81"]),("q82",Just '3',["q83"]),("q84",Just '4',["q85"]),("q86",Just '5',["q87"]),("q88",Just '6',["q89"]),
                        ("q90",Just '7',["q91"]),("q92",Just '8',["q93"]),("q94",Just '9',["q95"]),("q96",Nothing,["q92","q94"]),("q93",Nothing,["q97"]),
                        ("q95",Nothing,["q97"]),("q98",Nothing,["q90","q96"]),("q91",Nothing,["q99"]),("q97",Nothing,["q99"]),("q100",Nothing,["q88","q98"]),
                        ("q89",Nothing,["q101"]),("q99",Nothing,["q101"]),("q102",Nothing,["q86","q100"]),("q87",Nothing,["q103"]),("q101",Nothing,["q103"]),
                        ("q104",Nothing,["q84","q102"]),("q85",Nothing,["q105"]),("q103",Nothing,["q105"]),("q106",Nothing,["q82","q104"]),
                        ("q83",Nothing,["q107"]),("q105",Nothing,["q107"]),("q108",Nothing,["q80","q106"]),("q81",Nothing,["q109"]),("q107",Nothing,["q109"]),
                        ("q110",Nothing,["q78","q108"]),("q79",Nothing,["q111"]),("q109",Nothing,["q111"]),("q77",Nothing,["q110"]),("q112",Just '0',["q113"]),
                        ("q114",Just '1',["q115"]),("q116",Just '2',["q117"]),("q118",Just '3',["q119"]),("q120",Just '4',["q121"]),("q122",Just '5',["q123"]),
                        ("q124",Just '6',["q125"]),("q126",Just '7',["q127"]),("q128",Just '8',["q129"]),("q130",Just '9',["q131"]),
                        ("q132",Nothing,["q128","q130"]),("q129",Nothing,["q133"]),("q131",Nothing,["q133"]),("q134",Nothing,["q126","q132"]),
                        ("q127",Nothing,["q135"]),("q133",Nothing,["q135"]),("q136",Nothing,["q124","q134"]),("q125",Nothing,["q137"]),("q135",Nothing,["q137"]),
                        ("q138",Nothing,["q122","q136"]),("q123",Nothing,["q139"]),("q137",Nothing,["q139"]),("q140",Nothing,["q120","q138"]),
                        ("q121",Nothing,["q141"]),("q139",Nothing,["q141"]),("q142",Nothing,["q118","q140"]),("q119",Nothing,["q143"]),("q141",Nothing,["q143"]),
                        ("q144",Nothing,["q116","q142"]),("q117",Nothing,["q145"]),("q143",Nothing,["q145"]),("q146",Nothing,["q114","q144"]),
                        ("q115",Nothing,["q147"]),("q145",Nothing,["q147"]),("q148",Nothing,["q112","q146"]),("q113",Nothing,["q149"]),("q147",Nothing,["q149"]),
                        ("q150",Nothing,["q148","q151"]),("q149",Nothing,["q148","q151"]),("q111",Nothing,["q150"]),("q152",Nothing,["q34","q76"]),
                        ("q75",Nothing,["q153"]),("q151",Nothing,["q153"]),("q154",Nothing,["q0","q152"]),("q1",Nothing,["q155"]),("q153",Nothing,["q155"]),
                        ("q156",Just '#',["q157"]),("q155",Nothing,["q156"]),("q158",Just 'Z',["q159"]),("q157",Nothing,["q158"])],
        inicial = "q154",
        final = "q159"}


-- Operadores: (+)+(-)+(*)+(/) ------- etiqueta O
opAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17"],
        alfabeto = "#*+-/O",
        transiciones = [("q0",Just '+',["q1"]),("q2",Just '-',["q3"]),("q4",Just '*',["q5"]),("q6",Just '/',["q7"]),("q8",Nothing,["q4","q6"]),
                        ("q5",Nothing,["q9"]),("q7",Nothing,["q9"]),("q10",Nothing,["q2","q8"]),("q3",Nothing,["q11"]),("q9",Nothing,["q11"]),
                        ("q12",Nothing,["q0","q10"]),("q1",Nothing,["q13"]),("q11",Nothing,["q13"]),("q14",Just '#',["q15"]),("q13",Nothing,["q14"]),
                        ("q16",Just 'O',["q17"]),("q15",Nothing,["q16"])],
        inicial = "q12",
        final = "q17"}


-- opbool: (<) + (>) + (=) ------- etiqueta B
opboolAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13"],
        alfabeto = "#<=>B",
        transiciones = [("q0",Just '<',["q1"]),("q2",Just '>',["q3"]),("q4",Just '=',["q5"]),("q6",Nothing,["q2","q4"]),("q3",Nothing,["q7"]),
                        ("q5",Nothing,["q7"]),("q8",Nothing,["q0","q6"]),("q1",Nothing,["q9"]),("q7",Nothing,["q9"]),("q10",Just '#',["q11"]),
                        ("q9",Nothing,["q10"]),("q12",Just 'B',["q13"]),("q11",Nothing,["q12"])],
        inicial = "q8",
        final = "q13"}


-- Asignacion: (:=) ------- etiqueta A
asignacionAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7"],
        alfabeto = "#:=A",
        transiciones = [("q0",Just ':',["q1"]),("q2",Just '=',["q3"]),("q1",Nothing,["q2"]),("q4",Just '#',["q5"]),("q3",Nothing,["q4"]),
                        ("q6",Just 'A',["q7"]),("q5",Nothing,["q6"])],
        inicial = "q0",
        final = "q7"}


-- Palabras reservadas: (if + then  + else + while + do) ------- etiqueta R
reservadasAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21",
                   "q22","q23","q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36","q37","q38","q39","q40","q41","q42",
                   "q43","q44","q45"],
        alfabeto = "#Rdefhilnostw",
        transiciones = [("q0",Just 'i',["q1"]),("q2",Just 'f',["q3"]),("q1",Nothing,["q2"]),("q4",Just 't',["q5"]),("q6",Just 'h',["q7"]),
                        ("q5",Nothing,["q6"]),("q8",Just 'e',["q9"]),("q7",Nothing,["q8"]),("q10",Just 'n',["q11"]),("q9",Nothing,["q10"]),
                        ("q12",Just 'e',["q13"]),("q14",Just 'l',["q15"]),("q13",Nothing,["q14"]),("q16",Just 's',["q17"]),("q15",Nothing,["q16"]),
                        ("q18",Just 'e',["q19"]),("q17",Nothing,["q18"]),("q20",Just 'w',["q21"]),("q22",Just 'h',["q23"]),("q21",Nothing,["q22"]),
                        ("q24",Just 'i',["q25"]),("q23",Nothing,["q24"]),("q26",Just 'l',["q27"]),("q25",Nothing,["q26"]),("q28",Just 'e',["q29"]),
                        ("q27",Nothing,["q28"]),("q30",Just 'd',["q31"]),("q32",Just 'o',["q33"]),("q31",Nothing,["q32"]),("q34",Nothing,["q20","q30"]),
                        ("q29",Nothing,["q35"]),("q33",Nothing,["q35"]),("q36",Nothing,["q12","q34"]),("q19",Nothing,["q37"]),("q35",Nothing,["q37"]),
                        ("q38",Nothing,["q4","q36"]),("q11",Nothing,["q39"]),("q37",Nothing,["q39"]),("q40",Nothing,["q0","q38"]),("q3",Nothing,["q41"]),
                        ("q39",Nothing,["q41"]),("q42",Just '#',["q43"]),("q41",Nothing,["q42"]),("q44",Just 'R',["q45"]),("q43",Nothing,["q44"])],
        inicial = "q40",
        final = "q45"}


-- Simbolos especiales: (; + +) ------- etiqueta E
especialesAFNEP = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"],
        alfabeto = "#+;E",
        transiciones = [("q0",Just ';',["q1"]),("q2",Just '+',["q3"]),("q4",Nothing,["q0","q2"]),("q1",Nothing,["q5"]),("q3",Nothing,["q5"]),
                        ("q6",Just '#',["q7"]),("q5",Nothing,["q6"]),("q8",Just 'E',["q9"]),("q7",Nothing,["q8"])],
        inicial = "q4",
        final = "q9"}

-- conflictoSuma: operadores + especiales
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
conflictoSuma_AFNEp = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
                   "q24","q25","q26","q27","q28","q29"],
        alfabeto = "#*+-/;EO",
        transiciones = [("q0",Just '+',["q1"]),("q2",Just '-',["q3"]),("q4",Just '*',["q5"]),("q6",Just '/',["q7"]),("q8",Nothing,["q4","q6"]),
                        ("q5",Nothing,["q9"]),("q7",Nothing,["q9"]),("q10",Nothing,["q2","q8"]),("q3",Nothing,["q11"]),("q9",Nothing,["q11"]),
                        ("q12",Nothing,["q0","q10"]),("q1",Nothing,["q13"]),("q11",Nothing,["q13"]),("q14",Just '#',["q15"]),("q13",Nothing,["q14"]),
                        ("q16",Just 'O',["q17"]),("q15",Nothing,["q16"]),("q18",Just ';',["q19"]),("q20",Just '+',["q21"]),("q22",Nothing,["q18","q20"]),
                        ("q19",Nothing,["q23"]),("q21",Nothing,["q23"]),("q24",Just '#',["q25"]),("q23",Nothing,["q24"]),("q26",Just 'E',["q27"]),
                        ("q25",Nothing,["q26"]),("q28",Nothing,["q12","q22"]),("q17",Nothing,["q29"]),("q27",Nothing,["q29"])],
        inicial = "q28",
        final = "q29"}

-- conflictoABC: identificadores + reservadas
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
conflictoABC_AFNEp = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
                   "q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36","q37","q38","q39","q40","q41","q42","q43","q44","q45",
                   "q46","q47","q48","q49","q50","q51","q52","q53","q54","q55","q56","q57","q58","q59","q60","q61","q62","q63","q64","q65","q66","q67",
                   "q68","q69","q70","q71","q72","q73","q74","q75","q76","q77","q78","q79","q80","q81","q82","q83","q84","q85","q86","q87","q88","q89",
                   "q90","q91","q92","q93","q94","q95","q96","q97","q98","q99","q100","q101","q102","q103","q104","q105","q106","q107","q108","q109",
                   "q110","q111","q112","q113","q114","q115","q116","q117","q118","q119","q120","q121","q122","q123","q124","q125","q126","q127","q128",
                   "q129","q130","q131","q132","q133","q134","q135","q136","q137","q138","q139","q140","q141","q142","q143","q144","q145","q146","q147",
                   "q148","q149","q150","q151","q152","q153","q154","q155","q156","q157","q158","q159","q160","q161","q162","q163","q164","q165","q166",
                   "q167","q168","q169","q170","q171","q172","q173","q174","q175","q176","q177","q178","q179","q180","q181","q182","q183","q184","q185",
                   "q186","q187","q188","q189","q190","q191","q192","q193","q194","q195","q196","q197","q198","q199","q200","q201","q202","q203","q204",
                   "q205","q206","q207","q208","q209","q210","q211","q212","q213","q214","q215","q216","q217","q218","q219","q220","q221","q222","q223",
                   "q224","q225","q226","q227","q228","q229","q230","q231","q232","q233","q234","q235","q236","q237","q238","q239","q240","q241","q242",
                   "q243","q244","q245","q246","q247","q248","q249","q250","q251","q252","q253","q254","q255","q256","q257","q258","q259","q260","q261",
                   "q262","q263","q264","q265","q266","q267","q268","q269","q270","q271","q272","q273","q274","q275","q276","q277","q278","q279","q280",
                   "q281","q282","q283","q284","q285","q286","q287","q288","q289","q290","q291","q292","q293","q294","q295","q296","q297"],
        alfabeto = "#0123456789IRabcdefghijklmnopqrstuvwxyz",
        transiciones = [("q0",Just 'a',["q1"]),("q2",Just 'b',["q3"]),("q4",Just 'c',["q5"]),("q6",Just 'd',["q7"]),("q8",Just 'e',["q9"]),
                        ("q10",Just 'f',["q11"]),("q12",Just 'g',["q13"]),("q14",Just 'h',["q15"]),("q16",Just 'i',["q17"]),("q18",Just 'j',["q19"]),
                        ("q20",Just 'k',["q21"]),("q22",Just 'l',["q23"]),("q24",Just 'm',["q25"]),("q26",Just 'n',["q27"]),("q28",Just 'o',["q29"]),
                        ("q30",Just 'p',["q31"]),("q32",Just 'q',["q33"]),("q34",Just 'r',["q35"]),("q36",Just 's',["q37"]),("q38",Just 't',["q39"]),
                        ("q40",Just 'u',["q41"]),("q42",Just 'v',["q43"]),("q44",Just 'w',["q45"]),("q46",Just 'x',["q47"]),("q48",Just 'y',["q49"]),
                        ("q50",Just 'z',["q51"]),("q52",Nothing,["q48","q50"]),("q49",Nothing,["q53"]),("q51",Nothing,["q53"]),("q54",Nothing,["q46","q52"]),
                        ("q47",Nothing,["q55"]),("q53",Nothing,["q55"]),("q56",Nothing,["q44","q54"]),("q45",Nothing,["q57"]),("q55",Nothing,["q57"]),
                        ("q58",Nothing,["q42","q56"]),("q43",Nothing,["q59"]),("q57",Nothing,["q59"]),("q60",Nothing,["q40","q58"]),("q41",Nothing,["q61"]),
                        ("q59",Nothing,["q61"]),("q62",Nothing,["q38","q60"]),("q39",Nothing,["q63"]),("q61",Nothing,["q63"]),("q64",Nothing,["q36","q62"]),
                        ("q37",Nothing,["q65"]),("q63",Nothing,["q65"]),("q66",Nothing,["q34","q64"]),("q35",Nothing,["q67"]),("q65",Nothing,["q67"]),
                        ("q68",Nothing,["q32","q66"]),("q33",Nothing,["q69"]),("q67",Nothing,["q69"]),("q70",Nothing,["q30","q68"]),("q31",Nothing,["q71"]),
                        ("q69",Nothing,["q71"]),("q72",Nothing,["q28","q70"]),("q29",Nothing,["q73"]),("q71",Nothing,["q73"]),("q74",Nothing,["q26","q72"]),
                        ("q27",Nothing,["q75"]),("q73",Nothing,["q75"]),("q76",Nothing,["q24","q74"]),("q25",Nothing,["q77"]),("q75",Nothing,["q77"]),
                        ("q78",Nothing,["q22","q76"]),("q23",Nothing,["q79"]),("q77",Nothing,["q79"]),("q80",Nothing,["q20","q78"]),("q21",Nothing,["q81"]),
                        ("q79",Nothing,["q81"]),("q82",Nothing,["q18","q80"]),("q19",Nothing,["q83"]),("q81",Nothing,["q83"]),("q84",Nothing,["q16","q82"]),
                        ("q17",Nothing,["q85"]),("q83",Nothing,["q85"]),("q86",Nothing,["q14","q84"]),("q15",Nothing,["q87"]),("q85",Nothing,["q87"]),
                        ("q88",Nothing,["q12","q86"]),("q13",Nothing,["q89"]),("q87",Nothing,["q89"]),("q90",Nothing,["q10","q88"]),("q11",Nothing,["q91"]),
                        ("q89",Nothing,["q91"]),("q92",Nothing,["q8","q90"]),("q9",Nothing,["q93"]),("q91",Nothing,["q93"]),("q94",Nothing,["q6","q92"]),
                        ("q7",Nothing,["q95"]),("q93",Nothing,["q95"]),("q96",Nothing,["q4","q94"]),("q5",Nothing,["q97"]),("q95",Nothing,["q97"]),
                        ("q98",Nothing,["q2","q96"]),("q3",Nothing,["q99"]),("q97",Nothing,["q99"]),("q100",Nothing,["q0","q98"]),("q1",Nothing,["q101"]),
                        ("q99",Nothing,["q101"]),("q102",Just 'a',["q103"]),("q104",Just 'b',["q105"]),("q106",Just 'c',["q107"]),("q108",Just 'd',["q109"]),
                        ("q110",Just 'e',["q111"]),("q112",Just 'f',["q113"]),("q114",Just 'g',["q115"]),("q116",Just 'h',["q117"]),("q118",Just 'i',["q119"]),
                        ("q120",Just 'j',["q121"]),("q122",Just 'k',["q123"]),("q124",Just 'l',["q125"]),("q126",Just 'm',["q127"]),("q128",Just 'n',["q129"]),
                        ("q130",Just 'o',["q131"]),("q132",Just 'p',["q133"]),("q134",Just 'q',["q135"]),("q136",Just 'r',["q137"]),("q138",Just 's',["q139"]),
                        ("q140",Just 't',["q141"]),("q142",Just 'u',["q143"]),("q144",Just 'v',["q145"]),("q146",Just 'w',["q147"]),("q148",Just 'x',["q149"]),
                        ("q150",Just 'y',["q151"]),("q152",Just 'z',["q153"]),("q154",Nothing,["q150","q152"]),("q151",Nothing,["q155"]),
                        ("q153",Nothing,["q155"]),("q156",Nothing,["q148","q154"]),("q149",Nothing,["q157"]),("q155",Nothing,["q157"]),
                        ("q158",Nothing,["q146","q156"]),("q147",Nothing,["q159"]),("q157",Nothing,["q159"]),("q160",Nothing,["q144","q158"]),
                        ("q145",Nothing,["q161"]),("q159",Nothing,["q161"]),("q162",Nothing,["q142","q160"]),("q143",Nothing,["q163"]),("q161",Nothing,["q163"]),
                        ("q164",Nothing,["q140","q162"]),("q141",Nothing,["q165"]),("q163",Nothing,["q165"]),("q166",Nothing,["q138","q164"]),
                        ("q139",Nothing,["q167"]),("q165",Nothing,["q167"]),("q168",Nothing,["q136","q166"]),("q137",Nothing,["q169"]),("q167",Nothing,["q169"]),
                        ("q170",Nothing,["q134","q168"]),("q135",Nothing,["q171"]),("q169",Nothing,["q171"]),("q172",Nothing,["q132","q170"]),
                        ("q133",Nothing,["q173"]),("q171",Nothing,["q173"]),("q174",Nothing,["q130","q172"]),("q131",Nothing,["q175"]),("q173",Nothing,["q175"]),
                        ("q176",Nothing,["q128","q174"]),("q129",Nothing,["q177"]),("q175",Nothing,["q177"]),("q178",Nothing,["q126","q176"]),
                        ("q127",Nothing,["q179"]),("q177",Nothing,["q179"]),("q180",Nothing,["q124","q178"]),("q125",Nothing,["q181"]),("q179",Nothing,["q181"]),
                        ("q182",Nothing,["q122","q180"]),("q123",Nothing,["q183"]),("q181",Nothing,["q183"]),("q184",Nothing,["q120","q182"]),
                        ("q121",Nothing,["q185"]),("q183",Nothing,["q185"]),("q186",Nothing,["q118","q184"]),("q119",Nothing,["q187"]),("q185",Nothing,["q187"]),
                        ("q188",Nothing,["q116","q186"]),("q117",Nothing,["q189"]),("q187",Nothing,["q189"]),("q190",Nothing,["q114","q188"]),
                        ("q115",Nothing,["q191"]),("q189",Nothing,["q191"]),("q192",Nothing,["q112","q190"]),("q113",Nothing,["q193"]),("q191",Nothing,["q193"]),
                        ("q194",Nothing,["q110","q192"]),("q111",Nothing,["q195"]),("q193",Nothing,["q195"]),("q196",Nothing,["q108","q194"]),
                        ("q109",Nothing,["q197"]),("q195",Nothing,["q197"]),("q198",Nothing,["q106","q196"]),("q107",Nothing,["q199"]),("q197",Nothing,["q199"]),
                        ("q200",Nothing,["q104","q198"]),("q105",Nothing,["q201"]),("q199",Nothing,["q201"]),("q202",Nothing,["q102","q200"]),
                        ("q103",Nothing,["q203"]),("q201",Nothing,["q203"]),("q204",Nothing,["q202","q205"]),("q203",Nothing,["q202","q205"]),
                        ("q101",Nothing,["q204"]),("q206",Just '0',["q207"]),("q208",Just '1',["q209"]),("q210",Just '2',["q211"]),("q212",Just '3',["q213"]),
                        ("q214",Just '4',["q215"]),("q216",Just '5',["q217"]),("q218",Just '6',["q219"]),("q220",Just '7',["q221"]),("q222",Just '8',["q223"]),
                        ("q224",Just '9',["q225"]),("q226",Nothing,["q222","q224"]),("q223",Nothing,["q227"]),("q225",Nothing,["q227"]),
                        ("q228",Nothing,["q220","q226"]),("q221",Nothing,["q229"]),("q227",Nothing,["q229"]),("q230",Nothing,["q218","q228"]),
                        ("q219",Nothing,["q231"]),("q229",Nothing,["q231"]),("q232",Nothing,["q216","q230"]),("q217",Nothing,["q233"]),("q231",Nothing,["q233"]),
                        ("q234",Nothing,["q214","q232"]),("q215",Nothing,["q235"]),("q233",Nothing,["q235"]),("q236",Nothing,["q212","q234"]),
                        ("q213",Nothing,["q237"]),("q235",Nothing,["q237"]),("q238",Nothing,["q210","q236"]),("q211",Nothing,["q239"]),("q237",Nothing,["q239"]),
                        ("q240",Nothing,["q208","q238"]),("q209",Nothing,["q241"]),("q239",Nothing,["q241"]),("q242",Nothing,["q206","q240"]),
                        ("q207",Nothing,["q243"]),("q241",Nothing,["q243"]),("q244",Nothing,["q242","q245"]),("q243",Nothing,["q242","q245"]),
                        ("q205",Nothing,["q244"]),("q246",Just '#',["q247"]),("q245",Nothing,["q246"]),("q248",Just 'I',["q249"]),("q247",Nothing,["q248"]),
                        ("q250",Just 'i',["q251"]),("q252",Just 'f',["q253"]),("q251",Nothing,["q252"]),("q254",Just 't',["q255"]),("q256",Just 'h',["q257"]),
                        ("q255",Nothing,["q256"]),("q258",Just 'e',["q259"]),("q257",Nothing,["q258"]),("q260",Just 'n',["q261"]),("q259",Nothing,["q260"]),
                        ("q262",Just 'e',["q263"]),("q264",Just 'l',["q265"]),("q263",Nothing,["q264"]),("q266",Just 's',["q267"]),("q265",Nothing,["q266"]),
                        ("q268",Just 'e',["q269"]),("q267",Nothing,["q268"]),("q270",Just 'w',["q271"]),("q272",Just 'h',["q273"]),("q271",Nothing,["q272"]),
                        ("q274",Just 'i',["q275"]),("q273",Nothing,["q274"]),("q276",Just 'l',["q277"]),("q275",Nothing,["q276"]),("q278",Just 'e',["q279"]),
                        ("q277",Nothing,["q278"]),("q280",Just 'd',["q281"]),("q282",Just 'o',["q283"]),("q281",Nothing,["q282"]),("q284",Nothing,["q270","q280"]),
                        ("q279",Nothing,["q285"]),("q283",Nothing,["q285"]),("q286",Nothing,["q262","q284"]),("q269",Nothing,["q287"]),("q285",Nothing,["q287"]),
                        ("q288",Nothing,["q254","q286"]),("q261",Nothing,["q289"]),("q287",Nothing,["q289"]),("q290",Nothing,["q250","q288"]),
                        ("q253",Nothing,["q291"]),("q289",Nothing,["q291"]),("q292",Just '#',["q293"]),("q291",Nothing,["q292"]),("q294",Just 'R',["q295"]),
                        ("q293",Nothing,["q294"]),("q296",Nothing,["q100","q290"]),("q249",Nothing,["q297"]),("q295",Nothing,["q297"])],
        inicial = "q296",
        final = "q297"} 

-- conflictoIg: opBool + asig
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
conflictoIg_AFNEp = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23"],
        alfabeto = "#:<=>AB",
        transiciones = [("q0",Just '<',["q1"]),("q2",Just '>',["q3"]),("q4",Just '=',["q5"]),("q6",Nothing,["q2","q4"]),("q3",Nothing,["q7"]),
                        ("q5",Nothing,["q7"]),("q8",Nothing,["q0","q6"]),("q1",Nothing,["q9"]),("q7",Nothing,["q9"]),("q10",Just '#',["q11"]),
                        ("q9",Nothing,["q10"]),("q12",Just 'B',["q13"]),("q11",Nothing,["q12"]),("q14",Just ':',["q15"]),("q16",Just '=',["q17"]),
                        ("q15",Nothing,["q16"]),("q18",Just '#',["q19"]),("q17",Nothing,["q18"]),("q20",Just 'A',["q21"]),("q19",Nothing,["q20"]),
                        ("q22",Nothing,["q8","q14"]),("q13",Nothing,["q23"]),("q21",Nothing,["q23"])],
        inicial = "q22",
        final = "q23"}


-- conflictoABCseccionado: identificadores + reservadas
-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
conflictoABCseccionado_AFNEp = AFNEp {
        estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21",
                   "q22","q23","q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36","q37","q38","q39","q40","q41",
                   "q42","q43","q44","q45","q46","q47","q48","q49","q50","q51","q52","q53","q54","q55","q56","q57","q58","q59","q60","q61",
                   "q62","q63","q64","q65","q66","q67","q68","q69","q70","q71","q72","q73","q74","q75","q76","q77","q78","q79","q80","q81",
                   "q82","q83","q84","q85","q86","q87","q88","q89","q90","q91","q92","q93","q94","q95","q96","q97","q98","q99","q100","q101",
                   "q102","q103","q104","q105","q106","q107","q108","q109","q110","q111","q112","q113","q114","q115","q116","q117","q118",
                   "q119","q120","q121","q122","q123","q124","q125","q126","q127","q128","q129","q130","q131","q132","q133","q134","q135","q136",
                   "q137","q138","q139","q140","q141","q142","q143","q144","q145","q146","q147","q148","q149","q150","q151","q152","q153","q154",
                   "q155","q156","q157","q158","q159","q160","q161","q162","q163","q164","q165","q166","q167","q168","q169","q170","q171","q172",
                   "q173","q174","q175","q176","q177","q178","q179","q180","q181","q182","q183","q184","q185","q186","q187","q188","q189","q190",
                   "q191","q192","q193","q194","q195","q196","q197","q198","q199","q200","q201","q202","q203","q204","q205","q206","q207","q208",
                   "q209","q210","q211","q212","q213","q214","q215","q216","q217","q218","q219","q220","q221","q222","q223","q224","q225","q226",
                   "q227","q228","q229","q230","q231","q232","q233","q234","q235","q236","q237","q238","q239","q240","q241","q242","q243","q244",
                   "q245","q246","q247","q248","q249","q250","q251","q252","q253","q254","q255","q256","q257","q258","q259","q260","q261","q262",
                   "q263","q264","q265","q266","q267","q268","q269","q270","q271","q272","q273","q274","q275","q276","q277","q278","q279","q280",
                   "q281","q282","q283","q284","q285","q286","q287","q288","q289","q290","q291","q292","q293","q294","q295","q296","q297","q298",
                   "q299","q300","q301","q302","q303","q304","q305"],
        alfabeto = "#0123456789IRabcdefghijklmnopqrstuvwxyz",
        transiciones = [("q0",Just '#',["q1"]),("q2",Just 'a',["q3"]),("q4",Just 'b',["q5"]),("q6",Just 'c',["q7"]),("q8",Just 'd',["q9"]),
                        ("q10",Just 'e',["q11"]),("q12",Just 'f',["q13"]),("q14",Just 'g',["q15"]),("q16",Just 'h',["q17"]),
                        ("q18",Just 'i',["q19"]),("q20",Just 'j',["q21"]),("q22",Just 'k',["q23"]),("q24",Just 'l',["q25"]),
                        ("q26",Just 'm',["q27"]),("q28",Just 'n',["q29"]),("q30",Just 'o',["q31"]),("q32",Just 'p',["q33"]),
                        ("q34",Just 'q',["q35"]),("q36",Just 'r',["q37"]),("q38",Just 's',["q39"]),("q40",Just 't',["q41"]),
                        ("q42",Just 'u',["q43"]),("q44",Just 'v',["q45"]),("q46",Just 'w',["q47"]),("q48",Just 'x',["q49"]),
                        ("q50",Just 'y',["q51"]),("q52",Just 'z',["q53"]),("q54",Nothing,["q50","q52"]),("q51",Nothing,["q55"]),
                        ("q53",Nothing,["q55"]),("q56",Nothing,["q48","q54"]),("q49",Nothing,["q57"]),("q55",Nothing,["q57"]),
                        ("q58",Nothing,["q46","q56"]),("q47",Nothing,["q59"]),("q57",Nothing,["q59"]),("q60",Nothing,["q44","q58"]),
                        ("q45",Nothing,["q61"]),("q59",Nothing,["q61"]),("q62",Nothing,["q42","q60"]),("q43",Nothing,["q63"]),
                        ("q61",Nothing,["q63"]),("q64",Nothing,["q40","q62"]),("q41",Nothing,["q65"]),("q63",Nothing,["q65"]),
                        ("q66",Nothing,["q38","q64"]),("q39",Nothing,["q67"]),("q65",Nothing,["q67"]),("q68",Nothing,["q36","q66"]),
                        ("q37",Nothing,["q69"]),("q67",Nothing,["q69"]),("q70",Nothing,["q34","q68"]),("q35",Nothing,["q71"]),
                        ("q69",Nothing,["q71"]),("q72",Nothing,["q32","q70"]),("q33",Nothing,["q73"]),("q71",Nothing,["q73"]),
                        ("q74",Nothing,["q30","q72"]),("q31",Nothing,["q75"]),("q73",Nothing,["q75"]),("q76",Nothing,["q28","q74"]),
                        ("q29",Nothing,["q77"]),("q75",Nothing,["q77"]),("q78",Nothing,["q26","q76"]),("q27",Nothing,["q79"]),
                        ("q77",Nothing,["q79"]),("q80",Nothing,["q24","q78"]),("q25",Nothing,["q81"]),("q79",Nothing,["q81"]),
                        ("q82",Nothing,["q22","q80"]),("q23",Nothing,["q83"]),("q81",Nothing,["q83"]),("q84",Nothing,["q20","q82"]),
                        ("q21",Nothing,["q85"]),("q83",Nothing,["q85"]),("q86",Nothing,["q18","q84"]),("q19",Nothing,["q87"]),
                        ("q85",Nothing,["q87"]),("q88",Nothing,["q16","q86"]),("q17",Nothing,["q89"]),("q87",Nothing,["q89"]),
                        ("q90",Nothing,["q14","q88"]),("q15",Nothing,["q91"]),("q89",Nothing,["q91"]),("q92",Nothing,["q12","q90"]),
                        ("q13",Nothing,["q93"]),("q91",Nothing,["q93"]),("q94",Nothing,["q10","q92"]),("q11",Nothing,["q95"]),
                        ("q93",Nothing,["q95"]),("q96",Nothing,["q8","q94"]),("q9",Nothing,["q97"]),("q95",Nothing,["q97"]),
                        ("q98",Nothing,["q6","q96"]),("q7",Nothing,["q99"]),("q97",Nothing,["q99"]),("q100",Nothing,["q4","q98"]),
                        ("q5",Nothing,["q101"]),("q99",Nothing,["q101"]),("q102",Nothing,["q2","q100"]),("q3",Nothing,["q103"]),
                        ("q101",Nothing,["q103"]),("q104",Just 'a',["q105"]),("q106",Just 'b',["q107"]),("q108",Just 'c',["q109"]),
                        ("q110",Just 'd',["q111"]),("q112",Just 'e',["q113"]),("q114",Just 'f',["q115"]),("q116",Just 'g',["q117"]),
                        ("q118",Just 'h',["q119"]),("q120",Just 'i',["q121"]),("q122",Just 'j',["q123"]),("q124",Just 'k',["q125"]),
                        ("q126",Just 'l',["q127"]),("q128",Just 'm',["q129"]),("q130",Just 'n',["q131"]),("q132",Just 'o',["q133"]),
                        ("q134",Just 'p',["q135"]),("q136",Just 'q',["q137"]),("q138",Just 'r',["q139"]),("q140",Just 's',["q141"]),
                        ("q142",Just 't',["q143"]),("q144",Just 'u',["q145"]),("q146",Just 'v',["q147"]),("q148",Just 'w',["q149"]),
                        ("q150",Just 'x',["q151"]),("q152",Just 'y',["q153"]),("q154",Just 'z',["q155"]),("q156",Nothing,["q152","q154"]),
                        ("q153",Nothing,["q157"]),("q155",Nothing,["q157"]),("q158",Nothing,["q150","q156"]),("q151",Nothing,["q159"]),
                        ("q157",Nothing,["q159"]),("q160",Nothing,["q148","q158"]),("q149",Nothing,["q161"]),("q159",Nothing,["q161"]),
                        ("q162",Nothing,["q146","q160"]),("q147",Nothing,["q163"]),("q161",Nothing,["q163"]),("q164",Nothing,["q144","q162"]),
                        ("q145",Nothing,["q165"]),("q163",Nothing,["q165"]),("q166",Nothing,["q142","q164"]),("q143",Nothing,["q167"]),
                        ("q165",Nothing,["q167"]),("q168",Nothing,["q140","q166"]),("q141",Nothing,["q169"]),("q167",Nothing,["q169"]),
                        ("q170",Nothing,["q138","q168"]),("q139",Nothing,["q171"]),("q169",Nothing,["q171"]),("q172",Nothing,["q136","q170"]),
                        ("q137",Nothing,["q173"]),("q171",Nothing,["q173"]),("q174",Nothing,["q134","q172"]),("q135",Nothing,["q175"]),
                        ("q173",Nothing,["q175"]),("q176",Nothing,["q132","q174"]),("q133",Nothing,["q177"]),("q175",Nothing,["q177"]),
                        ("q178",Nothing,["q130","q176"]),("q131",Nothing,["q179"]),("q177",Nothing,["q179"]),("q180",Nothing,["q128","q178"]),
                        ("q129",Nothing,["q181"]),("q179",Nothing,["q181"]),("q182",Nothing,["q126","q180"]),("q127",Nothing,["q183"]),
                        ("q181",Nothing,["q183"]),("q184",Nothing,["q124","q182"]),("q125",Nothing,["q185"]),("q183",Nothing,["q185"]),
                        ("q186",Nothing,["q122","q184"]),("q123",Nothing,["q187"]),("q185",Nothing,["q187"]),("q188",Nothing,["q120","q186"]),
                        ("q121",Nothing,["q189"]),("q187",Nothing,["q189"]),("q190",Nothing,["q118","q188"]),("q119",Nothing,["q191"]),
                        ("q189",Nothing,["q191"]),("q192",Nothing,["q116","q190"]),("q117",Nothing,["q193"]),("q191",Nothing,["q193"]),
                        ("q194",Nothing,["q114","q192"]),("q115",Nothing,["q195"]),("q193",Nothing,["q195"]),("q196",Nothing,["q112","q194"]),
                        ("q113",Nothing,["q197"]),("q195",Nothing,["q197"]),("q198",Nothing,["q110","q196"]),("q111",Nothing,["q199"]),
                        ("q197",Nothing,["q199"]),("q200",Nothing,["q108","q198"]),("q109",Nothing,["q201"]),("q199",Nothing,["q201"]),
                        ("q202",Nothing,["q106","q200"]),("q107",Nothing,["q203"]),("q201",Nothing,["q203"]),("q204",Nothing,["q104","q202"]),
                        ("q105",Nothing,["q205"]),("q203",Nothing,["q205"]),("q206",Nothing,["q204","q207"]),("q205",Nothing,["q204","q207"]),
                        ("q103",Nothing,["q206"]),("q208",Just '0',["q209"]),("q210",Just '1',["q211"]),("q212",Just '2',["q213"]),
                        ("q214",Just '3',["q215"]),("q216",Just '4',["q217"]),("q218",Just '5',["q219"]),("q220",Just '6',["q221"]),
                        ("q222",Just '7',["q223"]),("q224",Just '8',["q225"]),("q226",Just '9',["q227"]),("q228",Nothing,["q224","q226"]),
                        ("q225",Nothing,["q229"]),("q227",Nothing,["q229"]),("q230",Nothing,["q222","q228"]),("q223",Nothing,["q231"]),
                        ("q229",Nothing,["q231"]),("q232",Nothing,["q220","q230"]),("q221",Nothing,["q233"]),("q231",Nothing,["q233"]),
                        ("q234",Nothing,["q218","q232"]),("q219",Nothing,["q235"]),("q233",Nothing,["q235"]),("q236",Nothing,["q216","q234"]),
                        ("q217",Nothing,["q237"]),("q235",Nothing,["q237"]),("q238",Nothing,["q214","q236"]),("q215",Nothing,["q239"]),
                        ("q237",Nothing,["q239"]),("q240",Nothing,["q212","q238"]),("q213",Nothing,["q241"]),("q239",Nothing,["q241"]),
                        ("q242",Nothing,["q210","q240"]),("q211",Nothing,["q243"]),("q241",Nothing,["q243"]),("q244",Nothing,["q208","q242"]),
                        ("q209",Nothing,["q245"]),("q243",Nothing,["q245"]),("q246",Nothing,["q244","q247"]),("q245",Nothing,["q244","q247"]),
                        ("q207",Nothing,["q246"]),("q248",Just '#',["q249"]),("q247",Nothing,["q248"]),("q250",Just 'I',["q251"]),
                        ("q249",Nothing,["q250"]),("q1",Nothing,["q102"]),("q252",Just '#',["q253"]),("q251",Nothing,["q252"]),
                        ("q254",Just '#',["q255"]),("q256",Just 'i',["q257"]),("q258",Just 'f',["q259"]),("q257",Nothing,["q258"]),
                        ("q260",Just 't',["q261"]),("q262",Just 'h',["q263"]),("q261",Nothing,["q262"]),("q264",Just 'e',["q265"]),
                        ("q263",Nothing,["q264"]),("q266",Just 'n',["q267"]),("q265",Nothing,["q266"]),("q268",Just 'e',["q269"]),
                        ("q270",Just 'l',["q271"]),("q269",Nothing,["q270"]),("q272",Just 's',["q273"]),("q271",Nothing,["q272"]),
                        ("q274",Just 'e',["q275"]),("q273",Nothing,["q274"]),("q276",Just 'w',["q277"]),("q278",Just 'h',["q279"]),
                        ("q277",Nothing,["q278"]),("q280",Just 'i',["q281"]),("q279",Nothing,["q280"]),("q282",Just 'l',["q283"]),
                        ("q281",Nothing,["q282"]),("q284",Just 'e',["q285"]),("q283",Nothing,["q284"]),("q286",Just 'd',["q287"]),
                        ("q288",Just 'o',["q289"]),("q287",Nothing,["q288"]),("q290",Nothing,["q276","q286"]),("q285",Nothing,["q291"]),
                        ("q289",Nothing,["q291"]),("q292",Nothing,["q268","q290"]),("q275",Nothing,["q293"]),("q291",Nothing,["q293"]),
                        ("q294",Nothing,["q260","q292"]),("q267",Nothing,["q295"]),("q293",Nothing,["q295"]),("q296",Nothing,["q256","q294"]),
                        ("q259",Nothing,["q297"]),("q295",Nothing,["q297"]),("q298",Just '#',["q299"]),("q297",Nothing,["q298"]),
                        ("q300",Just 'R',["q301"]),("q299",Nothing,["q300"]),("q255",Nothing,["q296"]),("q302",Just '#',["q303"]),
                        ("q301",Nothing,["q302"]),("q304",Nothing,["q0","q254"]),("q253",Nothing,["q305"]),("q303",Nothing,["q305"])],
        inicial = "q304",
        final = "q305"}

-- Comprobación de que la respuesta en terminal genera el AFNEp esperado
-- para cada una de las categorias lexicas de IMP.
testIMP_AFNEp_Eq :: [Bool]
testIMP_AFNEp_Eq =
  [ 
    -- indentificadores
    testAFNEp_ID == idAFNEP,
    -- enteros
    testAFNEp_Z == intAFNEP,
    -- operadores
    testAFNEp_OP == opAFNEP,
    -- opbool
    testAFNEp_OPB == opboolAFNEP,
    -- asignaciones
    testAFNEp_ASIG == asignacionAFNEP,
    -- palabras reservadas
    testAFNEp_RES == reservadasAFNEP,
    -- caracteres especiales
    testAFNEp_ESP == especialesAFNEP,
    -- conflictoSuma: operadores + especiales
    testAFNEp_conflictoSuma == conflictoSuma_AFNEp,
    -- conflictoABC: identificadores + reservadas
    testAFNEp_conflictoABC == conflictoABC_AFNEp,
    -- conflictoIg: opBool + asig
    testAFNEp_conflictoIg == conflictoIg_AFNEp,
    -- conflictoABCseccionado: identificadores + reservadas
    -- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
    testAFNEp_conflictoABCseccionado == conflictoABCseccionado_AFNEp

  ]

-- Comprobación de que todos los AFNEp equivalen a la impresion en terminal
imp_AFNEpEq :: Bool
imp_AFNEpEq = and testIMP_AFNEp_Eq


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFN con base a AFNEp generados en el lenguaje IMP
-- utiliza la función definida en AFN: afnEp_to_AFN :: AFNEp -> AFN
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D* ------- etiqueta I
testAFN_ID = afnEp_to_AFN idAFNEP
-- enteros = 0 + ZD* + (-ZD*) ------- etiqueta Z
testAFN_Z = afnEp_to_AFN intAFNEP
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
testAFN_OP = afnEp_to_AFN opAFNEP
-- opbool = (<) + (>) + (=) ------- etiqueta B
testAFN_OPB = afnEp_to_AFN opboolAFNEP
-- asignacion = := ------- etiqueta A
testAFN_ASIG = afnEp_to_AFN asignacionAFNEP
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFN_RES = afnEp_to_AFN reservadasAFNEP
-- simbolos especiales = ; + + ------- etiqueta E
testAFN_ESP = afnEp_to_AFN especialesAFNEP

-- conflictoSuma: operadores + especiales
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
testAFN_conflictoSuma = afnEp_to_AFN conflictoSuma_AFNEp

-- conflictoABC: identificadores + reservadas
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFN_conflictoABC = afnEp_to_AFN conflictoABC_AFNEp

-- conflictoIg: opBool + asig
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
testAFN_conflictoIg = afnEp_to_AFN conflictoIg_AFNEp

-- conflictoABCseccionado: identificadores + reservadas
-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFN_conflictoABCseccionado = afnEp_to_AFN conflictoABCseccionado_AFNEp


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFD con base a AFN generados en el lenguaje IMP
-- utiliza la función definida en AFD: afn_to_AFD :: AFN -> AFD
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D* ------- etiqueta I
testAFD_ID = afn_to_AFD testAFN_ID
-- enteros = 0 + ZD* + (-ZD*) ------- etiqueta Z
testAFD_Z = afn_to_AFD testAFN_Z
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
testAFD_OP = afn_to_AFD testAFN_OP
-- opbool = (<) + (>) + (=) ------- etiqueta B
testAFD_OPB = afn_to_AFD testAFN_OPB
-- asignacion = := ------- etiqueta A
testAFD_ASIG = afn_to_AFD testAFN_ASIG
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFD_RES = afn_to_AFD testAFN_RES
-- simbolos especiales = ; + + ------- etiqueta E
testAFD_ESP = afn_to_AFD testAFN_ESP

-- conflictoSuma: operadores + especiales
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
testAFD_conflictoSuma = afn_to_AFD testAFN_conflictoSuma

-- conflictoABC: identificadores + reservadas
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFD_conflictoABC = afn_to_AFD testAFN_conflictoABC

-- conflictoIg: opBool + asig
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
testAFD_conflictoIg = afn_to_AFD testAFN_conflictoIg

-- conflictoABCseccionado: identificadores + reservadas
-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFD_conflictoABCseccionado = afn_to_AFD testAFN_conflictoABCseccionado


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Prueba : Construccion de AFDmin con base a AFD generados en el lenguaje IMP
-- utiliza la función definida en AFDmin: minimizaAFD :: AFD -> AFD
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------

-- id = A(A*)D* ------- etiqueta I
testAFDmin_ID = minimizaAFD testAFD_ID
-- enteros = 0 + ZD* + (-ZD*) ------- etiqueta Z
testAFDmin_Z = minimizaAFD testAFD_Z
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
testAFDmin_OP = minimizaAFD testAFD_OP
-- opbool = (<) + (>) + (=) ------- etiqueta B
testAFDmin_OPB = minimizaAFD testAFD_OPB
-- asignacion = := ------- etiqueta A
testAFDmin_ASIG = minimizaAFD testAFD_ASIG
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFDmin_RES = minimizaAFD testAFD_RES
-- simbolos especiales = ; + + ------- etiqueta E
testAFDmin_ESP = minimizaAFD testAFD_ESP

-- conflictoSuma: operadores + especiales
-- op = (+)+(-)+(*)+(/) ------- etiqueta O
-- simbolos especiales = ; + + ------- etiqueta E
testAFDmin_conflictoSuma = minimizaAFD testAFD_conflictoSuma

-- conflictoABC: identificadores + reservadas
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFDmin_conflictoABC = minimizaAFD testAFD_conflictoABC

-- conflictoIg: opBool + asig
-- opbool = (<) + (>) + (=) ------- etiqueta B
-- asignacion = := ------- etiqueta A
testAFDmin_conflictoIg = minimizaAFD testAFD_conflictoIg

-- conflictoABCseccionado: identificadores + reservadas
-- #(((A(A*)D*)#I)# + #((if + then  +  else + while + do)#R))#
-- id = A(A*)D* ------- etiqueta I
-- palabras reservadas = if + then  +  else + while + do ------- etiqueta R
testAFDmin_conflictoABCseccionado = minimizaAFD testAFD_conflictoABCseccionado


-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
-- Ejecucion de  todas las pruebas
-- ------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------
main :: IO ()
main = do
  -- REGEX --
  putStrLn "---- Pruebas Expresiones Regulares Aleatorias ----"
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

  {--
  -- REGEX IMP --
  putStrLn "\n---- Expresiones Regulares IMP ----"
  putStrLn "\n---- Identificadores ----"
  testID
  putStrLn "\n---- Enteros ----"
  testZ
  putStrLn "\n---- Operadores ----"
  testOP
  putStrLn "\n---- Operadores Booleanos ----"
  testOPB
  putStrLn "\n---- Asignacion ----"
  testASIG
  putStrLn "\n---- Palabras Reservadas ----"
  testRES
  putStrLn "\n---- Caracteres Especiales ----"
  testESP
  putStrLn "\n---- Conflictos ----"
  putStrLn "\n---- ConflictoSuma ----"
  testConflictoSuma
  putStrLn "\n---- ConflictoIg ----"
  testConflictoIg
  putStrLn "\n---- ConflictoABC ----"
  testConflictoABC
  putStrLn "\n---- ConflictoABC Seccionado ----"
  testConflictoABCseccionado

  putStrLn "\n---- Coincidencia de REGEX en terminal con Regex definidas para pruebas AFNEp ----"
  print regexIMP_Eq
  
  
  -- AFNEp IMP --
  putStrLn "\n---- AFNEp para categorías léxicas de IMP ----"
  putStrLn "\n---- Identificadores ----"
  print testAFNEp_ID
  putStrLn "\n---- Enteros ----"
  print testAFNEp_Z
  putStrLn "\n---- Operadores ----"
  print testAFNEp_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFNEp_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFNEp_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFNEp_RES
  putStrLn "\n---- Caracteres Especiales ----"
  print testAFNEp_ESP
  
  putStrLn "\n---- AFNEp para union de categorías léxicas que generan conflictos ----"
  putStrLn "\n---- ConflictoSuma ----"
  print testAFNEp_conflictoSuma
  putStrLn "\n---- ConflictoIg ----"
  print testAFNEp_conflictoIg
  putStrLn "\n---- ConflictoABC ----"
  print testAFNEp_conflictoABC
  putStrLn "\n---- ConflictoABC Seccionado ----"
  print testAFNEp_conflictoABCseccionado
  
  putStrLn "\n---- Coincidencia de AFNEp en terminal con AFNEp definidas para pruebas AFN ----"
  print imp_AFNEpEq

  
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
  putStrLn "\n---- Caracteres Especiales ----"
  print testAFN_ESP

  putStrLn "\n---- AFN para union de categorías léxicas que generan conflictos ----"
  putStrLn "\n---- ConflictoSuma ----"
  print testAFN_conflictoSuma
  putStrLn "\n---- ConflictoIg ----"
  print testAFN_conflictoIg
  putStrLn "\n---- ConflictoABC ----"
  print testAFN_conflictoABC
  putStrLn "\n---- ConflictoABC Seccionado ----"
  print testAFN_conflictoABCseccionado
  
  
  -- AFD IMP --
  putStrLn "\n---- AFD para categorías léxicas de IMP ----"
  putStrLn "\n---- Identificadores ----"
  print testAFD_ID
  putStrLn "\n---- Enteros ----"
  print testAFD_Z
  putStrLn "\n---- Operadores ----"
  print testAFD_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFD_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFD_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFD_RES
  putStrLn "\n---- Caracteres Especiales ----"
  print testAFD_ESP

  putStrLn "\n---- AFD para union de categorías léxicas que generan conflictos ----"
  putStrLn "\n---- ConflictoSuma ----"
  print testAFD_conflictoSuma
  putStrLn "\n---- ConflictoIg ----"
  print testAFD_conflictoIg
  putStrLn "\n---- ConflictoABC ----"
  print testAFD_conflictoABC
  putStrLn "\n---- ConflictoABC Seccionado ----"
  print testAFD_conflictoABCseccionado


  -- AFDmin IMP --
  putStrLn "\n---- AFD minimizado para categorías léxicas de IMP ----"
  putStrLn "\n---- Identificadores ----"
  print testAFDmin_ID
  putStrLn "\n---- Enteros ----"
  print testAFDmin_Z
  putStrLn "\n---- Operadores ----"
  print testAFDmin_OP
  putStrLn "\n---- Operadores Booleanos ----"
  print testAFDmin_OPB
  putStrLn "\n---- Asignacion ----"
  print testAFDmin_ASIG
  putStrLn "\n---- Palabras Reservadas ----"
  print testAFDmin_RES
  putStrLn "\n---- Caracteres Especiales ----"
  print testAFDmin_ESP
  
  putStrLn "\n---- AFD minimizado para union de categorías léxicas que generan conflictos ----"
  putStrLn "\n---- ConflictoSuma ----"
  print testAFDmin_conflictoSuma
  putStrLn "\n---- ConflictoIg ----"
  print testAFDmin_conflictoIg
  putStrLn "\n---- ConflictoABC ----"
  print testAFDmin_conflictoABC

  putStrLn "\n---- ConflictoABC Seccionado ----"
  print testAFDmin_conflictoABCseccionado
  --}