-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa: Regex -> AFNEp
-- | Equipo: Los Discipulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
module AFNEp where

import Data.Set (toList, fromList)
import Regex 

-- ------------------------------------------------------------------------------
-- Definición del tipo de dato Trans_eps para representar 
-- las transiciones que el automata debe seguir.
-- Utilizamos una tercia para representar la función delta, donde:
-- El primer lugar se ocupa por el estado actual.
-- El segundo lugar se ocupa por: Nothing si se trata de una transicion epsilon,
-- o el caracter que se especifique en caso de no ser transicion epsilon.
-- El tercer lugar es una lista de estados alcanzables con el símbolo especificado.
-- ------------------------------------------------------------------------------
type Trans_eps = (String, Maybe Char, [String])


-- ------------------------------------------------------------------------------
-- Definicion del tipo de dato AFNEp para representar a un autómata finito no
-- determinista con transiciones épsilon.
-- AFNEp = (Q, Σ, δ, q_s, q_f), donde
-- > Q = estados. Representados en una lista de cadenas
-- > Σ = alfabeto. Representado como una lista de caracteres
-- > δ = transiciones. Conjunto de transiciones representadas en una lista de Trans_eps
-- > q_s = inicial. Estado inicial del automata representado por un string
-- > q_f = final. Estado final del automata representado por un string
-- ------------------------------------------------------------------------------
data AFNEp = AFNEp {
  estados :: [String],
  alfabeto :: [Char],
  transiciones :: [Trans_eps],
  inicial :: String,
  final :: String
} deriving (Show, Eq)



-- ------------------------------------------------------------------------------
-- Función que dada una cadena, obtiene la representación de su expresión regular
-- y, en caso de que exista, genera el autómata no determinista con 
-- transiciones epsilon para la regex.
-- Si no existe, notifica la ausencia de la expresion regular.
-- ------------------------------------------------------------------------------
getAFNEp :: String -> AFNEp
getAFNEp s = expr_to_AFNEp (getRegex s)


-- ------------------------------------------------------------------------------
-- | Traducción de expresiones regulares a automátas con transiciones epsilón.
-- Recibimos una expresión regular compuesta de caracteres y operadores
-- y devolvemos un automáta no determinista con transiciones epsilón.
-- ------------------------------------------------------------------------------
expr_to_AFNEp :: Regex -> AFNEp
expr_to_AFNEp e = fst (expr_to_AFNEp_aux e 0) -- Contador para no repetir nombres de estados


-- ------------------------------------------------------------------------------
-- Función auxiliar para realizar el autómata finito no determinista con
-- transiciones epsilon de una regex válida.
-- ------------------------------------------------------------------------------
expr_to_AFNEp_aux :: Regex -> Int -> (AFNEp, Int)
-- Caso base, cuando tenemos un terminal (caracter) y q es la lista de estados.
-- Con esto se define el automáta que reconoce el carácter a.
expr_to_AFNEp_aux (Symbol a) n =
  (AFNEp { 
           -- Agregamos al Automata
           -- Estados nuevos para ir de uno al otro con el simbolo que se indique
           estados = [q0, q1], 
           -- Simbolo indicado ahora forma parte del alfabeto 
           alfabeto = [a], 
           -- Nueva transicion de q0 a q1 con el simbolo especificado
           transiciones = [(q0, Just a, [q1])], 
           -- especificamos quien es el estado inicial y quien es el final 
           inicial = q0,
           final = q1 },
   n + 2) -- Contamos 2 estados mas que fueron añadidos en este paso
  where
    q0 = "q" ++ show n
    q1 = "q" ++ show (n + 1)

-- Automáta para la expresión a+b
expr_to_AFNEp_aux (Union a b) n =
  (AFNEp { 
           -- Agregamos al automata:
           -- Estados del automata que corresponde al parametro a,
           -- estados del automata que corresponde al parametro b 
           -- y dos estados para definir como nuevos estados inicial y final
           estados = estados m1 ++ estados m2 ++ [q0, q1],
           -- Realizamos la union de los alfabetos de ambos automatas eliminando replicados
           alfabeto = rmDup $ alfabeto m1 ++ alfabeto m2,
           -- Consideramos las transiciones del automata correspondiente al parametro a
           -- transiciones del automata que corresponde al parametro b
           -- agregamos transicion de q0 (nuevo inicial) a los iniciales de a,b
           -- agregamos transicion de los estados finales de a,b hacia el nuevo final q1
           transiciones = transiciones m1 ++ transiciones m2
                          ++ [(q0, Nothing, [inicial m1, inicial m2]),
                              (final m1, Nothing, [q1]),
                              (final m2, Nothing, [q1])],
           -- especificamos como estado inicial q0 y como estado final q1
           inicial = q0,
           final = q1 },
   n') -- El contador actualizado es: cantidad de estados en a + cantidad de estados en b + 2 nuevos
  where
    -- Construimos el automata que corresponde al parametro a con el contador inicial n
    (m1, na) = expr_to_AFNEp_aux a n
    -- Construimos el automata que corresponde al parametro b con el contador
    -- actualizado después de haber creado m1
    (m2, nb) = expr_to_AFNEp_aux b na
    -- Nombramos los nuevos estados inicial y final de la unión considerando el contador
    -- actualizado luego de construir m2
    q0 = "q" ++ show nb
    q1 = "q" ++ show (nb + 1)
    n' = nb + 2  -- Incrementamos contador con los dos nuevos estados q0,q1

-- Automata para la expresion ab
expr_to_AFNEp_aux (Concat a b) n =
  (AFNEp { 
           -- Agregamos al automata:
           -- Estados del automata que corresponde al parametro a,
           -- estados del automata que corresponde al parametro b
           estados = estados m1 ++ estados m2,
           -- Realizamos la union de los alfabetos de ambos automatas eliminando replicados
           alfabeto = rmDup $ alfabeto m1 ++ alfabeto m2,
           -- Consideramos las transiciones del automata correspondiente al parametro a
           -- transiciones del automata que corresponde al parametro b
           -- agregamos la epsilon transicion del estado final de el automata a
           -- al estado inicial del automata correspondiente a b
           transiciones = transiciones m1 ++ transiciones m2
                          ++ [(final m1, Nothing, [inicial m2])],
           -- especificamos como estado inicial al inicial del automata a
           inicial = inicial m1,
           -- especificamos como estado final, al final del automata b
           final = final m2 },
   n') -- El contador actualizado es: estados de a + estados de b
  where
    -- Construimos el automata que corresponde al parametro a con el contador inicial n
    (m1, na) = expr_to_AFNEp_aux a n
    -- Construimos el automata que corresponde al parametro b con el contador
    -- actualizado después de haber creado m1
    (m2, n') = expr_to_AFNEp_aux b na

-- Autómata para la expresión a*
expr_to_AFNEp_aux (Star a) n =
  (AFNEp { 
           -- Agregamos al automata:
           -- Estados del automata que corresponde al parametro y dos nuevos
           -- que seran el nuevo inicial y nuevo final
           estados      = estados m1 ++ [q0, q1],
           -- el alfabeto es el mismo que el del automata que corresponde al parametro
           alfabeto     = alfabeto m1,
           -- Consideramos las transiciones del automata correspondiente al parametro
           -- agregamos la epsilon transicion de q0 al estado inicial del automata
           -- que corresponde al parametro y al nuevo estado final q1
           -- agregamos la transicion del estado final del automata correspondiente 
           -- al parametro hacia el estado inicial de éste y hacia el nuevo final q1
           transiciones = transiciones m1
                          ++ [(q0, Nothing, [inicial m1, q1]),
                              (final m1, Nothing, [inicial m1, q1])],
           -- especificamos como estado inicial q0 y como estado final q1
           inicial      = q0,
           final        = q1 },
   n') --  El contador actualizado es: estados del automata a + 2 nuevos 
  where
    -- Construimos el automata que corresponde al parametro con el contador inicial n
    (m1, na) = expr_to_AFNEp_aux a n
    -- Nombramos los nuevos estados inicial y final de la unión considerando el contador
    -- actualizado luego de construir m1
    q0 = "q" ++ show na
    q1 = "q" ++ show (na + 1)
    n' = na + 2 -- Incrementamos contador con los dos nuevos estados q0,q1


-- ------------------------------------------------------------------------------
-- Función para eliminar elementos duplicados de una lista
-- ------------------------------------------------------------------------------
rmDup :: (Ord a) => [a] -> [a]
rmDup = toList . fromList 



-- Ejemplo de uso:
-- La expresión (0+1)*1
e1 = Concat (Star (Union (Symbol '0') (Symbol '1'))) (Symbol '1')

-- El automáta que reconoce la expresión (0+1)*1, probado, concuerda con el resultado esperado.
m1 = AFNEp {estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"],
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
              inicial = "q6", final = "q9"}