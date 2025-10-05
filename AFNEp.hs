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

-- | Abstracción de una transición epsilón,
-- utilizamos nothing para modelar una transición epsilón, en otro caso, un Maybe Char.
-- la tercia representa la función delta, el primero es
-- el estado donde se lee el cáracter o el epsilon y te lleva
-- a una lista de estados (no determinista).
type Trans_eps = (String, Maybe Char, [String])

-- Representación de un automáta no determinista con transiciones epsilon,
-- con un conjunto de estados (Strings), un alfabeto (Chars),
-- una función de transición (Trans_eps), un estado inicial y un estado final.
data AFNEp = AFNEp {
  estados :: [String],
  alfabeto :: [Char],
  transiciones :: [Trans_eps],
  inicial :: String,
  final :: String
} deriving (Show)


-- Abstracción de una expresión regular. 
--Puede ser un terminal (carácter), una concatenación, una disyunción o una estrella de Kleene, a partir de estas podemos construir otras.
--data Regex =  Symbol Char | Concat Regex Regex | Union Regex Regex | Star Regex
-- --Como las vamos a mostrar?
--instance Show Regex where
--  show (Symbol c)   = c:[] -- Un carácter solo: "a" (String).
--  show (Concat  l r) = "("++show l ++ show r ++ ")" -- Concatenación sin simbolo: ab.
--  show (Union   l r) = "("++show l ++ "+" ++ show r ++ ")" -- Disyunción con simbolo: a+b.
--  show (Star e) = show e ++ "*" --Estrella de Kleene con simbolo: a*.
--Buscamos construir un autómata a partir de una expresión regular(especificación en formato String).
getAFNEp :: String -> AFNEp
getAFNEp s = expr_to_AFNEp (getRegex s)


-- | Traducción de expresiones regulares a automátas con transiciones epsilón
-- recibimos una expresión regular compuesta de caracteres y operadores
-- y devolvemos un automáta no determinista con transiciones epsilón.
expr_to_AFNEp :: Regex -> AFNEp
expr_to_AFNEp e = expr_to_AFNEp_aux e [] -- La lista va a representar los estados del automáta.

expr_to_AFNEp_aux :: Regex -> [String] ->  AFNEp

-- Caso base, cuando tenemos un terminal (carácter) y q es la lista de estados.
-- Con esto se define el automáta que reconoce el carácter a.
expr_to_AFNEp_aux (Symbol a) q = AFNEp {estados = q++[q0, q1], --definimos dos estados, el inicial y a donde llegamos con a.
                                    alfabeto =  [a], -- agregamos el caracter al alfabeto
                                    transiciones = [(q0, Just a, [q1])], --definimos la transición del estado inicial al final con el caracter a
                                    inicial = q0, final = q1}
  where q0 = "q"++(show (length q)) -- q0
        q1 = "q"++(show ((length q) + 1)) -- q1

-- Automáta para la expresión a+b
expr_to_AFNEp_aux (Union a b) q =  AFNEp {estados = (estados m2) ++ [q0, q1], --accedemos a los estados de m2 y agregamos dos nuevos estados, el inicial y el final
                                    alfabeto =  rmDup $  (alfabeto m1) ++ (alfabeto m2), --unimos los alfabetos de m1 y m2 y eliminamos duplicados
                                    transiciones = (transiciones m1) ++ (transiciones m2)
                                      ++ [(q0, Nothing, [(inicial m1),(inicial m2)]), --Agregamos las transiciones epsilón del nuevo estado inicial a los iniciales de m1 y m2
                                         (final m1, Nothing, [q1]), -- Y de los finales de m1 y m2 al nuevo estado final
                                         (final m2, Nothing, [q1])],
                                    inicial = q0, final = q1} --Actualizamos los estados inicial y final
  where m1 = expr_to_AFNEp_aux a q -- Construimos el automáta para a
        m2 = expr_to_AFNEp_aux b (q++(estados m1)) -- Construimos el automáta para b, agregando los estados de m1 a la lista de estados
        q0 = "q"++(show  (length $ estados m2)) -- Sacrificamos el nombre q0 y q1 para que no se repitan, pero sigue siendo el estado inicial y final
        q1 = "q"++(show  ((length $ estados m2) + 1))
-- Automata para la expresion ab
expr_to_AFNEp_aux (Concat a b) q =  AFNEp {estados = estados m2,
                                    alfabeto =  rmDup $ (alfabeto m1) ++ (alfabeto m2),
                                    transiciones = (transiciones m1) ++ (transiciones m2)
                                      ++ [(final m1, Nothing, [inicial m2])],
                                    inicial = inicial m1, final = final m2}
  where m1 = expr_to_AFNEp_aux a q
        m2 = expr_to_AFNEp_aux b (q++(estados m1))
-- Autómata para la expresión a*
expr_to_AFNEp_aux (Star a) q =  AFNEp {estados = (estados m1) ++ [q0, q1],
                                    alfabeto =  (alfabeto m1),
                                    transiciones = (transiciones m1)
                                      ++ [(q0, Nothing, [qi, q1]),
                                         (qf, Nothing, [qi, q1])],
                                    inicial = q0, final = q1}
  where m1 = expr_to_AFNEp_aux a q
        q0 = "q"++(show  (length $ estados m1))
        q1 = "q"++(show  ((length $ estados m1) + 1))
        qi = inicial m1
        qf = final m1

-- Función para eliminar elementos duplicados de una lista
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