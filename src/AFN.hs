-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa: AFNEp -> AFN
-- | Equipo: Los Discipulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
module AFN where

import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import AFNEp


-- ------------------------------------------------------------------------------
-- Definición del tipo de dato Trans_afn para representar 
-- las transiciones que el automata debe seguir.
-- Utilizamos una tercia para representar la función delta, donde:
-- El primer lugar se ocupa por el estado actual.
-- El segundo lugar se ocupa por un caracter del alfabeto.
-- El tercer lugar es una lista de estados alcanzables con el símbolo especificado.
-- ------------------------------------------------------------------------------
type Trans_afn = (String, Char, [String])


-- ------------------------------------------------------------------------------
-- Definicion del tipo de dato AFN para representar a un autómata finito no
-- determinista.
-- AFN = (Q, Σ, δ, q_s, q_f), donde
-- > Q = estados. Representados en una lista de cadenas
-- > Σ = alfabeto. Representado como una lista de caracteres
-- > δ = transiciones. Conjunto de transiciones representadas en una lista de Trans_afn
-- > q_s = inicial. Estado inicial del automata representado por un string
-- > q_f = final. Estado final del automata representado por un string
-- ------------------------------------------------------------------------------
data AFN = AFN {
  estadosN :: [String],
  alfabetoN :: [Char],
  transicionesN :: [Trans_afn],
  inicialN :: String,
  finalN :: String
} deriving (Show)


-- ------------------------------------------------------------------------------
-- Función que dada una cadena, obtiene la representación de su expresión regular
-- y, en caso de que exista, genera el autómata no determinista para la regex.
-- Si no existe, notifica la ausencia de la expresion regular.
-- ------------------------------------------------------------------------------
getAFN :: String -> AFN
getAFN s = afnEp_to_AFN (getAFNEp s)

-- ------------------------------------------------------------------------------
-- Función que convierte un AFNEp (autómata finito no determinista con
-- transiciones epsilón) a AFN (autómata finito no determinista)
-- ------------------------------------------------------------------------------
afnEp_to_AFN :: AFNEp -> AFN
afnEp_to_AFN m =  AFN {
  estadosN = estados m,
  alfabetoN =  alfabeto m,
  transicionesN = filterEmptyTransitions (trans_eps_to_afn m),
  inicialN = inicial m,
  finalN = final m
  }


-- ------------------------------------------------------------------------------
-- Función que construye las transiciones de un AFN a partir de las transiciones
-- de un AFNEp.
-- Para cada estado del AFNEp, se calcula su eclosure y las transiciones
-- correspondientes respecto a cada símbolo del alfabeto.
-- ------------------------------------------------------------------------------
trans_eps_to_afn :: AFNEp -> [Trans_afn]
trans_eps_to_afn m = concatMap (trans_eps_to_afn_aux m (transiciones m) (alfabeto m)) (estados m)


-- ------------------------------------------------------------------------------
-- Función auxiliar que calcula las transiciones de un estado a otro eliminando
-- las transiciones épsilon del AFNEp dado.
-- Regresa la lista de transiciones en formato [(estado, símbolo, [estados destino])]
-- ------------------------------------------------------------------------------
trans_eps_to_afn_aux :: AFNEp -> [Trans_eps] -> String -> String -> [Trans_afn]
trans_eps_to_afn_aux _ _ [] _ = []
trans_eps_to_afn_aux m l (c:cs) q = (q, c, qn) : (trans_eps_to_afn_aux m l cs q)
  where qn = eclosure2 m $ do_trans_nep2 l c (eclosure l m q)


-- ------------------------------------------------------------------------------
-- Función para calcular la eclosure de un estado.
-- Devuelve el conjunto de estados alcanzables desde el estado q_i
-- mediante cero o más transiciones épsilon.
-- ------------------------------------------------------------------------------
eclosure ::  [Trans_eps] -> AFNEp -> String  -> [String]
eclosure [] _ q1 = [q1]
eclosure ((q2, Nothing, l):xs) m q1
  | q2 == q1 = rmDup $ (q1:l) ++ eclosure2 m l
  | otherwise = eclosure xs m q1
eclosure (x:xs) m q1  = eclosure xs m q1


-- ------------------------------------------------------------------------------
-- Función auxiliar para unir las eclosure de cada estado en una sola lista
-- ------------------------------------------------------------------------------
eclosure2 ::  AFNEp -> [String]  -> [String]
eclosure2 m eclist = rmDup (concatMap (eclosure (transiciones m) m) eclist)


-- ------------------------------------------------------------------------------
-- Función que, dado un símbolo del alfabeto, aplica las transiciones desde los
-- estados q_i correspondientes.
-- Devuelve una lista de estados destino segun las transiciones aplicadas
-- (ninguna es transicion epsilon).
-- ------------------------------------------------------------------------------
do_trans_nep2 :: [Trans_eps] -> Char -> [String] -> [String]
do_trans_nep2 l c qs = rmDup (concatMap (do_trans_nep l c) qs)


-- ------------------------------------------------------------------------------
-- Función que obtiene los estados destino de una transición que no es de tipo 
-- epsilon.
-- Devuelve la lista de estados alcanzables desde el estado q_i, con el símbolo
-- especificado.
-- ------------------------------------------------------------------------------
do_trans_nep :: [Trans_eps] -> Char -> String -> [String]
do_trans_nep l c q = concat [dest | (src, Just ch, dest) <- l, src == q, ch == c]


-- ------------------------------------------------------------------------------
-- Funcion para convertir Maybe Char a Char
-- Si el valor de Maybe Char es Nothing, lo caracterizamos con el simbolo ~
-- ------------------------------------------------------------------------------
to_char :: Maybe Char -> Char
to_char Nothing = '~'
to_char (Just a) = a

---------------------------------------------------------------------------
-- Elimina todas las transiciones que van al vacío.
---------------------------------------------------------------------------
filterEmptyTransitions :: [Trans_afn] -> [Trans_afn]
filterEmptyTransitions =
  filter (\(_, _, dest) -> dest /= [])


-- Ejemplo de uso:
-- El automáta que reconoce la expresión (0+1)*1, probado, concuerda con el resultado esperado.
m2 = AFN {
  estadosN = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"],
  alfabetoN = "01",
  transicionesN = [("q0",'0',["q0","q1","q2","q4","q5","q7","q8"]),
                   ("q0",'1',[]),
                   ("q1",'0',["q0","q1","q2","q4","q5","q7","q8"]),
                   ("q1",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
                   ("q2",'0',[]),
                   ("q2",'1',["q0","q2","q3","q4","q5","q7","q8"]),
                   ("q3",'0',["q0","q1","q2","q4","q5","q7","q8"]),
                   ("q3",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
                   ("q4",'0',["q0","q1","q2","q4","q5","q7","q8"]),
                   ("q4",'1',["q0","q2","q3","q4","q5","q7","q8"]),
                   ("q5",'0',["q0","q1","q2","q4","q5","q7","q8"]),
                   ("q5",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
                   ("q6",'0',["q0","q1","q2","q4","q5","q7","q8"]),
                   ("q6",'1',["q0","q2","q3","q4","q5","q7","q8","q9"]),
                   ("q7",'0',[]),("q7",'1',["q9"]),("q8",'0',[]),
                   ("q8",'1',["q9"]),
                   ("q9",'0',[]),
                   ("q9",'1',[])],
  inicialN = "q6",
  finalN = "q9"}