-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa: AFDmin -> MDD
-- | Equipo: Los Discipulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel

module MDD where

import qualified Data.Set as Set
import AFDmin
import AFD

-- ------------------------------------------------------------------------------
-- Definición del tipo de dato Trans_mdd para representar las
-- asignaciones de categorías léxicas a los estados finales.
-- Utilizamos una tupla para representar esta función, donde:
-- El primer lugar se opuca por un estado final.
-- El segundo lugar se ocupa por la categoría léxica.
-- ------------------------------------------------------------------------------
type Trans_mdd = (String, String)

data ErrorMDD = ErrorMDD
                deriving (Show)

-- ------------------------------------------------------------------------------
-- Definición del tipo de dato MDD para representar a una
-- Máquina Discriminadora Determinista.
-- MDD = (Q, Σ, δ, q_s, q_f, µ, ┴), donde
-- > Q = estados. Representados en una lista de cadenas
-- > Σ = alfabeto. Representado como una lista de caracteres
-- > δ =  transiciones. Conjunto de transiciones representadas en una lista de Trans_afd
-- > q_s = inicial. Estado inicial del automata representado por un string
-- > q_f = final. Lista de estados finales del automata representados por un string
-- > µ = 
-- > ┴ = 
-- ------------------------------------------------------------------------------
data MDD = MDD {
  estadosMDD :: [String],
  alfabetoMDD :: [Char],
  transicionesMDD :: [Trans_afd],
  inicialMDD :: String,
  finalMDD :: [String],
  asignaMDD :: [Trans_mdd],
  errorMDD :: ErrorMDD
} deriving (Show)

-- [(token, caracter)]
type LlaveTokens = [(String,Char)]


getMDD :: String -> LlaveTokens -> MDD
getMDD s xs = afd_to_MDD (getAFDmin s) xs

afd_to_MDD :: AFD -> LlaveTokens -> MDD
afd_to_MDD afd xs =MDD
      {
        estadosMDD = estadosMDD
      , alfabetoMDD = alfabetoD afd
      , transicionesMDD = transMDD
      , inicialMDD = inicialD afd
      , finalMDD = finales
      , asignaMDD = funAsig
      , errorMDD = ErrorMDD
      } where
        funAsig = obtenerFunMDD (finalD afd) afd xs
        finales = obtenerFinales funAsig
        
        transNoUtiles = (obtenerTransG finales afd) ++ (obtenerTansF (finalD afd) afd)
        estadosNoUtiles = estadosInutiles transNoUtiles

        estadosMDD =  Set.toList( Set.difference(Set.fromList(estadosD afd)) (estadosNoUtiles) )
        transMDD = Set.toList (Set.difference(Set.fromList(transicionesD afd)) ([Set.fromList (transNoUtiles)]) )




-----------------------------------------------------------------------
procesarTokens :: String -> MDD -> [(String, String)]
procesarTokens cadena mdd = let inicial = inicialMDD mdd in 
  procesarAux cadena inicial [] mdd

procesarAux :: String -> String -> [(String, String)] -> MDD -> [(String, String)]
procesarAux [] estadoA visitados mdd =
            if estadoA == inicial && (length visitados == 0) then []
            else
              if estadoA `elem` finales then
                [(obtenerToken estadoA mdd, obtenerContenido visitados) ]
              else
               [("error",".")]
            where
              inicial = inicialMDD mdd
              finales = finalMDD mdd

procesarAux (x:xs) estadoA visitados mdd = let transiciones = transicionesMDD mdd in
            if (hayTrancision x estadoA transiciones)then
              procesarAux xs (funcionTransicionCaracter x estadoA transiciones) (([x],estadoA):visitados) mdd
            else regresar (x:xs) estadoA visitados mdd

regresar :: String -> String -> [(String, String)] -> MDD -> [(String, String)]
regresar cadena estadoA [] mdd = [("error",".")]
regresar cadena estadoA ((c, estadoR):xs) mdd = if estadoA `elem` finales then
                                                [(obtenerToken estadoA mdd, obtenerContenido ((c, estadoR):xs))] ++ procesarAux cadena inicial [] mdd
                                              else
                                                regresar (c ++ cadena) estadoR xs mdd
                                              where 
                                                finales = finalMDD mdd
                                                inicial = inicialMDD mdd

obtenerToken :: String -> MDD -> String
obtenerToken estadoF mdd = do
              let finales = finalMDD mdd
                  funcionToken = asignaMDD mdd  
              case buscarToken estadoF funcionToken of
                [] -> error "No hay un token asociado al estado"
                (token):[] -> token
                _ -> error "El automata es no determinista"

buscarToken :: String -> [(String, String)] -> [String]
buscarToken estado tokens = [token | (estadof, token) <- tokens, estadof == estado]

obtenerContenido :: [(String, String)] -> String
obtenerContenido [] = ""
obtenerContenido ((c,estado):xs) = obtenerContenido xs ++ c

---------------------------------------------------------------


obtenerEstadosAnte :: AFD -> String -> Char -> [String]
obtenerEstadosAnte afd estadoA c = [x | (x, y, estadoF) <- transicionesD afd, estadoF == estadoA, c == y]

obtenerFunMDD :: [String] -> AFD -> LlaveTokens -> [Trans_mdd]
obtenerFunMDD [] _ _ = []
obtenerFunMDD (x:xs) afd tokens = (asignaToken x afd tokens) ++ (obtenerFunMDD xs afd tokens)

asignaToken :: String -> AFD -> LlaveTokens -> [Trans_mdd]
asignaToken estado afd [] = []
asignaToken estado afd ((token, c):xs) = do
            let 
              aux = obtenerEstadosAnte afd estado c 
            case (aux) of
              [] -> asignaToken estado afd xs 
              (y:ys) -> juntarTokenAux (y:ys) afd token ++ asignaToken estado afd xs
                
                
juntarTokenAux :: [String] -> AFD -> String -> [Trans_mdd]
juntarTokenAux [] _ _ = []
juntarTokenAux (y:ys) afd token = juntarToken (obtenerEstadosAnte afd y '#') token ++ juntarTokenAux ys afd token


juntarToken :: [String] -> String -> [Trans_mdd]
juntarToken [] _ = []
juntarToken (x:xs) token = ((x, token):(juntarToken xs token))

obtenerFinales :: [Trans_mdd] -> [String]
obtenerFinales [] = []
obtenerFinales ((estado, _):xs) = (estado):(obtenerFinales xs)

-- [String] finales de mdd
limpiarTransiciones :: [String] -> AFD ->  [Trans_afd]
limpiarTransiciones finales afd  = Set.toList(Set.difference transiciones transNoUtiles )
          where
            transiciones = Set.fromList (transicionesD afd)
            transNoUtiles = Set.fromList ((obtenerTransG finales afd) ++ (obtenerTansF (finalD afd) afd))
 

obtenerTransG :: [String] -> AFD -> [Trans_afd]
obtenerTransG [] _ = []
obtenerTransG (x:xs) afd = buscarEstados '#' x (transicionesD afd) ++ obtenerTransG xs afd

obtenerTansF :: [String] -> AFD -> [Trans_afd]
obtenerTansF [] _ = []
obtenerTansF (x:xs) afd = [transicion | transicion@(_, _, y) <- (transicionesD afd), y == x] ++ obtenerTansF xs afd

estadosInutiles :: [Trans_afd] -> Set.Set String
estadosInutiles estados = (Set.fromList([estadoI | (_, _, estadoI) <- estados]))

mddEjemplo = MDD
      {
        estadosMDD = ["q0", "q1", "q2", "q3", "q4"]
      , alfabetoMDD = ['a', 'b', 'c', 'd']
      , transicionesMDD = 
        [ ("q0",'a',"q1"), ("q0",'c',"q3")
        , ("q1",'b',"q2"), ("q3",'d',"q4")
        ]
      , inicialMDD = "q0"
      , finalMDD = ["q2", "q4"]
      , asignaMDD = [("q2","Tab"), ("q4","Tcd")]
      , errorMDD = ErrorMDD
      }

afdPruebas = AFD 
  { estadosD = ["q0", "q1", "q2", "q3", "q4", "q5", "q6", "q7","q8","q9"]
  , alfabetoD = ['a', 'b', 'c', 'd', '#', '{', '}','!']
  , transicionesD = 
      [ ("q0",'a',"q1"), ("q0",'c',"q3")
      , ("q1",'b',"q2"), ("q3",'d',"q4")
      , ("q2",'#',"q5"), ("q4",'#',"q6")
      , ("q5",'{',"q7"), ("q6",'}',"q7")
      , ("q2",'c',"q8"), ("q9",'!',"q7")
      , ("q8",'#',"q9")
      ]
  , inicialD = "q0"
  , finalD = ["q7"]
  }

MDD {estadosMDD = ["q1","q2","q3","q4","q5","q9"]
   , alfabetoMDD = "!#abcd{}"
   , transicionesMDD = 
    [("q1",'d',"q3"),("q2",'b',"q5")
    ,("q5",'c',"q4"),("q9",'a',"q2")
    ,("q9",'c',"q1")]
   , inicialMDD = "q9"
   , finalMDD = ["q5","q4","q3"]
   , asignaMDD = [("q5","Tab"),("q4","Tabc"),("q3","Tcd")]
   , errorMDD = ErrorMDD}

