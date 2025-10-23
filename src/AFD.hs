-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa de conversión AFN -> AFD
-- | Equipo: Los Discipulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
module AFD where
    
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import AFN 

-- ------------------------------------------------------------------------------
-- Definición del tipo de dato Trans_afd para representar 
-- las transiciones que el automata debe seguir.
-- Utilizamos una tercia para representar la función delta, donde:
-- El primer lugar se ocupa por el estado actual.
-- El segundo lugar se ocupa por un caracter del alfabeto.
-- El tercer lugar es el estado alcanzable con el símbolo especificado.
-- ------------------------------------------------------------------------------
type Trans_afd = (String, Char, String)

-- ------------------------------------------------------------------------------
-- Definición de 2 tipos StateSet y StateCache, siendo el primero 
-- útil para definir un conjunto de estados para el AFD, mientas que el último
-- siendo usado para ligar estados con conjuntos de estados y poder acceder 
-- a ellos en tiempo O(log n)
-- ------------------------------------------------------------------------------
type StateSet = Set.Set String
type StateCache = Map.Map StateSet String


-- ------------------------------------------------------------------------------
-- Definicion del tipo de dato AFD para representar a un autómata finito 
-- determinista.
-- AFD = (Q, Σ, δ, q_s, q_f), donde
-- > Q = estados. Representados en una lista de cadenas
-- > Σ = alfabeto. Representado como una lista de caracteres
-- > δ = transiciones. Conjunto de transiciones representadas en una lista de Trans_afd
-- > q_s = inicial. Estado inicial del automata representado por un string
-- > q_f = final. Lista de estados finales del automata representados por un string
-- ------------------------------------------------------------------------------
data AFD = AFD {
  estadosD :: [String],
  alfabetoD :: [Char],
  transicionesD :: [Trans_afd],
  inicialD :: String,
  finalD :: [String]
} deriving (Show)


-- ------------------------------------------------------------------------------
-- Función que dada una cadena, obtiene la representación de su expresión regular
-- y, en caso de que exista, genera el autómata determinista para la regex.
-- Si no existe, notifica la ausencia de la expresion regular.
-- ------------------------------------------------------------------------------
getAFD :: String -> AFD
getAFD s = afn_to_AFD (getAFN s)

-- ------------------------------------------------------------------------------
-- Función principal para convertir un AFN (autómata finito no determinista) 
-- a un AFD (autómata finito determinista).
-- ------------------------------------------------------------------------------
afn_to_AFD :: AFN -> AFD
afn_to_AFD n = AFD {
    estadosD = allStateNames,
    alfabetoD = alfabetoN n,
    transicionesD = allTransitions,
    inicialD = "q0",
    finalD = finalStates
}
    where
        -- Estado inicial en forma de conjunto
        initialSet = Set.singleton (inicialN n)
        -- Inicialización del cache con el estado inicial
        initialCache = Map.singleton initialSet "q0"
        
        -- Todos los estados y transiciones del AFD
        (finalCache, allTransitions) = 
            buildAFD n [initialSet] initialCache [] 1
        
        allStateNames = Map.elems finalCache
        
        -- Recopilación de estados finales
        finalStates = findFinalStates (finalN n) finalCache

-- ------------------------------------------------------------------------------
-- Función auxiliar para construir todos los estados del autómata
-- finito determinista que toma los siguientes parámetros:
--    n: El AFN original.
--    worklist: [StateSet] -> StateSets que falta por procesar.
--    cache: StateCache -> El Map que contiene todos los estados conocidos
--    al momento.
--    accTrans: [Trans_afd] -> Transiciones conocidas al momento.
--    nextStateId: Int -> El índice para nombrar el próximo nombre de estado.
-- ------------------------------------------------------------------------------
buildAFD :: AFN -> [StateSet] -> StateCache -> [Trans_afd] -> Int -> (StateCache, [Trans_afd])
buildAFD _ [] cache accTrans _ = (cache, accTrans) -- Caso base, worklist vacía
buildAFD n (currentSet:restWorklist) cache accTrans nextStateId =
    -- Recurisividad con los nuevos elementos generados al procesar el estado actual
    buildAFD n (newWorklistItems ++ restWorklist) newCache (accTrans ++ newTransitions) newNextStateId
    where
        -- Obtenemos el nombre del estado actual a procesar
        currentName = fromMaybe (error "Not in cache") (Map.lookup currentSet cache)
        
        --  Procesamos todos los simbolos del alfabeto sobre el conjunto de estados actual
        (newTransitions, newWorklistItems, newCache, newNextStateId) =
            foldl' (processSymbol n currentSet currentName) ([], [], cache, nextStateId) (alfabetoN n)

-- ------------------------------------------------------------------------------
-- Función auxiliar para procesar todos los estados alcanzables dentro de un 
-- StateSet, en caso de encontrar un nuevo conjunto de estados alcanzables
-- creamos una nueva entrada StateCache y transiciones, de otro modo solo  
-- regresamos las nuevas transiciones
-- ------------------------------------------------------------------------------
processSymbol :: AFN -> StateSet -> String
              -> ([Trans_afd], [StateSet], StateCache, Int)
              -> Char
              -> ([Trans_afd], [StateSet], StateCache, Int)
processSymbol n currentSet currentName (accTrans, accWorklist, accCache, accId) symbol

    | Set.null targetSet = (accTrans, accWorklist, accCache, accId)
    | otherwise =
        case Map.lookup targetSet accCache of
            
            -- Caso 1: El StateSet destino ya existe.
            Just targetName ->
                let newTrans = (currentName, symbol, targetName)
                in (newTrans:accTrans, accWorklist, accCache, accId)
            
            -- Caso 2: Es un StateSet nuevo.
            Nothing ->
                let newName = "q" ++ show accId
                    newCache = Map.insert targetSet newName accCache -- Insertamos clave/valor al cache
                    newTrans = (currentName, symbol, newName)
                in
                    -- Agegamos un nuevo conjunto de estados para ser procesado en la worklist
                    (newTrans:accTrans, targetSet:accWorklist, newCache, accId + 1)
    where
        -- Calulamos el conjunto de estados destino usando move 
        targetSet = move n currentSet symbol


-- ------------------------------------------------------------------------------
-- Función auxiliar para encontrar los estados alcanzables a partir de un 
-- conjunto de estados
-- ------------------------------------------------------------------------------
move :: AFN -> StateSet -> Char -> StateSet
move n fromStates symbol = Set.fromList allDestinations
    where
        allDestinations = [ dest
                        | (s', c, dests) <- transicionesN n 
                        , c == symbol                     
                        , s' `Set.member` fromStates  
                        , dest <- dests
                        ]

-- ------------------------------------------------------------------------------
-- Función auxiliar para encontrar todos los estados finales 
-- en función del diccionario de estados, un estado final es aquel que contiene a 
-- al menos un estado final del autómata finito no determinista
-- ------------------------------------------------------------------------------
findFinalStates :: String -> StateCache -> [String]
findFinalStates finalStateNFA cache =
    -- Recuperamos todos los super estados que cumplan con contener un estado final 
    Map.elems (Map.filterWithKey (\k _v -> finalStateNFA `Set.member` k) cache)





funcionTransicionCaracter :: Char -> String -> [Trans_afd] -> String
funcionTransicionCaracter inputChar currentState transitions =
    case resultado of
        [] -> error "No hay una transicion disponible"
        (start, c, end):[] -> end
        _ -> error "El automata es no determinista"
    where
        resultado = buscarEstados inputChar currentState transitions

buscarEstados :: Char -> String -> [Trans_afd] -> [Trans_afd]
buscarEstados inputChar currentState transitions = 
    [ t | t@(q1, c, _) <- transitions, q1 == currentState, c == inputChar ]

funcionTransicionCadena :: String -> String -> [Trans_afd] -> String
funcionTransicionCadena [] currentState _ = currentState
funcionTransicionCadena (c:cs) currentState transitions = 
    funcionTransicionCadena cs (funcionTransicionCaracter c currentState transitions) transitions

hayTrancision :: Char -> String -> [Trans_afd] -> Bool
hayTrancision inputChar currentState transitions = 
    not (null (buscarEstados inputChar currentState transitions))