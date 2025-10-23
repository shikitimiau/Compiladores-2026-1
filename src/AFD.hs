-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa: AFN -> AFD
-- | Equipo: Los Discipulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
module AFD where
    
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map
import  AFN

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
-- Definición del tipo de dato SuperState para representar 
-- a un estado con nombre y su lista de estados asociado
-- este tipo de dato tiene como propósito poder renombrar los estados del automata
-- determinista
-- Utilizamos una dupla para representar el super estado, donde:
-- El primer lugar se ocupa por el estado actual.
-- El segundo lugar se ocupa por la lista de estados que conforman al estado.
-- ------------------------------------------------------------------------------
type SuperState = (String, [String])

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
 estadosD = map fst allSuperStates,
 alfabetoD = alfabetoN n,
 transicionesD = allTransitions,
 inicialD = fst initialSuperState,
 finalD = finalStates
}  where 
    -- 1. Estado inicial del AFD: el conjunto que solo contiene el estado inicial del AFN.
    -- Usamos sort y nub para tener una representación canónica.
    initialAFNStates = Set.singleton (inicialN n)
    initialSuperState = ("q0", Set.toList initialAFNStates)
    
    -- 2. El ciclo principal: construir recursivamente los estados y transiciones.
    -- Empezamos con una worklist que solo tiene el estado inicial.
    -- El '1' es el contador para nombrar nuevos estados (S1, S2, ...).
    (allSuperStates, allTransitions) = buildAFD n [initialSuperState] [] [] 1
    
    -- 3. Determinar los estados finales del AFD.
    finalStates = findFinalStates (finalN n) allSuperStates



-- ------------------------------------------------------------------------------
-- Función auxiliar para construir todos los estados del automata finito 
-- determinista, requiere de los siguientes elementos
--   n, El AFN original.
--   worklist, Super-estados que faltan por procesar.
--   processed, Super-estados ya procesados.
--   transitions, Transiciones del AFD encontradas hasta ahora.
--   nextStateId, El índice para nombrar el próximo nuevo super-estado.
-- ------------------------------------------------------------------------------
buildAFD :: AFN -> [SuperState] -> [SuperState] -> [Trans_afd] -> Int -> ([SuperState], [Trans_afd])
buildAFD _ [] processed trans _ = (processed, trans) -- Caso base: la worklist está vacía.
buildAFD n (currentSuperState:restWorklist) processed trans nextStateId = 
    -- Regresamos la nueva worklist con los elementos nuevos a procesar, los estados ya procesados, las transiciones y el ID para nuevos estados
    buildAFD n (restWorklist ++ newWorklistItems) (currentSuperState:processed) (trans ++ newTransitions) finalNextStateId
    where 
        -- Descomponemos el Superestado en sus elementos de dupla
        (currentName, currentAFNStates) = currentSuperState
        -- Tomamos el alfabeto del Autómata Finito No Determinista
        alfabeto = alfabetoN n
        
        -- Iteramos sobre todos los estados que conforman al SuperEstado actual usando el alfabeto
        (newTransitions, newWorklistItems, finalNextStateId) =
            foldl (processSymbol n allKnownStates currentSuperState) ([], [], nextStateId) alfabeto

        allKnownStates = processed ++ (currentSuperState:restWorklist)

        

-- ------------------------------------------------------------------------------
-- Función auxiliar para procesar todos los estados alcanzables dentro de un 
-- superestado, en caso de encontrar un nuevo conjunto de estados alcanzables
-- creamos un nuevo superEstado, de otro modo solo regresamos las 
-- nuevas transiciones
-- ------------------------------------------------------------------------------
processSymbol :: AFN -> [SuperState] -> SuperState -> ([Trans_afd], [SuperState], Int) -> Char -> ([Trans_afd], [SuperState], Int)
processSymbol n allKnownStates (currentName, currentAFNStates) (accTrans, accWorklist, accId) symbol
    -- No encontramos transiciones
    | Set.null targetSet = (accTrans, accWorklist, accId)
    | otherwise =
        case maybeTargetSuperState of
            -- Caso 1: El super-estado destino ya existe.
            Just (targetName, _) -> ((currentName, symbol, targetName) : accTrans, accWorklist, accId)
            
            -- Caso 2: Es un super-estado nuevo.
            Nothing -> (newTrans : accTrans, newSuperState : accWorklist, accId + 1)
    where
        -- Se calcula el conjunto de estados destino y se verifica si está vacío.
        targetSet = move n (Set.fromList currentAFNStates) symbol

        -- Se busca si ya contamos con el superEstado.
        maybeTargetSuperState = findSuperStateByContent (Set.toList targetSet) allKnownStates

        -- Se crea un nuevo superEstado en función de los parametros y los estados alcanzables
        newName = "q" ++ show accId
        newSuperState = (newName, Set.toList targetSet)
        newTrans = (currentName, symbol, newName)


-- ------------------------------------------------------------------------------
-- Función auxiliar para encontrar los estados alcanzables a partir de un 
-- conjunto de estados (Funciona como la concatenación de estados)
-- ------------------------------------------------------------------------------
move :: AFN -> Set.Set String -> Char -> Set.Set String
move n fromStates symbol =
    Set.fromList [ dest
                 | s <- Set.toList fromStates
                 , (s', c, dests) <- transicionesN n
                 , s == s' && c == symbol
                 , dest <- dests
                 ]
-- ------------------------------------------------------------------------------
-- Función auxiliar para encontrar todas las transiciones para un estado 
-- dado un símbolo, basandonos en los estados de transición del 
-- autómata finito determinista
-- no es necesaria? 
-- ------------------------------------------------------------------------------
findTransitionsForState :: AFN -> String -> Char -> [String]
findTransitionsForState n state symbol =
    -- Se usa foldr para encontrar la transición y devolver la lista de estados, o [] si no existe.
    foldr (\(s, c, dest) acc -> if s == state && c == symbol then dest else acc) [] (transicionesN n)

-- ------------------------------------------------------------------------------
-- Busca un super-estado en una lista por su contenido (la lista de estados del AFN).
-- Función auxiliar para buscar un superEstado a partir de una lista de estados
-- es decir, a partir del contenido 
-- ------------------------------------------------------------------------------
findSuperStateByContent :: [String] -> [SuperState] -> Maybe SuperState
findSuperStateByContent _ [] = Nothing
findSuperStateByContent content ((name, stList):rest)
    | sContent == Set.fromList stList = Just (name, stList) -- Los contenidos son iguales, regresamos el superEstado
    | otherwise = findSuperStateByContent content rest -- Buscamos recursivamente en el resto de la lista
    where sContent = Set.fromList content

-- ------------------------------------------------------------------------------
-- Función auxiliar para encontrar todos los estados finales 
-- en función de los SuperEstados, un estado final es aquel que contiene a 
-- al menos un estado final del autómata finito no determinista
-- ------------------------------------------------------------------------------
findFinalStates :: String -> [SuperState] -> [String]
-- Recuperamos todos los super estados que cumplan con contener un estado final 
-- tener el elemento inicial de la tupla (El estado)
findFinalStates finalStateAFN allSuperStates = [name | (name, afnStates) <- allSuperStates, finalStateAFN `elem` afnStates]

-- ------------------------------------------------------------------------------
-- Función que realiza la el prosesamiento de un caracter 'c' con un estado 'q' con
-- una lista de trancisiones, si no existe la transicion o si hay mas de una, devuelve
-- un error.
-- 
-- x, el caracter a procesar
-- estadoA, el estado que se usará para la transicion
-- transiciones, todas las transiciones disponibles
-- ------------------------------------------------------------------------------
funcionTransicionCaracter :: Char -> String -> [Trans_afd] -> String
funcionTransicionCaracter x estadoA transiciones = do
        let resultado = buscarEstados x estadoA transiciones
        case resultado of
            [] -> error "No hay una transicion dispoble"
            [(estadoI, c, estadoF)] -> estadoF
            _ -> error "El automata es no determinista"

-- ------------------------------------------------------------------------------
-- Funcion que busca todas las transiciones posibles con el caracter 'c' y el estado
-- 'q'
-- 
-- x, el caracter a procesar
-- estadoA, el estado que se usará para la transicion
-- transiciones, todas las transiciones disponibles
-- ------------------------------------------------------------------------------
buscarEstados :: Char -> String -> [Trans_afd] -> [Trans_afd]
buscarEstados x estadoA transiciones = [transicion | transicion@(q1, c, _) <- transiciones, q1 == estadoA, c == x]

-- ------------------------------------------------------------------------------
-- Función auxiliar que dado un caracter, un estado y una lista de transiciones
-- revisa si hay transiciones con el caracter y el estado dado
-- 
-- x, el caracter a procesar
-- estadoA, el estado que se usará para la transicion
-- transiciones, todas las transiciones disponibles
-- ------------------------------------------------------------------------------
hayTrancision :: Char -> String -> [Trans_afd] -> Bool
hayTrancision x estadoA transiciones = not (null (buscarEstados x estadoA transiciones))