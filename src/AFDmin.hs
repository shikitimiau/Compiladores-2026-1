---------------------------------------------------------------------------
-- | Proyecto 1: Constructor de un Analizador Léxico
-- | Etapa: AFD -> AFDmin
-- | Equipo: Los Discípulos de Church
-- | Integrantes:
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
--
-- | Descripción:
-- |   Este módulo implementa la minimización de autómatas finitos deterministas (AFD).
-- |   La función principal `getAFDmin` recibe un AFD y devuelve otro AFD equivalente,
-- |   pero con el menor número posible de estados, siguiendo los pasos clásicos del
-- |   algoritmo de minimización:
-- |
-- |     1. Eliminación de estados inalcanzables.
-- |     2. Construcción y refinamiento de particiones (estados equivalentes).
-- |     3. Agrupación y reconstrucción del autómata resultante.
---------------------------------------------------------------------------
module AFDmin where

import AFD
import Data.List (sort, partition)
import qualified Data.Set as Set
import qualified Data.Map as Map


-- ------------------------------------------------------------------------------
-- Función que dada una cadena, obtiene la representación de su expresión regular
-- y, en caso de que exista, genera el autómata determinista minimizado para la regex.
-- Si no existe, notifica la ausencia de la expresion regular.
-- ------------------------------------------------------------------------------
getAFDmin :: String -> AFD
getAFDmin s = minimizaAFD $ getAFD s


---------------------------------------------------------------------------
-- Función principal: obtiene el AFD mínimo de un AFD dado
--
--   Aplica los pasos:
--   1. Eliminar estados inalcanzables.
--   2. Agrupar estados equivalentes.
--   3. Reconstruir el nuevo AFD con grupos como nuevos estados.
---------------------------------------------------------------------------
minimizaAFD :: AFD -> AFD
minimizaAFD = minimizaAFDDesde 0

-- | Función auxiliar que permite elegir desde qué número inician los nombres de los grupos.
minimizaAFDDesde :: Int -> AFD -> AFD
minimizaAFDDesde start afd =
  let 
      -- Paso 1: Eliminación de estados inalcanzables
      afdReachable = removeUnreachable afd

      -- Paso 2: Agrupar estados equivalentes mediante refinamiento
      gruposEquiv  = groupEquivalentsDesde start afdReachable

      -- Paso 3: Construcción de las transiciones del nuevo AFD
      transMinRaw  = buildTransitions afdReachable gruposEquiv

      -- Eliminamos las transiciones hacia "Trash"
      transMin     = filterTrashTransitions transMinRaw

      -- Nuevo estado inicial: el grupo que contiene al estado inicial original
      iniMin       = findGroupName (inicialD afdReachable) gruposEquiv

      -- Estados finales: grupos que contienen al menos un estado final original
      -- Usamos Set para evitar duplicados eficientemente
      finalSet     = Set.fromList (finalD afdReachable)
      finMin       = [ gName | (gName, gStates) <- gruposEquiv
                             , any (`Set.member` finalSet) gStates ]
  in 
      -- Se crea el nuevo AFD mínimo
      AFD
        { estadosD = map fst gruposEquiv
        , alfabetoD = alfabetoD afdReachable
        , transicionesD = transMin
        , inicialD = iniMin
        , finalD = finMin
        }


---------------------------------------------------------------------------
-- Paso 1: Eliminación de estados inalcanzables
--
--   Esta función filtra el AFD para conservar solo los estados
--   que pueden alcanzarse desde el estado inicial mediante alguna secuencia
--   de transiciones válidas.
---------------------------------------------------------------------------
removeUnreachable :: AFD -> AFD
removeUnreachable afd =
  let 
      -- Calcula el conjunto de estados alcanzables desde el inicial
      reachableSet = reachableStates afd (Set.singleton (inicialD afd))
      reachables   = Set.toList reachableSet
      
      -- Convertimos a Set para búsquedas eficientes
      reachableSet' = reachableSet
  in 
      -- Se filtran los componentes del AFD (estados, transiciones, finales)
      afd 
        { estadosD = reachables
        , transicionesD = filter (\(e,_,_) -> Set.member e reachableSet') (transicionesD afd)
        , finalD = filter (`Set.member` reachableSet') (finalD afd)
        }


---------------------------------------------------------------------------
-- Calcula recursivamente los estados alcanzables desde un conjunto inicial
--
--   Usa Set para operaciones eficientes de unión y pertenencia
---------------------------------------------------------------------------
reachableStates :: AFD -> Set.Set String -> Set.Set String
reachableStates afd visited =
  let 
      -- Obtiene los estados alcanzables directamente desde los visitados
      next = Set.fromList [ q' | (q,_,q') <- transicionesD afd, Set.member q visited ]

      -- Combina los nuevos con los ya visitados
      new  = visited `Set.union` next
  in 
      -- Caso base: si ya no hay nuevos estados, la búsqueda termina
      if new == visited 
        then visited 
        else reachableStates afd new   -- Recursión hasta alcanzar un conjunto estable


---------------------------------------------------------------------------
-- Paso 2 y 3: Construcción y refinamiento de particiones
--
--   Se definen grupos (particiones) de estados equivalentes.
--   El refinamiento separa grupos hasta alcanzar una partición estable.
--   Agrupa los estados equivalentes y asigna nombres nuevos a partir start, 
--   por ejemplo: start = 0, nombres "q0", "q1", ...
---------------------------------------------------------------------------
type Grupo = (String, [String])  -- (nombre del grupo, estados que contiene)
groupEquivalentsDesde :: Int -> AFD -> [Grupo]
groupEquivalentsDesde start afd =
  let 
      allStates   = estadosD afd
      finalSet    = Set.fromList (finalD afd)
      
      -- Usamos filter en lugar de \\ para mejor eficiencia
      nonFinals   = filter (`Set.notMember` finalSet) allStates

      -- Partición inicial: finales vs no finales (base de la minimización)
      initPart    = [finalD afd, nonFinals]

      -- Refinamiento de la partición inicial hasta que no cambie más
      refined     = refinePartition afd initPart

      -- Se renombran los grupos comenzando desde 'start'
      namedGroups = zipWith (\i g -> ("q" ++ show (start + i), g)) [0..] refined
  in 
      namedGroups


---------------------------------------------------------------------------
-- Refinar la partición hasta alcanzar un punto fijo.
--
--   Usa recursión: compara la nueva partición con la anterior.
--   Si son iguales (tras ordenarlas), termina.
---------------------------------------------------------------------------
refinePartition :: AFD -> [[String]] -> [[String]]
refinePartition afd part =
  let 
      -- Creamos un mapa de estado a grupo para búsquedas rápidas
      groupMap = buildGroupMap part
      
      -- 'concatMap' aplica la función 'splitGroup' a cada grupo de la partición actual.
      newPart = concatMap (splitGroup afd part groupMap) part
  in 
      -- Se compara la nueva partición con la anterior (tras ordenar los grupos).
      -- Si son idénticas, significa que la partición se estabilizó: 
      if sort newPart == sort part
        then newPart
        -- En caso contrario, se continúa refinando la nueva partición.
        else refinePartition afd newPart


---------------------------------------------------------------------------
-- Construye un mapa de estado a grupo para búsquedas rápidas
---------------------------------------------------------------------------
buildGroupMap :: [[String]] -> Map.Map String [String]
buildGroupMap part =
  Map.fromList [ (q, g) | g <- part, q <- g ]


---------------------------------------------------------------------------
-- Divide un grupo según las transiciones de sus estados.
--
--   Si dos estados se comportan diferente (van a distintos grupos para algún símbolo),
--   se separan en subgrupos.
---------------------------------------------------------------------------
splitGroup :: AFD -> [[String]] -> Map.Map String [String] -> [String] -> [[String]]
splitGroup afd part groupMap group =
  let 
      -- Creamos un mapa de transiciones para búsquedas O(1)
      transMap = Map.fromList [ ((q, a), q') | (q, a, q') <- transicionesD afd ]
      
      -- 'classify' construye, para cada estado q, una lista que representa su "firma":
      --   es decir, a qué grupo va a parar con cada símbolo del alfabeto.
      classify q = [ findGroupWithMap (delta' transMap q a) groupMap | a <- alfabetoD afd ]

      -- 'groupByEq' usa esa clasificación para reunir en el mismo subgrupo
      --   aquellos estados con firmas idénticas.
      grouped = groupByEq (\q1 q2 -> classify q1 == classify q2) group
  in 
      -- El resultado es una lista de subgrupos derivados del grupo original.
      grouped


---------------------------------------------------------------------------
-- Versión optimizada de delta usando Map para búsquedas O(1)
---------------------------------------------------------------------------
delta' :: Map.Map (String, Char) String -> String -> Char -> String
delta' transMap q a = Map.findWithDefault "" (q, a) transMap


---------------------------------------------------------------------------
-- Busca el grupo que contiene un estado dado usando el mapa (más rápido)
---------------------------------------------------------------------------
findGroupWithMap :: String -> Map.Map String [String] -> [String]
findGroupWithMap q groupMap = Map.findWithDefault ["Trash"] q groupMap


---------------------------------------------------------------------------
-- Busca el grupo que contiene un estado dado.
--
--   Si el estado no pertenece a ningún grupo (por ejemplo, no tiene transiciones),
--   se devuelve un grupo ficticio ["Trash"] que representa un estado trampa.
---------------------------------------------------------------------------
findGroup :: String -> [[String]] -> [String]
findGroup q part =
  case [g | g <- part, q `elem` g] of
    (x:_) -> x        -- Se encontró el grupo que contiene al estado q
    []    -> ["Trash"]    -- Si no pertenece a ningún grupo, retorna el grupo trampa


---------------------------------------------------------------------------
-- Evalúa la función de transición δ(q, a).
--
--   Busca en la lista de transiciones del AFD la que parte de q con el símbolo a.
--   Si la encuentra, devuelve el estado destino. Si no, devuelve cadena vacía ("").
---------------------------------------------------------------------------
delta :: AFD -> String -> Char -> String
delta afd q a =
  case [q' | (q0, x, q') <- transicionesD afd, q0 == q, x == a] of
    (r:_) -> r   -- caso normal: existe transición válida
    []    -> ""  -- sin transición definida


---------------------------------------------------------------------------
-- Paso 4: Construcción del AFD mínimo
--
--   Crea las nuevas transiciones reemplazando estados originales
--   por sus respectivos grupos.
---------------------------------------------------------------------------
buildTransitions :: AFD -> [Grupo] -> [Trans_afd]
buildTransitions afd grupos =
  let
      -- Precomputamos un mapa de estado a nombre de grupo para eficiencia
      stateToGroupMap = Map.fromList 
        [ (state, groupName) 
        | (groupName, states) <- grupos 
        , state <- states 
        ]
      
      -- Generamos todas las transiciones y usamos Map para eliminar duplicados
      -- usando la tripleta (origen, símbolo, destino) como clave
      transitionsMap = Map.fromListWith (\_ old -> old)  -- Mantener el primero encontrado
          [ ((gName, a), Map.findWithDefault "Trash" (delta afd (head gStates) a) stateToGroupMap)
          | (gName, gStates) <- grupos        -- Para cada grupo de estados equivalentes
          , not (null gStates)                -- (evita grupos vacíos)
          , a <- alfabetoD afd                -- Para cada símbolo del alfabeto
          ]
  in
      -- Reconstruimos las transiciones desde el mapa
      [ (origen, simbolo, destino) 
      | ((origen, simbolo), destino) <- Map.toList transitionsMap 
      ]


---------------------------------------------------------------------------
-- Dado un estado original, devuelve el nombre del grupo que lo contiene.
---------------------------------------------------------------------------
findGroupName :: String -> [Grupo] -> String
findGroupName q grupos =
  case [n | (n, qs) <- grupos, q `elem` qs] of
    (x:_) -> x    -- Se encontró el nombre del grupo correspondiente
    []    -> "Trash"  -- Si no está en ninguno, se asigna al grupo trampa


---------------------------------------------------------------------------
-- Devuelve los estados asociados a un grupo dado.
---------------------------------------------------------------------------
fromMaybeGroup :: String -> [Grupo] -> [String]
fromMaybeGroup name grupos =
  case [qs | (n, qs) <- grupos, n == name] of
    (x:_) -> x
    []    -> []


---------------------------------------------------------------------------
-- Agrupa elementos según una relación de equivalencia proporcionada.
--
--   Implementa una forma genérica de "group by" recursivo.
--   Dado un predicado eq, construye grupos de elementos equivalentes entre sí.
---------------------------------------------------------------------------
groupByEq :: (a -> a -> Bool) -> [a] -> [[a]]
groupByEq _ [] = []
groupByEq eq (x:xs) =
  let 
      -- Se separan los elementos que son equivalentes a 'x' 
      -- según la función de equivalencia 'eq'
      (iguales, distintos) = partition (eq x) xs
  in 
      -- Se forma un grupo con 'x' y todos sus equivalentes,
      -- y se continúa recursivamente con los restantes
      (x:iguales) : groupByEq eq distintos

---------------------------------------------------------------------------
-- Elimina todas las transiciones que van al estado "Trash".
---------------------------------------------------------------------------
filterTrashTransitions :: [Trans_afd] -> [Trans_afd]
filterTrashTransitions =
  filter (\(_, _, dest) -> dest /= "Trash")

---------------------------------------------------------------------------
-- Ejemplo de uso:
-- 
-- Supongamos que se tiene un AFD "m2" definido en el módulo AFD:
-- 
--     > getAFDmin m2
-- 
-- Esto devolverá un AFD equivalente pero con el número mínimo de estados.
---------------------------------------------------------------------------
