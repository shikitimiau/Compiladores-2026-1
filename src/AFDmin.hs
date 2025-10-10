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
import Data.List (nub, sort, (\\), partition)

---------------------------------------------------------------------------
-- Función principal: obtiene el AFD mínimo de un AFD dado
--
--   Aplica los pasos:
--   1. Eliminar estados inalcanzables.
--   2. Agrupar estados equivalentes.
--   3. Reconstruir el nuevo AFD con grupos como nuevos estados.
---------------------------------------------------------------------------
getAFDmin :: AFD -> AFD
getAFDmin afd =
  let 
      -- Paso 1: Eliminación de estados inalcanzables
      afdReachable = removeUnreachable afd

      -- Paso 2: Agrupar estados equivalentes mediante refinamiento
      gruposEquiv  = groupEquivalents afdReachable

      -- Paso 3: Construcción de las transiciones del nuevo AFD
      transMin     = buildTransitions afdReachable gruposEquiv

      -- Nuevo estado inicial: el grupo que contiene al estado inicial original
      iniMin       = findGroupName (inicialD afdReachable) gruposEquiv

      -- Estados finales: grupos que contienen al menos un estado final original
      finMin       = nub [ gName | gName <- map fst gruposEquiv
                                 , any (`elem` finalD afdReachable)
                                       (fromMaybeGroup gName gruposEquiv) ]
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
      -- Calcula la lista de estados alcanzables desde el inicial
      reachables = reachableStates afd [inicialD afd]
  in 
      -- Se filtran los componentes del AFD (estados, transiciones, finales)
      afd 
        { estadosD = reachables
        , transicionesD = filter (\(e,_,_) -> e `elem` reachables) (transicionesD afd)
        , finalD = filter (`elem` reachables) (finalD afd)
        }

---------------------------------------------------------------------------
-- Calcula recursivamente los estados alcanzables desde un conjunto inicial
--
--   Usa recursión sobre listas: en cada paso se obtienen los nuevos estados
--   que pueden alcanzarse desde los ya visitados, hasta que el conjunto no crezca más.
---------------------------------------------------------------------------
reachableStates :: AFD -> [String] -> [String]
reachableStates afd visited =
  let 
      -- Obtiene los estados alcanzables directamente desde los visitados
      next = nub [ q' | (q,_,q') <- transicionesD afd, q `elem` visited ]

      -- Combina los nuevos con los ya visitados (evitando duplicados con nub)
      new  = nub (next ++ visited)
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
---------------------------------------------------------------------------
type Grupo = (String, [String])  -- (nombre del grupo, estados que contiene)

-- | Agrupa los estados equivalentes y asigna nombres nuevos ("q0", "q1", ...)
groupEquivalents :: AFD -> [Grupo]
groupEquivalents afd =
  let 
      allStates   = estadosD afd
      finals      = finalD afd
      nonFinals   = allStates \\ finals

      -- Partición inicial: finales vs no finales (base de la minimización)
      initPart    = [finals, nonFinals]

      -- Refinamiento de la partición inicial hasta que no cambie más
      refined     = refinePartition afd initPart

      -- Se renombran los grupos con etiquetas legibles y únicas
      namedGroups = zipWith (\i g -> ("q" ++ show i, g)) [0..] refined
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
      -- 'concatMap' aplica la función 'splitGroup' a cada grupo de la partición actual.
      --   splitGroup devuelve una lista de subgrupos resultantes al dividir el grupo
      --   original según el comportamiento de sus estados.
      --   Luego 'concatMap' concatena todas esas listas de subgrupos en una sola lista,
      --   produciendo así la nueva partición 'newPart'.
      newPart = concatMap (splitGroup afd part) part
  in 
      -- Se compara la nueva partición con la anterior (tras ordenar los grupos).
      -- Si son idénticas, significa que la partición se estabilizó: 
      --   ya no hay más distinciones entre estados equivalentes, y se devuelve el resultado.
      if sort newPart == sort part
        then newPart
        -- En caso contrario, se continúa refinando la nueva partición.
        -- Se llama recursivamente a 'refinePartition' con 'newPart' como nueva entrada.
        -- Esto asegura que el proceso se repita hasta alcanzar un punto fijo.
        else refinePartition afd newPart

---------------------------------------------------------------------------
-- Divide un grupo según las transiciones de sus estados.
--
--   Si dos estados se comportan diferente (van a distintos grupos para algún símbolo),
--   se separan en subgrupos.
---------------------------------------------------------------------------
splitGroup :: AFD -> [[String]] -> [String] -> [[String]]
splitGroup afd part group =
  let 
      -- 'classify' construye, para cada estado q, una lista que representa su "firma":
      --   es decir, a qué grupo va a parar con cada símbolo del alfabeto.
      --   Si dos estados tienen la misma firma, son indistinguibles hasta este punto.
      classify q = [ findGroup (delta afd q a) part | a <- alfabetoD afd ]

      -- 'groupByEq' usa esa clasificación para reunir en el mismo subgrupo
      --   aquellos estados con firmas idénticas.
      grouped = groupByEq (\q1 q2 -> classify q1 == classify q2) group
  in 
      -- El resultado es una lista de subgrupos derivados del grupo original.
      grouped

---------------------------------------------------------------------------
-- Busca el grupo que contiene un estado dado.
--
--   Si el estado no pertenece a ningún grupo (por ejemplo, no tiene transiciones),
--   se devuelve un grupo ficticio ["∅"] que representa un estado trampa.
---------------------------------------------------------------------------
findGroup :: String -> [[String]] -> [String]
findGroup q part =
  case [g | g <- part, q `elem` g] of
    (x:_) -> x        -- Se encontró el grupo que contiene al estado q
    []    -> ["∅"]    -- Si no pertenece a ningún grupo, retorna el grupo trampa

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
  [ (gName, a, findGroupName (delta afd (head gStates) a) grupos)
  | (gName, gStates) <- grupos        -- Para cada grupo de estados equivalentes
  , not (null gStates)                -- (evita grupos vacíos)
  , a <- alfabetoD afd                -- Para cada símbolo del alfabeto
  ]
  -- La función toma el primer estado del grupo (todos son equivalentes)
  -- y calcula su transición bajo cada símbolo.
  -- Luego, con 'findGroupName', traduce el destino al nombre del grupo correspondiente.

---------------------------------------------------------------------------
-- Dado un estado original, devuelve el nombre del grupo que lo contiene.
---------------------------------------------------------------------------
findGroupName :: String -> [Grupo] -> String
findGroupName q grupos =
  case [n | (n, qs) <- grupos, q `elem` qs] of
    (x:_) -> x    -- Se encontró el nombre del grupo correspondiente
    []    -> "∅"  -- Si no está en ninguno, se asigna al grupo trampa

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
-- Ejemplo de uso:
-- 
-- Supongamos que se tiene un AFD "m2" definido en el módulo AFD:
-- 
--     > getAFDmin m2
-- 
-- Esto devolverá un AFD equivalente pero con el número mínimo de estados.
---------------------------------------------------------------------------
