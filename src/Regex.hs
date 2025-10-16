-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa: Reconocimiento de expresiones regulares (regex)
-- | Equipo: Discípulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
module Regex where

-- ------------------------------------------------------------------------------
-- Definicion del tipo de dato Regex para representar una expresión regular
-- Constructores: 
-- > Symbol. Regex de un solo caracter: a
-- > Concat. Concatenacion de dos regex: EE
-- > Union. Union de dos regex: E + E
-- > Star. Teorema de Kleene: E*
-- ------------------------------------------------------------------------------
data Regex = Symbol Char 
           | Concat Regex Regex 
           | Union Regex Regex 
           | Star Regex
           deriving (Show, Eq)


-- ------------------------------------------------------------------------------
-- Definicion de lista con caracteres reservados para determinar las operaciones
-- en las regex
-- ------------------------------------------------------------------------------
reserved :: [Char]
reserved = ['+', '*', '(', ')','[', ']']


-- ------------------------------------------------------------------------------
-- Función para obtener la representación de la expresión regular
-- Usamos Nothing para representar la ausencia de expresión (el inicio), y
-- Just para representar una expresión válida
-- ------------------------------------------------------------------------------
getRegex :: String -> Regex
getRegex s = case getRegexAux Nothing s of
              Just expr -> expr
              Nothing -> error "Expresión vacía no permitida"

-- ------------------------------------------------------------------------------
-- Función auxiliar que procesa un string y determina si es posible crear una
-- Regex valida segun el constructor o no lo es.
-- Si es posible generar la Regex, devuelve el acumulador con su representación.
-- Si no es posible generar una Regex valida, devuelve Nothing.
-- ------------------------------------------------------------------------------
getRegexAux :: Maybe Regex -> String -> Maybe Regex
-- Caso base: cadena vacía
getRegexAux acc [] = acc
 -- Caso base: Cadena de un solo caracter
getRegexAux acc [x]
    | x == ')' = error "Parentesis de cierre no esperado" -- Si sobró un parentesis, la regex no tiene parentesis balanceados
    | x `elem` reserved = error $ "Símbolo reservado '" ++ [x] ++ "' necesita de una regex valida para utilizarse"
    | otherwise = case acc of
                  Nothing -> Just (Symbol x) -- Solo un símbolo
                  Just a -> Just (Concat a (Symbol x)) -- Concatenamos con lo que ya teníamos
-- Caso especial: Si se desea utilizar un símbolo reservado como caracter dentro de la
-- regex, deberá escribirse en la cadena de entrada: '\\+', '\\*', '\\(' '\\)' según sea el caso.
-- Nota: En haskell se utiliza '\\'  porque '\' está reservada para saltos de linea, tabulacion, etc.
getRegexAux acc ('\\':x:xs)
    | x `elem` reserved = 
        let current = Symbol x
        in case acc of
             Nothing -> getRegexAux (Just current) xs
             Just a  -> getRegexAux (Just (Concat a current)) xs
-- Caso recursivo: Resuelve las llamadas para union, concatenacion y estrella de kleene
getRegexAux acc (x:xs)
    | x == ')' = error "Parentesis de cierre no esperado" -- Encontrar un parentesis de cierre indica que no estan balanceados
    | x == '(' = -- Si encontramos un '(', buscamos el contenido hasta el paréntesis de cierre correspondiente
        let (insideStr, restAfterParen) = extractParenthesized xs -- función auxiliar para encontrar el contenido entre paréntesis balanceados
        in if null insideStr -- No hay nada dentro de los parentesis
           then error "Expresión vacía no permitida"
           else 
             -- Parseamos el interior como una expresión completa
             case parseCompleteExpression insideStr of
               Just inside -> 
                 -- Aplicamos operadores postfijos al contenido entre paréntesis
                 let (exprWithPostOps, remainingAfterPostOps) = 
                       applyPostOperators inside restAfterParen
                 in 
                   case acc of
                    -- Si no hay expresión acumulada aún, hacemos la llamada recursiva
                    -- con el contenido dentro del paréntesis junto con sus operadores postfijos.
                     Nothing -> getRegexAux (Just exprWithPostOps) remainingAfterPostOps
                     Just a -> 
                       -- Verificamos si hay operador infijo
                       case remainingAfterPostOps of
                         -- Si hay un '+' después, es una unión donde el lado derecho es una
                         -- concatenacion del simbolo encontrado y el contenido entre parentesis
                         ('+':restAfterPlus) ->
                           case getRegexAux Nothing restAfterPlus of
                            -- Si no hay nada en el lado izquierdo, marcamos el error
                             Nothing -> error "Expresión esperada después de '+'"
                             -- En otro caso, construimos la union contemplando la concatenación 
                             Just right -> Just (Union (Concat a exprWithPostOps) right)
                         -- Si no hay operador infijo, entonces concatenamos directamente el síbolo
                         -- con el contenido de los parentesis y seguimos la recursion con la parte
                         -- de la cadena que no se ha procesado
                         _ -> getRegexAux (Just (Concat a exprWithPostOps)) remainingAfterPostOps

    -- Operador '+' como infijo
    | x == '+' =
        case acc of
          -- Si el acumulador es vacío, la regex está mal construida, pues 
          -- la union necesita lado derecho e izquierdo y no estamos encontrando
          -- el lado derecho de esta unión.
          Nothing -> error "Expresión esperada antes de '+'"
          -- Si encontramos el lado derecho, verificamos que haya lado izquierdo en la cadena.
          Just left -> 
            case getRegexAux Nothing xs of
              -- Si no hay lado izquierdo notificamos el error
              Nothing -> error "Expresión esperada después de '+'"
              -- Si todo salió bien, devolvemos la union
              Just right -> Just (Union left right)

    -- Si las operaciones no se han usado correctamente, notificamos el error
    | x `elem` reserved =
      error $ "Símbolo reservado '" ++ [x] ++ "' en posición inválida"

    -- Si hasta ahora las operaciones se han aplicado correctamente, entonces
    -- podemos asumir que la regex que antecede el operador '*' es válida 
    -- y se encuentra en el acumulador 
    | not (null xs) && head xs == '*' =
        -- Ignoramos el simbolo * y construimos expresion con estrella de kleene 
        let remaining = tail xs 
            starred = Star (Symbol x)
          -- hacemos la llamada recursiva con la regex construida contemplando la estrella
          -- de kleene que fue aplicada y el resto de la cadena sin tomar en cuenta *
        in case getRegexAux (Just starred) remaining of
          -- No encontramos una expresión después de E*
          Nothing -> case acc of
            -- No hay nada antes de E*
            -- devolvemos la expresion recien construida
            Nothing -> Just starred
            -- Hay algo antes de E*
            -- Concatenamos el resultado del acumulador previo  (que ya verificamos
            -- que es una regex valida) con la expresion recién construida
            Just a -> Just (Concat a starred)
          -- Encontramos una expresión después de E*
          Just next -> case acc of
            -- No hay nada antes de E*
            -- devolvemos la expresion E*E que se encuentra en el acumulador actualmente
            Nothing -> Just next
            -- Hay algo antes de E*
            -- Concatenamos el resultado del acumulador previo  (que ya verificamos
            -- que es una regex valida) con la expresion encontrada después de E*
            Just a -> Just (Concat a next)

    -- Si el caso no cae en los anteriores, entonces es un símbolo del alfabeto
    | otherwise =
        let current = Symbol x
        in case acc of
            -- Si no hay nada acumulado, es el primer simbolo que encontramos
             Nothing -> getRegexAux (Just current) xs
             -- Si hay algo acumulado, entonces este simbolo esta concatenado a 
             -- la expresion que se ha procesado hasta el momento
             Just a -> getRegexAux (Just (Concat a current)) xs


-- ------------------------------------------------------------------------------
-- Función para extraer el contenido entre paréntesis balanceados
-- Asumimos que si la funcion se mando a llamar es porque encontramos un paréntesis
--  que abre,  asi que iniciamos el contador de parentesis abiertos en 1
-- ------------------------------------------------------------------------------
extractParenthesized :: String -> (String, String)
extractParenthesized s = extract 1 [] s 
  where
    -- Función auxiliar donde verificamos el balance de paréntesis
    extract :: Int -> String -> String -> (String, String)
    -- parentesis que abre -> buscamos un paréntesis más.
    extract n acc ('(':rest) = extract (n+1) ('(':acc) rest
    extract n acc (')':rest)
      -- Si el contador está en 1, los parentesis estan balanceados
      -- el parentesis encontrado cierra el inicial.
      -- hacemos reverse, porque acumulamos insertando al inicio.
      | n == 1 = (reverse acc, rest)
      -- En otro caso, es el cierre de otro parentesis que fue abierto
      -- identificamos un cierre -> buscamos un paréntesis menos.
      | otherwise = extract (n-1) (')':acc) rest
    -- cualquier otro caracter, lo acumulamos.
    extract n acc (c:rest) = extract n (c:acc) rest
    -- Si termina de revisar la cadena y aún hay paréntesis sin cerrar,
    -- significa que faltan paréntesis de cierre.
    extract _ acc [] = error "Paréntesis de cierre incompletos"


-- ------------------------------------------------------------------------------
-- Parse para una expresión completa (puede contener operadores)
-- ------------------------------------------------------------------------------
parseCompleteExpression :: String -> Maybe Regex
parseCompleteExpression s = getRegexAux Nothing s --Nothing indica que no hay expresión acumulada al inicio


-- ------------------------------------------------------------------------------
-- Aplica operadores que se encuentran después de una expresión
-- ------------------------------------------------------------------------------
applyPostOperators :: Regex -> String -> (Regex, String)
applyPostOperators expr [] = (expr, []) -- Ya no queda cadena por procesar
--Si encontramos un '*', aplicamos el operador estrella y seguimos procesando
applyPostOperators expr ('*':rest) = applyPostOperators (Star expr) rest
-- Para el caso de la union:
-- Cuando llegamos a este punto, ya se ha generado la regex del lado izquierdo
-- Por lo cual, sólo generamos la regex del lado derecho.
applyPostOperators expr ('+':rest) =
    case parseCompleteExpression rest of
      Just right ->
         -- hacemos la llamada a parse del lado derecho con la lista vacía
         -- para no procesar dos veces ese lado de la expresión
        let (exprRight, restAfter) = applyPostOperators right []
        in (Union expr exprRight, restAfter)
       -- Si resulta Nothing no es una regex valida porque no tiene lado derecho.
      Nothing -> error "Expresión esperada después de '+'"
-- Si en la cabeza de la lista no hay un operador devolvemos la misma expresion
applyPostOperators expr rest = (expr, rest)