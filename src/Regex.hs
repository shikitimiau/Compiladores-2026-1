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
reserved = ['+', '*', '(', ')']


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
 -- Caso base: cadena de un solo caracter
getRegexAux acc [x]
    | x == ')' = error "Parentesis de cierre no esperado" -- Si sobró un parentesis, la regex no tiene parentesis balanceados
    | x `elem` reserved = error $ "Símbolo reservado '" ++ [x] ++ "' necesita de una regex valida para utilizarse"
    | otherwise = case acc of
                  Nothing -> Just (Symbol x) -- Solo un símbolo
                  Just a -> Just (Concat a (Symbol x)) -- Concatenamos con lo que ya teníamos
getRegexAux acc (x:xs)
    | x == ')' = error "Parentesis de cierre no esperado"
    | x == '(' = -- Si encontramos un '(', buscamos el contenido hasta el paréntesis de cierre correspondiente
        let (insideStr, restAfterParen) = extractParenthesized xs -- función auxiliar para manejar paréntesis balanceados
        in if null insideStr
           then getRegexAux acc restAfterParen
           else 
             -- Parseamos el interior como una expresión completa
             case parseCompleteExpression insideStr of
               Just inside -> 
                 -- Aplicamos operadores postfijos al contenido entre paréntesis
                 let (exprWithPostOps, remainingAfterPostOps) = 
                       applyPostOperators inside restAfterParen
                 in 
                   case acc of
                     Nothing -> getRegexAux (Just exprWithPostOps) remainingAfterPostOps
                     Just a -> 
                       -- En lugar de concatenar automáticamente, verificamos si hay operador infijo
                       case remainingAfterPostOps of
                         -- Si hay un '+' después, es una unión
                         ('+':restAfterPlus) ->
                           case getRegexAux Nothing restAfterPlus of
                             Nothing -> error "Expresión esperada después de '+'"
                             Just right -> Just (Union (Concat a exprWithPostOps) right)
                         -- Si no hay operador, entonces concatenamos
                         _ -> getRegexAux (Just (Concat a exprWithPostOps)) remainingAfterPostOps
               Nothing -> getRegexAux acc restAfterParen

    -- Operador '+' como infijo
    | x == '+' =
        case acc of
          Nothing -> error "Expresión esperada antes de '+'"
          Just left -> 
            case getRegexAux Nothing xs of
              Nothing -> error "Expresión esperada después de '+'"
              Just right -> Just (Union left right)

    | not (null xs) && head xs == '*' =
        let remaining = tail xs
            starred = Star (Symbol x)
        in case getRegexAux (Just starred) remaining of
            Nothing -> case acc of
                       Nothing -> Just starred
                       Just a -> Just (Concat a starred)
            Just next -> case acc of
                         Nothing -> Just next
                         Just a -> Just (Concat a next)
    -- Si las operaciones no se han usado correctamente, notificamos el error
    | x `elem` reserved =
      error $ "Símbolo reservado '" ++ [x] ++ "' en posición inválida"

    | otherwise =
        let current = Symbol x
        in case acc of
             Nothing -> getRegexAux (Just current) xs
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
applyPostOperators expr ('+':rest) = -- Operador de más alta precedencia
    case parseCompleteExpression rest of
      Nothing -> (expr, rest) --
      Just right -> (Union expr right, "")
applyPostOperators expr ('*':rest) = --Si encontramos un '*', aplicamos el operador estrella y seguimos procesando
    applyPostOperators (Star expr) rest
applyPostOperators expr rest = (expr, rest)