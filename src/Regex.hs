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
    | x == ')' = acc -- caso donde termina una subexpresión
    | otherwise = case acc of
                  Nothing -> Just (Symbol x) -- Solo un símbolo
                  Just a -> Just (Concat a (Symbol x)) -- Concatenamos con lo que ya teníamos
getRegexAux acc (x:xs)
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

    | otherwise =
        let current = Symbol x
        in case acc of
             Nothing -> getRegexAux (Just current) xs
             Just a -> getRegexAux (Just (Concat a current)) xs


-- ------------------------------------------------------------------------------
-- Función para extraer el contenido entre paréntesis balanceados
-- ------------------------------------------------------------------------------
extractParenthesized :: String -> (String, String)
extractParenthesized s = extract 0 [] s
  where
    -- Función auxiliar donde verificamos el balance de paréntesis
    extract :: Int -> String -> String -> (String, String)
    extract 0 acc (')':rest) = (reverse acc, rest) --solo se ha cerrado el paréntesis inicial, hacemos reverse, pues acumulamos insertando al inicio.
    extract n acc (')':rest) = extract (n-1) (')':acc) rest -- identificamos un cierre -> buscamos un paréntesis menos.
    extract n acc ('(':rest) = extract (n+1) ('(':acc) rest -- parentesis que abre -> buscamos un paréntesis más.
    extract n acc (c:rest)   = extract n (c:acc) rest -- cualquier otro caracter, lo acumulamos
    extract _ acc []         = (reverse acc, "") -- si se acaba la cadena, devolvemos lo acumulado y cadena vacía


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