module Regex where
    data Regex = Empty
               | Epsilon
               | Symbol Char
               | Concat Regex Regex
               | Union Regex Regex
               | Star Regex
               deriving (Show, Eq)

    getRegex :: String -> Regex -- String (lista de caracteres) -> Regex
    getRegex [] = Epsilon -- Caso base: cadena vacía
    getRegex [c] = Symbol c -- Caso base: cadena de un solo caracter
    getRegex ('(':cs) = let (inside, rest) = span (/= ')') cs in --Span devuelve el prefijo de la lista que cumple la condición y el resto
                        if head(tail inside) == '(' then getRegex ('(':inside++")"++rest) -- Manejo de paréntesis anidados  
                        else if null rest then error "Mismatched parentheses"
                        else if length rest > 1 && head (tail rest) == '*' then
                            Star (getRegex inside) -- Caso estrella con paréntesis
                        else if length rest > 1 && head (tail rest) == '+' then
                            Union (getRegex inside) (getRegex (tail (tail rest))) -- Caso unión con paréntesis
                        else
                            Concat (getRegex inside) (getRegex (tail rest)) -- Caso concatenación con paréntesis
    getRegex (c:cs) = if head cs == '*' then
                        Star (getRegex [c]) -- Caso estrella
                     else if head cs == '+' then
                        Union (getRegex [c]) (getRegex (tail cs)) -- Caso unión
                     else
                        Concat (getRegex [c]) (getRegex cs) -- Caso concatenación