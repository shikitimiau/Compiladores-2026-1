---------------------------------------------------------------------------
-- | Proyecto 1: Constructor de un analizador lexico
-- | Etapa: AFDmin -> MDD
-- | Equipo: Los Discipulos de Church
-- | Integrantes: 
-- | > Cabrera Sánchez Ana Dhamar
-- | > López Prado Emiliano
-- | > Peña Mata Juan Luis
-- | > Rodríguez Miranda Alexia
-- | > Rosas Franco Diego Angel
--
-- | Descripción:
-- |    Este módulo implementa la transformación de un autómatas finitos deterministas mínimo (AFDmin)
-- |    a una máquina discriminadora determinista.
-- |    La función principal `getMDD` recibe una cadena con una expresión regular,
-- |    se trasforma en un AFDmin y con eso se forma una máquina discriminadora determinista.
-- |    
-- |    También se modela `procesarTokens` que dada una cadena y una MDD, esta función
-- |    emite la lista de tokens que contiene la cadena.
-- |    
---------------------------------------------------------------------------
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

-- ------------------------------------------------------------------------------
-- Definicion del tipo de dato para indicar un error dentro de la mdd
-- ------------------------------------------------------------------------------
type ErrorMDD = String

-- ------------------------------------------------------------------------------
-- Definición de tipo de dato para representar los tokens de una mdd
-- ------------------------------------------------------------------------------
type Tokens = [String]

-- ------------------------------------------------------------------------------
-- Definición del tipo de dato MDD para representar a una
-- Máquina Discriminadora Determinista.
-- MDD = (Q, Σ, δ, q_s, q_f, µ, ┴), donde
-- > Q = estados. Representados en una lista de cadenas
-- > Σ = alfabeto. Representado como una lista de caracteres
-- > δ =  transiciones. Conjunto de transiciones representadas en una lista de Trans_afd
-- > q_s = inicial. Estado inicial del automata representado por un string
-- > q_f = final. Lista de estados finales del automata representados por un string
-- > µ = asignacion. Conjunto de asignaciones representadas en una lista de Trans_mdd
-- > ┴ = error. Error léxico representado como ErrorMDD
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


-- ------------------------------------------------------------------------------
-- Función que dada una cadena y una lista de tokens, obtiene la representación 
-- de su expresión regular y, en caso de que exista, genera la Máquina Discriminadora 
-- Determinista para la regex con los tokens dados.
-- Si no existe, notifica la ausencia de la expresion regular.
-- ------------------------------------------------------------------------------
getMDD :: String -> Tokens -> MDD
getMDD s xs = afd_to_MDD (getAFDmin s) xs

-- ------------------------------------------------------------------------------
-- Función principal para convertir un autómata finito determinista mínimo a una 
-- máquina discriminadora determinista con los tokens dados.
-- ------------------------------------------------------------------------------
afd_to_MDD :: AFD -> Tokens -> MDD
afd_to_MDD afd tokens =MDD
      {
        estadosMDD = estadosMDD
      , alfabetoMDD = alfabetoD afd
      , transicionesMDD = transMDD
      , inicialMDD = inicialD afd
      , finalMDD = finales
      , asignaMDD = funAsig
      , errorMDD = "Error"
      } where
        
        -- Se obtiene la función de asignacion de la mdd
        funAsig = generarAsignaciones afd tokens

        -- Dada la función de asignacion se obtienen los estados finales de la mdd
        finales = obtenerFinales funAsig
        
        -- Obtenemos las tranciociones y estados que hay entre los nuevos estados
        -- finales y los estados finales del afd
        transicionesNoUtiles = obtenerTransicionesInutilesG finales afd
        estadosNoUtiles = estadosResultantes transicionesNoUtiles

        -- Obtenemos las transiciones y los estados de la mdd, esto, quitando las
        -- las transiciones no utiles y los estados no utiles
        estadosMDD =  Set.toList( Set.difference(Set.fromList(estadosD afd)) (estadosNoUtiles) )
        transMDD = Set.toList (Set.difference(Set.fromList(transicionesD afd)) (Set.fromList (transicionesNoUtiles)) )

-- ------------------------------------------------------------------------------
-- Función que dado un autómata finito determinista mínimo y una lista de tokens,
-- asigna uno a uno los tokens de la lista.
-- 
-- afd, el autómata finito determinista mínimo del que saldran las transiciones
-- tokens, lista de tokens a asignar
-- ------------------------------------------------------------------------------
generarAsignaciones :: AFD -> Tokens -> [Trans_mdd]
generarAsignaciones afd [] = []
generarAsignaciones afd (x:tokens) = obtenerAsignacion afd x ++ generarAsignaciones afd tokens

-- ------------------------------------------------------------------------------
-- Función auxiliar que dado autómata finito determinista mínimo y un único token
-- asigna el token a uno o más estados del afd, esto procesando en reversa el token
-- para encontrar los estados a los que se debe asignar.
-- 
-- afd, el autómata finito determinista mínimo del que saldran las transiciones
-- tokens, el token a asignar
-- ------------------------------------------------------------------------------
obtenerAsignacion :: AFD -> String -> [Trans_mdd]
obtenerAsignacion afd token = let lista = (buscarFinales afd (finalD afd) (reverse ("#" ++ token ++ "#"))) in 
                            [(estadoF, token) | estadoF <- lista]

-- ------------------------------------------------------------------------------
-- Función auxiliar que dado un autómata finito determinista mínimo, una lista de
-- estados y una cadena se realiza una busqueda por estado de los estados alcanzables
-- si se procesa una cade en reversa.
--
-- afd, el autómata finito determinista mínimo que se recorrera 
-- estados, los estados donde se realizara la busqueda
-- cadena, la cadena que se desea procesar
-- ------------------------------------------------------------------------------
buscarFinales :: AFD -> [String] -> String -> [String]
buscarFinales afd [] cadena = []
buscarFinales afd (x:finales) cadena = buscaFinal afd x cadena ++ buscarFinales afd finales cadena

-- ------------------------------------------------------------------------------
-- Función auxiliar que busca los estados a los que pueda regresar desde el estado,
-- la cadena y el autómata finito determinista dado.
-- 
-- afd, el autómata finito determinista mínimo donde se buscaran los estados
-- estado, el estado desde donde se hará la busqueda
-- cadena, la cadena que se usara para buscar los estados
-- ------------------------------------------------------------------------------
buscaFinal :: AFD -> String -> String -> [String]
buscaFinal afd estado ('#':[]) = if (hayRegreso afd estado '#') then
                                (regresarG afd estado '#')
                               else
                                []
                               where trancisiones = transicionesD afd 
--                    cadena
buscaFinal afd estado (x:cadena) = if (hayRegreso afd estado x) then
                                buscaFinal afd y cadena ++ buscarFinales afd ys cadena
                               else
                                []
                               where (y:ys) = (regresarG afd estado x)

-- ------------------------------------------------------------------------------
-- Función auxiliar que dado un autómata finito determinista mínimo, un estado y
-- un caracter, regresa verdadero si hay una transicion en el automata que consuma
-- el caracter y lleve al estado dado, devuelve falso en caso contrario
-- 
-- afd, el autómata finito determinista mínimo donde se revisara el regreso
-- estadoF, el estado que verificamos
-- c, el caracter que se uso para llegar al estadoF
-- ------------------------------------------------------------------------------
hayRegreso :: AFD -> String -> Char -> Bool
hayRegreso afd estadoF c = not (null (buscarRegreso trancisiones estadoF c))
                          where trancisiones = transicionesD afd

-- ------------------------------------------------------------------------------
-- Función auxiliar que dado una lista de tranciciones, un estado y un caracter,
-- devuelve todas las transiciones que consuman el caracter y lleve al estado dado 
-- 
-- tranciciones, la lista de trancisiones completa
-- estadoF, el estado resultante de las tansiciones buscadas
-- c, el caracter que se uso para llegar al estadoF
-- ------------------------------------------------------------------------------
buscarRegreso:: [Trans_afd] -> String -> Char -> [Trans_afd]
buscarRegreso tranciciones estadof c = [transicion | transicion@(_, x, q1) <- tranciciones, q1 == estadof, c == x]

-- ------------------------------------------------------------------------------
--  Función auxiliar que dado un automata, un estado 'q' y un caracter 'c' devuelve una 
-- lista con los estados que tengan una regla de produccion que lleve al estado 'q'
-- con el caracter 'c'.
-- 
-- afd, el autómata finito determinista mínimo donde hara el regreso
-- estadoA, el estado resultante de las tansiciones buscadas
-- c, el caracter que se uso para llegar al estadoF
-- ------------------------------------------------------------------------------
regresarG :: AFD -> String -> Char -> [String]
regresarG afd estadoA c = obtenerEstadosAnte afd estadoA c

-- ------------------------------------------------------------------------------
--  Función auxiliar que dada una lista de asignaciones de una máquina discriminadora 
-- determinista devuelve la lista de estados finales, es decir, los estados a los que
-- se les asigna un token.
-- 
-- asignaciones, lista de asignaciones de una máquina discriminadora determinista
-- ------------------------------------------------------------------------------
obtenerFinales :: [Trans_mdd] -> [String]
obtenerFinales [] = []
obtenerFinales ((estado, _):asignaciones) = (estado):(obtenerFinales asignaciones)

-- ------------------------------------------------------------------------------
-- Función auxiliar que dado un autómata finito determinista, un estado 'q' y un 
-- caracter 'c', devuelve los estados 'qs' cuyas transiciones son de la forma ('qs','c','q')
--
-- afd, el autómata finito determinista donde
-- estadoA, el estado al que se llegará consumiendo el caracter 'c'
-- c, el caracter que se uso para llegar al estadoF
-- ------------------------------------------------------------------------------
obtenerEstadosAnte :: AFD -> String -> Char -> [String]
obtenerEstadosAnte afd estadoA c = [x | (x, y, estadoF) <- transicionesD afd, estadoF == estadoA, c == y]

-- ------------------------------------------------------------------------------
-- Función auxiliar que obetendra las transiciones que se consideran inutiles, es
-- decir, todas las trancisiones de los estados dados con el simbolo '#'
-- 
-- estados, los estados desde donde se buscaran las transiciones inutiles
-- afd, el autómata finito determinista mínimo donde
-- ------------------------------------------------------------------------------
--                             estados 
obtenerTransicionesInutilesG :: [String] -> AFD -> [Trans_afd]
obtenerTransicionesInutilesG [] afd = []
obtenerTransicionesInutilesG (x:estados) afd = encontrarDesde aux afd  ++ obtenerTransicionesInutilesG estados afd
                                        where 
                                          aux = buscarEstados '#' x trancisiones
                                          trancisiones = transicionesD afd

-- ------------------------------------------------------------------------------
-- Función auxiliar que devuelve todas las transiciones posibles desde una lista
-- de transiciones dadas y un autómata finito determinista.
--
-- transiciones, las transiciones desde donde se buscaran las posibles transiciones.
-- afd, el autómata finito determinista mínimo donde
-- ------------------------------------------------------------------------------
encontrarDesde :: [Trans_afd] -> AFD -> [Trans_afd]
encontrarDesde [] afd = []
encontrarDesde ((qa,c,qf):transicionesA) afd = encontrarDesde ([trancision | trancision@(estado,_,_) <- trancisiones, estado == qf]) afd ++ encontrarDesde transicionesA afd ++[(qa,c,qf)] 
                                    where 
                                      trancisiones = transicionesD afd

-- ------------------------------------------------------------------------------
-- Función que dada una lista de transiciones recopila los estados a los que se 
-- llega con esas transiciones 
--
-- transiciones, las transiciones de las que se sacaran los estados resultantes
-- ------------------------------------------------------------------------------
estadosResultantes :: [Trans_afd] -> Set.Set String
estadosResultantes estados = (Set.fromList([estadoI | (_, _, estadoI) <- estados]))

-- ------------------------------------------------------------------------------
-- Función que dada una cadena, emite una lista de tokens, esta funcion utiliza el
-- estado inicial de la mdd para empezar el procesamiento de la cadena, requiere 
-- los siguientes elementos:
--
-- cadena, la cadena que se procesará,
-- mdd, la Máquina Discriminadora Determinista con la que se procesará la cadena
-- ------------------------------------------------------------------------------
procesarTokens :: String -> MDD -> [(String, String)]
procesarTokens cadena mdd = let inicial = inicialMDD mdd in 
  procesarAux cadena inicial [] mdd

-- ------------------------------------------------------------------------------
-- Función auxiliar para obtener la lista de tokens de una cadena,requiere los 
--siguientes elementos:
-- 
-- x:xs, la cadena que será procesada
-- estadoA, el estado en el que se enceuntra el procesamiento
-- visitados, lista de par de caracter y estado que representan la trancision que se 
--            realizo para obtener el estadoA
-- mdd, la Máquina Discriminadora Determinista con la que se procesa la cadena
-- ------------------------------------------------------------------------------
procesarAux :: String -> String -> [(Char, String)] -> MDD -> [(String, String)]
procesarAux [] estadoA visitados mdd =
            -- Si la cadena ya se consumio por completo y el estadoA es inicial, entonces la cadena
            -- se termino de procesar por lo que no se devuelve ningún token.
            if estadoA == inicial && (length visitados == 0) then []
            else
              -- Si la cadena ya se consumio por completo y el estaoA es final, se devuelve
              -- la lista con el token asociado a ese estado y la cadena que se guarda en visitados
              if estadoA `elem` finales then
                [(obtenerToken estadoA mdd, obtenerContenido visitados) ]
              -- Si el estadoA no es final se devuelve un error ya que la cadena que se proceso
              -- no se asocia a nungún token.
              else
               [(errorMDD mdd,"")]
            where
              inicial = inicialMDD mdd
              finales = finalMDD mdd

procesarAux (x:xs) estadoA visitados mdd =
            -- Si hay una trancision con el primer caracter de la cadena y el estadoA se procesa el
            -- resto dde la cadena con el estado resultante de la trancision y se agrega la tupla del
            -- caracter y el estadoA a los visitados.
            if (hayTrancision x estadoA transiciones)then
              
              procesarAux xs (funcionTransicionCaracter x estadoA transiciones) ((x,estadoA):visitados) mdd

            else 
              -- En caso contrario se inicia el proceso de regreso con los mismos parametros.
              regresar (x:xs) estadoA visitados mdd
            where
              transiciones = transicionesMDD mdd

-- ------------------------------------------------------------------------------
-- Función auxiliar para  
--
-- cadena, la cadena que esta procesando la mdd.
-- estadoA, el estado en el que se encuentra el procesamiento,
-- ((c,estadoR):xs), lista de par de caracter y estado que representan la trancision que se 
--                   realizo para obtener el estadoA
-- mdd, la Máquina Discriminadora Determinista con la que se procesa la cadena
-- ------------------------------------------------------------------------------
regresar :: String -> String -> [(Char, String)] -> MDD -> [(String, String)]

-- Se manda un error ya que al regresar no se encontro un estado final, haciendo que a
-- la cadena no se le pueda asignar un token existente.
regresar cadena estadoA [] mdd = [(errorMDD mdd,"")]


regresar cadena estadoA ((c, estadoR):xs) mdd = 
                                              if estadoA `elem` finales then
                                                -- Si el estadoA es un estado final, se obtiene el token asignado a ese estado final y
                                                -- se vacia el conjunto de visitados, y se procesa el resto de la cadena desde el estado
                                                -- inicial.
                                                [(obtenerToken estadoA mdd, obtenerContenido ((c, estadoR):xs))] ++ procesarAux cadena inicial [] mdd
                                              else
                                                -- Si estadoA no es un estado final, se restaura el estado anterior de la mdd, y se
                                                -- continua el regreso desde ese estado.
                                                regresar ([c] ++ cadena) estadoR xs mdd
                                              where 
                                                finales = finalMDD mdd
                                                inicial = inicialMDD mdd

-- ------------------------------------------------------------------------------
-- Función auxiliar que busca que token se le asignará al estado dado.
--
-- estadoF, el estado al que se le asignara un token
-- mdd, la Máquina Discriminadora Determinista en la que se buscara el token.
-- ------------------------------------------------------------------------------
obtenerToken :: String -> MDD -> String
obtenerToken estadoF mdd = do
              let finales = finalMDD mdd
                  funcionToken = asignaMDD mdd  
              case buscarToken estadoF funcionToken of
                [] -> error "No hay un token asociado al estado"
                (token):[] -> token
                _ -> error "El automata es no determinista"

-- ------------------------------------------------------------------------------
-- Función auxiliar que busca que tokens se le podria asignar al estado dado.
--
-- estado, el estado al que se le asignara un token
-- tokens, lista que representa la función de asignacion de una mdd
-- ------------------------------------------------------------------------------
buscarToken :: String -> [(String, String)] -> [String]
buscarToken estado tokens = [token | (estadof, token) <- tokens, estadof == estado]

-- ------------------------------------------------------------------------------
-- Funcion auxiliar que reconstrulle una cadena apartir de un lista de tuplas con
-- caracteres y cadenas que representan estados.
--
-- ((c,estado):xs), lista de par de caracter y estado a recuperar
-- ------------------------------------------------------------------------------
obtenerContenido :: [(Char, String)] -> String
obtenerContenido [] = ""
obtenerContenido ((c,estado):xs) = obtenerContenido xs ++ [c]



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
      , errorMDD = "Error"
      }

afdPruebas = AFD {
  estadosD = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14"]
  , alfabetoD = "#abcd"
  , transicionesD = 
    [("q1",'a',"q2"),("q1",'c',"q8")
    ,("q10",'#',"q7"),("q11",'#',"q14")
    ,("q12",'#',"q13"),("q12",'c',"q11")
    ,("q13",'a',"q3"),("q14",'a',"q4")
    ,("q2",'b',"q12"),("q3",'b',"q5")
    ,("q4",'b',"q6"),("q5",'#',"q0")
    ,("q6",'c',"q5"),("q7",'c',"q9")
    ,("q8",'d',"q10"),("q9",'d',"q5")]
    , inicialD = "q1", finalD = ["q0"]}

mddEjemplo2 = MDD {estadosMDD = ["q1","q10","q11","q12","q2","q8"]
, alfabetoMDD = "#abcd"
, transicionesMDD = 
  [("q1",'a',"q2"),("q1",'c',"q8")
  ,("q12",'c',"q11"),("q2",'b',"q12")
  ,("q8",'d',"q10")]
  , inicialMDD = "q1", finalMDD = ["q12","q11","q10"]
  , asignaMDD = [("q12","ab"),("q11","abc"),("q10","cd")
  ], errorMDD = "Error"}


