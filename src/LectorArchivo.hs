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
-- |    Este módulo implementa la lectura de un archivo .md y su procesamiento
-- |    para poder generar una máquina discriminadora determinista.
---------------------------------------------------------------------------
module LectorArchivo where

import MDD
import Regex
import System.IO()

-- ------------------------------------------------------------------------------
-- Definicion del tipo de dato para indicar el nombre de un archivo
-- ------------------------------------------------------------------------------
type Archivo = String

-- ------------------------------------------------------------------------------
-- Función que dado el nombre de un archivo, carga su contenido en un arreglo de 
-- cadenas, cada elemento del arreglo es una linea del archivo.
-- 
-- archivo, el nombre del archivo
-- ------------------------------------------------------------------------------
cargarArchivo :: FilePath -> IO [String]
cargarArchivo archivo = do 
    contenido <- readFile archivo
    return $ filter (not . null) $ lines contenido

-- ------------------------------------------------------------------------------
-- Función que carga el archivo y procesa su contenido, ademas imprime en terminal
-- la expresión regular y los tokens leidos.
--
--
-- archivo, el nombre del archivo
-- ------------------------------------------------------------------------------
procesarArchivo :: FilePath -> IO (String, [String])
procesarArchivo archivo = do
    lista <- cargarArchivo archivo

    let (cadena, tokens) = procesarComponente lista

    putStr "\nLa expresión regular leida es:\n"
    putStrLn cadena
    putStr "\nLos tokens son:\n"
    putStrLn (show tokens) 

    return (cadena, tokens)

-- ------------------------------------------------------------------------------
-- Función auxiliar que toma dos elementos del arreglo y crea un par de expresion
-- regular y token, además, agrega "#token#" a la exprecion regular para que esta
-- pueda ser recuperada después.
--
-- lineas, el arreglo con las lineas necesarias para formar cada par.
-- ------------------------------------------------------------------------------
procesarComponente :: [String] -> (String, [String])
procesarComponente [] = ("", [])
procesarComponente [_] = error "Error: El archivo contiene un número impar de líneas. Se esperan pares [token, regex]."
procesarComponente [x,y] =
    let expresionR = "(" ++ y ++ ")" ++ "#" ++ x ++ "#"
    in (expresionR, [x])

procesarComponente (x:y:lineas) =
    let
        (expresionRA, tokens) = procesarComponente lineas
        expresionR = "(" ++ y ++ ")" ++ "#" ++ x ++ "#"
        combinada = expresionRA ++ "+" ++ expresionR
    in
        (combinada, x : tokens)
