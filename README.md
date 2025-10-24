# Compiladores-2026-1

# Compilado del proyecto

Para este proyecto es estrictamente necesario el uso de **Cabal**, pues esta es la herramienta principal que utilizamos para poder organizar el proyecto y que este se ejecute como debe sin preocuparnos de que todo se encuentre dentro de la misma carpeta y resolución de dependencias.

## instalación de Cabal
En el caso de que no se tenga instalado Cabal en el sistema, se puede descargar fácilmente con alguno de los siguentes comandos:

    sudo apt-get install cabal-install // En distribuciones Debian

o bien

    sudo pacman -S cabal-install // En distribuciones Arch

## Actualización de Cabal
Para que cabal funcione correctamente es necesario que este se actualice, por lo que simplemente podemos ejecutar el siguiente comando

    cabal update

## Construcción del proyecto
Ahora bien, es necesario construir el proyecto usando cabal, que utilizará el archivo **.cabal** que se encuentra en la raiz del proyecto para construir los modulos necesarios asociados al proyecto y se encargará también de descargar las respectivas dependencias que pueda tener el proyecto

Para esto ejecutamos el siguiente comando dentro de la carpeta raiz del proyecto

    cabal build

## Ejecución del proyecto
El proyecto cuenta con archivos de ejecución especificos, que se ejecutarán al escribir instrucciones especificas con cabal

aquí se enlistan las operaciones posibles

    cabal run Test
> Para ejecutar las diferentes pruebas del proyecto
