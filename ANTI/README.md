﻿# Para procesar ANTI
Falta poner acá los procedimientos que se hacen en la maquina virtual

## Script_anti.R
Este script junta todos los resultados que de la máquina virtual, se puede ejecutar ahi o en otro lado.

Instrucciones:

1.	Poner todos los archivos de resultados en una carpeta, da igual si hay otros archivos en la carpeta porque el script solo tomará aquellos que contengan en el nombre `NA.trial.txt`

2.	Abrir un R o un RStudio, se puede dentro de la máquina virtual, pero mejor hacerlo fuera.

3.	Asegurarse de instalar las librerías necesarias: <br> 
`install.packages("dplyr")` <br> `install.packages("stringr")`

4.	En el Script se indica donde poner la ruta a la carpeta donde estarán los archivos de resultados. Está súper señalizado, de todas formas es la **linea 15**.

5.	Hacer source, es decir ejecutar todo el Script **RStudio** tiene un botón para eso (en la esquina superior derecha del script), sino en el menú `Code >> Source`.

El script se va a detener si es que hay algún error, utilizar el ultimo file que muestra la consola para mirar a mano el archivo con problemas. Habían unos con 99 trials que arrojaban error por ejemplo.

Ya no hace un Excel por lo que no es necesario tener Rtools y esas cosas, en su lugar crea 2 CSV, uno con las stats y otro con los datos originales.

> Van archivos de ejemplo para correr el programa

## Para descarar el script usar este enlace
Hacer clic con el botón derecho y seleccionar **descargar enlace como...** o algo así.

> [**script_anti.R**](https://raw.githubusercontent.com/olitroski/inta/master/ANTI/script_anti.R)
