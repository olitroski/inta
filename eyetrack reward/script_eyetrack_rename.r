# Script para arreglar los nombres de los archivos EYD de Eyetrack de Reward del
# grupo de anemia. Al hacer el registro pueden quedar malos y se hacen de nuevo y se
# producen problemas con los nombres, entonces se tiene que precisar si el archivo es 
# bloque 1 o 2 u otro 
# Se hace antes de pasar por la maquina virtual se queda ok el file id.
# 
# por ejemplo se pasa de 
# 123reward.eyd    -> 123reward1.eyd
# 123reward_2.eyd  -> 123reward2.eyd
# 
# También para evitar esto
# 1232reward.eyd y 123reward2.eyd
# va a arrojar el mismo resultado porque el programa conserva el número y borra todas
# las letras. Entonces sobre escribe uno de los resultados y se pierde información.
# Va con full de comentarios para que se entienda

# Cargar librerías y borrar espacio de trabajo
library(openxlsx); library(dplyr); library(stringr)
rm(list=ls())

# Indicar la carpeta en dónde están los originales
# setwd("D:/Varios INTA/Bases de datos 21y/NIH21y/Eyetrack reward originales")
setwd("D:/Varios INTA/Bases de datos 21y/NIH21y/Eyetrack reward originales/Eyetrack reward originales")

# revisar repetidos
archivos <- dir()
rep <- data.frame(table(archivos))
rep <- filter(rep, Freq > 1)  # No hay repetidos
rep

# La logica de esto es que como un vector en R no se desordena y se conservan las posiciones 
# se sacan caracteres alfa numericos y así se va capturando la información

# Rutina para capturar solo el id
id <- archivos
id <- sub(".eyd","", id)       
# se saca el numero y letras del final
id <- sub("[a-z]$+", "", id)    
id <- sub("[0-9]$", "", id)   
# Se sacan todas las letra
id <- sub("[a-z]+", "", id)     


# Rutina para caputurar el block
b <- archivos
# se saca el eyd
b <- sub(".eyd","",b)       
# Elimina todos los números del principio
b <- sub("^[0-9]+", "", b)
# Saca todas las letras
b <- sub("[a-z]+", "", b)  


# Junta todo (si no hay block es porque es 1)
idfiles <- data.frame(id = id, block = b, file = archivos, stringsAsFactors = FALSE)
idfiles <- mutate(idfiles, block = ifelse(block == "", 1, block))


# Hacer un nuevo block
idfiles <- mutate(idfiles, order = row.names(idfiles))
idfiles <- arrange(idfiles, id, block)
idfiles <- filter(idfiles, id != "")

     # En mega simple, no brain a esta hora
     idfiles$newblock <- 999
     idfiles$newblock[1] <- 1

     for (i in 2:dim(idfiles)[1]){
          if (idfiles$id[i] == idfiles$id[i-1]){
               idfiles$newblock[i] <- idfiles$newblock[i-1] + 1
          } else {
               idfiles$newblock[i] <- 1
          }
     }
     
     # Esto porque con el arrange inicial se pierde el orden original
     idfiles <- arrange(idfiles, order)
     
     # 2358 por ejemplo.... para eso se hace
     filter(idfiles, id == 2358)


# nombre nuevo
# Se incluye el "new" porque puede tomar un nombre ya cambiado y volver a cambiarlo
idfiles <- mutate(idfiles, newname = paste(id, "rewardnew", newblock, ".eyd", sep=""))
View(idfiles)

head(idfiles)
filter(idfiles, id == 7092)
filter(idfiles, id == 2761)
filter(idfiles, id == 2358)
stop()



## Renombrar los archivos
# file.remove("reward5.eyd") no deja, se borra a mano
for (i in 1:dim(idfiles)[1]){
     old <- idfiles$file[i]
     new <- idfiles$newname[i]
     file.rename(old, new)
}


# Volver a poner el nombre original
archivos <- dir()
for (file in archivos){
     newname <- sub("rewardnew", "reward", file)
     file.rename(file, newname)
}
     

# Revisar repetidos si es que funcionó
newfiles <- dir()
newfiles <- sub(".eyd", "", newfiles)
newfiles <- sub("reward", "", newfiles)

rep <- data.frame(table(newfiles))
rep <- filter(rep, Freq > 1)  # No hay repetidos
rep

# Exito
# Revisión 07.06.2019_15:00 (O.Rojas)
