# Script para emparejar los ids del eyetrack, porque el programa toma el nombre
# del archivo y al parecer deja solo lo numeros presentes en el namefile y eso lo tira al id
# tonces acá se hace un archivo de emparejamiento

# Cargar antecedentes
rm(list=ls())
setwd("D:/Varios INTA/Bases de datos 21y/CV21y/eyetrack reward/Folder 1")
library(dplyr); library(stringr); library(openxlsx)


# Cargar nombres de archivos 
archivos <- dir()
head(archivos)
archivos <- archivos[grep(".eyd", archivos)]
archivos2 <- sub(" ", "", archivos)


# Rutina para capturar el id
id <- archivos2
# se saca el eyd
id <- sub(".eyd","", id)       
# se saca el numero del final
id <- sub("[0-9]$", "", id)   
# Se sacan todas las letra
id <- sub("[a-z]+", "", id)  
id <- sub("[A-Z]+", "", id)  


# Rutina para el block o si hay varios archivos por persona
b <- archivos2
# se saca el eyd
b <- sub(".eyd","",b)       
# Elimina todos los números del principio
b <- sub("^[0-9]+", "", b)
# Saca todas las letras
b <- sub("[a-z]+", "", b)  
b <- sub("[A-Z]+", "", b)  


# Junta todo (si no en b es porque sería versión 1)
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

# Revisar si hay conflictos
# 1. que empiece el nombre con un cero
idfiles <- mutate(idfiles, startzero = grepl("^['0']", idfiles$file))

# 2. Espacio en el nombre
idfiles <- mutate(idfiles, espacio = grepl(" ", idfiles$file))

# 3. tiene más de 1 file  --hay que traer el otable-- OJO si se cambia de lugar el repositorio
source('https://raw.githubusercontent.com/olitroski/sources/master/exploratory/otable.r')
repetido <- otable("id", data = idfiles)
repetido <- filter(repetido, freq >= 2)
repetido <- filter(repetido, id != 'total')
repetido <- select(repetido, -pct)
repetido <- rename(repetido, repetido = freq)
repetido <- mutate(repetido, repetido = TRUE)
idfiles <- merge(x = idfiles, y = repetido, by = "id", all = TRUE)
idfiles <- mutate(idfiles, repetido = ifelse(is.na(repetido) == TRUE, FALSE, repetido))


# 4. ahora si sacamos las letras del filename se repite???
archivos2 <- data.frame(file = idfiles$file, test = archivos2)
archivos2 <- mutate(archivos2, test = sub(".eyd","", test))      
archivos2 <- mutate(archivos2, test = sub("[a-z]+", "", test))
archivos2 <- mutate(archivos2, test = sub("[A-Z]+", "", test))  

repetido <- otable("test", data = archivos2)
repetido <- filter(repetido, freq >= 2)

if (dim(repetido)[1] == 1){
    idfiles <- mutate(idfiles, nameproblem = FALSE)
} else {
    repetido <- filter(repetido, test != 'total')
    repetido <- select(repetido, -pct)
    repetido <- rename(repetido, nameproblem = freq)
    repetido <- mutate(repetido, nameproblem = TRUE)
    
    archivos2 <- merge(x = archivos2, y = repetido, by = "test", all = TRUE)
    archivos2 <- mutate(archivos2, nameproblem = ifelse(is.na(nameproblem) == TRUE, FALSE, nameproblem))
    archivos2 <- select(archivos2, -test)
    
    idfiles <- merge(x = idfiles, y = archivos2, by = "file", all = TRUE)
}


# Ahora si checamos cuales ids tienen problema
idfiles <- mutate(idfiles, problem = startzero + espacio + repetido + nameproblem)
write.xlsx(filter(idfiles, problem >= 1), "archivos con problemas de nombre.xlsx")
View(filter(idfiles, problem >= 1))
