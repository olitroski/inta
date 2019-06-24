# --------------------------------------------------------------------------------------- #
# ---- Script para abrir y compilar archivos epi 13.05.2019 ----------------------------- #
# ---- Agrego separacion entre food y netural 20.05.2019 -------------------------------- #
# --------------------------------------------------------------------------------------- #
# Antes que todo hay que instalar en este orden <R -> RStudio -> Rtools> todo lo m�s 
# actualizado posible. Lo siguiente es instalar algunas librer�as.
# install.packages("dplyr")
# install.packages("xlsx")
# install.packages("stringr")
# Borrar todo para testear rm(list=ls())
# Rtools no es necesario porque ya nose usa el xlsx

# ##############################################
# ##### Seleccionar la carpeta de trabajo. #####
setwd("D:/OneDrive/INTA/Sources/ANTI")
# ##############################################

# ---- Lo primero es cargar el espacio de trabajo y cargar las librer�as ---------------- #
# Cargar librer�as
library(dplyr)
library(stringr)

# Vector de estimulos 
estimulo <- c(1, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2, 1, 1, 
              1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 
              1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 
              1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1, 1)


# ---- Luego hay que crear un vector que tenga todos los archivos para leer ------------- #
# Guardar lo que hay en el directorio y mirar
archivos <- dir()

# Seleccionar solo los que necesitamos, en este caso terminan con "NA.trial.txt" entonces 
# usamos esto para seleccionarlos
match.file <- grep("NA.trial.txt", archivos)    # Para sacar la posici�n del texto dentro del vector

# Ahora selccionamos del vector archivos s�lo los que est�nen match.file
archivos <- archivos[match.file]


# ---- Ahora vamos a crear una funci�n para que le haga todos los calculos a un file ----- #
# file <- archivos[1]

procesar.anti <- function(file = NULL){
    # ---- 1. Lo primero es capturar el id que est� en el nombre del archivos
    # vemos que est� al principio y separado por puntos
    id <- str_split_fixed(file, "\\.", 10)
    id <- id[1,1]
    id <- data.frame(id = as.numeric(id))
    
    # ---- 2. Lo segundo es abrir el archivo, es un txt separado por tabulaciones 
    anti.data <- read.table(file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    
    # Agregar el id a la data
    anti.data <- mutate(anti.data, id = id[1,1])
    
    # mirar como qued� (si hay problemas es porque se alter� el txt)
    head(anti.data, 15)
    
    # hacer una variable para determinar si es correcta, incorrecta o dropeada
        datos <- mutate(anti.data, respuesta = ifelse(Count < 0, "drop", 
                                        ifelse(fstCorrect == TRUE, "correct", "incorrect")),
                                estimulo = estimulo)
    
    # Factor el estimulo 1 = alimento, 2 = neutro
    datos$estimulo <- factor(datos$estimulo, levels = c(1,2), labels = c("food", "neutral"))
    anti.data <- datos
    
    # ---- 3. Funcion para calcular stats ---------------------------------------------------
    stat.fun <- function(datos){
		# Sacar conteos y promedios
		datos <- group_by(datos, respuesta)
		datos <- summarize(datos, n = n(), mean = mean(lat, na.rm = TRUE))
		datos <- data.frame(datos)
		
		
		# Pudiera ser que falta una categor�a, hay que rellenar
		categorias <- c("correct", "drop", "incorrect")
		falta <- c("correct", "drop", "incorrect") %in% datos[["respuesta"]]
		categorias <- categorias[!falta]

			# encontes si falta una o m�s se agrega
			if (length(categorias)>0){
				falta <- data.frame(respuesta = categorias, n = NA, mean = NA, stringsAsFactors = FALSE)
				
				# Se agrega lo que falta al data frame de los datos
				datos <- rbind(datos, falta)
			}
		
		# Ordenamos seg�n categor�a para que siempre quede igual y poder transponer
		datos <- arrange(datos, respuesta)
		
		# Primero con los conteos + calcular porentajes
		conteos <- data.frame(t(datos["n"]))
		names(conteos) <- c("n.corr", "n.drop", "n.inco")   # Ponerle nombre a las variables
		
		conteos <- mutate(conteos, trial = sum(n.corr, n.inco, n.drop, na.rm = TRUE), 
								pct.corr = n.corr/trial,
								pct.inco = n.inco/trial,
								pct.drop = n.drop/trial,
								pct.trial = sum(pct.corr, pct.inco, pct.drop, na.rm = TRUE))

		# Ahora los porcentajes y quitar el de drop
		means <- data.frame(t(datos[["mean"]]))
		names(means) <- c("mean.corr", "mean.drop", "mean.inco")   # Ponerle nombre a las variables
		means <- select(means, -mean.drop)
		
		# Ahora juntamos todo y sacamos este resultado de la funci�n 
		stats <- cbind(id, conteos, means)
	     return(stats)
	}
    # -------------------------------------------------------------------------------------
    
    # Ahora calcular las stats para food y neutral por separado
    food <- filter(anti.data, estimulo == 'food')
    food <- stat.fun(food)
    
    neutral <- filter(anti.data, estimulo == 'neutral')
    neutral <- stat.fun(neutral)
    
    # Hay que arreglar los nombres porque ahroa es food y neutral
    varname <-  c("id", "Ncorr", "Ndrop", "Ninc", "Ntrial", "Pcorr", "Pinc",
                  "Pdrop", "Ptrial", "Mcorr", "Minc")
    
    names(food) <- paste("food.", varname, sep = "")
    names(neutral) <- paste("neu.", varname, sep = "")
    
    stats <- bind_cols(food, neutral)
    stats <- rename(stats, id = food.id)
    stats <- select(stats, -neu.id)
    
        # Juntamos las estadisticas y el original en una lista
    resultado <- list(stats = stats, data = anti.data)
    return(resultado)
}


# ---- Ahora vamos a procesar todos los datos que esten en el verctor "archivos" --------- #
# mirar archivos que participan
archivos

# Crear un data.frame en blanco para poner ahi cada archivos,
# uno para stats y otro para los files
anti.stat <- NULL
anti.data <- NULL

# Ejecutar la funci�n en cada file, y pasar cada resutlado a su data.frame final
for (anti.file in archivos){
#     print(anti.file)
    # Parsear la funci�n
    anti.list <- procesar.anti(anti.file)
    
    # Sacar las stats y pasarlas al data.frame final
    anti.stat <- rbind(anti.stat, anti.list[["stats"]])
    
    # Mismo para los files
    anti.data <- rbind(anti.data, anti.list[["data"]])
}


# ---- Finalmente guardar en un excel --------------------------------------------------- #
# Lo guardo en un CSV no mas, menos atado.
write.csv(anti.stat, file = "anti.stat.csv",  row.names = FALSE)
write.csv(anti.data, file = "anti.data.csv",  row.names = FALSE)



