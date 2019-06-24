# --------------------------------------------------------------------------------------- #
# ---- Script para abrir y compilar archivos epi 13.05.2019 ----------------------------- #
# ---- Agrego separacion entre food y netural 20.05.2019 -------------------------------- #
# --------------------------------------------------------------------------------------- #
# Antes que todo hay que instalar en este orden <R -> RStudio -> Rtools> todo lo más 
# actualizado posible. Lo siguiente es instalar algunas librerías.
# install.packages("dplyr")
# install.packages("xlsx")
# install.packages("stringr")
# Borrar todo para testear rm(list=ls())
# Rtools no es necesario porque ya nose usa el xlsx

# ##############################################
# ##### Seleccionar la carpeta de trabajo. #####
setwd("D:/OneDrive/GitHub/inta/ANTI")
# ##############################################

# ---- Lo primero es cargar el espacio de trabajo y cargar las librerías ---------------- #
# Cargar librerías
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
match.file <- grep("NA.trial.txt", archivos)    # Para sacar la posición del texto dentro del vector

# Ahora selccionamos del vector archivos sólo los que estánen match.file
archivos <- archivos[match.file]


# ---- Ahora vamos a crear una función para que le haga todos los calculos a un file ----- #
# file <- archivos[5]

procesar.anti <- function(file = NULL){
    # ---- 1. Lo primero es capturar el id que está en el nombre del archivos
    # vemos que está al principio y separado por puntos
    id <- str_split_fixed(file, "\\.", 10)
    id <- id[1,1]
    id <- data.frame(id = as.numeric(id))
    
    
    # ---- 2. Lo segundo es abrir el archivo, es un txt separado por tabulaciones 
    anti.data <- read.table(file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    
    # Agregar el id a la data y el tipo de estimulo
    anti.data <- mutate(anti.data, id = id[1,1])
    anti.data$estimulo <- estimulo
    
    # Factor el estimulo 1 = alimento, 2 = neutro
    anti.data$estimulo <- factor(anti.data$estimulo, levels = c(1,2), labels = c("food", "neutral"))

    # hacer una variable para determinar si es correcta, incorrecta o dropeada
    # head(anti.data, 15); table(anti.data$Count)
    datos <- mutate(anti.data, respuesta = ifelse(Count == -1, "drop",
                                           ifelse(Count ==  0, "inc",
                                           ifelse(Count ==  1, "corr",
                                           ifelse(Count ==  2, "incCo", "ERROR")))))
    anti.data <- datos
    # View(anti.data)

    
    # ---- 3. Funcion para calcular stats ---------------------------------------------------
    # stat.fun(datos)
    stat.fun <- function(datos){
		# Sacar conteos y promedios
		# datos <- filter(anti.data, estimulo == 'food')
		datos.bak <- datos
		datos <- group_by(datos, respuesta)
		datos <- summarize(datos, n = n(), mean = mean(lat, na.rm = TRUE))
		datos <- data.frame(datos)
		
		# Pudiera ser que falta una categoría, hay que rellenar
		categorias <- c("corr", "drop", "inc", "incCo")
		falta <- categorias %in% datos[["respuesta"]]
		categorias <- categorias[!falta]

			# encontes si falta una o más se agrega
			if (length(categorias)>0){
				falta <- data.frame(respuesta = categorias, n = NA, mean = NA, stringsAsFactors = FALSE)
				
				# Se agrega lo que falta al data frame de los datos
				datos <- rbind(datos, falta)
			}
		
		# Ordenamos según categoría para que siempre quede igual y poder transponer
		datos <- arrange(datos, respuesta)
		categorias <- c("corr", "drop", "inc", "incCo")
		
		## Primero con los conteos + calcular porentajes
		conteos <- data.frame(t(datos["n"]))
		names(conteos) <- paste("N", categorias, sep = "")   # Ponerle nombre a las variables
		conteos <- mutate(conteos, NincTo = sum(Ninc, NincCo, na.rm = TRUE))
		
		# Ncorr Ndrop Ninc NincCo
		conteos <- mutate(conteos, trial = sum(Ncorr, Ndrop, Ninc, NincCo, na.rm = TRUE), 
								Pcorr = Ncorr/trial,
								Pdrop = Ndrop/trial,
								Pinc  = Ninc/trial,
								PincCo = NincCo/trial,
								PincTo = sum(Pinc, PincCo, na.rm = TRUE),
								Ptrial = sum(Pcorr, Pdrop, Pinc, PincCo, na.rm = TRUE))

		## Ahora los promedios y quitar el de drop
		means <- data.frame(t(datos[["mean"]]))
		names(means) <- c("Mcorr", "Mdrop", "Minc", "MincCo")   # Ponerle nombre a las variables
		means <- select(means, -Mdrop)
		
		INCmean <- filter(datos.bak, Count == 0 | Count == 2)
		INCmean <- mean(INCmean$lat, na.rm = TRUE)
		means$MincTo <- INCmean
		
		
		# Ahora juntamos todo y sacamos este resultado de la función 
		stats <- cbind(id, conteos, means)
        return(stats)
	}
    # -------------------------------------------------------------------------------------
    
    # Ahora calcular las stats para food y neutral por separado
    food <- filter(anti.data, estimulo == 'food')
    food <- stat.fun(food)
    names(food) <- paste("food.", names(food), sep = "")
    
    neutral <- filter(anti.data, estimulo == 'neutral')
    neutral <- stat.fun(neutral)
    names(neutral) <- paste("neut.", names(neutral), sep = "")
    
    # Combinar y dejar ok
    stats <- bind_cols(food, neutral)
    stats <- rename(stats, id = food.id)
    stats <- select(stats, -neut.id)
    
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
iderror <- NULL

# Ejecutar la función en cada file, y pasar cada resutlado a su data.frame final
# anti.file <- archivos[1]
for (anti.file in archivos){
    print(anti.file)

    # Parsear la función
    anti.list <- procesar.anti(anti.file)
    
    # Sacar las stats y pasarlas al data.frame final
    anti.stat <- rbind(anti.stat, anti.list[["stats"]])
    
    # Mismo para los files
    anti.data <- rbind(anti.data, anti.list[["data"]])
}
# View(anti.stat)


# ---- Finalmente guardar en un excel --------------------------------------------------- #
# Lo guardo en un CSV no mas, menos atado.
write.csv(anti.stat, file = "anti.stat.csv",  row.names = FALSE, na = "")
write.csv(anti.data, file = "anti.data.csv",  row.names = FALSE, na = "")




