## Script para unir archivos de Eyetrack luego del analisis en la maquina virtual
library(openxlsx); library(dplyr)
rm(list=ls())

# Para configurar un path en windows
# cmd en modo administrador
# setx path "%PATH%;C:\Rtools\bin" /M
# setx path "%PATH%;C:\Rtools\gcc-4.6.3\bin" /M

# setwd("D:/Varios INTA/Bases de datos 21y/CV21y/Eyetrack reward/Resultado 1")
# setwd("D:/Varios INTA/Bases de datos 21y/CV21y/Eyetrack reward/Resultado 2")
# setwd("D:/Varios INTA/Bases de datos 21y/NIH21y/Eyetrack reward resultado")
# setwd("C:/Users/Oliver/Desktop/7391")
setwd("C:/Users/Oliver/Desktop/id4950/ok")

# ---------------------------------------------------------------------------------------------- #
# --- Cargar funciones y antecedentes para hacer la combinacion y calculos  -------------------- #
# ---------------------------------------------------------------------------------------------- #
# Valores de corte en latencia. Van en la funcion "stats"
lat.min <- 70
lat.max <- Inf

# yesno <- readline(paste("Las latencias de corte al original son:\n Minimo: ", lat.min, 
# 	"\n Maximo:", lat.max, "\nDesea cambiarlas [y/n]"))
# 
# if (yesno == "y"){
# 	lat.min <- readline("Indique la latencia mínima")
# 	lat.min <- as.numeric(lat.min)
# 	lat.max <- readline("Indique la latencia máxima")
# 	lat.max <- as.numeric(lat.max)
# } else if (yesno == "n"){
# 	cat("Continue...\n")
# } else {
# 	cat("Valor ingresado no es válido\n")
# 	break()
# }
# rm(yesno)	


# Indices de archivos 
archivos <- dir()
archivos <- archivos[grep("trial", archivos)]
# file <- archivos[19]; file # para prueba


# Mensajes
# gato <- function() {cat("\n\nListos los calculos\n\n     /\\_/\\\n    (='_'=)\n    (\")_(\")\n\n\n")}
# time <- function() {cat("\n Se está guardando ... \n el Excel se guarda en el mismo folder que los archivos del eyetrack...\n")}
# dog <- function() {cat("\n   __","\n", "o-''|\\_____/)","\n"," \\_/|_)     )","\n","    \\  __  /","\n","    (_/ (_/\n\n")}


## Funcion lectura del numero del id
lee_id <- function(txt) {
	id <- as.numeric(sub(".trial.txt", "", txt))
	return(id)
}



## Lee el archivo "trial.txt" y deja legible para mas adelante
lee_trial <- function(file) {
     datos <- read.table(file, sep="\t", header=TRUE)
     datos <- select(datos, trial, xdat, lat, Count)
     datos <- mutate(datos, response = substr(xdat,2,2), position = substr(xdat, 3, 3))
     datos <- select(datos, -xdat)
     datos$response <- factor(datos$response, levels = c(7,8,9), labels=c("Neutral", "Reward", "Loss"))
     return(datos)
}
# lee_trial(file)



## Acá se usa el mismo file del trial y se transforma para leer la sacada
lee_sac <- function(file){
	datos <- read.table(sub(".trial.txt", ".sac.txt", file), sep="\t", header=TRUE)
	datos <- select(datos, subj, trial, onset, cordir, gtMinLen, intime)
	datos <- mutate(datos, onset = onset*1000) %>% rename(latencia = onset)
	datos <- mutate(datos, latencia = round(latencia, 0))
	return(datos)
}
# lee_sac(file)
# stop()



## Toma cada parte de la lista y crea un data frame
# file <- archivos[19]; file
# datos <- lee_trial(file); datos
stats <- function(datos) {
     # Crea una lista con cada estimulo (antes estaba separado)
     loss <- filter(datos, response=="Loss")
     rewa <- filter(datos, response=="Reward")
     neut <- filter(datos, response=="Neutral")
     lista <- list(Loss = loss, Reward = rewa, Neutral=neut)
     
     # Loop para los calculos sobre cada elemento de la lista
     eyetrack <- NULL
     for (i in 1:3) {

          # Corte a missing de outliers, <<<<<esto esta al inicio>>>>>>
          part <- lista[[i]]
          part <- mutate(part, lat = ifelse(lat < lat.min | lat > lat.max, NA, lat))
          part <- group_by(part, Count, response) 
          part <- as.data.frame(summarize(part, lat = mean(lat, na.rm=TRUE), n = n()))
		
		
		# El count es -1(drop), 0(error), 1(correcta), 2(correctiva). 
		# Si no hay alguna de estas respuestas se corrige.
		rows <- c(-1:2)[!(-1:2 %in% part$Count)]
		if (length(rows) > 0) {part <- rbind(part, data.frame(Count = rows, response=NA, lat = NA, n = NA))}
		part <- arrange(part, Count) %>% mutate(n = ifelse(is.na(n), 0, n))
		
          # Crea base conteo
          # Como esta ordenado el "Count" se le puede poner nombre al tipo de respuesta
          conteo <- as.data.frame(t(part$n))
          names(conteo) <- c("n_drop", "n_error", "n_corr", "n_ctva")
          conteo <- mutate(conteo, n_etotal = sum(n_error, n_ctva)) 
          conteo <- select(conteo, n_drop, n_error, n_ctva, n_etotal, n_corr)
          conteo <- mutate(conteo, trials = sum(n_drop, n_error, n_corr, n_ctva), n_valid = trials - n_drop)
          conteo <- mutate(conteo, pct_error = n_error/n_valid, pct_ecorr = n_ctva/n_valid,
                                   pct_etotal = n_etotal/n_valid, pct_corr = n_corr/n_valid)
		conteo <- rename(conteo, n_ecorr = n_ctva)
          
          
          # Crea base latencias
          # Como esta ordenado el "Count" se le puede poner nombre al tipo de respuesta
          tiempo <- as.data.frame(t(part$lat))
          names(tiempo) <- c("lat_drop", "lat_error", "lat_corr", "lat_ctva")
          tiempo <- select(tiempo, -lat_drop)
          tiempo <- select(tiempo, lat_error, lat_ctva, lat_corr)
          tiempo <- mutate(tiempo, lat_etotal = mean(c(lat_error, lat_ctva), na.rm=TRUE))   # <<<<<ojo con esto>>>>>
		tiempo <- rename(tiempo, lat_ecorr = lat_ctva)
          for (j in 1:dim(tiempo)[2]) {if (is.nan(tiempo[1,j])==TRUE) {tiempo[1,j] <- NA}}
          
          
          # Respuesta que en realidad es estimuo
          response <- select(part, response)
          response <- filter(response, !(is.na(response)))
          response <- response[1,1]
          
          
          # Combinar los resultados y pasa a NA si el pct es cero
          finaldf <- cbind(estimulo = response, conteo, tiempo)
          for (var in 9:12) {
			if (finaldf[1,var]==0 | is.na(finaldf[1,var])) {
				finaldf[1,var] <- NA}
			}
          
          eyetrack <- rbind(eyetrack, finaldf)
     }
return(eyetrack)
}
# file <- archivos[19]; file
# datos <- lee_trial(file); datos
# stats(datos)
# stop() 




## ----- FUNCION FINAL STATS ------ #
# Integra y computa las latencias correctivas, requiere de otras funciones
# e irá como funcion final para los cálculos. Argumento el nombre del file
# file <- archivos[19]; file
stats.file <- function(file){
	# Estmaciones previas
	id <- lee_id(file)
	trial.data <- lee_trial(file)
	sac.data <- lee_sac(file)
		
	# Rescatar las latencia correctivas (puede que no hayan) del archivo trial.txt
	trial.ctva <- filter(trial.data, Count == 2) %>% select(trial, lat)
		
	if (dim(trial.ctva)[1] == 0){
		# Pasar la funcion de stats y pegarle las latencias correctivas
		datos <- stats(trial.data)
		datos <- cbind(id, datos)
		datos <- mutate(datos, lat_ctva = NA)
				
	} else {
		lat.ctva <- NULL
		for (trial in trial.ctva$trial){
			# Captura la data sac.txt del trial y agrega indicador del TRUEx3
			ctva <- sac.data[sac.data$trial %in% trial, ]
			ctva <- mutate(ctva, test = ifelse(cordir==TRUE & gtMinLen==TRUE & intime==TRUE, 1, 0))
			
			# Captura el index de la latencia en sac.txt que es igual a latencia trial.txt
			# se hace a lo bestia porque el redondeado no es tan ok para hacer el match entre latencias
			x <- trial.ctva[trial.ctva$trial == trial, 2]
			x <- mutate(ctva, diff_lat = abs(latencia - x), grep = ifelse(diff_lat<=1, 1, 0))
			x <- grep(1, x$grep)
			
			# A partir de X (que debiera ser 1, pero weh) buscar el test == 1
			for (i in x:dim(ctva)[1]){
				if (ctva$test[i]==1){
					indx <- i
					break()
				} else {
					indx <- NA
				}
			}
			
			# Guardar y compilar
			if (is.na(indx)==TRUE){
				ctva <- select(ctva, trial, latencia) 
				ctva <- slice(ctva, 1) %>% mutate(latencia = NA)
				lat.ctva <- rbind(lat.ctva, ctva)
			} else {
				ctva <- select(ctva, trial, latencia) 
				ctva <- slice(ctva, indx)
				lat.ctva <- rbind(lat.ctva, ctva)	
			}
		}
		
		# Pegarle el response a las latencias correctivas y calcular promedios por response
		lat.ctva <- merge(lat.ctva, trial.data, by="trial")
		lat.ctva <- select(lat.ctva, trial, response, latencia)
		lat.ctva <- group_by(lat.ctva, response)
		lat.ctva <- summarize(lat.ctva, lat_ctva=mean(latencia, na.rm=TRUE))
		lat.ctva <- as.data.frame(lat.ctva)
		lat.ctva <- rename(lat.ctva, estimulo = response)
			# Esto se le pega al resultado de la funcion stats
			
		# Pasar la funcion de stats y pegarle las latencias correctivas
		datos <- stats(trial.data)
		datos <- merge(datos, lat.ctva, by="estimulo", all.x=TRUE)
		datos <- cbind(id, datos)
	}
return(datos)
}
# file <- archivos[19]; file
# stats.file(file)
# stop()



## ----- Funcion final Raw ----- #
raw.file <- function(file){
	# Estmaciones previas
	id <- lee_id(file)
	trial.data <- lee_trial(file)
	sac.data <- lee_sac(file)
	
	# REPETIDO DE LA FUNCION ANTERIOR # Pa no ponerse complicado
	# Rescatar las latencia correctivas (puede que no hayan) del archivo trial.txt
	trial.ctva <- filter(trial.data, Count == 2) %>% select(trial, lat)
		
	if (dim(trial.ctva)[1] == 0){
		# Pasar la funcion de stats y pegarle las latencias correctivas
		lat.ctva <- NULL
			
	} else {
		lat.ctva <- NULL
		for (trial in trial.ctva$trial){
			# Captura la data sac.txt del trial y agrega indicador del TRUEx3
			ctva <- sac.data[sac.data$trial %in% trial, ]
			ctva <- mutate(ctva, test = ifelse(cordir==TRUE & gtMinLen==TRUE & intime==TRUE, 1, 0))
			
			# Captura el index de la latencia en sac.txt que es igual a latencia trial.txt
			# se hace a lo bestia porque el redondeado no es tan ok para hacer el match entre latencias
			x <- trial.ctva[trial.ctva$trial == trial, 2]
			x <- mutate(ctva, diff_lat = abs(latencia - x), grep = ifelse(diff_lat<=1, 1, 0))
			x <- grep(1, x$grep)
			
			# A partir de X (que debiera ser 1, pero weh) buscar el test == 1
			for (i in x:dim(ctva)[1]){
				if (ctva$test[i]==1){
					indx <- i
					break()
				} else {
					indx <- NA
				}
			}
			
			# Guardar y compilar
			if (is.na(indx)==TRUE){
				ctva <- select(ctva, trial, latencia) 
				ctva <- slice(ctva, 1) %>% mutate(latencia = NA)
				lat.ctva <- rbind(lat.ctva, ctva)
			} else {
				ctva <- select(ctva, trial, latencia) 
				ctva <- slice(ctva, indx)
				lat.ctva <- rbind(lat.ctva, ctva)	
			}
		}	
	}
	
	
	# Volvemos a hacer la funcion del lectura de datos para crear algo mas completo
     raw <- read.table(file, sep="\t", header=TRUE)
     raw <- select(raw, trial, xdat, lat, Count)
     
     # completamos de datos las latencias correctivas
     if (is.null(lat.ctva) == TRUE){
		raw.ctva = NULL
     } else {
          raw.ctva <- merge(lat.ctva, raw, by="trial", all.x=TRUE)
		raw.ctva <- select(raw.ctva, trial, xdat, latencia, Count) %>% rename(lat = latencia)
		raw.ctva <- mutate(raw.ctva, Count = 3)
	}
     
     # Combinamos
     raw <- rbind(raw, raw.ctva)
     raw <- arrange(raw, trial)
     
     # Data Management final
     raw <- mutate(raw, response = substr(xdat,2,2), position = substr(xdat, 3, 3))
     raw$estimulo <- factor(raw$response, levels = c(7,8,9), labels=c("Neutral", "Reward", "Loss"))
     raw$resultado <- factor(raw$Count, levels=c(-1,0,1,2,3), labels=c("drop", "error", "corr", "ecorr", "ctva"))
	raw <- cbind(id, raw)
	   
     return(raw)
}
# stop()




# ---------------------------------------------------------------------------------------------- #
# --- Pasar las funciones por cada archivo y compilar un excel --------------------------------- #
# ---------------------------------------------------------------------------------------------- #
## Consolidacion de stats por response e id
base_eyetrack_stat <- NULL
filemalo <- NULL
# Loop en cada archivo (id)
# file <- archivos[19]; file

for (file in archivos) {
	cat("Calculando ", round((grep(file, archivos)/length(archivos))*100, digits=2), "%\n", sep="")
	
	trial.data <- lee_trial(file)
	if (sum(is.na(trial.data$response)) == 60) { filemalo <- c(filemalo, file); next() }
    
     datos <- stats.file(file)
     base_eyetrack_stat <- rbind(base_eyetrack_stat, datos)
}
# Exportado
# time()
write.xlsx(base_eyetrack_stat, "base_eyetrack_stat.xlsx", sheetName="BaseEyeTrackStat", rowNames=FALSE, keepNA=FALSE)



## Consolidacion de archivos para ver lo de la posicion
base_eyetrack_raw <- NULL
filemalo2 <- NULL
# Loop en cada archivo
for (file in archivos) {
	cat("Llevamos ", round((grep(file, archivos)/length(archivos))*100, digits=2), "%\n", sep="")

	trial.data <- lee_trial(file)
	if (sum(is.na(trial.data$response)) == 60) { filemalo2 <- c(filemalo2, file); next() }
     
     id <- lee_id(file)
     datos <- raw.file(file)
     base_eyetrack_raw <- rbind(base_eyetrack_raw, datos)
}
# Exportando
# time()
write.xlsx(base_eyetrack_raw, "base_eyetrack_raw.xlsx", sheetName="BaseEyeTrackRaw", rowNames=FALSE, keepNA=FALSE)

# gato()
# dog()


# Guarda los malos
malos <- data.frame(malo1 = filemalo, malo2 = filemalo2, stringsAsFactors=FALSE)
write.xlsx(malos, "base_eyd.malos.xlsx", sheetName="eyd.malo", rowNames=FALSE, keepNA=FALSE)

