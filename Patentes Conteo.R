library(dplyr)
library(readxl)
library(stringr)
library(genero)
library(gender)
library(openxlsx)

#LECTURA DE BASE DE DATOS 
datos<-read_xlsx("Otorgadas 2022.xlsx") #Base de Datos PATENTES
wipo<- read.csv("WIPO.csv",  sep = ",") #Base de Datos WIPO

#PREPARACION DE LA BASE DE DATOS 
#WIPO
#Intercambiamos las letras por los generos para que coincidan con la funcion genero 
wipo$gender <- gsub("M", "male", wipo$gender) 
wipo$gender <- gsub("F", "female", wipo$gender)

#OTORGADAS 
## Separamos todos los nombres incluidos en la columna Inventores que estan separados por ; y un espacio para que esten separados unicamente por ;
datos$Inventores <- gsub("; ", ";", datos$Inventores, fixed = TRUE) #esto se realiza para que después no tengamos columnas vacías al momento de crear una nueva base de datos
## Contamos cuantas celdas necesitaremos dependiendo del total de nombres
max_cols <- max(str_count(datos$Inventores, ";")) + 1 
## Creamos una nueva base de datos en donde únicamente tendremos los nombres y cada uno de ellos estará en una columna
nombres_separados <- str_split_fixed(datos$Inventores, ";", max_cols)
## Convertimos los nombres a mayúsculas
nombres_separados <- toupper(nombres_separados)
## Asignamos los nombres a las nuevas columnas
colnames(nombres_separados) <- paste0("Nombre_", 1:max_cols)

## Unir los nombres separados a los datos originales para extraer las columnas en donde realizaremos el conteo
datos1 <- cbind(datos, nombres_separados)
datos_c<-datos1%>%dplyr::select(1,7:11,36:58)

for (i in 7:29) { # Recorre las columnas de la 7 a la 29
  datos_c[,i] <- ifelse(is.na(datos_c[,i]), "", # Si la columna está vacía, la rellena con un espacio en blanco
                      sub("\\s.*", "", as.character(datos_c[,names(datos_c)[i]]))) # Sino, toma la primera palabra antes de un espacio en blanco o cualquier otro caracter
  datos_c[,i] <- iconv(datos_c[,i], to = "ASCII//TRANSLIT") # Elimina los caracteres especiales
}


datos_c[datos_c=="MA."]<-"MARIA" #por ultimo se identifico que MA existia en varias observaciones, por lo que lo reemplazamos por MARIA

# CLASIFICACION DE GENERO

##GENERO WIPO
# crea un vector con los nombres de las columnas que contienen los nombres

#Funcion de asignación de genero
genesni <- function(nombre) {
  indice <- match(nombre, wipo$name)
  resultado <- ifelse(nombre %in% c("GUADALUPE", "YUNUEN", "TAYDE", "ROSARIO", "ALYED"), "neutral", NA)
  resultado[!is.na(indice)] <- wipo$gender[indice[!is.na(indice)]]
  resultado
}

# Aplicar la función a varias columnas del dataframe usando sapply
columnas <- colnames(datos_c)[7:28]
resultados_wipo <- sapply(datos_c[columnas], genesni) #resultados WIPO

wipo_results <-data.frame(resultados_wipo)
colnames(wipo_results)<-paste0("Genero1_",1:22) #Dar un nuevo nombre a las columnas que contiene el genero

## GENERO FUNCIÓN LIBRERÍA GENERO 

nombres<-datos_c[,7:28]
nombres[nombres==""] <-NA 
nombres[is.na(nombres)]<-"Missing" #Al poder colocar "Missing" la funcion genero podrá correr el código de manera correcta y no influenciarse por los posibles NA's

# Aplicamos la funcion de la libreria genero
gen_results<-data.frame(lapply(nombres,genero))

#Comparacion WIPO vs funcion genero
generossni <- function(col1, col2) {
  resultado <- vector("character", length = length(col1))
  for (i in 1:length(col1)) {
    if (is.na(col1[i]) & is.na(col2[i])) {
      resultado[i] <- NA
    }
    else if (is.na(col2[i])) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == col2[i]) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == "neutral") {
      resultado[i] <- as.character(col1[i])
    }
    else {
      resultado[i] <- "checar"
    }
  }
  return(resultado)
}

#En este data frame guardaremos el genero ginal
resultados_gen <- data.frame(matrix(ncol = 22, nrow = 507))

#Establecemos el genero final
for (i in 1:22) {
  col_df1 <- wipo_results[, i]
  col_df2 <- gen_results[, i]
  resultados <- generossni(col_df1, col_df2)
  resultados_gen[, i] <- resultados
}

colnames(resultados_gen)<-paste("Gen_",1:22) #Cambiamos el nombre de las columnas

# De nuestros resultados final hacemos una ultima conversión para poder hacer el conteo posterior
resultados_gen[resultados_gen=="male"] <-"M"
resultados_gen[resultados_gen=="female"] <-"F"

#Conteo total de Total de Hombres y Total de Mujeres
datos_c$Hombres <- rowSums(sapply(resultados_gen, grepl, pattern = "M"))
datos_c$Mujeres <- rowSums(sapply(resultados_gen, grepl, pattern = "F"))

#Asignamos valores de 0 y 1 para poder hacer realizar la clasificación de mejor manera
datos45 <- resultados_gen
datos45[datos45 == "M"] <- 1
datos45[datos45 == "F"] <- 0

datos45 <- datos45%>% 
  mutate("Solo Hombres" = apply(., 1, function(x) ifelse(sum(x == 1, na.rm = TRUE) == sum(!is.na(x)), 1, 0)),
         "Solo Mujeres" = apply(., 1, function(x) ifelse(sum(x == 0, na.rm = TRUE) == sum(!is.na(x)), 1, 0)),
         "Mixto" = apply(., 1, function(x) ifelse(sum(x == 1, na.rm = TRUE) > 0 & sum(x == 0, na.rm = TRUE) > 0, 1, 0)))

datos_c[,2:4]<-datos45[,23:25] #reemplazamos los resultados en nuestra base de datos con los nombres

datos[,7:11]<-datos_c[,2:6] #reemplazamos los resultados en nuestra base de datos original

write.xlsx(datos, "Otorgadas_conteo.xlsx") #Exportamos
