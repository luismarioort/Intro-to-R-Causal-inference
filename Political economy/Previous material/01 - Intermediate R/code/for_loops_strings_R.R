# For Loops in R
# Author: Eduardo Zago

# Cargamos los paquetes:
library(tidyverse)
library(arrow)

rm(list = ls()) # Limpia todo lo almacenado en el workspace

# Definimos el directorio de trabajo

# Establecemos el directorio de trabajo al directorio de este documento con el paquete rstudioapi
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Lo primero, para entender los tres tipos de loops, observemos un vector que va 
# del 1 al 10 y realicemos operaciones al mismo.

a <- c(1:10)

# Generamos nuestra primera lista de datos aleatorios, ahora, trabajar con listas es diferente
# a trabajar con frames, no podemos usar funciones precargadas de dplyr para 
# aplicar a los elementos de esta lista

# rnorm(n, mean, sd)

# For loop normal
list <- list()

for (i in a){
  list[[i]] <- rnorm(10, i)
}

# Función map, paquete purrr
list1 <- a %>% map(~ rnorm(10, .x))

# lapply:
list2 <- a %>% lapply(function(x){rnorm(10, x)})

# La primera ventaja de usar estas funciones es no tener que definir una lista
# y llenarla con un índice

# También podemos realizar operaciones sobre esta lista, por ejemplo, sacar la media
means1 <- c()

for (i in a){
  means1[i] <- mean(list[[i]])
}

# Con la función map es más facil, podemos decirle a la función que realicé 
# operaciones sobre los elementos de la lista
# que sean doubles:

means2 <- list1 %>% map_dbl(mean)

# lapply siempre regresa una lista, pero podemos usar unlist() o sapply

means3 <- list2 %>% lapply(function(x){mean(x)})

means3 <- unlist(means3)

means4 <- list2 %>% sapply(function(x){mean(x)})


# Ahora podemos observar un caso práctico que les puede servir para jalar grandes
# cantidades de archivos, (vease carpeta data en folder tutorial2)

# Generamos el vector por donde correra el loop
years <- c(2015:2020)

# Definimos la función que importara los documentos.

import_xlsx <- function(year){
  df <- readxl::read_xlsx(paste0("./data/gdp_", year, ".xlsx"))
  df$year <- year  # add year variable
  df <- df %>% mutate(state = tolower(state)) # states to lower case
  df
}

list_df1 <- years %>% lapply(function(x){import_xlsx(x)})

list_df2 <- years %>% map(~ import_xlsx(.x))

# Para convertirlo en un data frame, hay dos opciones:

# 1) Otro loop sobre los dataframes usando do.call: (unica opcion para lapply)

df1 <- do.call(rbind.data.frame, list_df1)

# 2) Podemos utilizar mejor la función de purrr que te lo convierte automaticamente

df2 <- years %>% map_dfr(~ import_xlsx(.x))

# En conclusión, la mejor forma de hacer loops en R sencillos es usando el paquete
# purrr, otras funciones que pueden ser de utilidad son: map_int, map_chr, map_if

#################################################################################

# Por último, me gustaría introducirlos al análisis de texto, principalmente 
# como limpiar y manejar strings para que no tengan problemas definiendo fechas o 
# haciendo joins con caracteres.

rm(list = ls()) # Limpia todo lo almacenado en el workspace

# Importamos unos datos de tweets:

df <- read_parquet("./data/likes_58Gulcan.parquet")

# Nos quedamos solo con los tweets en ingles, el texto y la fecha

df <- df %>% filter(lang == "en") %>% select(text, created_at)

# Podemos limpiar el texto, por ejemplo, remover virgulillas, acentos, urls, 
# usernames, etc.

# Todo a lower case:
df$text_clean <- tolower(df$text)

df$text_clean <- str_replace_all(df$text_clean, "'","") 
df$text_clean <- str_replace_all(df$text_clean, "~","")
df$text_clean <- str_replace_all(df$text_clean, "`","")
df$text_clean <- str_replace_all(df$text_clean, "#","")
df$text_clean <- str_replace_all(df$text_clean, "  ", " ") # double spaces

# Ahora limpiamos ubicando patterns:
df$text_clean <- gsub("[^\\s]*https://[^\\s]*","", df$text_clean, perl=T) #remove urls
df$text_clean <- gsub("[^\\s]*@[^\\s]*","", df$text_clean, perl=T) # remove user names 
df$text_clean <- gsub('[0-9]+', '',df$text_clean) # remove numbers 
df$text_clean <- str_replace_all(df$text_clean, "[^[:alnum:]]", " ")

df$text_clean <- str_replace_all(df$text_clean, "  ", " ") # double spaces
df$text_clean <- str_replace_all(df$text_clean, "  ", " ")


# Finalmente trabajamos con la fecha:

df <- df %>% mutate(created_at = as.Date(created_at))

df <- df %>% mutate(year = as.numeric(format(created_at, format="%Y")),
                    month = as.numeric(format(created_at, format="%m")),
                    week = as.numeric(strftime(created_at, format = "%V")), 
                    week_year = paste0(week, "-", year),
                    month_year_sen = paste0(month, "-", year))

# Y esto nos servirá para realizar los análisis de diff-in-diff cuando llegue el
# momento


