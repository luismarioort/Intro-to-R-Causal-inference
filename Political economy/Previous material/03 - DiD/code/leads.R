## Leads / Lags 

#Tutorial:
# Author: Eduardo Zago

library(plm)
library(tidyverse)
library(lfe)

df<- haven::read_dta('../data/DID_data.dta')

### Vamos a trabajar con nuestra data de siempre, tratamiento continuo (se puede hacer dicotomico)
## Pero vamos a hacer unas modificaciones para entender lo que nos dice Horacio de como hacer leads sin perder data

## 1.0) Tenemos una base de tratamiento y de outcomes, la regla principal para poder hacer leads sin perder datos es
## tener más periodos en la data de tratamiento que en la de outcomes. Nosotros solo tenemos la base df ya echa, 
## para ver esto más claro dividamosla:

df_outcomes <- df |> select(unidades, tiempo, outcome)

df_tratamiento <- df |> select(unidades, tiempo, tratamiento)

## Ambas bases tienen el mismo número de periodos, podemos suponer que no: tratamiento llega 2 periodos más que los outcomes

df_2020 <- tibble(unidades = c(1:20), tiempo = 2020, tratamiento = 1)
df_2021 <- tibble(unidades = c(1:20), tiempo = 2021, tratamiento = 1)

# Esto es un poco obvio, pero en sus proyectos no lo pueden hacer: solo si ya tienen su base con diferencias en periodos se puede hacer:

df_tratamiento <- rbind(df_tratamiento, df_2020, df_2021)

df_tratamiento <- df_tratamiento[order(df_tratamiento[,1], df_tratamiento[,2]),]

# Noten que ya agregamos dos periodos mas de tratamiento, por simplicidad igual a 1 todos.

# Ahora, nos conviene entonces hacer los leads en esta base, usamos el código del tutorial, intentemos entenderlo mejor:

# Todas las unidades fueron tratadas en algun momento por lo que no es necesario generar la variable treat, ni mandar esos a 0

df_tratamiento <- pdata.frame(df_tratamiento, index = c("unidades","tiempo")) # creamos formato pdata de plm, por facilidad a la base de TRATAMIENTO

# Para entender estas funciones hay que ver los helps o buscar en Google, no se trata solo de correr mi 
# código en su base y esperar que corra, hay que saber que darle a las funciones, que nos regresan, etc:

# Observen en particular como te regresa el frame la función:
view(df_tratamiento)

# Las observaciones ya no estan númeradas del 1 al 200, si no que tienen un identificador único de unidad-tiempo:
# Por eso se puede usar la función lead

df_tratamiento <- cbind(df_tratamiento, plm::lead(df_tratamiento$tratamiento, c(1,2))) # creamos 2 leads, los unimos con cbind()
df_tratamiento <- as.data.frame(transform(df_tratamiento)) # reveritmos el formato de pdata al formato original

# Noten que en esta base si perdemos observaciones, 2 por unidad al ser dos leads, sin embargo, podemos intuir que en la base final 
# no se perderá ninguna, ya que solo llega al 2019

names(df_tratamiento)[(length(df_tratamiento) - 1):length(df_tratamiento)] <- c("tratamiento_Lead_1", 
                                                                                "tratamiento_Lead_2") 

# Ojo con como renombran sus variables, muy importante para la interpretacion

# Revertimos los factores creados por plm
library(varhandle)
df_tratamiento <- unfactor(df_tratamiento)

# Y podemos ya unir con los outcomes (claramente es un left_join con la de outcomes)

df_outcomes <- df_outcomes |> left_join(df_tratamiento, 
                                        by = c("unidades", "tiempo"))

# Noten no perdemos ninguna observación, entonces podemos correr nuestro análisis:

reg_leads <- felm(outcome ~ tratamiento + tratamiento_Lead_1 + tratamiento_Lead_2 |
                    unidades + tiempo | 0 | unidades, data = df_outcomes)

# Y lo visualizamos en stargazer:
library(stargazer)
tablaleads <- stargazer(reg_leads,
                        omit = c("Constant", "unidades", "time"),
                        covariate.labels = c("Tratamiento", "Adelanto 1", 
                                             "Adelanto 2"),
                        omit.stat = c("f", "ser","adj.rsq"),
                        add.lines = list(c("Unit Fixed Effects", "Yes", "Yes"),
                                         c("Time Fixed Effects", "Yes", "Yes")),
                        title = "Leads and Lags",
                        type = "text")
