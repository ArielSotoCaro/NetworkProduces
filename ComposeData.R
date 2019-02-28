# Consolidation of scrapped data
library(data.table) # rbindlist
library(stringr)
library(dplyr)

setwd("C:/Users/Ariel/Dropbox/UFL/Spatial Networks/Project")

filesFrom <- "C:/Users/Ariel/Dropbox/RESEARCH/Material tentativo"


# ==================
# AVOCADOS
# ..................

# 1998 - 2018
#Load data for avocados varieties
avo1 <- get(load(paste0(filesFrom,"/","PricesMexico_lst_AVOCADO-extra.RData")))
avo2 <- get(load(paste0(filesFrom,"/","PricesMexico_lst_AVOCADO-hass.RData")))
avo3 <- get(load(paste0(filesFrom,"/","PricesMexico_lst_AVOCADO-pagua.RData")))
rm(list=ls(pattern="PricesMexico"))

# Extra start at 2008, so I drop the first 8 years
avo1 <- avo1[-c(seq(1,8))]

# Change variable class
# using a function
CorrectClass <- function(x){
      for(i in 1:length(x)){
        dta <- data.frame(x[[i]])
        dta$Destino <-  sapply(dta$Destino,toString)
        dta$Origen <-  sapply(dta$Origen,toString)
        dta$Precio <- as.numeric(as.character(dta$Precio))
        x[[i]] <- dta
      }
  rm(dta)
return(x)
}
# Applying the function
avo1 <- CorrectClass(avo1)
avo2 <- CorrectClass(avo2)
avo3 <- CorrectClass(avo3)


# Function to change names
ChangeNames <- function(x){
  for(i in 1:length(x)){
    dta <- data.frame(x[[i]])
    dta$Destino <- dta$Destino %>%
      str_c() %>%  str_extract("^(\\w*).*$") %>%
      str_replace_all(c("(Aguascalientes).*$" = "Aguascalientes",
                        "(Baja California).*$" = "Baja California",
                        "(Baja California Sur).*$" = "Baja California Sur",
                        "(Campeche).*$" = "Campeche",
                        "(Coahuila).*$" = "Coahuila",
                        "(Colima).*$" = "Colima",
                        "(Chiapas).*$" = "Chiapas",
                        "(Chihuahua).*$" = "Chihuahua",
                        "(Distrito Federal).*$" = "Distrito Federal",
                        "(DF:).*$" = "Distrito Federal",
                        "(Durango).*$" = "Durango",
                        "(Guanajuato).*$" = "Guanajuato",
                        "(Guerrero).*$" = "Guerrero",
                        "(Hidalgo).*$" = "Hidalgo",
                        "(Jalisco).*$" = "Jalisco",
                        "(México).*$" = "Mexico",
                        "(Michoacán).*$" = "Michoacan",
                        "(Morelos).*$" = "Morelos",
                        "(Nayarit).*$" = "Nayarit",
                        "(Nuevo León).*$" = "Nuevo Leon",
                        "(Oaxaca).*$" = "Oaxaca",
                        "(Puebla).*$" = "Puebla",
                        "(Querétaro).*$" = "Queretaro",
                        "(Sonora).*$" = "Sonora",
                        "(Quintana).*$" = "Quintana Roo",
                        "(Sinaloa).*$" = "Sinaloa",
                        "(Tamaulipas).*$" = "Tamaulipas",
                        "(Tabasco).*$" = "Tabasco",
                        "(San Luis Potosí).*$" = "San Luis Potosi",
                        "(Veracruz).*$" = "Veracruz",
                        "(Yucatán).*$" = "Yucatan",
                        "(Zacatecas).*$" = "Zacatecas")) 

  dta$Origen <- dta$Origen %>%
    str_c() %>%  str_extract("^(\\w*).*$") %>%
    str_replace_all(c("Aguascalientes" = "Aguascalientes",
                      "Baja California" = "Baja California",
                      "Baja California Sur" = "Baja California Sur",
                      "Campeche" = "Campeche",
                      "Coahuila" = "Coahuila",
                      "Colima" = "Colima",
                      "Chiapas" = "Chiapas",
                      "Chihuahua" = "Chihuahua",
                      "Distrito Federal" = "Distrito Federal",
                      "DF" = "Distrito Federal",
                      "Durango" = "Durango",
                      "Guanajuato" = "Guanajuato",
                      "Guerrero" = "Guerrero",
                      "Hidalgo" = "Hidalgo",
                      "Jalisco" = "Jalisco",
                      "México" = "Mexico",
                      "Michoacán" = "Michoacan",
                      "Nayarit" = "Nayarit",
                      "Nuevo León" = "Nuevo Leon",
                      "Oaxaca" = "Oaxaca",
                      "Puebla" = "Puebla",
                      "Querétaro" = "Queretaro",
                      "Sonora" = "Sonora",
                      "Quintana" = "Quintana Roo",
                      "Sinaloa" = "Sinaloa",
                      "Tamaulipas" = "Tamaulipas",
                      "Tabasco" = "Tabasco",
                      "San Luis Potosí" = "San Luis Potosi",
                      "Veracruz" = "Veracruz",
                      "Yucatán" = "Yucatan",
                      "Zacatecas" = "Zacatecas")) 
  x[[i]] <- dta
  }
  rm(dta)
  return(x)
}

# Changing names of Origin and Destination observations
avo1 <- ChangeNames(avo1)
avo2 <- ChangeNames(avo2)
avo3 <- ChangeNames(avo3)


## add year ----
# Extra (start at 2006)
for( i in seq_along(avo1)){
  avo1[[i]]$year <- rep(2005+i,nrow(avo1[[i]]))
  avo1[[i]] <- as.data.table(avo1[[i]]) # should be data.table to use function "rbindlist"
}
# Hass
for( i in seq_along(avo2)){
  avo2[[i]]$year <- rep(1997+i,nrow(avo2[[i]]))
  avo2[[i]] <- as.data.table(avo2[[i]])
}
# Pagua
for( i in seq_along(avo3)){
  avo3[[i]]$year <- rep(1997+i,nrow(avo3[[i]]))
  avo3[[i]] <- as.data.table(avo3[[i]])
}

# Appending each list
avocadoList1 <- rbindlist(avo1)
avocadoList2 <- rbindlist(avo2)
avocadoList3 <- rbindlist(avo3)

# Generating Mean per year per interaction

avocadoList1 <- avocadoList1 %>% group_by(year,Origen,Destino) %>% summarise(mPrice = mean(Precio))
avocadoList2 <- avocadoList2 %>% group_by(year,Origen,Destino) %>% summarise(mPrice = mean(Precio))
avocadoList3 <- avocadoList3 %>% group_by(year,Origen,Destino) %>% summarise(mPrice = mean(Precio))


# Appending
avocadoTotal <- list(avocadoList1,avocadoList2,avocadoList3)
avocadoTotal <- data.frame(avocadoTotal,produce=rep("avocado",nrow(avocadoTotal)))


Avocado <- rbindlist(avocadoTotal)
Avocado <- data.frame(Avocado,produce=rep("avocado",nrow(Avocado)))
Avocado <- data.table(Avocado)
# Uniques.Origins.Avocado <- unique(Avocado$Origen)

# ==================
# CORN
# ..................


# 1998 - 2018
#Load data for avocados varieties
corn <- get(load(paste0(filesFrom,"/","PricesMexico_lst_Corn.RData")))
rm(list=ls(pattern="PricesMexico"))

# Applying the function to Correct Class
corn <- CorrectClass(corn)

# Changing names of Origin and Destination observations
corn <- ChangeNames(corn)


## add year ----
for( i in seq_along(corn)){
  corn[[i]]$year <- rep(1997+i,nrow(corn[[i]]))
  corn[[i]] <- as.data.table(corn[[i]])
}

# Appending each list
cornList <- rbindlist(corn)


# Generating Mean per year per interaction
cornList <- cornList %>% group_by(year,Origen,Destino) %>% summarise(mPrice = mean(Precio))
cornList <- data.frame(cornList,produce=rep("corn",nrow(cornList)))

Corn <- data.table(cornList)

# ==================
# TOMATO
# ..................


# 1998 - 2018
#Load data for avocados varieties
tomato <- get(load(paste0(filesFrom,"/","PricesMexico_lst_TOMATO.RData")))
rm(list=ls(pattern="PricesMexico"))

# Applying the function to Correct Class
tomato <- CorrectClass(tomato)

# Changing names of Origin and Destination observations
tomato <- ChangeNames(tomato)


## add year ----
for( i in seq_along(tomato)){
  tomato[[i]]$year <- rep(1997+i,nrow(tomato[[i]]))
  tomato[[i]] <- as.data.table(tomato[[i]])
}

# Appending each list
tomatoList <- rbindlist(tomato)


# Generating Mean per year per interaction
tomatoList <- tomatoList %>% group_by(year,Origen,Destino) %>% summarise(mPrice = mean(Precio))
tomatoList <- data.frame(tomatoList,produce=rep("tomato",nrow(tomatoList)))


Tomato <- data.table(tomatoList)

# ==================
# STORING DATA
# ..................
library(xlsx)
write.xlsx(Avocado, "Avocado_data.xlsx")
write.xlsx(Corn, "Corn_data.xlsx")
write.xlsx(Tomato, "Tomato_data.xlsx")

# create just one file
Produces <- bind_rows(Avocado,Tomato)
Produces <- bind_rows(Produces,Corn)

write.xlsx(Produces, "Total_produce_data.xlsx")
