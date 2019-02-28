

setwd("C:/Users/Ariel/Dropbox/RESEARCH/Material tentativo")

# Web Scrapping of Maize Prices from the Mexican system information platform.
# https://datacritics.com/2018/03/20/scrape-it-yourself-spotify-charts/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com

library(rvest)
library(tidyverse)
# library(magrittr)
# library(scales)
# library(knitr)
# library(lubridate)
# library(tibble)

# Every month
Months <- seq(1,12)
# The availble data range is 2000 - 2018
Years <- seq(2000,2018)

# Setting the constant part of the URL
url1 <- "http://www.economia-sniim.gob.mx/2010prueba/GranosMes.asp?Cons=M&prod=605&dest=T&dqMesMes="
url2 <- "&dqAnioMes="
url3 <- "&Formato=Nor&submit=Ver+Resultados"

# Funtion to create URLs for each month and year
unitedata <- function(mm,yy){
                full_url <- paste0(url1,mm,url2,yy,url3)
                return(full_url)
                }
# Scrapping function:
PriceMexicoScrape <- function(x){
                      page <- x

                      Destino <- page %>% 
                                  read_html() %>% 
                                  html_nodes('.Datos:nth-child(1)') %>% 
                                  html_text() %>% 
                                  as.data.frame()
                      Origen <- page %>% 
                                  read_html() %>% 
                                  html_nodes('.Datos+ .Datos') %>% 
                                  html_text() %>% 
                                  as.data.frame()
                      LenghTab <- page %>%
                                  read_html() %>%
                                  html_nodes('.EncabTab') %>%
                                  html_text() %>%
                                  as.data.frame()
                      if(nrow(LenghTab)==10){
                      PromMes <- page %>%
                                  read_html() %>%
                                  html_nodes('.DatosNum:nth-child(8)') %>%
                                  html_text() %>%
                                  as.data.frame()} else{
                      PromMes <- page %>%
                                  read_html() %>%
                                  html_nodes('.DatosNum:nth-child(7)') %>%
                                  html_text() %>%
                                  as.data.frame()}
                      
                      #combine, name, and make it a tibble
                        chart <- cbind(Destino,Origen,PromMes)
                        names(chart) <- c("Destino", "Origen", "Precio")
                        chart <- as.tibble(chart)
                        return(chart)
                    }
#### IMPLEMENTATION

final_url <- matrix(0,12,19)

for(m in 1:12){
  for(y in 1:19){
    final_url[m,y] <- unitedata(Months[m],Years[y])
  }
}

# Convert matrix to 1x1 vector
final_url <- c(final_url)

options(timeout = 30000000)
PricesMexico_lst <- list()
for(k in 1:228){
      PricesMexico_lst[[k]] <- PriceMexicoScrape(final_url[k]) 
      cat(k, base::date(), "\n")
      Sys.sleep(5)
      # break
}



# PricesMexico <- map_df(final_url, PriceMexicoScrape)

library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)

save(PricesMexico_lst, file="PricesMexico_lst.RData")
rm(list = ls(pattern = "url"))
load("PricesMexico_lst.RData")

# This fn create a NA value where are missing values
na_fun <- function(data,x) {
  if(nchar(data[x])==1){ Ndat <- NA} else {Ndat <- data[x]}
  return(Ndat)
}

tables_networks <- list()
  for(i in 1:length(PricesMexico_lst)){
    dta <- data.frame(PricesMexico_lst[[i]])
    dta$Destino <-  sapply(dta$Destino,toString)
    dta$Origen <-  sapply(dta$Origen,toString)
    dta$Precio <- as.numeric(as.character(dta$Precio))
    dta$Destino <- map(dta$Destino, na_fun)
    dta$Destino <- na.locf(dta$Destino)
    
    dta$Destino <- dta$Destino %>%
      str_c() %>%  str_extract('\\w*') %>%
      str_replace_all(c("Ags" = "Aguascalientes",
                        "BC" = "Baja California",
                        "Camp" = "Campeche",
                        "Coah" = "Coahuila",
                        "Col" = "Colima",
                        "Chis" = "Chiapas",
                        "Chih" = "Chihuahua",
                        "DF" = "Distrito Federal",
                        "Dgo" = "Durango",
                        "Gto" = "Guanajuato",
                        "Jal" = "Jalisco",
                        "Mex" = "Mexico",
                        "Mich" = "Michoacan",
                        "Nay" = "Nayarit",
                        "NL" = "Nuevo Leon",
                        "Oax" = "Oaxaca",
                        "Pue" = "Puebla",
                        "Qro" = "Queretaro",
                        "Son" = "Sonora",
                        "Sin" = "Sinaloa",
                        "Tamps" = "Tamaulipas",
                        "Tab" = "Tabasco",
                        "SLP" = "San Luis Potosi",
                        "Ver" = "Veracruz",
                        "Yuc" = "Yucatan",
                        "Zac" = "Zacatecas")) 
    dta$Origen <- iconv(dta$Origen,from="UTF-8",to="ASCII//TRANSLIT")
    
    dta <- dta[dta$Origen!=dta$Destino,] # drop self selling
    dta <- dta[!duplicated(dta[,1:2]), ] # drop origin-destination duplicated 
    tables_networks[[i]] <- dta
  }


head(tables_networks)
# -------------------------------------------

write.xlsx(tables_networks[[1]],'market1.xlsx', sheetName = 'year1')
write.xlsx(tables_networks[[2]],'market1.xlsx', sheetName = 'year2',append=TRUE)
write.xlsx(tables_networks[[3]],'market1.xlsx', sheetName = 'year3',append=TRUE)
write.xlsx(tables_networks[[4]],'market1.xlsx', sheetName = 'year4',append=TRUE)
write.xlsx(tables_networks[[5]],'market1.xlsx', sheetName = 'year5',append=TRUE)
write.xlsx(tables_networks[[6]],'market1.xlsx', sheetName = 'year6',append=TRUE)



# 
# 
# 
# 
# test2$Origen <- iconv(test2$Origen,from="UTF-8",to="ASCII//TRANSLIT")
# 
# test2 <- test2[test2$Origen!=test2$Destino,] # drop self selling
# test2 <- test2[!duplicated(test2[,1:2]), ] # drop origin-destination duplicated
# 
# 
# test2
