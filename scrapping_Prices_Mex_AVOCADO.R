# ======================
# SCRAPING 
# AVOCADO
# ======================

setwd("C:/Users/Ariel/Dropbox/RESEARCH/Material tentativo")

# Web Scrapping of Maize Prices from the Mexican system information platform.
# https://datacritics.com/2018/03/20/scrape-it-yourself-spotify-charts/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com

library(rvest)
library(tidyverse)

# The availble data range is 2000 - 2018
Years <- seq(1998,2018)


# Setting the constant part of the URL ----
# Variety Hass
url1 <- "http://www.economia-sniim.gob.mx/Nuevo/Consultas/MercadosNacionales/PreciosDeMercado/Agricolas/ResultadosConsultaFechaFrutasYHortalizas.aspx?fechaInicio=01/01/"
url2 <- "&fechaFinal=31/12/"
url3 <- "&ProductoId=133&OrigenId=-1&Origen=Todos&DestinoId=-1&Destino=Todos&PreciosPorId=2&RegistrosPorPagina=11000"


# Funtion to create URLs for each month and year
unitedata <- function(yy){
  full_url <- paste0(url1,yy,url2,yy,url3)
  return(full_url)
}

# Scrapping function:
PriceMexicoScrape <- function(x){
  page <- x
  
  Destino <- page %>% 
    read_html() %>% 
    html_nodes('.Datos2:nth-child(4)') %>% 
    html_text() %>% 
    as.data.frame()
  Origen <- page %>% 
    read_html() %>% 
    html_nodes('.Datos2:nth-child(3)') %>% 
    html_text() %>% 
    as.data.frame()
  PromMes <- page %>%
    read_html() %>%
    html_nodes('.Datos2:nth-child(7)') %>%
    html_text() %>%
    as.data.frame()
  
  
  #combine, name, and make it a tibble
  chart <- cbind(Destino,Origen,PromMes)
  names(chart) <- c("Destino", "Origen", "Precio")
  chart <- as.tibble(chart)
  return(chart)
}

#### IMPLEMENTATION ----

final_url <- matrix(0,1,21)

for(y in 1:21){
    final_url[y] <- unitedata(Years[y])
  }

# Convert matrix to 1x1 vector
final_url <- c(final_url)

## SCRAP!! ----
options(timeout = 30000000)
PricesMexico_lst <- list()

for(k in 1:21){
  PricesMexico_lst[[k]] <- PriceMexicoScrape(final_url[k]) 
  cat(k, base::date(), "\n")
  Sys.sleep(5)
  # break
}

save(PricesMexico_lst, file="PricesMexico_lst_AVOCADO-hass.RData")

# VARIETY 2 ----
# Aguacate pagua

url3 <- "&ProductoId=139&OrigenId=-1&Origen=Todos&DestinoId=-1&Destino=Todos&PreciosPorId=2&RegistrosPorPagina=11000"
#### IMPLEMENTATION

# Funtion to create URLs for each month and year
unitedata <- function(yy){
  full_url <- paste0(url1,yy,url2,yy,url3)
  return(full_url)
}

final_url <- matrix(0,1,21)

for(y in 1:21){
  final_url[y] <- unitedata(Years[y])
}

# Convert matrix to 1x1 vector
final_url <- c(final_url)

options(timeout = 30000000)
PricesMexico_lst_2 <- list()
for(k in 1:21){
  PricesMexico_lst_2[[k]] <- PriceMexicoScrape(final_url[k]) 
  cat(k, base::date(), "\n")
  Sys.sleep(5)
  # break
}
save(PricesMexico_lst_2, file="PricesMexico_lst_AVOCADO-pagua.RData")

# VARIETY 3 ----
# Aguacate Hass - calidad extra

url3 <- "&ProductoId=136&OrigenId=-1&Origen=Todos&DestinoId=-1&Destino=Todos&PreciosPorId=2&RegistrosPorPagina=11000"

#### IMPLEMENTATION

# Funtion to create URLs for each month and year
unitedata <- function(yy){
  full_url <- paste0(url1,yy,url2,yy,url3)
  return(full_url)
}

final_url <- matrix(0,1,21)

for(y in 1:21){
  final_url[y] <- unitedata(Years[y])
}

# Convert matrix to 1x1 vector
final_url <- c(final_url)

options(timeout = 30000000)
PricesMexico_lst_3 <- list()
for(k in 1:21){
  PricesMexico_lst_3[[k]] <- PriceMexicoScrape(final_url[k]) 
  cat(k, base::date(), "\n")
  Sys.sleep(5)
  # break
}
save(PricesMexico_lst_3, file="PricesMexico_lst_AVOCADO-extra.RData")
