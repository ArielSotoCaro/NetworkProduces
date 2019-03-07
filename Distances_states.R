# GEnerating matrix of distances

statesAvocado_o <- Avocado %>% select(Origen, Latitude.Origin,Longitude.Origin)
statesAvocado_d <- Avocado %>% select(Destino, Latitude.Destiny,Longitude.Destiny)
statesAvocado <- as.data.frame(bind_rows(statesAvocado_o,statesAvocado_d))
statesAvocado <- statesAvocado[!duplicated(statesAvocado), ]

round(GeoDistanceInMetresMatrix(df.cities) / 1000)