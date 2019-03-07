# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/

library(ape)

state.dists <- as.matrix(dist(cbind(avo_vertex$Longitude, avo_vertex$Latitude)))

state.dists.inv <- 1/state.dists

diag(state.dists.inv) <- 0


Moran.I(avo_vertex$, state.dists.inv) # here we need a vertex atribute apart of coordenates