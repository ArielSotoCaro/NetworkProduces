# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/

library(ape)

state.dists <- as.matrix(dist(cbind(Wver$Longitude, Wver$Latitude)))

state.dists.inv <- 1/state.dists

diag(state.dists.inv) <- 0


Moran.I(Wver$y1998, state.dists.inv) # here we need a vertex atribute apart of coordenates

# If p-value is under 0.05 we can reject the Ho, that there is zero spatial autocorrelation.
# If p-value > 0.05 means that there is really no evidence of negative (or positive) auto-correlation
# here, as with random data you would expect it to be a negative value more often than positive.


# another approach
# https://rdrr.io/rforge/spdep/man/moran.html

library(spdep)
