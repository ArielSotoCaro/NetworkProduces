setwd("C:/Users/Ariel/Dropbox/UFL/Spatial Networks/Project/NetworkProduces")

library(pacman)
p_load(readxl,dplyr,igraph,purrr)

# To work into R is requiere 2 types of dataframe.
# One for vertices w/attributes and other with the edges w/attributes

# ....................................
# Creating matrix of distances =======
#

# Loading data of coordetanes Lat-Lon for every Mexican State
coor <- read_xlsx("States_coordenates.xlsx")

# the function to compute strigt-line distances is: "function_matrixDistances.R"
# https://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r/
source("function_matrixDistances.R")

# Creating a dataframe compatible with the function above
States_data <- data.frame(States=coor$States,lat=coor$Latitude,lon=coor$Longitude)

# Creating matrix of distances itself:
MatrixD <- round(GeoDistanceInMetresMatrix(States_data) / 1000)
# distance represented in straigt line in KM
# Adding names
colnames(MatrixD) <- coor$States
rownames(MatrixD) <- coor$States
# ..............................

# Importing GDP percapita data
gdp <- read_xlsx("GDP-pc-MEX.xlsx")



# adding distances FUNCTION 
FindDistance <- function(origin,destiny){
  d <- MatrixD[as.matrix(origin),as.matrix(destiny)]
  return(d)
}



# .............................
# forming the data
#
# AVOCADO =========
# 
Avocado <- read_excel("Avocado_data.xlsx")
Avocado <- Avocado[,-1]







DT <- Avocado

vertices_list <- c()
vertices_list_w <- c()

# .............................
# from 1998 to 2014 (17 years)
for(y in 1:21){

# preparing vertices data
vertex <- DT %>% filter(year==1997+y) %>% select(Origen,Destino)

vertex <- unique(rbind(as.matrix(vertex[,1]),as.matrix(vertex[,2]))) %>%
  data.frame()
names(vertex) <- "States"
vertex <- inner_join(vertex,coor,by = "States")

# adding attributes to vertices
if(y<=17){
vertexW <- vertex %>% inner_join(gdp[,c(1,1+y)],by = "States")
} else {vertexW <-0}

# Saving data into the LIST
vertices_list[[y]] <- vertex
vertices_list_w[[y]] <- vertexW
}
# .............................

# preparing edges data
# This is a list because there are data for 21 years: 1998:2018
edges_list <- list()
for(i in 1:21){
  edges_list[[i]] <- DT[DT$year==1997+i,c("Origen","Destino","mPrice")]
}

# This procedure compute the distances for each state for each year
for(i in 1:21){
  dis <- c()
  edges_lst <- edges_list[[i]]
  for(j in 1:nrow(edges_lst)){
    dis[j] <- FindDistance(edges_lst[j,1],edges_lst[j,2])
  }
  edges_list[[i]]$distance <- dis
}


# Data for year 1,2,...
ver   <- vertices_list[[1]]
Wver  <- vertices_list_w[[1]] 
edg   <- edges_list[[1]]


# ............................
# Creating Undirected NW ======
#
g_undir <- graph_from_data_frame(d = edg, vertices = ver, directed = FALSE)
plot(g_undir)

# ............................
# Creating Directed NW ======
#
g_dir <- graph_from_data_frame(d = edg, vertices = ver, directed = TRUE)
plot(g_dir)


#- ---- UNDIRECTED
# Find the most strongly connected markets
nodes_with_centrality_U <- ver %>%
  mutate(
    degree = degree(g_undir),
    # Add a column containing the strength of each node
    strength = strength(g_undir)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))
# See the result
nodes_with_centrality_U
# ----

#- ---- DIRECTED
# Find the most strongly connected markets
nodes_with_centrality_D <- ver %>%
  mutate(
    degree = degree(g_dir),
    # Add a column containing the strength of each node
    strength = strength(g_dir)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))
# See the result
nodes_with_centrality_D
# ----

# the same

# ---- UNDIRECTED
# Calculate the reciprocal of the tie weights
dist_price_U <- 1 / E(g_undir)$mPrice

ties_with_betweenness_U <- edg %>%
  # Add an edge betweenness column weighted by dist_weight
  mutate(betweenness = edge_betweenness(g_undir, weights = dist_price_U))

# Review updated ties
ties_with_betweenness_U
# -------

# ---- DIRECTED
# Calculate the reciprocal of the tie weights
dist_price_D <- 1 / E(g_dir)$mPrice

ties_with_betweenness_D <- edg %>%
  # Add an edge betweenness column weighted by dist_weight
  mutate(betweenness = edge_betweenness(g_dir, weights = dist_price_D))

# Review updated ties
ties_with_betweenness_D
# -------

# Basic network indicators
# Cont of edges and vertices

# Creating undirected NW
g_undir_lst <-c()
for (k in 1:21){
  g_undir_lst[[k]] <- graph_from_data_frame(d = edges_list[[k]], vertices = vertices_list[[k]], directed = FALSE)
}

# Creating directed NW
g_dir_lst <-c()
for (k in 1:21){
  g_dir_lst[[k]] <- graph_from_data_frame(d = edges_list[[k]], vertices = vertices_list[[k]], directed = TRUE)
}

# edges
nEdges_undir <- map(g_undir_lst,gsize)
nEdges_dir <- map(g_dir_lst,gsize)

# vertices
nVertex_undir <- map(g_undir_lst,gorder)
nVertex_dir <- map(g_dir_lst,gorder)

# Density
density_undir <- g_undir_lst %>% map(~ edge_density(., loops = TRUE))
density_dir <- g_dir_lst %>% map(~ edge_density(., loops = TRUE))

plot(unlist(density_undir), col='red', ylim = c(0.05,0.2))
points(unlist(density_dir), col='blue')

# Degree
degree_undir <- g_undir_lst %>% map(~ degree(., v = V(.), mode = 'all',loops = TRUE, normalized = FALSE))
degree_dir <- g_dir_lst %>% map(~ degree(., v = V(.), mode = 'out',loops = TRUE, normalized = FALSE))

Mdegree_undir <- map_dbl(degree_undir,mean)
Mdegree_dir <- map_dbl(degree_dir,mean)

plot(Mdegree_undir, col='red', ylim = c(1,3.5))
points(Mdegree_dir, col='blue')

# Centrality
centrality_undir <- g_undir_lst %>% map_dbl(~ centr_degree(.)$centralization)
centrality_dir <- g_dir_lst %>% map_dbl(~ centr_degree(.)$centralization)

plot(centrality_undir, col='red', ylim = c(.2,.85))
points(centrality_dir, col='blue')


# SOME PLOTS

plot(g_dir_lst[[1]], 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     vertex.size = 5,
     edge.arrow.size =.3,
     layout = layout_nicely(g_undir_lst[[1]]))

par(mfrow=c(1,3)) 
plot(g_undir_lst[[1]])
plot(g_undir_lst[[10]])
plot(g_undir_lst[[21]])



# Community detection 
# 1998
comm_undir <-  cluster_walktrap(g_undir_lst[[1]])  
# Determine sizes of each community
sizes(comm_undir)
# Determine which individuals belong to which community
membership(comm_undir)
# Plot the community structure of the network
plot(comm_undir, g_undir_lst[[1]])

# Community detection 
# 2008
comm_undir <-  cluster_walktrap(g_undir_lst[[10]])  
# Determine sizes of each community
sizes(comm_undir)
# Determine which individuals belong to which community
membership(comm_undir)
# Plot the community structure of the network
plot(comm_undir, g_undir_lst[[10]])

# Community detection 
# 2018
comm_undir <-  cluster_walktrap(g_undir_lst[[21]])  
# Determine sizes of each community
sizes(comm_undir)
# Determine which individuals belong to which community
membership(comm_undir)
# Plot the community structure of the network
plot(comm_undir, g_undir_lst[[21]])

# setting some examples of weights
E(g_undir_lst[[1]])$weight <- edges_list[[1]]$mPrice
E(g_undir_lst[[1]])$weight <- edges_list[[1]]$distance
E(g_undir_lst[[1]])$weight <- edges_list[[1]]$mPrice/edges_list[[1]]$distance

# CLustering Coeff
transitivity(g_undir_lst[[1]], type = "weighted")
transitivity(g_undir_lst[[1]], type = "local")
transitivity(g_undir_lst[[1]], type = "global")


# Authority scores... different of HITS alg
authority_score(g_undir_lst[[1]], scale = TRUE, weights = E(g_undir_lst[[1]])$weight,
                options = arpack_defaults)

# create an adjacency matrix from a Network object
as_adjacency_matrix(g_undir_lst[[1]], edges = T)

# To comput HITS algorith to authorities and hubs
library(networkR)
hits(as_adjacency_matrix(g_undir_lst[[1]], edges = T))
