setwd("C:/Users/Ariel/Dropbox/UFL/Spatial Networks/Project/NetworkProduces")

# folderData <- "C:/Users/Ariel/Dropbox/UFL/Spatial Networks/Project/NetworkProduces"
# paste0(folderData,"/","PricesMexico_lst_AVOCADO-extra.RData")
library(pacman)
p_load(readxl,dplyr,causaleffect,igraph,purrr)

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


# .............................
# forming the data
#
# AVOCADO =========
# 
Avocado <- read_excel("Avocado_data.xlsx")
Avocado <- Avocado[,-1]

# preparing vertices data
avo_vertex <- Avocado %>% filter(year=='1998') %>% select(Origen,Destino)

avo_vertex <- unique(rbind(as.matrix(avo_vertex[,1]),as.matrix(avo_vertex[,2]))) %>%
                data.frame()
names(avo_vertex) <- "States"
avo_vertex <- inner_join(avo_vertex,coor,by = "States")

# adding attributes to vertices
avo_vertex2 <- avo_vertex %>% inner_join(gdp[,c('States','y1998')],by = "States")

# preparing edges data
# This is a list because there are data for 21 years: 1998:2018
avo_edges <- list()
for(i in 1:21){
avo_edges[[i]] <- Avocado[Avocado$year==1997+i,c("Origen","Destino","mPrice")]
}

# adding distances FUNCTION 
FindDistance <- function(origin,destiny){
  d <- MatrixD[as.matrix(origin),as.matrix(destiny)]
  return(d)
}

# FindDistance("Sonora","Aguascalientes")

# This procedure compute the distances for each state for each year
for(i in 1:21){
  dis <- c()
  avo_edges_lst <- avo_edges[[i]]
for(j in 1:nrow(avo_edges_lst)){
  dis[j] <- FindDistance(avo_edges_lst[j,1],avo_edges_lst[j,2])
}
  avo_edges[[i]]$distance <- dis
}

# avo_edges[[3]]


# ............................
# Creating Undirected NW ======
#
g_avo <- graph_from_data_frame(d = avo_edges[[1]], vertices = avo_vertex, directed = FALSE)
plot(g_avo)
g_avo$name <- "Avocado network"


V(g_avo) # to see vertices
E(g_avo) # to see edges
gsize(g_avo) # count of edges
gorder(g_avo) # count of vertices


# to see the atributes
vertex_attr(g_avo)
# to see the attributes for selected vertices
V(g_avo)[[1:10]]
# to see the attributes of edges
edge_attr(g_avo)

# to find the atributes of specific vertex
E(g_avo)[[inc('Oaxaca')]]  
# or specific values for and edge
E(g_avo)[[mPrice>=5]] 
E(g_avo)[[distance>=200]] 


# create weight
w1 <- E(g_avo)$mPrice/1

# Plot the network varying edges by weights
m1 <- layout_nicely(g_avo)
plot(g_avo, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w1,
     layout = m1)

# ............................
# Directed Network ==========
#
g <- graph_from_data_frame(d = avo_edges[[1]], vertices = avo_vertex, directed = T)

is.directed(g)

is.weighted(g)

w2 <- E(g)$mPrice/1


# basic plot
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     vertex.size = .8,
     edge.arrow.size =0.5,
     layout = layout_nicely(g))



library(ggplot2)
library(ggraph)

# Visualize the network with the Kamada-Kawai layout 
ggraph(g_avo, layout = "with_kk") + 
  # Add an edge link geometry mapping transparency to weight 
  geom_edge_link(aes(alpha = mPrice)) + 
  # Add a node point geometry
  geom_node_point()


# Visualize the network in a circular layout
ggraph(g_avo, layout = "in_circle") + 
  # Map tie transparency to its prices
  geom_edge_link(aes(alpha = mPrice*10)) + 
  geom_node_point()


#- ----
# Find the most strongly connected markets
nodes_with_centrality <- avo_vertex %>%
  mutate(
    degree = degree(g_avo),
    # Add a column containing the strength of each node
    strength = strength(g_avo)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))

# See the result
nodes_with_centrality
# ----
# Calculate the reciprocal of the tie weights
dist_price <- 1 / E(g_avo)$mPrice

ties_with_betweenness <- avo_edges[[1]] %>%
  # Add an edge betweenness column weighted by dist_weight
  mutate(betweenness = edge_betweenness(g_avo, weights = dist_price))

# Review updated ties
ties_with_betweenness
# -------



# Is there an edge going from vertex 184 to vertex 178?
# -> 
g['Oaxaca', 'Baja California']
g['Jalisco', 'Colima']
g['Distrito Federal', 'Quintana Roo']

# Show all edges going to or from vertex Oaxaca
incident(g, 'Oaxaca', mode = c("all"))
# Show all edges going out/in from vertex Oaxaca
incident(g, 'Oaxaca', mode = c("out"))
incident(g, 'Oaxaca', mode = c("in"))

# Identify all neighbors of vertex 12 regardless of direction
neighbors(g, 'Oaxaca', mode = c('all'))

# Identify other vertices that direct edges towards vertex 12
neighbors(g, 'Oaxaca', mode = c('in'))


n1 <- neighbors(g, 'Michoacan', mode = c('out'))
n2 <- neighbors(g, 'Oaxaca', mode = c('in'))
intersection(n1, n2)


# Which two vertices are the furthest apart in the graph ?
farthest_vertices(g) 

# Shows the path sequence between two furthest apart vertices.
get_diameter(g)  

# Identify vertices that are reachable within two connections from vertex 42
ego(g, 2, 'Oaxaca', mode = c('out'))

# Identify vertices that can reach vertex 42 within two connections
ego(g, 2, 'Oaxaca', mode = c('in'))


# Calculate the out-degree of each vertex
g.outd <- degree(g, mode = c("out"))
# View a summary of out-degree
table(g.outd)
# Make a histogram of out-degrees
hist(g.outd, breaks = 30)
# Find the vertex that has the maximum out-degree
which.max(g.outd)

# Calculate betweenness of each vertex
g.b <- betweenness(g, directed = T)
# Show histogram of vertex betweenness
hist(g.b, breaks = 80)

# Create plot with vertex size determined by betweenness score
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.1,
     layout = layout_nicely(g))


# Visualizing important nodes and edges

# One issue with the measles dataset is that there are three individuals
# for whom no information is known about who infected them. One of these 
# individuals (vertex 184) appears ultimately responsible for spreading 
# the disease to many other individuals even though they did not directly 
# infect too many individuals. However, because vertex 184 has no incoming 
# edge in the network they appear to have low betweenness. One way to 
# explore the importance of this vertex is by visualizing the geodesic 
# distances of connections going out from this individual. In this exercise
# you shall create a plot of these distances from this patient zero.

# Make an ego graph
g184 <- make_ego_graph(g, diameter(g), nodes = 'Michoacan', mode = c("all"))[[1]]
# Get a vector of geodesic distances of all vertices from vertex 184 
dists <- distances(g184, "Michoacan")
# Create a color palette of length equal to the maximal geodesic distance plus one.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")
# Set color attribute to vertices of network g184.
V(g184)$color <- colors[dists+1]

# Visualize the network based on geodesic distance from vertex 184 (patient zero).
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05,
     main = "Geodesic Distances from Patient Zero"
)


# plot(g,
#      vertex.label.color = "black", 
#      vertex.label.cex = 0.6,
#      vertex.size = 25*(g.ec$vector),
#      edge.color = 'gray88',
#      main = "Forrest Gump Network"
# )



# Get density of a graph
gd <- edge_density(g)
# Get the diameter of the graph g
diameter(g, directed = FALSE)

# Get the average path length of the graph g
g.apl <-mean_distance(g, directed = FALSE)
g.apl


# Show all triangles in the network.
matrix(triangles(g), nrow = 3)

count_triangles(g, vids='Michoacan')

# Calculate  the global transitivity of the network.
g.tr <- transitivity(g)
g.tr

# Calculate the local transitivity for vertex Michoacan
transitivity(g, vids='Michoacan', type = "local")

# Identify the largest cliques in the network
largest_cliques(g)
# Determine all maximal cliques in the network and assign to object 'clq'
clq <- max_cliques(g)
# Calculate the size of each maximal clique.
table(unlist(lapply(clq, length)))
# Assign largest cliques output to object 'lc'
lc <- largest_cliques(g)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(subgraph(g, lc[[1]]))
gs2 <- as.undirected(subgraph(g, lc[[2]]))

# Plot the two largest cliques side-by-side

par(mfrow=c(1,2)) # To plot two plots side-by-side

plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 2",
     layout = layout.circle(gs2)
)


# Calculate the assortativity of the network based on gender
assortativity(g_avo, mPrices) ??
  # Perform fast-greedy community detection on network graph
kc =  fastgreedy.community(g_avo)  
    
wtc <- cluster_walktrap(g_avo)
# Determine sizes of each community
sizes(wtc)
# Determine which individuals belong to which community
membership(wtc)

# Plot the community structure of the network
plot(wtc, g_avo)

# modularity(g_avo)


# Perform edge-betweenness community detection on network graph
gc = edge.betweenness.community(g_avo)

# Determine sizes of each community
sizes(gc)
# Plot community networks determined by cluster_walktrap and edge-betweenness methods side-by-side
par(mfrow = c(1, 2)) 
plot(wtc, g_avo)
plot(gc, g_avo)
