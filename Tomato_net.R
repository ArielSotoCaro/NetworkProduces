# run this afger ManagingNetworks2.R

rm(vertices_list,vertices_list_w)

DT <- Tomato

vertices_list <- c()
vertices_list_w <- c()

# from 1998 to 2018 (21 years)
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

# Creating directed NW
g_dir_lst <-c()
for (k in 1:21){
  g_dir_lst[[k]] <- graph_from_data_frame(d = edges_list[[k]], vertices = vertices_list[[k]], directed = TRUE)
}

# Creating weighted networks
g_dir_lst_W <- g_dir_lst

# setting some examples of weights
for(w in 1:21){
  # We have to choose one out of 3 weighting options

  # DIR .......................
  E(g_dir_lst_W[[w]])$weight <- edges_list[[w]]$mPrice
  # E(g_dir_lst_W[[w]])$weight <- edges_list[[w]]$distance
  # E(g_dir_lst_W[[w]])$weight <- edges_list[[w]] %>%
  # mutate(pond = ifelse(distance == 0,1,mPrice/distance)) %>%
  # select(pond) %>% unlist()
}

is.weighted(g_dir_lst_W[[21]])

# DIRECTED
par(mfrow=c(1,3))
plot.igraph(g_dir_lst_W[[1]],vertex.label=V(g_dir_lst_W[[1]])$name,
            layout=layout.fruchterman.reingold,
            edge.color="black",
            edge.width=E(g_dir_lst_W[[1]])$weight,
            edge.arrow.size =.3,
            main="Tomato: 1998")

plot.igraph(g_dir_lst_W[[10]],vertex.label=V(g_dir_lst_W[[10]])$name,
            layout=layout.fruchterman.reingold,
            edge.color="black",
            edge.width=E(g_dir_lst_W[[10]])$weight*.3,
            edge.arrow.size =.2,
            main="Tomato: 2008")

plot.igraph(g_dir_lst_W[[21]],vertex.label=V(g_dir_lst_W[[21]])$name,
            layout=layout.fruchterman.reingold,
            edge.color="black",
            edge.width=E(g_dir_lst_W[[21]])$weight*.2,
            edge.arrow.size =.2,
            main="Tomato: 2018")
dev.off()


# edges
print(nEdges_undir <- map_dbl(g_undir_lst_W,gsize))
print(nEdges_dir <- map_dbl(g_dir_lst_W,gsize))

# vertices
print(nVertex_undir <- map_dbl(g_undir_lst_W,gorder))
print(nVertex_dir <- map_dbl(g_dir_lst_W,gorder))

# Density
density_undir <- g_undir_lst_W %>% map_dbl(~ edge_density(., loops = TRUE))
density_dir <- g_dir_lst_W %>% map_dbl(~ edge_density(., loops = TRUE))

plot(unlist(density_undir_W), col='red', ylim = c(0.05,0.2))
points(unlist(density_dir_W), col='blue')




# SOME MEASURES COMPUTATIONS
# ..........................
# Degree
degree_dir <- g_dir_lst %>% map(~ degree(., v = V(.), mode = 'out',loops = TRUE, normalized = FALSE))

Mdegree_dir <- map_dbl(degree_dir,mean)

plot(Mdegree_dir, col='blue')

# Centrality UNW
centrality_dir <- g_dir_lst %>% map_dbl(~ centr_degree(.)$centralization)

plot(centrality_dir, col='blue')

plot(comm_undir, g_undir_lst_W[[1]])


# Strength or density for Weighted matrices
Strength_dir <- g_dir_lst_W %>% map(~ strength(., vids = V(.), mode = "out",
                                               loops = TRUE, weights = E(.)$weight))
hist(Strength_dir[[21]])



# Community detection 
# For 1998
comm_dir <-  cluster_walktrap(g_dir_lst_W[[1]])  
# For all years (list)
comm_dir <-  g_dir_lst_W %>% map(~ cluster_walktrap(.))

comm_dir[[1]]$modularity # modularity
max(comm_dir[[1]]$membership) # Number of communities


# Determine sizes of each community
sizes(comm_dir)
# Determine which individuals belong to which community
membership(comm_dir)
# Plot the community structure of the network
groups_prices <- data.frame(Destino=comm_dir$names,
                            Membership=comm_dir$membership) %>% 
  inner_join(as.data.frame(edges_list[[1]]),by="Destino") %>%
  group_by(Membership) %>% summarise(meanPrice = mean(mPrice))

groups_prices
print(comm_undir$names[comm_undir$membership==1])
print(comm_undir$names[comm_undir$membership==2])
print(comm_undir$names[comm_undir$membership==3])

# Plot the community structure of the network
plot(comm_dir, g_dir_lst_W[[1]],
     edge.arrow.size =.2)

# DISTANCE
# Mean distance
Mean_dist_w <- g_dir_lst_W %>% map_dbl( ~ mean_distance(., directed = TRUE, unconnected = TRUE))


# CLustering Coeff
transitivity(g_dir_lst_W[[1]], type = "weighted")

# Authority scores... different of HITS alg

auth_score <- g_dir_lst_W %>% map ( ~ authority_score(., scale = TRUE, 
                                                      weights = E(.)$weight,
                                                      options = arpack_defaults))
auth_score[[1]]$vector
auth_score[[1]]$value
# vector: The authority/hub scores of the vertices.
# value: The corresponding eigenvalue of the calculated principal eigenvector.


