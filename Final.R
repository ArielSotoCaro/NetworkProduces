setwd("C:/Users/Ariel/Dropbox/UFL/Spatial Networks/Project/NetworkProduces")

library(pacman)
p_load(readxl,dplyr,igraph,purrr,stargazer,data.table)

# To work into R is requiere 2 types of dataframe.
# One for vertices w/attributes and other with the edges w/attributes


#.....................................
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
#.....................................


#.....................................
# Importing GDP percapita data
gdp <- read_xlsx("GDP-pc-MEX.xlsx")
#.....................................


#.....................................
# Importing International Prices
# actualy, this data also have quantities
# 1990:2018
prices <- read_xlsx("Prices&Quantities.xlsx") 
Iprice_Avocado <- ts(prices$avocado_p, start=c(1990,1), end = c(2018,1),frequency = 1)
Iprice_Avocado <- window(Iprice_Avocado,start=c(1998), end=c(2018))

Iprice_Tomato <- ts(prices$tomatoN_p, start=c(1990,1), end = c(2018,1),frequency = 1)
Iprice_Tomato <- window(Iprice_Tomato,start=c(1998), end=c(2018))

Iprice_Corn <- ts(prices$cornNSeed_p, start=c(1990,1), end = c(2018,1),frequency = 1)
Iprice_Corn <- window(Iprice_Corn,start=c(1998), end=c(2018))

# adding distances FUNCTION 
FindDistance <- function(origin,destiny){
  d <- MatrixD[as.matrix(origin),as.matrix(destiny)]
  return(d)
}
#.....................................

# .............................
# Loading the data by crop
#
# AVOCADO =========
# 
Avocado <- read_excel("Avocado_data.xlsx")
Avocado <- Avocado[,-1]
# CORN =========
# 
Corn <- read_excel("Corn_data.xlsx")
Corn <- Corn[,-1]
# TOMATO =========
# 
Tomato <- read_excel("Tomato_data.xlsx")
Tomato <- Tomato[,-1]
# ............................

#.....................................
# Creating a list of vertices by crop
Tomato_vertices_list <- c()
Tomato_vertices_list_w <- c()

Avocado_vertices_list <- c()
Avocado_vertices_list_w <- c()

Corn_vertices_list <- c()
Corn_vertices_list_w <- c()

# from 1998 to 2018 (21 years)
for(y in 1:21){
  
  # preparing vertices data
  Tomato_vertex <- Tomato %>% filter(year==1997+y) %>% select(Origen,Destino)
  Avocado_vertex <- Avocado %>% filter(year==1997+y) %>% select(Origen,Destino)
  Corn_vertex <- Corn %>% filter(year==1997+y) %>% select(Origen,Destino)
  # TOMATO
  Tomato_vertex <- unique(rbind(as.matrix(Tomato_vertex[,1]),
                                as.matrix(Tomato_vertex[,2]))) %>%
    data.frame()
  names(Tomato_vertex) <- "States"
  Tomato_vertex <- inner_join(Tomato_vertex,coor,by = "States")
  # AVOCADO
  Avocado_vertex <- unique(rbind(as.matrix(Avocado_vertex[,1]),
                                 as.matrix(Avocado_vertex[,2]))) %>%
    data.frame()
  names(Avocado_vertex) <- "States"
  Avocado_vertex <- inner_join(Avocado_vertex,coor,by = "States")
  # CORN
  Corn_vertex <- unique(rbind(as.matrix(Corn_vertex[,1]),
                              as.matrix(Corn_vertex[,2]))) %>%
    data.frame()
  names(Corn_vertex) <- "States"
  Corn_vertex <- inner_join(Corn_vertex,coor,by = "States")
  
  # adding attributes to vertices
  if(y<=17){
    Tomato_vertexW <- Tomato_vertex %>% inner_join(gdp[,c(1,1+y)],by = "States")
  } else {Tomato_vertexW <-0}
  if(y<=17){
    Avocado_vertexW <- Avocado_vertex %>% inner_join(gdp[,c(1,1+y)],by = "States")
  } else {Avocado_vertexW <-0}
  if(y<=17){
    Corn_vertexW <- Corn_vertex %>% inner_join(gdp[,c(1,1+y)],by = "States")
  } else {Corn_vertexW <-0}
  
  # Saving data into the LIST
  Tomato_vertices_list[[y]]   <- Tomato_vertex
  Tomato_vertices_list_w[[y]] <- Tomato_vertexW
  
  Avocado_vertices_list[[y]]   <- Avocado_vertex
  Avocado_vertices_list_w[[y]] <- Avocado_vertexW
  
  Corn_vertices_list[[y]]   <- Corn_vertex
  Corn_vertices_list_w[[y]] <- Corn_vertexW
}
#.....................................

#.....................................
# preparing edges data
# This is a list because there are data for 21 years: 1998:2018
Tomato_edges_list <- list()
Avocado_edges_list <- list()
Corn_edges_list <- list()

for(i in 1:21){
  Tomato_edges_list[[i]] <- Tomato[Tomato$year==1997+i,c("Origen","Destino","mPrice")]
  Avocado_edges_list[[i]] <- Avocado[Avocado$year==1997+i,c("Origen","Destino","mPrice")]
  Corn_edges_list[[i]] <- Corn[Corn$year==1997+i,c("Origen","Destino","mPrice")]
}
#.....................................
#.....................................


#.....................................
#.....................................
# This procedure compute the distances for each state for each year
for(produce in 1:3){
  if(produce==1){edges_list<-Tomato_edges_list}
  else if(produce==2){edges_list<-Avocado_edges_list}
  else{edges_list<-Corn_edges_list}
      for(i in 1:21){
      dis <- c()
      edges_lst <- edges_list[[i]]
          for(j in 1:nrow(edges_lst)){
            dis[j] <- FindDistance(edges_lst[j,1],edges_lst[j,2])
          }
      edges_list[[i]]$distance <- dis
      }
  if(produce==1){Tomato_edges_list<-edges_list}
  else if(produce==2){Avocado_edges_list<-edges_list}
  else{Corn_edges_list<-edges_list}
}
#.....................................
#.....................................


#.....................................
#.....................................
# Creating directed NW
g_dir_Tomato_lst  <-c()
g_dir_Avocado_lst <-c()
g_dir_Corn_lst    <-c()

for (k in 1:21){
  g_dir_Tomato_lst[[k]] <- graph_from_data_frame(d = Tomato_edges_list[[k]],
                                                 vertices = Tomato_vertices_list[[k]],
                                                 directed = TRUE)
  g_dir_Avocado_lst[[k]] <- graph_from_data_frame(d = Avocado_edges_list[[k]],
                                                  vertices = Avocado_vertices_list[[k]],
                                                  directed = TRUE)
  g_dir_Corn_lst[[k]] <- graph_from_data_frame(d = Corn_edges_list[[k]],
                                               vertices = Corn_vertices_list[[k]],
                                               directed = TRUE)
}
#.....................................
#.....................................


#.....................................
#.....................................
# Creating weighted networks
g_dir_Tomato_lst_W  <- g_dir_Tomato_lst
g_dir_Avocado_lst_W <- g_dir_Avocado_lst
g_dir_Corn_lst_W    <- g_dir_Corn_lst

# setting some examples of weights
for(w in 1:21){
  # We have to choose one out of 3 weighting options
  
  # DIR .......................
  # E(g_dir_lst_W[[w]])$weight <- edges_list[[w]]$mPrice
  # E(g_dir_lst_W[[w]])$weight <- edges_list[[w]]$distance
  # E(g_dir_lst_W[[w]])$weight <- edges_list[[w]] %>%
  # mutate(pond = ifelse(distance == 0,1,mPrice/distance)) %>%
  # select(pond) %>% unlist()
  
  E(g_dir_Tomato_lst_W[[w]])$weight   <- Tomato_edges_list[[w]]$mPrice
  E(g_dir_Avocado_lst_W[[w]])$weight  <- Avocado_edges_list[[w]]$mPrice
  E(g_dir_Corn_lst_W[[w]])$weight     <- Corn_edges_list[[w]]$mPrice
}

# is.weighted(g_dir_Tomato_lst_W[[21]])
#.....................................
#.....................................


# PLOTS =============================
# ...................................
# ...................................
# ...................................
# TOMATO
par(mfrow=c(1,1),mar=c(0,0,0,0))
# png('tomato_1998.png', 600,600)
pdf('tomato_1998.pdf')

plot(g_dir_Tomato_lst_W[[1]],vertex.label=V(g_dir_Tomato_lst_W[[1]])$name,
     layout=layout_nicely(g_dir_Tomato_lst_W[[1]]),
     edge.width=E(g_dir_Tomato_lst_W[[1]])$weight/4,
     edge.arrow.size =.4,
     edge.color="gray35",
     vertex.size = degree_Tomato_dir[[1]]*1.1,
     vertex.label.color='black',
     vertex.label.cex=.7,
     vertex.label.font=3,
     vertex.color="dodgerblue4",
     main="Tomato: 1998")
dev.off()

pdf('tomato_2008.pdf')
plot(g_dir_Tomato_lst_W[[10]],vertex.label=V(g_dir_Tomato_lst_W[[10]])$name,
     edge.width=E(g_dir_Tomato_lst_W[[10]])$weight/5,
     edge.arrow.size =.5,
     edge.color="gray35",
     vertex.size = degree_Tomato_dir[[10]]*1.2,
     vertex.label.color='black',
     vertex.label.cex=.7,
     vertex.label.font=3,
     vertex.color="dodgerblue4",
     main="Tomato: 2008")
dev.off()

pdf('tomato_2018.pdf')
plot.igraph(g_dir_Tomato_lst_W[[21]],vertex.label=V(g_dir_Tomato_lst_W[[21]])$name,
            layout=layout_nicely(g_dir_Tomato_lst_W[[21]]),
            edge.width=E(g_dir_Tomato_lst_W[[21]])$weight/5,
            edge.arrow.size =.5,
            edge.color="gray35",
            vertex.size = degree_Tomato_dir[[21]]*1.3,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Tomato: 2018")
dev.off()

# AVOCADO
par(mfrow=c(1,1), mar=c(0,0,0,0))
pdf('avocado_1998.pdf')
plot(g_dir_Avocado_lst_W[[1]],vertex.label=V(g_dir_Avocado_lst_W[[1]])$name,
            layout=layout_nicely(g_dir_Avocado_lst_W[[1]]),
            edge.width=E(g_dir_Avocado_lst_W[[1]])$weight/2,
            edge.arrow.size =.4,
            edge.color="gray35",
            vertex.size = degree_Avocado_dir[[1]]*1,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Avocado: 1998")
dev.off()
pdf('avocado_2008.pdf')
plot(g_dir_Avocado_lst_W[[10]],vertex.label=V(g_dir_Avocado_lst_W[[10]])$name,
            layout=layout_nicely(g_dir_Avocado_lst_W[[10]]),
            edge.width=E(g_dir_Avocado_lst_W[[10]])$weight/4,
            edge.arrow.size =.4,
            edge.color="gray35",
            vertex.size = degree_Avocado_dir[[10]]*1,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Avocado: 2008")
dev.off()
pdf('avocado_2018.pdf')
plot(g_dir_Avocado_lst_W[[21]],vertex.label=V(g_dir_Avocado_lst_W[[21]])$name,
            layout=layout_nicely(g_dir_Avocado_lst_W[[21]]),
            edge.width=E(g_dir_Avocado_lst_W[[21]])$weight/7,
            edge.arrow.size =.4,
            edge.color="gray35",
            vertex.size = degree_Avocado_dir[[21]]*1,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Avocado: 2018")
dev.off()

# CORN
par(mfrow=c(1,1))
pdf('corn_1998.pdf')
plot(g_dir_Corn_lst_W[[1]],vertex.label=V(g_dir_Corn_lst_W[[1]])$name,
            layout=layout_nicely(g_dir_Corn_lst_W[[1]]),
            edge.width=E(g_dir_Corn_lst_W[[1]])$weight/1,
            edge.arrow.size =.4,
            edge.color="gray35",
            vertex.size = degree_Corn_dir[[1]]*1.5,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Corn: 1998")
dev.off()
pdf('corn_2008.pdf')
plot(g_dir_Corn_lst_W[[10]],vertex.label=V(g_dir_Corn_lst_W[[10]])$name,
            layout=layout_nicely(g_dir_Corn_lst_W[[10]]),
            edge.width=E(g_dir_Corn_lst_W[[10]])$weight/1.5,
            edge.arrow.size =.4,
            edge.color="gray35",
            vertex.size = degree_Corn_dir[[10]]*1.5,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Corn: 2008")
dev.off()
pdf('corn_2018.pdf')
plot.igraph(g_dir_Corn_lst_W[[21]],vertex.label=V(g_dir_Corn_lst_W[[21]])$name,
            layout=layout_nicely(g_dir_Corn_lst_W[[21]]),
            edge.width=E(g_dir_Corn_lst_W[[21]])$weight/2,
            edge.arrow.size =.4,
            edge.color="gray35",
            vertex.size = degree_Corn_dir[[21]]*1.5,
            vertex.label.color='black',
            vertex.label.cex=.7,
            vertex.label.font=3,
            vertex.color="dodgerblue4",
            main="Corn: 2018")
dev.off()
# ...................................
# ...................................
# ...................................

# MEASURES ==================

# edges
print(nEdges_Tomato_dir <- map_dbl(g_dir_Tomato_lst_W,gsize))
print(nEdges_Avocado_dir <- map_dbl(g_dir_Avocado_lst_W,gsize))
print(nEdges_Corn_dir <- map_dbl(g_dir_Corn_lst_W,gsize))

table_Edges <- rbind(seq(1998,2018),nEdges_Tomato_dir,nEdges_Avocado_dir,nEdges_Corn_dir)
stargazer(table_Edges,type = "text",flip=TRUE)

# vertices
print(nVertex_Tomato_dir <- map_dbl(g_dir_Tomato_lst_W,gorder))
print(nVertex_Avocado_dir <- map_dbl(g_dir_Avocado_lst_W,gorder))
print(nVertex_Corn_dir <- map_dbl(g_dir_Corn_lst_W,gorder))

table_Vertex <- rbind(seq(1998,2018),nVertex_Tomato_dir,nVertex_Avocado_dir,nVertex_Corn_dir)
stargazer(table_Vertex,type = "text",flip=TRUE)

# Density
density_Tomato_dir <- g_dir_Tomato_lst_W %>% map_dbl(~ edge_density(., loops = TRUE))
density_Avocado_dir <- g_dir_Avocado_lst_W %>% map_dbl(~ edge_density(., loops = TRUE))
density_Corn_dir <- g_dir_Corn_lst_W %>% map_dbl(~ edge_density(., loops = TRUE))

table_Density <- rbind(seq(1998,2018),density_Tomato_dir,density_Avocado_dir,density_Corn_dir)
stargazer(table_Density,type = "text",flip=TRUE)

plot(density_Tomato_dir, col='red', ylim = c(0.01,0.17))
points(unlist(density_Avocado_dir), col='blue')
points(unlist(density_Corn_dir), col='magenta')

# Degree
degree_Tomato_dir <- g_dir_Tomato_lst_W %>% 
        map(~ degree(., v = V(.), mode = 'out',loops = TRUE, normalized = FALSE))
degree_Avocado_dir <- g_dir_Avocado_lst_W %>% 
        map(~ degree(., v = V(.), mode = 'out',loops = TRUE, normalized = FALSE))
degree_Corn_dir <- g_dir_Corn_lst_W %>% 
        map(~ degree(., v = V(.), mode = 'out',loops = TRUE, normalized = FALSE))

Mdegree_Tomato_dir <- map_dbl(degree_Tomato_dir,mean)
Mdegree_Avocado_dir <- map_dbl(degree_Avocado_dir,mean)
Mdegree_Corn_dir <- map_dbl(degree_Corn_dir,mean)

png("degree.png",width = 800,height = 400)
plot(Mdegree_Tomato_dir, col='red', ylim = c(.8,3),pch=15,type = "b",
     main='Total degree per crop', ylab = 'degree', xlab='year',xaxt = "n")
points(Mdegree_Avocado_dir, col='blue',pch=16,type = "b")
points(Mdegree_Corn_dir, col='magenta',pch=17,type = "b")
axis(1, at=1:21, labels=seq(1998,2018))
legend('topright',legend=c("Tomato","Avocado","Corn"),
       col=c("red","blue","magenta"), pch = c(15,16,17))
dev.off()

# Centrality
centrality_Tomato_dir <- g_dir_Tomato_lst_W %>% map_dbl(~ centr_degree(.)$centralization)
centrality_Avocado_dir <- g_dir_Avocado_lst_W %>% map_dbl(~ centr_degree(.)$centralization)
centrality_Corn_dir <- g_dir_Corn_lst_W %>% map_dbl(~ centr_degree(.)$centralization)

png("centrality.png",width = 800,height = 400)
plot(centrality_Tomato_dir, col='red', ylim = c(0,.5),pch=15,type = "b",
     main='Total Centrality per crop', ylab = 'centrality', xlab='year',xaxt = "n")
points(centrality_Avocado_dir, col='blue',pch=16,type = "b")
points(centrality_Corn_dir, col='magenta',pch=17,type = "b")
axis(1, at=1:21, labels=seq(1998,2018))
legend('topright',legend=c("Tomato","Avocado","Corn"),
       col=c("red","blue","magenta"), pch = c(15,16,17))
dev.off()
# plot(comm_undir, g_undir_lst_W[[1]])


# Strength or density for Weighted matrices
Strength_Tomato_dir <- g_dir_Tomato_lst_W %>% 
          map(~ strength(., vids = V(.), mode = "out",
          loops = TRUE, weights = E(.)$weight))
Strength_Avocado_dir <- g_dir_Avocado_lst_W %>%
          map(~ strength(., vids = V(.), mode = "out",
          loops = TRUE, weights = E(.)$weight))
Strength_Corn_dir <- g_dir_Corn_lst_W %>%
          map(~ strength(., vids = V(.), mode = "out",
          loops = TRUE, weights = E(.)$weight))

# There are different size for each matrix
map(Strength_Tomato_dir,length)
#Adding year and organizing
Strength_Tomato_dirL <- lst()
Strength_Avocado_dirL <- lst()
Strength_Corn_dirL <-lst()
for (i in 1:21){
  Strength_Tomato_dirL[[i]] <- as.data.frame(Strength_Tomato_dir[[i]])
  Strength_Tomato_dirL[[i]]$year <- rep(1997+i,length(Strength_Tomato_dirL[[i]]))
  Strength_Tomato_dirL[[i]]$States <- rownames(Strength_Tomato_dirL[[i]])
  
  Strength_Avocado_dirL[[i]] <- as.data.frame(Strength_Avocado_dir[[i]])
  Strength_Avocado_dirL[[i]]$year <- rep(1997+i,length(Strength_Avocado_dirL[[i]]))
  Strength_Avocado_dirL[[i]]$States <- rownames(Strength_Avocado_dirL[[i]])
  
  Strength_Corn_dirL[[i]] <- as.data.frame(Strength_Corn_dir[[i]])
  Strength_Corn_dirL[[i]]$year <- rep(1997+i,length(Strength_Corn_dirL[[i]]))
  Strength_Corn_dirL[[i]]$States <- rownames(Strength_Corn_dirL[[i]])
}

# DATAset of Strenghts
Strength_Tomato_total <- rbindlist(Strength_Tomato_dirL)
names(Strength_Tomato_total)[1] <- "Strengt_tomato"
Strength_Avocado_total <- rbindlist(Strength_Avocado_dirL)
names(Strength_Avocado_total)[1] <- "Strengt_avocado"
Strength_Corn_total <- rbindlist(Strength_Corn_dirL)
names(Strength_Corn_total)[1] <- "Strengt_corn"


# Community detection 
comm_Tomato_dir <-  g_dir_Tomato_lst_W %>% map(~ cluster_walktrap(.))
comm_Avocado_dir <-  g_dir_Avocado_lst_W %>% map(~ cluster_walktrap(.))
comm_Corn_dir <-  g_dir_Corn_lst_W %>% map(~ cluster_walktrap(.))

comm_Tomato_dir[[21]]$modularity # modularity
max(comm_Tomato_dir[[1]]$membership) # Number of communities

commu_tomato <- lst()
commu_avocado <- lst()
commu_corn <- lst()
for(i in 1:21){
  commu_tomato[[i]] <- cbind(comm_Tomato_dir[[i]]$names,
                             comm_Tomato_dir[[i]]$modularity,
                             rep(1997+i,length(comm_Tomato_dir[[i]]$names)))
  commu_avocado[[i]] <- cbind(comm_Avocado_dir[[i]]$names,
                              comm_Avocado_dir[[i]]$modularity,
                             rep(1997+i,length(comm_Avocado_dir[[i]]$names)))
  commu_corn[[i]] <- cbind(comm_Corn_dir[[i]]$names,
                           comm_Corn_dir[[i]]$modularity,
                             rep(1997+i,length(comm_Corn_dir[[i]]$names)))
}



# Determine sizes of each community
sizes(comm_Tomato_dir[[1]])
# Determine which individuals belong to which community
membership(comm_Tomato_dir[[1]])
# Plot the community structure of the network
groups_prices <- data.frame(Destino=comm_Tomato_dir[[1]]$names,
                            Membership=comm_Tomato_dir[[1]]$membership) %>% 
  inner_join(as.data.frame(Tomato_edges_list[[1]]),by="Destino") %>%
  group_by(Membership) %>% summarise(meanPrice = mean(mPrice))

# Plot the community structure of the network
plot(comm_Tomato_dir[[1]], g_dir_Tomato_lst_W[[1]],
     edge.arrow.size =.2)

# Authority scores... different of HITS alg

auth_score_Tomato <- g_dir_Tomato_lst_W %>% map ( ~ authority_score(., scale = TRUE, 
                                                      weights = E(.)$weight,
                                                      options = arpack_defaults))
auth_score_Avocado <- g_dir_Avocado_lst_W %>% map ( ~ authority_score(., scale = TRUE, 
                                                                    weights = E(.)$weight,
                                                                    options = arpack_defaults))
auth_score_Corn <- g_dir_Corn_lst_W %>% map ( ~ authority_score(., scale = TRUE, 
                                                                    weights = E(.)$weight,
                                                                    options = arpack_defaults))

#Adding year and organizing
auth_score_TomatoL <- lst() 
auth_score_AvocadoL <-lst() 
auth_score_CornL <- lst()
for (i in 1:21){
  auth_score_TomatoL[[i]] <- as.data.frame(auth_score_Tomato[[i]]$vector)
  auth_score_TomatoL[[i]]$year <- rep(1997+i,length(auth_score_TomatoL[[i]]))
  auth_score_TomatoL[[i]]$state <- rownames(auth_score_TomatoL[[i]])
  
  auth_score_AvocadoL[[i]] <- as.data.frame(auth_score_Avocado[[i]]$vector)
  auth_score_AvocadoL[[i]]$year <- rep(1997+i,length(auth_score_AvocadoL[[i]]))
  auth_score_AvocadoL[[i]]$state <- rownames(auth_score_AvocadoL[[i]])
  
  auth_score_CornL[[i]] <- as.data.frame(auth_score_Corn[[i]]$vector)
  auth_score_CornL[[i]]$year <- rep(1997+i,length(auth_score_CornL[[i]]))
  auth_score_CornL[[i]]$state <- rownames(auth_score_CornL[[i]])
}

# DATAset of Authorities
auth_score_Tomato_total <- rbindlist(auth_score_TomatoL)
names(auth_score_Tomato_total)[1] <- "Auth_tomato"
auth_score_Avocado_total <- rbindlist(auth_score_AvocadoL)
names(auth_score_Avocado_total)[1] <- "Auth_avocado"
auth_score_Corn_total <- rbindlist(auth_score_CornL)
names(auth_score_Corn_total)[1] <- "Auth_corn"


# as.data.frame(auth_score_Tomato[[1]]$vector)
# auth_score_Tomato[[1]]$value
