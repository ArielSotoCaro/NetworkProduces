# Weigthed netwokr analysis
# Requiere data from ManagingNetwork2.R

# https://toreopsahl.com/tnet/weighted-networks/node-centrality/


library(tnet)

r <- 1
m =
data.matrix(data.frame(x=edges_list[[r]]$Origen,
           y=edges_list[[r]]$Destino,
           w=edges_list[[r]]$mPrice)
)

colnames(m) <- NULL
# m <- as.data.frame(m)

library(lubridate)

y <- ymd_hms(paste(lm$year, "01", "01", sep="-", " ",01,":",00,":","00"),quiet=T)

lm <- as.data.frame(DT[,c("year","Origen","Destino","mPrice")])
lm$year <-  ymd_hms(paste(lm$year, "01", "01", sep="-", " ",01,":",00,":","00"),quiet=T)


lm2 <- data.matrix(data.frame(year=lm$year,origin=lm$Origen,destiny=lm$Destino,price=lm$mPrice))
lm2 <- as.data.frame(lm2)
lm2[,1] <- lm$year

directed.net <- as.tnet(m, type="weighted one-mode tnet")
directed.net2 <- as.tnet(m, type="weighted two-mode tnet")
directed.netL <- as.tnet(lm2, type="longitudinal tnet")

# directed.net <- as.tnet(m, type="weighted two-mode tnet")


# sample <- rbind(c(1,2,4),c(1,3,2),c(2,1,4),c(2,3,4),c(2,4,1),c(2,5,2),c(3,1,2),c(3,2,4),c(4,2,1),c(5,2,2),c(5,6,1),c(6,5,1))
## Run the programme
# as.tnet(sample)

growth_l(directed.netL, perspective="actor" ,effects="indegree", nstrata=2)
clustering_w(directed.netL)
# Convert to tnet (and avoiding warning due to it being undirected)
net <- cbind(get.edgelist(g_dir_lst_W[[1]], names=FALSE), E(g_dir_lst_W[[1]])$weight)
net <- rbind(net, cbind(net[,c(2,1,3)]))

# You can also do the following instead of line 11
net <- suppressWarnings(symmetrise_w(net))

# Compute weighted betweeness
out <- betweenness_w(directed.net)

g <- igraph::erdos.renyi.game(n=10, p.or.m=0.25, type="gnp", directed=FALSE)
E(g)$weight <- sample.int(n=4, size=ecount(g), replace=TRUE)
net <- cbind(get.edgelist(g, names=FALSE), E(g)$weight)
net <- rbind(net, cbind(net[,c(2,1,3)]))



# Calculate the out-degree of neurons and the generalised measures (alpha=0.5)
degree_w(net=directed.net, measure=c("degree","output","alpha"), alpha=0.5)

# Calculate the in-degree of neurons and the generalised measures (alpha=0.5)
degree_w(net=directed.net, measure=c("degree","output","alpha"), alpha=0.5, type="in")

# Calculate the binary closeness scores
closeness_w(net=directed.net, alpha=0)

# Calculate the first generation weighted closeness scores
closeness_w(net=directed.net, alpha=1)

# Calculate the second generation weighted closeness scores (alpha=0.5)
closeness_w(net=directed.net, alpha=0.5)

# Calculate the binary betweenness measure
betweenness_w(directed.net, alpha=0)

# Calculate the first generation weighted betweenness measure
betweenness_w(directed.net, alpha=1)

# Calculate the first generation weighted betweenness measure
betweenness_w(m, alpha=0.5)


weighted_richclub_tm(directed.net2)
