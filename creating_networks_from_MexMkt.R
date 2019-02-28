data1 <- tables_networks[[1]]

library(multiplex)
library(igraph)
library(xlsx)


bind_rows(sapply(data1[,1:2], unique))

States <- data.frame(unique(c(
                  data1[!duplicated(data1[,c('Destino')]),][,1],
                  data1[!duplicated(data1[,c('Origen')]),][,2])))

Market <- data.frame(
  from = data1$Origen,
  to = data1$Destino,
  price = data1$Precio
)

g <- graph_from_data_frame(Market, directed=TRUE, vertices=States)
print(g, e=TRUE, v=TRUE)


write_graph(g, "netMex.gml", "gml") 

write.xlsx(data1, "netMex.xlsx") 
