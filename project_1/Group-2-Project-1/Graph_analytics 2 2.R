############################# packages installation#############################
install.packages("igraph")
install.packages("dplyr")



############################# task 1 ###########################################
# Determine how to create a graph and a plot, label edges with one of the four
#labels mentioned in the data set.

library(igraph)
data <- read.table(
  '/Users/josegarcia/Desktop/GW/FALL2021/BIG DATA/PROJ_1_GRAPH/PROJECT_1.txt', 
  header=F
  )
net <- graph_from_data_frame(data, directed= TRUE)
#edge.attributes(net)$weight <- data[,c('V3')]
plot.igraph(net, edge.arrow.size=0.1)
hist(degree(net))

##################simplifying the graph############

#My approach:
#Certain redundant relationships in the data make the graph difficult to see. 
#For instance, A -customer-B, is the same relation as B-provider-A. 
#Therefore, we simplified the data set filtering the relationship value that 
#equals 1: <FromNodeId> is a provider of <ToNodeId>. 

library(dplyr)
providers <- filter(data,V3==1)
providers_graph <- graph_from_data_frame(providers, directed = T) 
plot(providers_graph,edge.arrow.size=0.1) #Still unable to visualize
hist(degree(providers_graph)) #Seems most of the nodes have degrees <500
filter_graph <- delete.vertices(providers_graph, #deleted vertices with edges
                                V(providers_graph)[degree(providers_graph) < 100])
plot(filter_graph)

hist(degree(filter_graph))


########### suggestion  ################
# plot a selection of 50 nodes



# suggestion 2: random nodes

random_nodes <- c(sample(vcount(net), 10))
degree(net,v=V(net)[random_nodes])
plot(net, v=V(net)[random_nodes])


############################ FUNCTIONS ####################
#vertices of the graph:
V(net) # 26475 vertices in the graph. 
E(net) # 106762 edges in the graph.
net.adj <- get.adjacency(net) #adjacency matrix
net.adj
# density of a graph:
net.density <- edge_density(net)
net.density ## 0.0001523215

# degree of nodes in a graph:
hist(degree(net))

# centrality metrics:
centr_betw(net) #Between Centrality
centr_clo(net) # Closeness Centrality 


#### PLOTS LAYOUT ############3

V(net_1)$size <- 15
V(net_1)$color <- "red"
V(net_1)$label <- "" 
plot(net_1)


#SHORTEST PATH FUNCTION:
paths <- get.shortest.paths(net, 50, 200, 
                            output = "both")

paths

#A DIFFERENT APROXIMATION WOULD BE TO FIND ORGANIZATIONS. RELATIONSHIP == 2 
# MEANS THE SAME ORGANIZATION

organizations <- filter(data,V3==2)
organizations_graph <- graph_from_data_frame(organizations, directed = T)
plot(organizations_graph)
head(organizations)

get.shortest.paths(net)
?get.shortest.paths

## some resources:
# https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html
# https://rpubs.com/pjmurphy/igraphCentrality
# 
