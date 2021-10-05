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

#Map node names
name.map <- c(
  "-1" = "customer",
  "1" = "provider",
  "0" = "peers",
  "2" = "siblings"
)
edge_name <- c()
for (i in data$V3){
  edge_name <- append(edge_name, name.map[toString(i)])
}

#edge.attributes(net)$weight <- data[,c('V3')]
plot.igraph(net, edge.arrow.size=0.1, edge.label = edge_name)
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


########### suggestions to simplify graph ################
# plot a selection of 50 nodes
# 
slice <- data[50:150,]
simplyfied <- graph_from_data_frame(slice, directed= T)
edge_name <- c()
for (i in slice$V3){
  edge_name <- append(edge_name, name.map[toString(i)])
}

plot(simplyfied, edge.label = edge_name)


# select random nodes

smpl_nodes <- sample(1:vcount(net), 300)
sub.g <- induced_subgraph(net, V(net)[smpl_nodes], impl = 'create_from_scratch')
degree(net,v=V(net)[smpl_nodes])
plot(sub.g)

sub.g
V(net)[random_nodes]

############################ FUNCTIONS ####################
#vertices of the graph:
V(simplyfied) 
E(simplyfied) 
smpl.adj <- get.adjacency(simplyfied) #adjacency matrix
smpl.adj
# density of a graph:
smpl.density <- edge_density(simplyfied)
smpl.density ## 0.0001523215

# degree of nodes in a graph:
hist(degree(simplyfied))

# centrality metrics:
centr_betw(simplyfied) #Between Centrality

# connected components of a graph:
clu <- components(simplyfied)
clu

# dominator tree:
dominator_tree(simplyfied, 3, mode = c("out"))


#SHORTEST PATH FUNCTION:
paths <- get.shortest.paths(simplyfied, V(simplyfied)[1, 3], 
                            output = "both")

# igraph-are-connected: Decides whether two vertices are connected

are.connected(net,"35621","16442")

neighborhood_graph <- neighborhood(simplyfied, c(sample(vcount(simplyfied),1)))
plot(net, V(net)[c(neighborhood_graph)])

# Get isolates: nodes with degree 0:

isolates <- vertex_attr(net, c(V(net)[degree(net)==0]))
isolates
plot(net,V(net[isolates]))

isolates

# Cliques:
igraph_options(arrow.size=0.1, arrow.width=0.1)
a <- largest_cliques(net)
clique1 <- a[[1]]

cliques_graph <- induced.subgraph(net, vids=clique1)

plot(cliques_graph)

#tkplot:
tkplot(simplyfied)

# Minimum spanning tree
MST <- minimum.spanning.tree(simplyfied)
plot(MST, vertex.shapes='none', vertex.label.cex=.7)

# Vertex connectivity 
vertex_connectivity(simplyfied, "15011", "21573", checks = TRUE)



## some resources:
# https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html
# https://rpubs.com/pjmurphy/igraphCentrality
# 
