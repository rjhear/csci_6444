# PROJECT 1 ---------------------------------------------------------------
# If package not installed, install it
required.packages <- c("igraph", "sna", "doParallel")
new.packages <-
  required.packages[!(required.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

# Load packages
require(igraph)

# Setup computing environment
working.dir <- "~/Documents/ms_cs/csci_6444/csci_6444/project_1"
setwd(working.dir)
doParallel::registerDoParallel()
set.seed(2021)

# Load data as a dataframe
local.path.to.dataset <-
  "/Users/raymondhear/Documents/ms_cs/csci_6444/csci_6444/project_1/as-caida20071105.txt"
caida.as.dataset <- read.table(file = local.path.to.dataset)

# HELPER_FUNCS ------------------------------------------------------------
# Set defaults for igraph's plot function
plot_with_args <- function(g, plot.name) {
  plot.styles <-
    c(
      "vertex.color" =  "#00563f",
      "vertex.shape" = "circle",
      "vertex.label.color" = "white"
    )
  
  minC <- rep(-Inf, vcount(g))
  maxC <- rep(Inf, vcount(g))
  minC[1] <- maxC[1] <- 0
  plot.layout <-
    layout.fruchterman.reingold(
      g,
      niter = 5000,
      minx = minC,
      maxx = maxC,
      miny = minC,
      maxy = maxC
    )
  plot(
    main = plot.name,
    x = g,
    # === Layout
    layout = plot.layout,
    asp = .5,
    margin = -0.001,
    rescale = FALSE,
    xlim = range(plot.layout[, 1]),
    ylim = range(plot.layout[, 2]),
    # === vertex
    vertex.color = plot.styles["vertex.color"],
    vertext.frame.color = plot.styles["vertex.color"],
    vertix.shape = plot.styles["vertex.shape"],
    vertex.size = 8,
    vertex.label.color = plot.styles["vertex.label.color"],
    vertex.label.cex = .5,
    vertex.label.dist = 0,
    # === edge
    edge.label = edge_name,
    edge.label.color = "black",
    edge.color = "grey",
    edge.width = .5,
    edge.arrow.size = .3,
    edge.arrow.width = .2,
    edge.curved = .3
  )
}

# Take a vector and output a vector of the N largest
find_max_n <- function(vec, n) {
  partial <- length(vec) - n + 1
  vec[vec >= sort(vec, partial = partial)[partial]]
}

# PART 2 ------------------------------------------------------------------
# Map edge names
name.map <- c(
  "-1" = "customer",
  "1" = "provider",
  "0" = "peers",
  "2" = "siblings"
)
edge.name <- c()
for (i in caida.as.dataset$V3) {
  edge.name <- append(edge.name, name.map[toString(i)])
}
caida.as.dataset <- cbind(caida.as.dataset, edge.name)

g <-
  igraph::graph_from_data_frame(d = caida.as.dataset, directed = TRUE)

plot_with_args(g, "Part 2 | Labelled Edges")

# PART 3 ------------------------------------------------------------------
#' Simplify the graph
#' After using igraph's provided `Simplify` function, we'll try different ways
#' to simplify the graph
# Remove loops and multiple edges
g.simplified.0 <-
  igraph::simplify(
    graph = g,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", function(x)
      length(x), type = "ignore")
  )

# 1 - Keep nodes whose degrees are above the graph's average degree
average.degree <-
  mean(igraph::degree(graph = g.simplified.0, mode = "total"))
g.simplified.1 <-
  delete.vertices(graph = g.simplified.0,
                  v = degree(g.simplified.0) < average.degree)
plot_with_args(g.simplified.1, "Simplify | Test 1")

# 2 - Keep nodes above average centrality
average.coreness <- mean(coreness(graph = g.simplified.0))
g.simplified.2 <-
  delete.vertices(graph = g.simplified.0,
                  v = coreness(g.simplified.0) < average.coreness)
plot_with_args(g.simplified.2, "Simplify | Test 2")

# 3 - Find maximum betweenness centrality and maximum closeness centrality
# simplified_3_top_
TOP_N <- 15
betweenness.centrality <- igraph::betweenness(g.simplified.0)
top.betweeness <- find_max_n(betweenness.centrality, TOP_N)
closeness.centrality <- igraph::closeness(g.simplified.0)
top.closeness <- find_max_n(closeness.centrality, TOP_N)
g.simplified.3 <-
  igraph::induced_subgraph(graph = g.simplified.0, v = base::union(names(top.betweeness), names(top.closeness)))
plot_with_args(g.simplified.3, "Simplify | Test 3")

part.3.graph <- g.simplified.3
#' Get the vertices of a graph V()
igraph::V(part.3.graph)

#' Get the edges of a graph E()
igraph::E(part.3.graph)

#' Get the adjacency matrix igraph::get.adjacency()
igraph::get.adjacency(part.3.graph)

#' Get betweenness centrality igraph::centr_betw()
igraph::centr_betw(part.3.graph)

#' Get closeness centrality igraph::centr_clo()
igraph::centr_clo(part.3.graph)

#' Get shortest path igraph::shortest.paths()
igraph::shortest_paths(graph = part.3.graph, from = 1)

#' Get density
part.3.adjacency <- get.adjacency(part.3.graph)
sna::gden(as.matrix(part.3.adjacency))

#' Get edge density
igraph::edge_density(part.3.graph)

#' Get geodesic shortest path
sna::geodist(as.matrix(part.3.adjacency))

#' Find Bonacich alpha centrality scores of network positions igraph::alpha_centrality
igraph::alpha_centrality(graph = part.3.graph, alpha = 0.99)

# PART 4 ------------------------------------------------------------------
part.4.graph <- g.simplified.3

#' Shortest (directed or undirected) paths between vertices igraph::all_shortest_paths
igraph::all_shortest_paths(graph = part.4.graph, from = 1)

#' Find reciprocity of graphs
igraph::reciprocity(part.4.graph)

#' Vertex and edge betweenness centrality igraph::betweeness
igraph::betweenness(graph = part.4.graph)

#' Kleinberg's authority centrality scores. igraph::authority_score
igraph::authority_score(graph = part.4.graph)

#' Find Bonacich Power Centrality Scores of Network Positions igraph::bonpow
igraph::bonpow(graph = part.4.graph, exponent = 0.99)

# PART 5 ------------------------------------------------------------------
part.5.graph <- g.simplified.3

#' Determine:
TOP_N <- 15
#' (a) central node(s)
## alpha_centrality
katz.centrality <- alpha_centrality(part.5.graph, alpha = 0.9)
find_max_n(katz.centrality, TOP_N)

## degree centrality
degree.centrality <- igraph::degree(part.5.graph)
find_max_n(degree.centrality, TOP_N)

## betweenness centrality
betweenness.centrality <- igraph::betweenness(part.5.graph)
find_max_n(betweenness.centrality, TOP_N)

## closeness centrality
closeness.centrality <- igraph::closeness(part.5.graph)
find_max_n(closeness.centrality, TOP_N)

## eigen centrality
eigen.centrality <- igraph::eigen_centrality(part.5.graph)
find_max_n(eigen.centrality$vector, TOP_N)

## power centrality
power.centrality <-
  igraph::power_centrality(part.5.graph, exponent = .9)
find_max_n(power.centrality, TOP_N)

## PageRank
pagerank.centrality <- igraph::page_rank(part.5.graph)
find_max_n(pagerank.centrality$vector, TOP_N)

## eccentricity
eccentricity.centrality <- igraph::eccentricity(part.5.graph)
find_max_n(eccentricity.centrality, TOP_N)

## hubs and authorities
hubs.authorities <- igraph::hub_score(part.5.graph)
find_max_n(hubs.authorities$vector, TOP_N)

## subgraph centrality
subgraph.centrality <- igraph::subgraph_centrality(part.5.graph)
find_max_n(subgraph.centrality, TOP_N)

#' (b) longest path(s)
igraph::diameter(graph = part.5.graph, directed = TRUE) # Length
igraph::get_diameter(graph = part.5.graph, directed = TRUE) # Nodes

#' (c) largest clique(s)
igraph::max_cliques(graph = part.5.graph) # List
igraph::count_max_cliques(graph = part.5.graph) # Count

#' (d) ego(s)
igraph::ego(part.5.graph)
igraph::ego_size(part.5.graph)

#' (e) power centrality
igraph::power_centrality(graph = part.5.graph, exponent = .9)
