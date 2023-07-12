#' Helper functions to create igraphs, get graph charactersitics etc.
#'
#' @param pcgraph graph obtained by \code{pcalg::\link[pcalg]{pc}}
#' @param without character, deletes variables from the graph
#' @param g an igraph object
#' @param knoten character, names of the nodes in the graph
#'
#' @return pc2igraph returns an igraph object;
#'         feature.igraph returns an igraph object;
#'         graph.descriptives returns a matrix
#' @export
#'
#' @examples
#' load(pcalg)
#' data(gmB)
#' V <- colnames(gmB$x)
#' # estimate CPDAG
#' pcg <- pc(suffStat = list(dm = gmB$x, adaptDF = FALSE),
#'           indepTest = binCItest, alpha = 0.01, labels = V, verbose = TRUE)
#'
#' # make igraph
#' g <- pc2igraph(pcg)
#' gi <- feature.igraph(g[[2]], labels = V(g[[2]])$name)
#' graph.descriptives(gi)
#' subplot(gi, 2, d=1, edge.label = TRUE)
#' plot.BC(gi)
#' plot.sp(gi, from = "V4", to = "V5")




pc2igraph <- function(pcgraph, without = NULL)
{
  if (class(pcgraph)[1] != "pcAlgo") stop("graph must be a pcAlgo object")

  # ---   make adjecency matrix  ---
  am <- wgtMatrix(getGraph(pcgraph), transpose = FALSE)
  am[am == 2] <- 1

  # delete vertices and edges from context variables
  if(length(without) > 0){
    # delete from adjm
    rausda <- which(colnames(am) %in% without)
    am <- am[-rausda, -rausda]
  }

  # build graph
  g <- graph.adjacency(am)

  # # giant component
  comps <- decompose.graph(g)
  comps.size <- sapply(comps, gorder)
  gc <- comps[[which.max(comps.size)]]

  #return
  list(g, gc)
}



feature.igraph <- function(g, labels = NULL)
{
  if (class(g)[1] != "igraph") stop("graph must be a igraph object")

  # nodes
  if(!is.null(labels)){
    V(g)$name <- labels
    # V(g)$color[V(g) %in% grep("_el", V(g)$name)] <- "olivedrab3"
    V(g)$color <- "olivedrab3"
    V(g)$color[V(g) %in% grep(".0", V(g)$name)] <- "lightskyblue1" #  "lightskyblue1"
    V(g)$color[V(g) %in% grep(".1", V(g)$name)] <- "deepskyblue"
    V(g)$color[V(g) %in% grep(".2", V(g)$name)] <- "dodgerblue1" # "cornflowerblue"
    V(g)$color[V(g) %in% which(c("sex", "country3", "migrant") %in% V(g)$name)] <- "grey"

  }

  degAll <- igraph::degree(g, v = V(g), mode = "all")
  betAll <- igraph::betweenness(g, v = V(g), directed = FALSE, normalized = TRUE)
  betdir <- igraph::betweenness(g, v = V(g), directed = TRUE, normalized = TRUE)
  cloAll <- igraph::closeness(g, mode = "total", normalized = TRUE)
  betedg <- igraph::edge.betweenness(g, directed = TRUE)

  ### Set graph attributes
  g <- set.vertex.attribute(g, "degree", index = V(g), value = degAll)
  g <- set.vertex.attribute(g, "betweenness", index = V(g), value = betAll)
  g <- set.vertex.attribute(g, "betw_direct", index = V(g), value = betdir)
  g <- set.vertex.attribute(g, "closeness", index = V(g), value = cloAll)
  g <- set.edge.attribute(g, "betweenness", index = E(g), value = betedg)
  g <- set.edge.attribute(g, "similarity", index = E(g), value = 0)

  # Jaccard similarity
  dsAll <- similarity.jaccard(g, V(g), mode = "all")
  ### The order of edges is different from the dataSet;
  ### for that reason these values cannot be assigned directly
  F1 <- function(x,c1,c2)
  {
    dsAll[which(V(g)$name ==x[c1]), which(V(g)$name == x[c2])]
    #c(which(V(g)$name ==x[c1]), which(V(g)$name == x[c2]))
  }
  kantenliste <- get.edgelist(g)
  dimnames(kantenliste)[[2]] <- c("out","in")
  E(g)$similarity <- apply(kantenliste, 1, F1, c1 = "out", c2 = "in")

  g
}




graph.descriptives <- function(g){

  if (class(g)[1] != "igraph") stop("graph must be a igraph object")

  # edges
  am <-  as_adjacency_matrix(g, sparse = FALSE)
  um <- table(am + t(am))

  # ---   output matrix   ---
  out <- matrix(NA, nrow = 15, ncol = 1,
                dimnames = list(c("Graph density",
                                  "Graph transitivity",
                                  "Graph reciprocity",
                                  "Longest shortest path (diameter)",
                                  "Avg. shotest path length",
                                  "Articulation points",
                                  "Number of selected edges",
                                  "Number of undirected edges",
                                  "Max. Degree",
                                  "Avg. number of outgoing edges",
                                  "Avg. Degree",
                                  "Avg. Betweenness centrality [0-100]",
                                  "Avg. Betweenness (direct) centrality  [0-100]",
                                  "Avg. Closeness centrality  [0-100]",
                                  "Avg. Jaccard similarity  [0-100]"), "value"))

  # graph based
  out[1] <- edge_density(as.undirected(g))
  out[2] <- transitivity(g)       #~20% connected triples close to form triangles.
  out[3] <- reciprocity(g)
  out[4] <- length(get_diameter(g, weights = NA))
  out[5] <- mean_distance(g, weights = NA)
  out[6] <- length(articulation.points(g)) # A single vertex that disconnects the graph
  # is called an articulation point
  out[7] <- 0.5 * sum(um[2], um[3], na.rm = TRUE)
  out[8] <- um[3]/2
  if(is.na(out[8]))  out[8] <- 0
#  out[9] <- max(ego_size(g, order = 1, V(g), mindist = 1))
  out[9] <- max(V(g)$degree)
  out[10] <- mean(ego_size(g, order = 1, V(g), mindist = 1, mode = "out"))
  out[11] <- mean(V(g)$degree)
  out[12] <- mean(V(g)$betweenness * 100)
  out[13] <- mean(V(g)$betw_direct * 100)
  out[14] <- mean(V(g)$closeness * 100)
  out[15] <- mean( E(g)$similarity * 100)

  # return
  out
}






##### schnelles Plotten von Subplots ###############
colorit <- function(g, var, farbe = "firebrick1")
{
  V(g)[var]$color <- farbe
  #E(g)$width <- E(g)$weight*2
  g
}



subplot <- function(g, knoten, d = 1, edge.label=FALSE, colornodes = TRUE,
                    nodeSize = NULL, nodegewicht = 1.5, edgeSize = 1.75)
{
  set.seed(5974)

  n <- neighborhood(g, order = d, nodes = knoten)
  x <- unlist(n)

  if(colornodes){
    g <- colorit(g, var = knoten)
  }

  gi <- induced_subgraph(g, x)

  V(gi)$size <- 10
  V(gi)$label.dist <- 1.5

  ### Node Size
  if(!is.null(nodeSize)){
    if(nodeSize == "bet"){
      V(gi)$size <- (V(gi)$betweenness * 100 + 1) * nodegewicht
    } else if(nodeSize == "clos"){
      V(gi)$size <- round((V(gi)$closeness * 100 + 1) * nodegewicht, 0)
    } else if(nodeSize == "degree"){
      V(gi)$size <- (V(gi)$degree + 1) * nodegewicht
  }}

  ### Edgesize
  E(gi)$width <- edgeSize

  if(edge.label)
  {
    plot(gi,
         edge.label=round(E(gi)$weight * 100,0), edge.arrow.size = 0.5,
         edge.label.color="grey30", edge.label.family="sans", edge.label.cex = 0.75,
         vertex.label.color="black", vertex.label.family="sans",
         vertex.frame.color= "white", vertex.label.cex=0.75, layout = layout_on_grid(gi))
  } else {
    plot(gi,
         edge.arrow.size=0.75,
         vertex.label.color="black",
         vertex.label.family="sans",
         vertex.frame.color= "white",
         vertex.label.cex=0.75, layout = layout_with_graphopt(gi))
  }
}

#subplot(g.BC1.25,119,d=2,edge.label=TRUE)



### RESET par-Options
# par(mfrow = c(2,2)) ## some random par change
# par("mfrow")
# par(resetPar())     ## reset the pars to defaults
# par("mfrow")        ## back to default
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}



plot.BC <- function(g, nodeSize=NULL,
                    nodegewicht = 1.5,
                    layout=layout.grid(g),
                    vertex.label = NULL,
                    vertex.label.cex = 0.75,
                    vertex.label.color = "black",
                    vertex.label.family = "sans",
                    edge.arrow.size = 1,
                    edgeSize = 1.5,
                    edge.curved = TRUE,
                    knotenbeschriftung = 10,
                    beschriftung = "zahlen",...)
{

  ### Node Size
  if(is.null(nodeSize)){
    V(g)$size <- nodegewicht * 10
  } else if(nodeSize == "bet"){
    V(g)$size <- (V(g)$betweenness * 100 + 1)*nodegewicht
  } else if(nodeSize == "clos"){
    V(g)$size <- round((V(g)$closeness * 100 + 1)*nodegewicht,0)
  } else if(nodeSize == "degree"){
    V(g)$size <- (V(g)$degree + 1)*nodegewicht
  }

  ### vertex label
  if(is.null(vertex.label)) vertex.label <- V(g)$name

  ### Edge Size
  E(g)$width <- edgeSize
  #E(g)$width <- E(g)$weight*2
  if(is.null(E(g)$color)){
    E(g)$color <- "darkgrey"
    E(g)$color[E(g)$weight > 0.25] <- "dimgrey"
  }

  ### Labelbeschriftung
  if(!is.null(vertex.label)){ V(g)$name  <- vertex.label }
  if(beschriftung == "zahlen"){ V(g)$name <- 1:vcount(g) }
  #V(g)$label.dist <- ifelse(V(g)$size >= knotenbeschriftung, 0, 0.35)
  V(g)$label.dist <- ifelse(V(g)$size >= knotenbeschriftung, 0, V(g)$size / 100 + 0.3)

  ### curved
  if (edge.curved) {
    E <- t(apply(get.edgelist(g), 1, sort))
    E(g)$curved <- 0.25
    E(g)[duplicated(E) | duplicated(E, fromLast = TRUE)]$curved <- 0
  }

  par(mar = c(0,0,2,0))
  plot(g, layout=layout,
       vertex.label = vertex.label,
       vertex.label.cex = vertex.label.cex,
       vertex.label.family = vertex.label.family,
       vertex.label.color = vertex.label.color,
       vertex.frame.color = "white",
       edge.arrow.size = edge.arrow.size,
       ...)}




plot.sp <- function(g, from, to, d = 1, nodeSize = 10, sp = NULL, seed = 2454){

  # asp <- all_simple_paths(g, from = from, to = to, mode = "out")
  if(is.null(sp)){
    sp <- all_shortest_paths(g, from = from, to = to, mode = "out")$res
  }
    spn <- names(unlist(sp)[1])

# browser()
  # Build sub-graphs to test
  # sub_graphs <- lapply(asp, function(vs) induced_subgraph(g, vs))
  # tmp <- unique(unlist(lapply(sub_graphs, function(x) V(x)$name)))

  # Colour and style vertecies
  # V(g)$color[unique(unlist(asp))] <- "gray"
  V(g)$color[V(g) %in% which(V(g)$name %in% from)] <- "yellow"
  V(g)$color[V(g) %in% which(V(g)$name %in% to)] <- "firebrick1"

  # Colour each of the paths
  E(g)$color <- "gray"
  E(g)$width <- 1.5

  if(is.list(sp)){
    # lapply(asp, function(x) E(g, path = x)$color <<- "dodgerblue")
    lapply(sp, function(x) E(g, path = x)$color <<- "darkorange")
    lapply(sp, function(x) E(g, path = x)$width <<-  2)

    # plot.BC(g, vertex.label = V(g)$name, nodeSize="degree",
    #         edgeSize = E(g)$width, edge.curved = TRUE)

    knoten <- do.call(c, lapply(sp, function(x) x$name))

  } else {
    E(g, path = sp)$color <- "darkorange"
    E(g, path = sp)$width <-  2
    knoten <- sp$name
  }

  g <- colorit(g, var = setdiff(knoten, c(to, from)), farbe = "darkorange")
  n <- neighborhood(g, order = d, nodes = knoten)
  knoten <- unlist(n)
  gi <- induced_subgraph(g, knoten)


  set.seed(seed)
  plot(gi,
       vertex.label.color = "black",
       vertex.label.family = "sans",
       vertex.frame.color = "white",
       vertex.label.cex = 0.75,
       vertex.size = nodeSize,
       edge.arrow.size = 0.75,
       layout = layout_with_lgl(gi))

}

