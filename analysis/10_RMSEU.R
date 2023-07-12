# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: RMSEU
#
# ------------------------------------------------------------------------------
boot <- readRDS("data/boot100_mi1-graph.rds")


### make adjacency matrix of all pc-graphs
amat <- lapply(boot, function(x){
  tmp <- wgtMatrix(getGraph(x), transpose = FALSE)
  wm2 <- (tmp + t(tmp))
  # undirected edges get 0.5 (in sum they count as 1)
  tmp[which(wm2 > 1)] <- 0.5
  tmp
})

### average edges
g.avg <- Reduce('+', amat)
g.avg <- Reduce('+', amat) / length(amat)



### sum up directed edges
t1 <- t(g.avg)[lower.tri(t(g.avg))] 
t2 <- g.avg[lower.tri(g.avg)]       
t3 <- apply(cbind(t1, t2), 1, sum)  


t44 <- t75 <- t3
t44[t44 < 0.44] <- 0
t75[t75 < 0.75] <- 0
(rmseu0  <-  gum(t3, vertices = nrow(g.avg), threshold = 0.5))
(rmseu44 <-  gum(t44, vertices = nrow(g.avg), threshold = 0.5))
(rmseu75 <-  gum(t75, vertices = nrow(g.avg), threshold = 0.5))


# --- save -----------------------------------------------
save(rmseu0, rmseu44, rmseu75, file = "data/rmseu.RData")