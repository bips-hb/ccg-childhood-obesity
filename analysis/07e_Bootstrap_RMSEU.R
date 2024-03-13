# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    JUL 2021
#
# Purpose: RMSEU
#
# ------------------------------------------------------------------------------
#boot <- readRDS("data_not_load/boot.RDS")
boot <- readRDS("data_not_load/boot100_mi1-graph.RDS")


# make adjacency matrix of all pc-graphs
amat <- lapply(boot, function(x){
  tmp <- wgtMatrix(getGraph(x), transpose = FALSE)
  wm2 <- (tmp + t(tmp))
  # undirected edges get 0.5 (in sum they count as 1)
  tmp[which(wm2 > 1)] <- 0.5
  tmp
})

## average edges
g.avg <- Reduce('+', amat)
g.avg <- Reduce('+', amat) / length(amat)



# sum up directed edges
t1 <- t(g.avg)[lower.tri(t(g.avg))] # untere Hälfte
t2 <- g.avg[lower.tri(g.avg)]       # obere Hälfte
t3 <- apply(cbind(t1, t2), 1, sum)  # Summe

# select_all_but_diag <- function(x){
#   apply(cbind(t(x)[lower.tri(x, diag = F)], x[lower.tri(x, diag = F)]), 1, sum)
# }
# a <- select_all_but_diag(g.avg)




t44 <- t50 <- t75 <- t3
t44[t44 < 0.44] <- 0
t50[t50 < 0.50] <- 0
t75[t75 < 0.75] <- 0
(rmseu0  <-  gum(t3, vertices = nrow(g.avg), threshold = 0.5))
(rmseu44 <-  gum(t44, vertices = nrow(g.avg), threshold = 0.5))
(rmseu50 <-  gum(t50, vertices = nrow(g.avg), threshold = 0.5))
(rmseu75 <-  gum(t75, vertices = nrow(g.avg), threshold = 0.5))

# compare - RMSEU = 0.183
gum(rep(0.9085, times = choose(51,2)), vertices = 51, threshold = 0.5)$rmseu
1-0.9085
# MEU 0.0443451
gum(rep(0.005, times = choose(51,2)), vertices = 51, threshold = 0.5)$meu
1-0.9085

# compare - RMSEU = 0.246
gum(rep(0.877, times = choose(51,2)), vertices = 51, threshold = 0.5)
1-0.877
# compare - MEE = 0.059
gum(rep(0.0069, times = choose(51,2)), vertices = 51, threshold = 0.5)$mee
1-0.0069


# compare - 0.183 for edges that were at least selected once (here 570)
gum(rep(0.9085, times = 570), vertices = 34, threshold = 0.5)$rmseu
gum(rep(0.877, times = 570), vertices = 34, threshold = 0.5)$rmseu





# compare - 0.267481
gum(rep(0.8665, times = choose(51,2)), vertices = 51, threshold = 0.5)$rmseu
gum(rep(1-0.8665, times = choose(51,2)), vertices = 51, threshold = 0.5)$rmseu
gum(matrix(rep(0.0936, times = 51*51), nrow = 51, ncol = 51), threshold = 0.5)$rmseu
gum(matrix(rep(0.35, times = 51*51), nrow = 51, ncol = 51), threshold = 0.5)

# summary(t3): mean = 0.09917, rmseu = 0.1048
gum(rep(0.2457, times = choose(51,2)), 51, threshold = 0.75)$rmseu
gum(rep(0.2457, times = choose(51,2)), 51, threshold = 0.25)$rmseu
gum(rep(0.09917, times = choose(51,2)), 51, threshold = 0.05)$rmseu
gum(rep(0.09917, times = choose(51,2)), 51, threshold = 0.05)$mee

# --- save -----------------------------------------------
save(rmseu0, rmseu44, rmseu50, rmseu75, file = "results/rmseu.RData")


mymeu <- function(x){
  0.0443451 - gum(rep(x, times = choose(51,2)), vertices = 51, threshold = 0.5)$meu
}
optim(seq(0.01,0.15, length = 100), mymeu)




# uncertaintz shcie- 10 Knoten -> 45 kanten
medges <- rep(0.5)
gum(medges, vertices = 2, threshold = 0.05)$mee
