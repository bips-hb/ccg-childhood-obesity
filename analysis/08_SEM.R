# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Recover graph using structural EM
#
# ------------------------------------------------------------------------------
library(bnlearn)
source("R/little_helpers.R")
load("data/original-data.RData")
load("data/graph.RData")
rm(daten, daten.pa.mids, daten.mids, daten100.mids, ebenen.pa)


# -------   Graph   ----------------------------------------------------------------------
# bnlearn cannot deal with ordered variables
original.data$isced   <- factor(original.data$isced, ordered = FALSE)
original.data$isced.1 <- factor(original.data$isced.1, ordered = FALSE)
original.data$isced.2 <- factor(original.data$isced.2, ordered = FALSE)

original.data$income   <- factor(original.data$income, ordered = FALSE)
original.data$income.1 <- factor(original.data$income.1, ordered = FALSE)
original.data$income.2 <- factor(original.data$income.2, ordered = FALSE)


### Blacklisting
bl <- matrix2df(fg.pa)

# Structural EM
set.seed(4360987)
sem <- structural.em.rf(original.data,
                        maximize = "hc",
                        maximize.args = list(blacklist = bl, score = "bic-cg"))

g.sem <- bnlearn::as.igraph(sem)
g.sem <- feature.igraph(g.sem, labels = V(g.sem)$name)

# ---! save   ----------------------------------------------------------------------------
save(sem, g.sem, file = "data/graph-sem-hc_bic-cg.RData")
