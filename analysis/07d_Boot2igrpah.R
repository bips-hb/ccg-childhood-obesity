# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    JUL 2021
#
# Purpose: Bootstrap zu einem Graphen basteln
#
# ------------------------------------------------------------------------------
boot <- readRDS("data/boot100_mi1-graph.RDS")
load("data/graph-10trees.RData")


# g.pa to igraph
g <- pc2igraph(g.pa)
g <- feature.igraph(g[[1]])
V(g)$name <- c("Sex", "Country", "Migrant", "Income", "ISCED", "Age_at_birth",
               "Breastfeeding", "Birthweight", "Weeks_of_pregnancy", "Formula_milk",
               "HH_Diet", "Smoking_pregnancy",
               "Age", "School", "AVM", "BMI", "Mothers_BMI", "Familymeal", "PA",
               "Sleep", "Well-being", "YHEI", "HOMA",
               "Age.FU1", "School.FU1", "AVM.FU1", "BMI.FU1", "Mothers_BMI.FU1",
               "Familymeal.FU1", "Income.FU1", "ISCED.FU1","PA.FU1",
               "Sleep.FU1", "Well-being.FU1", "YHEI.FU1", "HOMA.FU1",
               "Age.FU2", "AVM.FU2", "BMI.FU2", "Mothers_BMI.FU2", "Alcohol.FU2",
               "Familymeal.FU2", "Income.FU2", "ISCED.FU2","PA.FU2", "Puberty.FU2",
               "Sleep.FU2", "Smoking.FU2", "Well-being.FU2", "YHEI.FU2", "HOMA.FU2")

edges.pa <- as.data.frame(as_edgelist(g))
names(edges.pa) <- c("from", "to")


# make adjacency matrix of all pc-graphs
amat <- lapply(boot, function(x){
  tmp <- wgtMatrix(getGraph(x), transpose = FALSE)
  wm2 <- (tmp + t(tmp))
  tmp[which(wm2 > 1)] <- 0.5
  tmp
})

g.avg <- Reduce('+', amat)

# igraph
g.boot <- graph.adjacency(g.avg, weighted = TRUE)
V(g.boot)$name <- c("Sex", "Country", "Migrant", "Income", "ISCED", "Age_at_birth",
                    "Breastfeeding", "Birthweight", "Weeks_of_pregnancy", "Formula_milk",
                    "HH_Diet", "Smoking_pregnancy",
                    "Age", "School", "AVM", "BMI", "Mothers_BMI", "Familymeal", "PA",
                    "Sleep", "Well-being", "YHEI", "HOMA",
                    "Age.FU1", "School.FU1", "AVM.FU1", "BMI.FU1", "Mothers_BMI.FU1",
                    "Familymeal.FU1", "Income.FU1", "ISCED.FU1","PA.FU1",
                    "Sleep.FU1", "Well-being.FU1", "YHEI.FU1", "HOMA.FU1",
                    "Age.FU2", "AVM.FU2", "BMI.FU2", "Mothers_BMI.FU2", "Alcohol.FU2",
                    "Familymeal.FU2", "Income.FU2", "ISCED.FU2","PA.FU2", "Puberty.FU2",
                    "Sleep.FU2", "Smoking.FU2", "Well-being.FU2", "YHEI.FU2", "HOMA.FU2")


# more information about edges
edges <- as.data.frame(as_edgelist(g.boot))
names(edges) <- c("from", "to")
edges$weight <- E(g.boot)$weight

# ---! SAVE   -------------------------------------------------------
save(g.boot, edges, file = "data/boot100igraph.RData")



