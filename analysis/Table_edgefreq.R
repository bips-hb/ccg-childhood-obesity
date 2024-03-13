# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    JUL 2021
#
# Purpose: Table edge frequencies
#
# ------------------------------------------------------------------------------
library(gt)
library(gtsummary)

# load data
boot <- readRDS("data_not_load/boot100_mi1-graph.RDS")
load("data_not_load/06_graph-10trees-pa.RData")






# g.pa to igraph
g <- pc2igraph(g.pa)
g <- feature.igraph(g[[1]])
V(g)$name <- c("Sex", "Region", "Migrant", "Income", "ISCED",
               "Mother's age at birth", "Total breastfeeding",
               "Birthweight", "Weeks of pregnancy", "Formula milk", "HH diet",
               "Smoking during pregnancy", "Age (B)", "School (B)", "AVM (B)",
               "BMI (B)", "Mother's BMI (B)", "Daily family meals (B)", "PA (B)",
               "Sleep (B)", "Well-being (B)", "YHEI (B)", "HOMA (B)",
               "Age (FU1)", "School (FU1)", "AVM (FU1)",
               "BMI (FU1)", "Mother's BMI (FU1)", "Daily family meals (FU1)",
               "Income (FU1)", "ISCED (FU1)","PA (FU1)",
               "Sleep (FU1)", "Well-being (FU1)", "YHEI (FU1)", "HOMA (FU1)",
               "Age (FU2)", "AVM (FU2)", "BMI (FU2)", "Mother's BMI (FU2)",
               "Alcohol (FU2)", "Daily family meals (FU2)", "Income (FU2)", "ISCED (FU2)",
               "PA (FU2)", "Puberty (FU2)","Sleep (FU2)", "Smoking (FU2)",
               "Well-being (FU2)", "YHEI (FU2)", "HOMA (FU2)")

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



# Build igraph
g.boot <- graph.adjacency(g.avg, weighted = TRUE)
V(g.boot)$name <- c("Sex", "Region", "Migrant", "Income", "ISCED",
                 "Mother's age at birth", "Total breastfeeding",
                 "Birthweight", "Weeks of pregnancy", "Formula milk", "HH diet",
                 "Smoking during pregnancy", "Age (B)", "School (B)", "AVM (B)",
                 "BMI (B)", "Mother's BMI (B)", "Daily family meals (B)", "PA (B)",
                 "Sleep (B)", "Well-being (B)", "YHEI (B)", "HOMA (B)",
                 "Age (FU1)", "School (FU1)", "AVM (FU1)",
                 "BMI (FU1)", "Mother's BMI (FU1)", "Daily family meals (FU1)",
                 "Income (FU1)", "ISCED (FU1)","PA (FU1)",
                 "Sleep (FU1)", "Well-being (FU1)", "YHEI (FU1)", "HOMA (FU1)",
                 "Age (FU2)", "AVM (FU2)", "BMI (FU2)", "Mother's BMI (FU2)",
                 "Alcohol (FU2)", "Daily family meals (FU2)", "Income (FU2)", "ISCED (FU2)",
                 "PA (FU2)", "Puberty (FU2)","Sleep (FU2)", "Smoking (FU2)",
                 "Well-being (FU2)", "YHEI (FU2)", "HOMA (FU2)")

# more information about edges
edges <- as.data.frame(as_edgelist(g.boot))
names(edges) <- c("from", "to")
edges$weight <- E(g.boot)$weight

# merge two data frames
edges.joint <- left_join(edges.pa, edges, by = c("from", "to"))


# median and IQR of selected edges
summary(edges$weight)
IQR(edges$weight)


# edges with more than 80%
e80 <- edges[which(edges$weight >= 80),]
e80[order(e80$weight, decreasing = TRUE),]


# Table ----------------------------------------------------------------------------------
ej <- edges.joint[order(edges.joint$weight, decreasing = TRUE),]

write.csv2(ej, file = "results/tab_edgefreq.csv")

