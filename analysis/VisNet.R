library(visNetwork)
library(glue)



# ------------   Main graph   ------------------------------------------------------------
load("data_not_load/06_graph-10trees-pa.RData")
g <- pc2igraph(g.pa)[[1]]
graph.descriptives(g)


# --- Prepare edges ---
edges <- as.data.frame(as_edgelist(g))
names(edges) <- c("from", "to")
edges$from[which(edges$from == "income")] <- "income.0"
edges$from[which(edges$from == "isced")] <- "isced.0"
edges$to[which(edges$to == "income")] <- "income.0"
edges$to[which(edges$to == "isced")] <- "isced.0"

edges <- data.frame(edges, edges$from,
                    arrowheads = rep("arrow", nrow(edges)),
                    arrows.to.enabled = TRUE)

# ungerichtete Kanten bearbeiten (d.h., delete arrowhead für diese Kanten)
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten:c(21, 80, 86, 87, 91, 92, 93, 95, 96, 113, 115, 116)
edges <- edges[-c(21, 80, 86, 87, 91, 92, 93, 95, 96, 113, 115, 116),]
edges.pa <- edges


# --- prepre nodes ---
farbpalette <- c(rep("#afa8b3",3), rep("#4F3824",7),
                 rep("#D1603D",13),rep("#DDB967",13), rep("#D0E37F",15))

nodes <- data.frame(id = g.pa@graph@nodes, label = g.pa@graph@nodes)
nodes <- rbind(nodes[1:3,], nodes[6:12,], nodes[13:18,], nodes[4:5,], nodes[19:51,])
nodes$id[which(nodes$id == "income")] <- "income.0"
nodes$id[which(nodes$id == "isced")] <- "isced.0"

nodes$group <- c(rep("background",3), rep("early life",7),
                rep("baseline", 13), rep("FU1",13), rep("FU2",15))
nodes$level <- c(rep(1,3), rep(2,7), rep(3,13), rep(4,13), rep(5,15))
nodes$color.background <- farbpalette
nodes$color.border <- farbpalette
nodes$color.highlight.border <- rep("#C62F4B",51)
row.names(nodes) <- 1:51
nodes$label <- c("Sex", "Region", "Migrant", "Mother's age\nat birth", "Total\nbreastfeeding",
                 "Birthweight", "Weeks of\npregnancy", "Formula milk", "HH diet",
                 "Smoking\nduring pregnancy", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED", "PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED","PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "AVM",
                 "zBMI", "Mother's BMI", "Alcohol", "Daily\nfamily meals", "Income", "ISCED",
                 "PA", "Puberty","Sleep", "Smoking", "Well-being", "YHEI", "HOMA")
nodes.pa <- nodes


# legend nodes
legendNodes <- data.frame(
  # label = c("Context","Early life", "B", "FU1", "FU2"),
  label = c("","", "", "", ""),
  shape = c("square", "square", "dot", "dot", "dot"),
  color.background = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37]),
  color.border = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37])
)




# --- vis network ---
vis.pa <-
visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
   # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.pa

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.pa, file = "vis-pa.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")






# ----------------------------------------------------------------------------------------
#
#                          Sensitivity graphs
#
# ------------   MVPA   ------------------------------------------------------------------
load("data_not_load/06_graph-10trees-mvpa.RData")
g <- pc2igraph(g.mvpa)[[1]]


# --- Prepare edges ---
edges <- as.data.frame(as_edgelist(g))
names(edges) <- c("from", "to")
edges$from[which(edges$from == "income")] <- "income.0"
edges$from[which(edges$from == "isced")] <- "isced.0"
edges$to[which(edges$to == "income")] <- "income.0"
edges$to[which(edges$to == "isced")] <- "isced.0"

edges <- data.frame(edges, edges$from,
                    arrowheads = rep("arrow", nrow(edges)),
                    arrows.to.enabled = TRUE)

# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten:c(21, 80, 83, 86, 88, 90, 107, 109, 110)
edges <- edges[-c(21, 80, 83, 86, 88, 90, 107, 109, 110),]



# --- prepre nodes ---
farbpalette <- c(rep("#afa8b3",3), rep("#4F3824",7),
                 rep("#D1603D",14),rep("#DDB967",14), rep("#D0E37F",16))

nodes <- data.frame(id = g.mvpa@graph@nodes, label = g.mvpa@graph@nodes)
nodes <- rbind(nodes[1:3,], nodes[6:12,], nodes[13:18,], nodes[4:5,], nodes[19:54,])
nodes$id[which(nodes$id == "income")] <- "income.0"
nodes$id[which(nodes$id == "isced")] <- "isced.0"

nodes$group <- c(rep("background",3), rep("early life",7),
                 rep("baseline", 14), rep("FU1",14), rep("FU2",16))
nodes$level <- c(rep(1,3), rep(2,7), rep(3,14), rep(4,14), rep(5,16))
nodes$color.background <- farbpalette
nodes$color.border <- farbpalette
nodes$color.highlight.background <- rep("#C62F4B",54)
nodes$color.highlight.border <- rep("#C62F4B",54)
row.names(nodes) <- 1:54
nodes$label <- c("Sex", "Region", "Migrant", "Mother's age\nat birth", "Total\nbreastfeeding",
                 "Birthweight", "Weeks of\npregnancy", "Formula milk", "HH diet",
                 "Smoking\nduring pregnancy", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED", "MVPA",
                 "Sedentary", "Sleep", "Well-being", "YHEI", "HOMA", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED","MVPA",
                 "Sedentary", "Sleep", "Well-being", "YHEI", "HOMA", "Age", "AVM",
                 "zBMI", "Mother's BMI", "Alcohol", "Daily\nfamily meals", "Income", "ISCED",
                 "MVPA", "Puberty", "Sedentary","Sleep", "Smoking", "Well-being", "YHEI", "HOMA")



# legend nodes
legendNodes <- data.frame(
  label = c("Context","Early life", "B", "FU1", "FU2"),
  shape = c("square", "square", "dot", "dot", "dot"),
  color.background = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[25], farbpalette[39]),
  color.border = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[25], farbpalette[39])
)




# --- vis network ---
vis.mvpa <-
  visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.mvpa

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.mvpa, file = "vis-mvpa.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")

nodes.mvpa <- nodes
edges.mvpa <- edges
legendNodes.mvpa <- legendNodes






# ------------   Test-wise deletion   ----------------------------------------------------
#load("data_not_load/09_twd-pa.RData")
load("data_not_load/09_twd-pa-220718.RData")
g <- pc2igraph(g.tw)[[1]]


# --- Prepare edges ---
edges <- as.data.frame(as_edgelist(g))
names(edges) <- c("from", "to")
edges$from[which(edges$from == "income")] <- "income.0"
edges$from[which(edges$from == "isced")] <- "isced.0"
edges$to[which(edges$to == "income")] <- "income.0"
edges$to[which(edges$to == "isced")] <- "isced.0"

edges <- data.frame(edges, edges$from,
                    arrowheads = rep("arrow", nrow(edges)),
                    arrows.to.enabled = TRUE)

# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten OLD:c(32,61,63,80,94,95,98,102,123,136,142,148,150,153)
# edges <- edges[-c(32,61,63,80,94,95,98,102,123,136,142,148,150,153),]

# 19.07.2022: Fehler bei mixCItwd bereinigt:
# delete doppelte Kanten OLD:c(98, 102, 121, 139. 141)
edges <- edges[-c(98, 102, 121, 139, 141),]


# --- prepre nodes ---
farbpalette <- c(rep("#afa8b3",3), rep("#4F3824",7),
                 rep("#D1603D",13),rep("#DDB967",13), rep("#D0E37F",15))

nodes <- data.frame(id = g.tw@graph@nodes, label = g.tw@graph@nodes)
nodes <- rbind(nodes[1:3,], nodes[6:12,], nodes[13:18,], nodes[4:5,], nodes[19:51,])
nodes$id[which(nodes$id == "income")] <- "income.0"
nodes$id[which(nodes$id == "isced")] <- "isced.0"

nodes$group <- c(rep("background",3), rep("early life",7),
                 rep("baseline", 13), rep("FU1",13), rep("FU2",15))
nodes$level <- c(rep(1,3), rep(2,7), rep(3,13), rep(4,13), rep(5,15))
nodes$color.background <- farbpalette
nodes$color.border <- farbpalette
nodes$color.highlight.background <- rep("#C62F4B",51)
nodes$color.highlight.border <- rep("#C62F4B",51)
row.names(nodes) <- 1:51
nodes$label <- c("Sex", "Region", "Migrant", "Mother's age\nat birth", "Total\nbreastfeeding",
                 "Birthweight", "Weeks of\npregnancy", "Formula milk", "HH diet",
                 "Smoking\nduring pregnancy", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED", "PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED","PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "AVM",
                 "zBMI", "Mother's BMI", "Alcohol", "Daily\nfamily meals", "Income", "ISCED",
                 "PA", "Puberty","Sleep", "Smoking", "Well-being", "YHEI", "HOMA")



# legend nodes
legendNodes <- data.frame(
  label = c("Context","Early life", "B", "FU1", "FU2"),
  shape = c("square", "square", "dot", "dot", "dot"),
  color.background = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37]),
  color.border = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37])
)






# --- vis network ---
vis.twd <-
  visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.twd

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.twd, file = "vis-twd.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")

nodes.twd <- nodes
edges.twd <- edges






# ------------   SEM   -------------------------------------------------------------
load("data_not_load/10_sem-hc_bic-cg.RData")
g.sem <- bnlearn::as.igraph(sem)
g.sem <- feature.igraph(g.sem, labels = V(g.sem)$name)


# --- Prepare edges ---
edges <- as.data.frame(as_edgelist(g.sem))
names(edges) <- c("from", "to")
edges$from[which(edges$from == "income")] <- "income.0"
edges$from[which(edges$from == "isced")] <- "isced.0"
edges$to[which(edges$to == "income")] <- "income.0"
edges$to[which(edges$to == "isced")] <- "isced.0"

edges <- data.frame(edges, edges$from,
                    arrowheads = rep("arrow", nrow(edges)),
                    arrows.to.enabled = TRUE)

# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
# edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten: keine doppelte Kanten



# --- prepre nodes ---
farbpalette <- c(rep("#afa8b3",3), rep("#4F3824",7),
                 rep("#D1603D",13),rep("#DDB967",13), rep("#D0E37F",15))

nodes <- data.frame(id = V(g.sem)$name, label = V(g.sem)$name)
nodes <- rbind(nodes[1:3,], nodes[6:12,], nodes[13:18,], nodes[4:5,], nodes[19:51,])
nodes$id[which(nodes$id == "income")] <- "income.0"
nodes$id[which(nodes$id == "isced")] <- "isced.0"

nodes$group <- c(rep("background",3), rep("early life",7),
                 rep("baseline", 13), rep("FU1",13), rep("FU2",15))
nodes$level <- c(rep(1,3), rep(2,7), rep(3,13), rep(4,13), rep(5,15))
nodes$color.background <- farbpalette
nodes$color.border <- farbpalette
nodes$color.highlight.background <- rep("#C62F4B",51)
nodes$color.highlight.border <- rep("#C62F4B",51)
row.names(nodes) <- 1:51
nodes$label <- c("Sex", "Region", "Migrant", "Mother's age\nat birth", "Total\nbreastfeeding",
                 "Birthweight", "Weeks of\npregnancy", "Formula milk", "HH diet",
                 "Smoking\nduring pregnancy", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED", "PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED","PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "AVM",
                 "zBMI", "Mother's BMI", "Alcohol", "Daily\nfamily meals", "Income", "ISCED",
                 "PA", "Puberty","Sleep", "Smoking", "Well-being", "YHEI", "HOMA")



# legend nodes
legendNodes <- data.frame(
  label = c("Context","Early life", "B", "FU1", "FU2"),
  shape = c("square", "square", "dot", "dot", "dot"),
  color.background = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37]),
  color.border = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37])
)






# --- vis network ---
vis.sem <-
  visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.sem

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.sem, file = "vis-sem.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")


nodes.sem <- nodes
edges.sem <- edges








# ------------   Bootstrap   ----------------------------------------------------
#
# -------------------------------------------------------------------------------
#load("data_not_load/07_boot-pa.RData")
load("data_not_load/07_boot100igraph.RData")

# verwende alte Knotennamen
V(g.boot)$name <-
  c("sex", "country3", "migrant", "income", "isced", "bage", "bf",
 "bweight", "preg_week", "formula", "hdiet", "preg_smoke", "age.0", "school.0",
 "media.0", "bmi.0", "bmi_m.0", "familymeal.0", "pa.0", "sleep.0", "wb.0",
 "yhei.0", "homa.0", "age.1", "school.1", "media.1", "bmi.1", "bmi_m.1",
 "familymeal.1", "income.1", "isced.1", "pa.1", "sleep.1", "wb.1", "yhei.1",
 "homa.1", "age.2", "media.2", "bmi.2", "bmi_m.2", "alc.2", "familymeal.2",
 "income.2", "isced.2", "pa.2", "pub.2", "sleep.2", "smoke.2", "wb.2",
 "yhei.2", "homa.2")

# --- Prepare edges ---
edges <- as.data.frame(as_edgelist(g.boot))
names(edges) <- c("from", "to")
edges$from[which(edges$from == "income")] <- "income.0"
edges$from[which(edges$from == "isced")] <- "isced.0"
edges$to[which(edges$to == "income")] <- "income.0"
edges$to[which(edges$to == "isced")] <- "isced.0"

edges <- data.frame(edges, edges$from,
                    arrowheads = rep("arrow", nrow(edges)),
                    arrows.to.enabled = TRUE,
                    weight = E(g.boot)$weight)
edges.boot <- edges



# --- prepre nodes ---
farbpalette <- c(rep("#afa8b3",3), rep("#4F3824",7),
                 rep("#D1603D",13),rep("#DDB967",13), rep("#D0E37F",15))

nodes <- data.frame(id = V(g.boot)$name, label = V(g.boot)$name)
nodes <- rbind(nodes[1:3,], nodes[6:12,], nodes[13:18,], nodes[4:5,], nodes[19:51,])
nodes$id[which(nodes$id == "income")] <- "income.0"
nodes$id[which(nodes$id == "isced")] <- "isced.0"

nodes$group <- c(rep("background",3), rep("early life",7),
                 rep("baseline", 13), rep("FU1",13), rep("FU2",15))
nodes$level <- c(rep(1,3), rep(2,7), rep(3,13), rep(4,13), rep(5,15))
nodes$color.background <- farbpalette
nodes$color.border <- farbpalette
nodes$color.highlight.background <- rep("#C62F4B",51)
nodes$color.highlight.border <- rep("#C62F4B",51)
row.names(nodes) <- 1:51
nodes$label <- c("Sex", "Region", "Migrant", "Mother's age\nat birth", "Total\nbreastfeeding",
                 "Birthweight", "Weeks of\npregnancy", "Formula milk", "HH diet",
                 "Smoking\nduring pregnancy", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED", "PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED","PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "AVM",
                 "zBMI", "Mother's BMI", "Alcohol", "Daily\nfamily meals", "Income", "ISCED",
                 "PA", "Puberty","Sleep", "Smoking", "Well-being", "YHEI", "HOMA")
nodes.boot <- nodes


# legend nodes
legendNodes <- data.frame(
  label = c("Context","Early life", "Baseline", "FU1", "FU2"),
  shape = c("square", "square", "dot", "dot", "dot"),
  color.background = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37]),
  color.border = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37])
)






# --- vis network: Bootstrap 75% ---
vis.boot.75 <-
  visNetwork(nodes, edges[edges$weight >= 75,], width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  # visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T))
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.boot.75

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.boot.75, file = "vis-boot_75perc.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")




# --- vis network: Bootstrap 60% ---
edges <- edges.boot[edges.boot$weight >= 60,]
rownames(edges) <- 1:nrow(edges)
# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten:
# edges <- edges[-c(15, 60, 73, 74, 88, 99, 123, 124, 128),]


vis.boot.60 <-
  visNetwork(nodes, edges[edges$weight >= 60,], width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  # visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T))
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.boot.60

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.boot.60, file = "vis-boot-60perc.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")
#




# --- vis network: Bootstrap 44% ---
edges <- edges.boot[edges.boot$weight > 44,]
rownames(edges) <- 1:nrow(edges)
# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE


vis.boot.44 <-
  visNetwork(nodes, edges[edges$weight >= 44,], width = "100%", height = "800px") %>%
  visLayout(randomSeed = 756) %>%
  # visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T))
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.boot.44

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.boot.44, file = "vis-boot_44perc.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")






# --- vis network: Bootstrap 35% ---
edges <- edges.boot[edges.boot$weight >= 35,]
rownames(edges) <- 1:nrow(edges)
# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten:
edges <- edges[-c(15, 60, 73, 74, 88, 99, 123, 124, 128),]





vis.boot.35 <-
  visNetwork(nodes, edges[edges$weight >= 35,], width = "100%", height = "800px") %>%
  visLayout(randomSeed = 156) %>%
  # visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T))
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.boot.35

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.boot.35, file = "vis-boot-35perc.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")



# ------------   Main graph, alpha = .10  --------------------------------------
load("data_not_load/06sens_graph-10trees-pa-alpha.RData")
g <- pc2igraph(g.pa.alpha)[[1]]
graph.descriptives(g)


# --- Prepare edges ---
edges <- as.data.frame(as_edgelist(g))
names(edges) <- c("from", "to")
edges$from[which(edges$from == "income")] <- "income.0"
edges$from[which(edges$from == "isced")] <- "isced.0"
edges$to[which(edges$to == "income")] <- "income.0"
edges$to[which(edges$to == "isced")] <- "isced.0"

edges <- data.frame(edges, edges$from,
                    arrowheads = rep("arrow", nrow(edges)),
                    arrows.to.enabled = TRUE)

# ungerichtete Kanten bearbeiten
tmp <- tmp2 <- vector()
for(i in 1:(nrow(edges))){
  tmp  <- c(tmp,  glue_collapse(edges[i,1:2],"-"))
  tmp2 <- c(tmp2, glue_collapse(edges[i,2:3],"-"))
}

isele <- which(is.element(tmp2, tmp))
edges[isele,]
edges$arrows.to.enabled[isele] <- FALSE

# delete doppelte Kanten:c(40, 69, 71, 90, 100, 101, 102, 104, 105, 106, 121, 124, 126)
edges <- edges[-c(40, 69, 71, 90, 100, 101, 102, 104, 105, 106, 121, 124, 126),]
edges.pa.alpha <- edges


# --- prepre nodes ---
farbpalette <- c(rep("#afa8b3",3), rep("#4F3824",7),
                 rep("#D1603D",13),rep("#DDB967",13), rep("#D0E37F",15))

nodes <- data.frame(id = g.pa@graph@nodes, label = g.pa@graph@nodes)
nodes <- rbind(nodes[1:3,], nodes[6:12,], nodes[13:18,], nodes[4:5,], nodes[19:51,])
nodes$id[which(nodes$id == "income")] <- "income.0"
nodes$id[which(nodes$id == "isced")] <- "isced.0"

nodes$group <- c(rep("background",3), rep("early life",7),
                 rep("baseline", 13), rep("FU1",13), rep("FU2",15))
nodes$level <- c(rep(1,3), rep(2,7), rep(3,13), rep(4,13), rep(5,15))
nodes$color.background <- farbpalette
nodes$color.border <- farbpalette
nodes$color.highlight.border <- rep("#C62F4B",51)
row.names(nodes) <- 1:51
nodes$label <- c("Sex", "Region", "Migrant", "Mother's age\nat birth", "Total\nbreastfeeding",
                 "Birthweight", "Weeks of\npregnancy", "Formula milk", "HH diet",
                 "Smoking\nduring pregnancy", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED", "PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "School", "AVM",
                 "zBMI", "Mother's BMI", "Daily\nfamily meals", "Income", "ISCED","PA",
                 "Sleep", "Well-being", "YHEI", "HOMA", "Age", "AVM",
                 "zBMI", "Mother's BMI", "Alcohol", "Daily\nfamily meals", "Income", "ISCED",
                 "PA", "Puberty","Sleep", "Smoking", "Well-being", "YHEI", "HOMA")
nodes.pa.alpha <- nodes


# legend nodes
legendNodes <- data.frame(
  label = c("Context","Early life", "B", "FU1", "FU2"),
  shape = c("square", "square", "dot", "dot", "dot"),
  color.background = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37]),
  color.border = c(farbpalette[1], farbpalette[4], farbpalette[11], farbpalette[24], farbpalette[37])
)




# --- vis network ---
vis.pa.alpha <-
  visNetwork(nodes.pa.alpha, edges.pa.alpha, width = "100%", height = "800px") %>%
  visLayout(randomSeed = 96) %>%
  visIgraphLayout(layout = "layout_on_grid") %>%   #layout = "layout_on_grid", "layout_with_sugiyama
  # visHierarchicalLayout(direction = "LR", levelSeparation = 200) %>%
  visNodes(
    shape = "dot",
    label = nodes$label,
    group = nodes$group,
    font = list(size = 32))  %>%
  visEdges(
    shadow = FALSE,
    arrows = "to",
    color = list(color = "#466286", highlight = "#C62F4B"),
    width = 2,
    selectionWidth = 3
  ) %>%
  visLegend(width = 0.15, addNodes = legendNodes, useGroups = FALSE)
vis.pa.alpha

# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph/results")
# visSave(vis.pa.alpha, file = "vis-pa-alpha.html")
# setwd("G:/I-Family/Auswertung/Papers/DAG_overall_model_FORAITA/2020/cohort_causal_graph")




# --- !SAVE ------------------------------------------------------------------------------
save(nodes.pa, nodes.mvpa, nodes.sem, nodes.boot, nodes.twd, nodes.pa.alpha,
     edges.pa, edges.mvpa, edges.twd, edges.sem, edges.boot, edges.pa.alpha,
     legendNodes, legendNodes.mvpa,
     file = "data_not_load/vis_pa.RData")


#setwd("C:/Users/foraita/Documents/BIPS/software/cohort_causal_graph/results")
visSave(vis.pa, file = "results/vis-pa.html")
visSave(vis.mvpa, file = "results/vis-mvpa.html")
visSave(vis.twd, file = "results/vis-twd.html")
visSave(vis.sem, file = "results/vis-sem.html")
visSave(vis.sem, file = "results/vis-pa-alpha.html")
visSave(vis.boot.75, file = "results/vis-boot_75perc.html")
visSave(vis.boot.60, file = "results/vis-boot-60perc.html")
visSave(vis.boot.35, file = "results/vis-boot-35perc.html")
visSave(vis.boot.44, file = "results/vis-boot-44perc.html")
#setwd("C:/Users/foraita/Documents/BIPS/software/cohort_causal_graph/")



# setEPS()
# postscript("results/vispa.eps")
# vis.pa
# dev.off()



