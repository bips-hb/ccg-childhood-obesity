# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Create Table 2
#
# ------------------------------------------------------------------------------
library(gt)
library(gtsummary)
library(glue)
load("data/graph-10trees.RData")
load("data/rmseu.RData")

gii <- pc2igraph(g.pa)

gii1 <- feature.igraph(gii[[1]], labels = V(gii[[1]])$name)
gii2 <- feature.igraph(gii[[2]], labels = V(gii[[2]])$name)
singeltons <- setdiff(V(gii1)$name, V(gii2)$name)

# articulation points
ap1 <- articulation_points(gii1)
ap2 <- articulation_points(gii2)

# diameter
diam1 <- get_diameter(gii1)
diam2 <- get_diameter(gii2)

neighb2 <- V(gii2)$name[which(ego_size(gii2, order = 1, V(gii2), mindist = 1) ==
                        max(ego_size(gii2, order = 1, V(gii2), mindist = 1)))]


gd <- graph.descriptives(gii2)
gdt <- tibble(
         characteristic = rownames(gd),
         val = gd[,1])




# ----------------------------------------------------------------------------
#       TWD, SEM, Bootstrap, MI0.1
# ----------------------------------------------------------------------------
load("data/graph-twd.RData")
load("data/graph-sem-hc_bic-cg.RData")
load("data/boot100igraph.RData")
load("data/graph-10trees.RData")
load("data/graph-10trees-alpha.RData")

gii <- pc2igraph(g.pa)
bn.pa  <- bnlearn::as.bn(gii[[1]])

V(g.boot)$name <- V(gii[[1]])$name
g.bot <- g.boot %>% delete_edges(which(E(g.boot)$weight < 44))
g.bot <- feature.igraph(g.bot, labels = V(g.bot)$name)
(bn.bot <- bnlearn::as.bn(g.bot))
g.b50 <- g.boot %>% delete_edges(which(E(g.boot)$weight < 50))
g.b50 <- feature.igraph(g.b50, labels = V(g.b50)$name)
(bn.b50 <- bnlearn::as.bn(g.b50))
g.b75 <- g.boot %>% delete_edges(which(E(g.boot)$weight < 75))
g.b75 <- feature.igraph(g.b75, labels = V(g.b75)$name)
(bn.b75 <- bnlearn::as.bn(g.b75))


g.sem <- bnlearn::as.igraph(sem)
g.sem <- feature.igraph(g.sem, labels = V(g.sem)$name)

gtw <- pc2igraph(g.tw)
gtw <- feature.igraph(gtw[[1]], labels = V(gtw[[1]])$name)
bn.twd <- bnlearn::as.bn(gtw)

g.mi <- pc2igraph(g.pa.alpha)
g.mi <- feature.igraph(g.mi[[1]], labels = V(g.mi[[1]])$name)
bn.mi <- bnlearn::as.bn(g.mi)


# diameter
diam.twd <- get_diameter(gtw)
diam.sem <- get_diameter(g.sem)
diam.bot <- get_diameter(g.bot)
diam.b50 <- get_diameter(g.b50)
diam.b75 <- get_diameter(g.b75)
diam.mi <- get_diameter(g.mi)

neighb.twd <- V(gtw)$name[which(ego_size(gtw, order = 1, V(gtw), mindist = 1) ==
                            max(ego_size(gtw, order = 1, V(gtw), mindist = 1)))]
neighb.sem <- V(g.sem)$name[which(ego_size(g.sem, order = 1, V(g.sem), mindist = 1) ==
                            max(ego_size(g.sem, order = 1, V(g.sem), mindist = 1)))]
neighb.bot <- V(g.bot)$name[which(ego_size(g.bot, order = 1, V(g.bot), mindist = 1) ==
                                    max(ego_size(g.bot, order = 1, V(g.bot), mindist = 1)))]
neighb.b50 <- V(g.b50)$name[which(ego_size(g.b50, order = 1, V(g.b50), mindist = 1) ==
                                    max(ego_size(g.b50, order = 1, V(g.b50), mindist = 1)))]
neighb.b75 <- V(g.b75)$name[which(ego_size(g.b75, order = 1, V(g.b75), mindist = 1) ==
                                    max(ego_size(g.b75, order = 1, V(g.b75), mindist = 1)))]
neighb.mi <- V(g.mi)$name[which(ego_size(g.mi, order = 1, V(g.mi), mindist = 1) ==
                                    max(ego_size(g.mi, order = 1, V(g.mi), mindist = 1)))]


# Hamming distance and structural Haming distance
hamming.twd <- bnlearn::hamming(bn.pa, bn.twd)
hamming.sem.old <- bnlearn::hamming(bn.pa, sem.old)
hamming.sem <- bnlearn::hamming(bn.pa, sem)
hamming.bot <- bnlearn::hamming(bn.pa, bn.bot)
hamming.b50 <- bnlearn::hamming(bn.pa, bn.b50)
hamming.b75 <- bnlearn::hamming(bn.pa, bn.b75)
hamming.mi <- bnlearn::hamming(bn.pa, bn.mi)
shd.twd <- bnlearn::shd(bn.pa, bn.twd)
shd.sem <- bnlearn::shd(bn.pa, sem)
shd.bot <- bnlearn::shd(bn.pa, bn.bot)
shd.b50 <- bnlearn::shd(bn.pa, bn.b50)
shd.b75 <- bnlearn::shd(bn.pa, bn.b75)
shd.mi <- bnlearn::shd(bn.pa, bn.mi)

#twd
gd <- graph.descriptives(gtw)
rn <- rownames(gd)
gd2 <- matrix(NA, nrow = 17, ncol = 1)
gd2[1:15,1] <- gd
gd2[16,1] <- hamming.twd
gd2[17,1] <- shd.twd
rownames(gd2) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.twd <- tibble(
  characteristic = rownames(gd2),
  val = gd2[,1])

# sem
gd <- graph.descriptives(g.sem)
gd[7,] <- nrow(sem$arcs)
gd[8,] <- 0
gd2 <- matrix(NA, nrow = 17, ncol = 1)
gd2[1:15,1] <- gd
gd2[16,1] <- hamming.sem
gd2[17,1] <- shd.sem
rownames(gd2) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.sem <- tibble(
  characteristic = rownames(gd2),
  val = gd2[,1])

# mi
gd <- graph.descriptives(g.mi)
rn <- rownames(gd)
gd2 <- matrix(NA, nrow = 17, ncol = 1)
gd2[1:15,1] <- gd
gd2[16,1] <- hamming.mi
gd2[17,1] <- shd.mi
rownames(gd2) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.mi <- tibble(
  characteristic = rownames(gd2),
  val = gd2[,1])


# boot
gd <- graph.descriptives(g.bot)
rn <- rownames(gd)
gd2 <- matrix(NA, nrow = 17, ncol = 1)
gd2[1:15,1] <- gd
gd2[16,1] <- hamming.bot
gd2[17,1] <- shd.bot
rownames(gd2) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.bot <- tibble(
  characteristic = rownames(gd2),
  val = gd2[,1])

# boot 50
gd <- graph.descriptives(g.b50)
rn <- rownames(gd)
gd2 <- matrix(NA, nrow = 17, ncol = 1)
gd2[1:15,1] <- gd
gd2[16,1] <- hamming.b50
gd2[17,1] <- shd.b50
rownames(gd2) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.b50 <- tibble(
  characteristic = rownames(gd2),
  val = gd2[,1])

# boot 75
gd <- graph.descriptives(g.b75)
rn <- rownames(gd)
gd2 <- matrix(NA, nrow = 17, ncol = 1)
gd2[1:15,1] <- gd
gd2[16,1] <- hamming.b75
gd2[17,1] <- shd.b75
rownames(gd2) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.b75 <- tibble(
  characteristic = rownames(gd2),
  val = gd2[,1])

# Average bootstrap graph
boot <- readRDS("data_not_load/boot100_mi1-graph.RDS")
bi <- lapply(boot, function(x){
              tmp <- pc2igraph(x)[[1]]
              feature.igraph(tmp)} )
bn.bi <- lapply(boot, function(x){
                  bnlearn::as.bn(x@graph, check.cycles = FALSE)})

tmp <- do.call(cbind, lapply(bi, graph.descriptives))
# tmp <- rbind(tmp, tmp[7,] - tmp[8,])
# rownames(tmp)[16] <- "Number of directed edges"
b.mean <- apply(tmp, 1, mean, na.rm = TRUE)

gd.boot <- matrix(NA, nrow = 17, ncol = 1)
gd.boot[1:15,1] <- b.mean
gd.boot[16,1] <- mean(sapply(bn.bi, function(x){bnlearn::hamming(bn.pa, x)}))
gd.boot[17,1] <- mean(sapply(bn.bi, function(x){bnlearn::shd(bn.pa, x)}))
rownames(gd.boot) <- c(rn, "Hamming distance", "Structural Hamming distance")

gd.boot <- tibble(
  characteristic = rownames(gd.boot),
  val = gd.boot[,1])



## add rmseu
  gdt[16:17,1:2] <- NA
  gdt[16:17,1] <- c("Hamming distance", "Structural Hamming distance")
  gd.alle2 <- bind_cols(gdt, gd.mi[,2], gd.twd[,2], gd.sem[,2], gd.bot[,2],
                        gd.b50[,2], gd.b75[,2], gd.boot[,2])
  names(gd.alle2)[2:9] <- c("val.main","val.mi","val.twd", "val.sem", "val.boot44",
                            "val.boot50", "val.boot75", "avg.boot")

  gd.alle2[18,1] <- "Mean edge uncertainty"
  gd.alle2[18,6] <- round(rmseu44$meu * 100, 1)
  gd.alle2[18,7] <- round(rmseu50$meu * 100, 1)
  gd.alle2[18,8] <- round(rmseu75$meu * 100, 1)
  gd.alle2[18,9] <- round(rmseu0$meu * 100, 1)


table.alle2 <-
    gd.alle2[c(7,8,11,9,5,4,16,17,18),] %>% gt %>%
    fmt_number(
      columns = 2:9,
      rows = c(1,2,4,6:8),
      decimals = 0
    ) %>%
    fmt_number(
      columns = 2:9,
      rows = c(3,5,9),
      decimals = 1
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam2), " > "),
      locations = cells_body(columns = 2, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam.mi), " > "),
      locations = cells_body(columns = 3, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam.twd), " > "),
      locations = cells_body(columns = 4, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam.sem), " > "),
      locations = cells_body(columns = 5, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam.bot), " > "),
      locations = cells_body(columns = 6, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam.b50), " > "),
      locations = cells_body(columns = 7, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(names(diam.b75), " > "),
      locations = cells_body(columns = 8, rows = 6)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb2), ", "),
      locations = cells_body(columns = 2, rows = 4)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb.mi), ", "),
      locations = cells_body(columns = 3, rows = 4)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb.twd), ", "),
      locations = cells_body(columns = 4, rows = 4)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb.sem), ", "),
      locations = cells_body(columns = 5, rows = 4)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb.bot), ", "),
      locations = cells_body(columns = 6, rows = 4)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb.b50), ", "),
      locations = cells_body(columns = 7, rows = 4)
    ) %>%
    tab_footnote(
      footnote = glue_collapse(sort(neighb.b75), ", "),
      locations = cells_body(columns = 8, rows = 4)
    ) %>%
    cols_label(
      characteristic = md("**Characteristics**"),
      val.main   = md("**Main**"),
      val.mi     = md("**MI**"),
      val.twd    = md("**TWD**"),
      val.sem    = md("**SEM**"),
      val.boot44 = md("**BOOT44**"),
      val.boot50 = md("**BOOT50**"),
      val.boot75 = md("**BOOT75**"),
      avg.boot = md("**Avg.BOOT**")
    ) %>%
    cols_align(
      align = "right", columns = c("val.main","val.mi","val.twd", "val.sem", "val.boot44",
                                   "val.boot50", "val.boot75",  "avg.boot"))

table.alle2




### save table ---------------------------------------------------------------
  save(gd.alle2, file = "data/table_network.RData")
  table.alle2 %>% gtsave("table_network.rtf", path = "results/")
# ----------------------------------------------------------------------------




