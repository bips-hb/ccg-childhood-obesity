# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    Apr 2023
#
# Purpose: Create Pathways
#
# ------------------------------------------------------------------------------
library(gt)
library(gtsummary)

# load data
boot <- readRDS("data_not_load/boot100_mi1-graph.RDS")
load("data_not_load/06_graph-10trees-pa.RData")

delete_undirected_edges <- function(adm){
  x <- cbind(as.vector(adm), as.vector(t(adm)))
  y <- apply(x, 1, sum)
  print(table(y))
  u <- which(y == 4)
  print(paste0("Number of undirected edges: ", length(u)))

  adm[u] <- 0
  adm
}

# Data management ------------------------------------------------------------------------
g1 <- pc2igraph(g.pa)[[1]]
g1 <- feature.igraph(g1, labels = V(g1)$name)
gb <- lapply(boot, function(x){
             tmp <- pc2igraph(x)[[1]]
             feature.igraph(tmp, labels = V(tmp)$name)})
knoten <- g.pa@graph@nodes


# 0: no edge
# 1: directed edge
# 2: undirected edge -> set to 0
g1.undirected <-  wgtMatrix(getGraph(g.pa), transpose = FALSE)
tmp <- delete_undirected_edges(g1.undirected)
g1.undirected <- graph_from_adjacency_matrix(tmp)

# # only for test purposes
# g1.el <- as_edgelist(g1)
# g1.el <- data.frame(A = paste(g1.el[,1], g1.el[,2], sep="-"))
# g1.un.el <- as_edgelist(g1.undirected)
# g1.un.el <- data.frame(A = paste(g1.un.el[,1], g1.un.el[,2], sep="-"), B = 1)
# gg <- merge(g1.el, g1.un.el, by = "A", all = TRUE)
# gg[is.na(gg$B),]

# boot[[100]]
# # 0     1      2   4
# # 2335  250    4   12
# table(wgtMatrix(getGraph(boot[[100]]), transpose = FALSE))
# as_edgelist(gb.undirected[[100]])  #125+4

gb.undirected <- lapply(boot, function(x){
                    graph_from_adjacency_matrix(
                      delete_undirected_edges(
                        wgtMatrix(getGraph(x), transpose = FALSE)
                        )
                      )
                   })

# # Average matrix without undirected edges
#
# boot.adm <- lapply(boot, function(x){
#                     wgtMatrix(getGraph(x),transpose = FALSE)})
# boot.adm.dir <- boot.adm <- Reduce('+', boot.adm)
# tt <- t(boot.adm)
# boot.adm.dir[upper.tri(boot.adm.dir)] <-
#      boot.adm.dir[upper.tri(boot.adm.dir)] > tt[upper.tri(boot.adm.dir)]
# boot.adm.dir[lower.tri(boot.adm.dir)] <-
#      boot.adm.dir[lower.tri(boot.adm.dir)] > tt[lower.tri(boot.adm.dir)]
# boot.adm.dir[15:25, 15:25]
# g.adm <- graph_from_adjacency_matrix(boot.adm.dir)
# a <- as.data.frame(get.edgelist(g.adm))
# filter(a, V1 %in% c("media.0", "media.1", "wb.1", "homa.1", "homa.2", "bmi.2") &
#           V2 %in% c("media.0", "media.1", "wb.1", "homa.1", "homa.2", "bmi.2"))
#
# plot(induced_subgraph(g.adm, vids = which(knoten %in% c("media.0", "media.1", "wb.1", "homa.1", "homa.2", "bmi.2"))))




# set.seed(6514)
# mat <- list(matrix(sample(1:20, 16), ncol = 4, nrow = 4),
#               matrix(sample(1:20, 16), ncol = 4, nrow = 4),
#               matrix(sample(1:20, 16), ncol = 4, nrow = 4))
# mat.old <- mat <- Reduce('+', mat)
# diag(mat.old) <- diag(mat) <- 0
# tt <- t(mat)
# mat[upper.tri(mat)] <-
#   mat[upper.tri(mat)] > tt[upper.tri(mat)]
# mat[lower.tri(mat)] <-
#   mat[lower.tri(mat)] > tt[lower.tri(mat)]


# Original graph -------------------------------------------------------------------------
makepfade <- function(graph, boot, FROM, TO){
  if(class(graph) != "igraph") stop("object graph must be an igraph")
  if(!is.list(boot)) stop("object boot must be a list")
  if(any(sapply(boot, function(x) class(x) != "igraph"))) stop("all boot list elements must be igraph objects")

  #if(class(graph) == "igraph"){
     # ---   shortest paths  ---
     sp <- unlist(shortest_paths(graph, from = FROM, to = TO, mode = "out")$vpath)
     sp_chr <- paste(sp, collapse="-")

     # ---   all simple paths  ---
     ap <- all_simple_paths(graph, from = FROM, to = TO, mode = "out")
     ap_chr <- unlist(map(ap, paste, collapse="-"))

     gp <- list("sp" = sp, "sp_chr" = sp_chr, "ap" = ap, "ap_chr" = ap_chr)
  # }
  #
  # if(is.list(graph)){
    # ---   shortest paths  ---
    sp_b <- lapply(boot, function(x)
            try(shortest_paths(x, from = FROM, to = TO, mode = "out")$vpath,
            silent = TRUE))
    sp_b_ul <- lapply(sp_b, unlist)
    # delete graphs without a path
    sp_b_short <- discard(sp_b_ul,
                        function(x) length(x) == 0)

    sp_b_chr <- map(sp_b_short, paste, collapse="-")

    # ---   all simple paths  ---
    # list of all simple path for all BGs (list length 100)
    ap_b <- lapply(boot, function(x)
              try(all_simple_paths(x, from = FROM, to = TO, mode = "out"),
              silent = TRUE))
    # list of all paths (no assignment to BG)
    ap_b_short <- flatten(discard(ap_b, function(x) length(x) == 0))
    ap_b_chr   <- map(ap_b_short, paste, collapse="-")
    # vector of all paths (no assignment to BG)
    ap_b_chr_v <- unlist(ap_b_chr)


    bp <- list("sp_b" = sp_b, "sp_b_short" = sp_b_short, "sp_b_chr" = sp_b_chr,
               "ap_b" = ap_b,  "ap_b_short" = ap_b_short, "ap_b_chr" = ap_b_chr,
               "ap_b_chr_v" = ap_b_chr_v)
  # }
    list("ccg" = gp, "boot" = bp)

}


descr_paths <- function(liste){
 browser()
  out.mat <- matrix(NA, nrow = 12, ncol = 2)
  row.names(out.mat) <- c("CCG: Shortest path",
                          "CCG: Number of all paths",
                          "Boot: Number of graphs with any path",
                          "Boot: Number of (unique) paths",
                          "Boot: Median (IQR) number of paths in each BG",
                          "Boot: Median (IQR) path length",
                          "How often is the CCG shortest path in any (shortest) bootstrap?",
                          "","","","","")

  # Shortest path in original path
  rownames(out.mat)[1] <- paste("CCG-SP: ",
                                paste0(
                                  knoten[as.numeric(unlist(str_split(liste$ccg$sp_chr, "-")))],
                                  collapse = " > "))
  out.mat[1,1] <- 1

  # Number of paths in original graph
  out.mat[2,1] <- length(liste$ccg$ap_chr)

  # Number of BG with any path
  out.mat[3,1] <- length(liste$boot$sp_b_short)
  # Number of Bootstrap paths
  out.mat[4,1] <- length(liste$boot$ap_b_chr_v)
  # Number of unique Bootstrap paths
  out.mat[4,2] <- length(unique(liste$boot$ap_b_chr_v))
  # Median (IQR) number of paths in each BG
  out.mat[5,1] <- median(sapply(liste$boot$ap_b, length)[sapply(liste$boot$ap_b, length) != 0])
  out.mat[5,2] <- IQR(sapply(liste$boot$ap_b, length)[sapply(liste$boot$ap_b, length) != 0])
  # Median path length and IQR in Bootstrap
  out.mat[6,1] <- median(sapply(liste$boot$ap_b_short, length))
  out.mat[6,2] <- IQR(sapply(liste$boot$ap_b_short, length))

  ### how often is the shortest path in the any bootstrap path?
  tab_allpaths <- table(liste$boot$ap_b_chr_v)
#  out.mat[7,1] <- as.vector(tab_allpaths[which(names(tab_allpaths) == liste$ccg$sp_chr)])
  ### how often is the shortest path in the bootstrap shortest paths?
  tab_spaths <-  table(unlist(liste$boot$sp_b_chr))
#  out.mat[7,2] <- as.vector(tab_spaths[which(names(tab_spaths) == liste$ccg$sp_chr)])

  ### which shortest path is the most frequent one?
  pfadname <- which(tab_spaths == max(tab_spaths))
  out.mat[8,1] <- as.vector(tab_spaths[pfadname])[1]
  tmp <- paste("Boot max% ShoP: ",
               paste0(
                 knoten[as.numeric(unlist(str_split(names(pfadname), "-")))],
                 collapse = " > "))
  rownames(out.mat)[8] <- str_replace(tmp, "bmi.2 >", "bmi.2;")

  ### which simple path is the most frequent one?
  pfadname <- which(tab_allpaths == max(tab_allpaths))
  out.mat[9,1] <- as.vector(tab_allpaths[pfadname])[1]
  rownames(out.mat)[9] <- paste("Boot max% AnyP: ",
                                paste0(
                                knoten[as.numeric(unlist(str_split(names(pfadname), "-")))],
                                collapse = " > "))

  # 3  most visited nodes on any bootstrap graph
  # browser()
  freq_nodes <- sort(table(unlist(liste$boot$ap_b_short)), decreasing = TRUE)
  freq_nodes <- freq_nodes[3:5]
  names(freq_nodes) <- knoten[as.numeric(names(freq_nodes))]
  perc_nodes <- round(freq_nodes * 100 / length(liste$boot$ap_b_chr_v),0)
  out.mat[10,] <- c(freq_nodes[1], perc_nodes[1])
  out.mat[11,] <- c(freq_nodes[2], perc_nodes[2])
  out.mat[12,] <- c(freq_nodes[3], perc_nodes[3])
  rownames(out.mat)[10:12] <- paste(names(freq_nodes), " N(%)")

  out.mat
}



p_avm   <- makepfade(g1, gb, FROM = "media.0", TO = "bmi.2")
p_avm_un   <- makepfade(g1, gb.undirected, FROM = "media.0", TO = "bmi.2")
p_pa    <- makepfade(g1, gb, FROM = "pa.0", TO = "bmi.2")
p_pa_un    <- makepfade(g1, gb.undirected, FROM = "pa.0", TO = "bmi.2")
p_sleep <- makepfade(g1, gb, FROM = "sleep.0", TO = "bmi.2")
p_sleep_un <- makepfade(g1, gb.undirected, FROM = "sleep.0", TO = "bmi.2")
p_wb    <- makepfade(g1, gb, FROM = "wb.0", TO = "bmi.2")
p_wb_un    <- makepfade(g1, gb.undirected, FROM = "wb.0", TO = "bmi.2")
p_yhei  <- makepfade(g1, gb, FROM = "yhei.0", TO = "bmi.2")
p_yhei_un  <- makepfade(g1, gb.undirected, FROM = "yhei.0", TO = "bmi.2")

t_avm  <- descr_paths(p_avm)
t_avm_un <- descr_paths(p_avm_un)
t_pa   <- descr_paths(p_pa)
t_pa_un   <- descr_paths(p_pa_un)
t_sleep<- descr_paths(p_sleep)
t_sleep_un<- descr_paths(p_sleep_un)
t_wb   <- descr_paths(p_wb)
t_wb_un   <- descr_paths(p_wb_un)
t_yhei <- descr_paths(p_yhei)
t_yhei_un <- descr_paths(p_yhei_un)

# save(t_yhei, file = "data_not_load/oslo_pfad_yhei.RData")

# RTF output -----------------------------------------------------------------------------
data.frame(V0 = rownames(t_avm),
           as.data.frame(t_avm, row.names = FALSE),
           V1 = rownames(t_avm_un),
           as.data.frame(t_avm_un, row.names = FALSE)) %>%
  as_tibble() %>% gt() %>%
  cols_label(V0 = md("**AVM**"),
             V1 = md("**N**"),
             V2 = md("**N/%**"),
             V1.1 = md("**directed**"),
             V1.2 = md("**N**"),
             V2.1 = md("**N, %**")) %>%
  sub_missing(columns = c(2,3,5,6), missing_text = "-") %>%
  gtsave("paths_avm.rtf", path = "results/")

data.frame(V0 = rownames(t_pa),
           as.data.frame(t_pa, row.names = FALSE),
           V1 = rownames(t_pa_un),
           as.data.frame(t_pa_un, row.names = FALSE)) %>%
  as_tibble() %>% gt() %>%
  cols_label(V0 = md("**Physical Activity**"),
             V1 = md("**N**"),
             V2 = md("**N/%**"),
             V1.1 = md("**directed**"),
             V1.2 = md("**N**"),
             V2.1 = md("**N, %**")) %>%
  sub_missing(columns = c(2,3,5,6), missing_text = "-") %>%
  gtsave("paths_pa.rtf", path = "results/")

data.frame(V0 = rownames(t_sleep),
           as.data.frame(t_sleep, row.names = FALSE),
           V1 = rownames(t_sleep_un),
           as.data.frame(t_sleep_un, row.names = FALSE)) %>%
  as_tibble() %>% gt() %>%
  cols_label(V0 = md("**Sleep**"),
             V1 = md("**N**"),
             V2 = md("**N/%**"),
             V1.1 = md("**directed**"),
             V1.2 = md("**N**"),
             V2.1 = md("**N, %**")) %>%
  sub_missing(columns = c(2,3,5,6), missing_text = "-") %>%
  gtsave("paths_sleep.rtf", path = "results/")

data.frame(V0 = rownames(t_wb),
           as.data.frame(t_wb, row.names = FALSE),
           V1 = rownames(t_wb_un),
           as.data.frame(t_wb_un, row.names = FALSE)) %>%
  as_tibble() %>% gt() %>%
  cols_label(V0 = md("**Well-being**"),
             V1 = md("**N**"),
             V2 = md("**N/%**"),
             V1.1 = md("**directed**"),
             V1.2 = md("**N**"),
             V2.1 = md("**N, %**")) %>%
  sub_missing(columns = c(2,3,5,6), missing_text = "-") %>%
  gtsave("paths_wb.rtf", path = "results/")

data.frame(V0 = rownames(t_yhei),
           as.data.frame(t_yhei, row.names = FALSE),
           V1 = rownames(t_yhei_un),
           as.data.frame(t_yhei_un, row.names = FALSE)) %>%
  as_tibble() %>% gt() %>%
  cols_label(V0 = md("**YHEI**"),
             V1 = md("**N**"),
             V2 = md("**N/%**"),
             V1.1 = md("**directed**"),
             V1.2 = md("**N**"),
             V2.1 = md("**N, %**")) %>%
  sub_missing(columns = c(2,3,5,6), missing_text = "-") %>%
  gtsave("paths_yhei.rtf", path = "results/")


# ----------------------------------------------------------------------------------------

# how many edges have the BGs on average?
my.pc.print(g.pa)
apply(sapply(boot, my.pc.print), 1, mean)
