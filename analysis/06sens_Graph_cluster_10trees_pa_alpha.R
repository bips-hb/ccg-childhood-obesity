# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Recover graph based on data, not on residuals, rf trees = 10, alpha = 0.1
#
# ------------------------------------------------------------------------------
# Load Data and libraries    ---------------------------------------------------
rm(list = ls())

load("graph.RData")
 library(micd)
 library(parallel)
 library(snow)
 library(Rmpi)


 my_load_function <- function(){
   setwd("/home/foraita/ccg/R")
   files.sources <- list.files()
   sapply(files.sources, source)
   setwd("/home/foraita/CausalGraph")
 }
 my_load_function()

## suffienct statistic
suff.all <- getSuff(daten.pa.mids, test = "flexMItest")



# all tiers
numCores <- detectCores()
  g.pa.alpha <- tpc(suffStat = suff.all,
                   indepTest = flexMItest,
                 skel.method = "stable.parallel",
                       label = V.pa,
                       alpha = 0.1,
                     verbose = FALSE,
                       tiers = ebenen.pa,
                   forbEdges = fg.pa,
                    numCores = numCores)

save(g.pa, file = "graph-10trees-alpha.RData")


