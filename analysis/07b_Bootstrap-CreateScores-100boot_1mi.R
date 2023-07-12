# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Built scores on multiple data
#
# ------------------------------------------------------------------------------
rm(list = ls())
datapath <- "/registries/ccg-boot100.dbp/results/"

files.sources <- list.files(path = "/ccg/R")
files.sources <- paste0("/ccg/R/", files.sources)
sapply(files.sources, source)


for(nummer in 1:100){
  x <- readRDS(paste0(datapath, nummer, ".rds"))
  x <- mice::complete(x, action = "long", include = FALSE)

  x <- makescores(x)
  daten <- datenmanagement(x)

  el <- grep("_el", names(daten))
  t0 <- grep("_t0", names(daten))
  t1 <- grep("_t1", names(daten))
  t3 <- grep("_t3", names(daten))

  names(daten) <- gsub("_t0", ".0", names(daten))
  names(daten) <- gsub("_t1", ".1", names(daten))
  names(daten) <- gsub("_t3", ".2", names(daten))
  names(daten) <- gsub("_el", "", names(daten))

  daten.list <- split(daten, daten$.imp)

### ! SAVE ---------------------------------------------------------------------
  saveRDS(daten.list[[1]], file = paste0("/CausalGraph/boot-mi1/", nummer, ".rds"))
}
