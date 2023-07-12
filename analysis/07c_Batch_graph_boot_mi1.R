------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Bootstrap Data imputation
#
# ------------------------------------------------------------------------------
library(batchtools)

# Function   -------------------------------------------------------------------

batch_bgraph <- function(nummer){
                  library(pcalg)
                  library(Rfast)
                  library(graph)
                  files.sources <- list.files(path = "/ccg/R")
                  files.sources <- paste0("/ccg/R/", files.sources)
                  sapply(files.sources, source)

                  ### Data
                  data <- readRDS(paste0("/CausalGraph/boot-mi1/",
                                         nummer, ".rds"))
                  data <- data[,-c(1,2)]

                  ### Levels, Tiers
                  load("/CausalGraph/data/bt_sonst.RData")

                  ss <- getSuff(data,
                                test = "flexCItest",
                                adaptDF = FALSE)

                  bg <- tpc( suffStat = ss,
                             indepTest = flexCItest,
                             skel.method = "stable",
                             alpha = 0.05,
                             labels = V.pa,
                             forbEdges = fg.pa,
                             verbose = FALSE,
                             tiers = ebenen.pa)
                 bg
		          }



# Registry  --------------------------------------------------------------------
reg_name <- "ccg-boot100_mi1-graph.dbp"
reg_dir <- file.path("/registries", reg_name)
#unlink(reg_dir, recursive = TRUE)
#myreg <- makeRegistry(file.dir = reg_dir)
myreg <- batchtools::loadRegistry(reg_dir, writeable = TRUE)


# Create jobs ----------------------------------------------------------------
 ids <- batchMap(batch_bgraph,
                 nummer = 1:100)

# Test run -----------------------------------------------------------
# testJob(id = 1)

# Submit -----------------------------------------------------------
# walltime in seconds, 10 days max, memory in MB
n_c <- 1 # number of cpus
risurzes <- list(name = reg_name,
                 chunks.as.arrayjobs = TRUE,
                 ncpus = n_c,
                 memory = 6000,
                 max.concurrent.jobs = 100,
                 walltime = 10*24*3600)


if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotDone()
#   ids <- findErrors()
    ids[, chunk := chunk(job.id, chunk.size = 20)]
    submitJobs(ids = ids, resources = risurzes, reg = myreg)
} else {
  submitJobs(ids = ids, reg = myreg)
}


waitForJobs()
getErrorMessages()

# Results   --------------------------------------------------------------------
res <- reduceResultsList()

# Save   -----------------------------------------------------------------------
postfix <- "boot100_mi1-graph.rds"
saveRDS(res, file = file.path("/CausalGraph/data", postfix))


