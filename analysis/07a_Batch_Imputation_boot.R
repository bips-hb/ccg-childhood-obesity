# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Bootstrap Data imputation
#
# ------------------------------------------------------------------------------
library(mice)
library(batchtools)

set.seed(5463)
pfad <- "/CausalGraph"
postfix <- "BOOTruns100.rds"



# Registry  --------------------------------------------------------------------
reg_name <- "ccg-boot100.dbp"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE)
myreg <- makeRegistry(file.dir = reg_dir)
#myreg <- batchtools::loadRegistry(reg_dir, writeable = FALSE)


# Function   -------------------------------------------------------------------
batch_mice <- function(m){
              library(mice)
              imputation_data <- readRDS( "/CausalGraph/data/MI-input.RDS")
              ### Bootstrap
              bootsample <- sample(nrow(imputation_data),
                                   nrow(imputation_data),
                                   replace = TRUE)
              imp <- mice::mice(data = imputation_data[bootsample,], m = m, meth = 'rf')
              imp
		          }


# Create jobs ----------------------------------------------------------------
ids <-   batchMap(batch_mice,
                  m = rep(10,100),
                  reg = myreg)


# Test run -----------------------------------------------------------
#testJob(id = 1)


# Submit -----------------------------------------------------------
# ccg-boot100.dbp
ids <- findNotStarted()
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotDone()
  ids[, chunk := chunk(job.id, chunk.size = 20)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
  				                    memory = 6000, walltime = 10*24*3600,
  							  max.concurrent.jobs = 100))
} else {
  submitJobs()
}
waitForJobs()

# Results   --------------------------------------------------------------------
res <- reduceResultsList()

# Save   -----------------------------------------------------------------------
saveRDS(res, file = file.path(pfad, "data", postfix))

