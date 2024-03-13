# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    Sep 2020
#
# Purpose: Data imputation
#
# ------------------------------------------------------------------------------
# --- was bisher geschah: ------------------------------------------------------
# source("data/01_DataPreparation.R")
# source("data/02_CreateAnalysisData.R)
analysis_data <- readRDS("data_not_load/02_data2impute.RDS")
# ------------------------------------------------------------------------------

# ---------------------   Missing Value Imputation   ---------------------------
# 1) Patterns & dump variables (such as constants, id_no, etc.)
outlist1 <- c("id_no", "ifam_no", "family_id")
#pattern <- md.pattern(analysis_data)
#saveRDS(pattern, "data/missingness-pattern.RDS")
ini <- mice(dplyr::select(analysis_data, -all_of(outlist1)), meth = 'rf', maxit = 0)
#table(ini$nmis)
names(ini$nmis[ini$nmis == 0])
names(ini$nmis[ini$nmis > nrow(analysis_data)/2])


# 2) Logged events
ini$loggedEvents
outlist2 <- c(outlist1, as.character(ini$loggedEvents[,"out"]))


# 3)  Outflux (which variables should be used for imputation)
fx <- flux(analysis_data)
# delete:
sort(fxout <- row.names(fx)[fx$outflux < 0.25])
tmp <- analysis_data[, !names(analysis_data) %in% fxout]
fx2 <- flux(tmp)
sort(fxout2 <- row.names(fx2)[fx2$outflux < 0.25])
sort(c(fxout, fxout2))

outlist <- sort(c(outlist1, outlist2, fxout, fxout2, "drink_9_t3.1"))
tmp <- analysis_data[, !names(analysis_data) %in% outlist3]

fx3 <- flux(tmp)
plot(fx3$outflux, fx3$influx)



# 4) Remove variables & quickpred
# quickpred is not used for graphical model imputation
# new.data <- tmp[, !names(tmp) %in% outlist3]
saveRDS(outlist, file = "data_not_load/outlist.RDS")


# 2) Check methods for imputation
#make.method(new.data)



# 6) Imputation (1 ca. 10 min, 50 ca. 30 sec)
#system.time(
imp <- mice(new.data, m = 10, meth = 'rf', seed = 16873, print = TRUE)
#)

# --- SAVE! --------------------------------------------------------------------
saveRDS(imp, "data/03_MI-10.RDS")
saveRDS(new.data, "data_not_load/03_MI-input.RDS")
# ------------------------------------------------------------------------------

# 7) Check logged events
as.character(imp$loggedEvents[, "out"])




