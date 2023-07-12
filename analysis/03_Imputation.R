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

analysis_data <- readRDS("data/data2impute.rds")


# ---------------------   Missing Value Imputation   ---------------------------
# 1) Patterns & dump variables (such as constants, id_no, etc.)
outlist1 <- c("id_no", "ifam_no", "family_id")
ini <- mice(dplyr::select(analysis_data, -all_of(outlist1)), meth = 'rf', maxit = 0)
table(ini$nmis)
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
# plot(fx3$outflux, fx3$influx)

# 4) Imputation (1 ca. 10 min, 50 ca. 30 sec)
#system.time(
imp <- mice(new.data, m = 10, meth = 'rf', seed = 16873, print = TRUE)
#)

# 7) Check logged events
as.character(imp$loggedEvents[, "out"])


# --- SAVE! --------------------------------------------------------------------
saveRDS(imp, "data/MI-10.rds")
saveRDS(new.data, "data_not_load/MI-input.rds")
# ------------------------------------------------------------------------------



