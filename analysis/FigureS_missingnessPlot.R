# ----------------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Figure Missingness Plot
#
# ----------------------------------------------------------------------------------------
load("data/graph.RData")


### Inspect imputed values

pdf(file = "results/MI_diagnostics.pdf")
mice::densityplot(daten.pa.mids)
mice::bwplot(daten.pa.mids)
mice::stripplot(daten.pa.mids)
dev.off()