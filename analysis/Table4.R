# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    JUL 2021
#
# Purpose: Create Table 4
#
# ------------------------------------------------------------------------------
library(gt)
library(gtsummary)
library(glue)
load("data_not_load/06_graph-10trees-pa.RData")
# load("data_not_load/07_boot-pa.RData")
# load("data_not_load/07_boot100igraph.RData")
load("data_not_load/06_graph-10trees-pa.RData")

g <- pc2igraph(g.pa)
g1 <- feature.igraph(g[[1]], labels = V(g[[1]])$name)

V(g1)$name <- c("Sex", "Country", "Migrant", "Income", "ISCED", "Age_at_birth",
                "Breastfeeding", "Birthweight", "Weeks_of_pregnancy", "Formula_milk",
                "HH_Diet", "Smoking_pregnancy",
                "Age", "School", "AVM", "BMI", "Mothers_BMI", "Familymeal", "PA",
                "Sleep", "Well-being", "YHEI", "HOMA",
                "Age.FU1", "School.FU1", "AVM.FU1", "BMI.FU1", "Mothers_BMI.FU1",
                "Familymeal.FU1", "Income.FU1", "ISCED.FU1","PA.FU1",
                "Sleep.FU1", "Well-being.FU1", "YHEI.FU1", "HOMA.FU1",
                "Age.FU2", "AVM.FU2", "BMI.FU2", "Mothers_BMI.FU2", "Alcohol.FU2",
                "Familymeal.FU2", "Income.FU2", "ISCED.FU2","PA.FU2", "Puberty.FU2",
                "Sleep.FU2", "Smoking.FU2", "Well-being.FU2", "YHEI.FU2", "HOMA.FU2")

edges.pa <- as.data.frame(as_edgelist(g1))
names(edges.pa) <- c("from", "to")
edges.pa <- edges.pa[order(edges.pa$from, edges.pa$to),]

edges <- edges[order(edges$from, edges$to),]

# merge pa-data and bootstrapped data frames
edges.joint <- left_join(edges.pa, edges, by = c("from", "to"))
edges.joint <- edges.joint[order(edges.joint$weight, decreasing = TRUE),]

# summary table
summary(edges.joint$weight)
IQR(edges.joint$weight)

# --- Table ---
table4 <-
  edges.joint %>% gt %>%
  tab_spanner(
      label = md("**Edges**"),
      columns = c("from", "to")) %>%
  cols_label(
    from = md("**from**"),
    to = md("**to**"),
    weight = md("**%**")) %>%
  cols_align(
    align = "left", columns = c("from", "to"))

table4

### save table ---------------------------------------------------------------
table4 %>% gtsave("table-edge-freqs.rtf", path = "results/")
# ----------------------------------------------------------------------------


Fn <- ecdf(edges.joint$weight)
# Prob. of edge frequencies larger than x P(f(e) > 0.9) = 1 - P(f(e) <= 0.9)
1 - Fn(90)
summary(edges.joint$weight)
