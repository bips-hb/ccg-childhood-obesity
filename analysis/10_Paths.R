# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    FEB 2023
#
# Purpose: Pathway structures between repeated measurements
#
# ------------------------------------------------------------------------------
library(gt)

load("data_not_load/06_graph-10trees-pa.RData")

g <- pc2igraph(g.pa)
g <- feature.igraph(g[[1]], labels = V(g[[1]])$name)

V(g)$name <- c("Sex", "Country", "Migrant", "Income", "ISCED", "Age_at_birth",
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



### Shortest and simple path outgoing from exposure variables at baseline ------
# Distanz: number of hops
search.for.causes <- function(g, variables, mode, minus = NULL){
  require(gt)
  distanz <- igraph::distances(g, mode = mode)

  dist.tab <- t(distanz[which(names(V(g)) %in% variables), ])
  table.dist <- data.frame(nodes = rownames(dist.tab), dist.tab) %>%
                 gt(rowname_col = "nodes")
  if(!is.null(minus)){
    table.dist$'_data' <- table.dist$'_data'[-minus,]
  }
  table.dist
}

bmivariables <- c("BMI", "BMI.FU1", "BMI.FU2")
exposurevariables <- c("AVM","PA","Sleep","Well-being","YHEI")

table.dist.bmi <- search.for.causes(g, bmivariables, mode = "in")
table.dist.exp <- search.for.causes(g, exposurevariables, mode = "out")#, minus = c(1:12))

### save table ---------------------------------------------------------------
table.dist.bmi %>% gtsave("table-dist-bmi.rtf", path = "results/")
table.dist.exp %>% gtsave("table-dist.rtf", path = "results/")
# ----------------------------------------------------------------------------


# --------------------------------------------------------------
whichexposure <- which(names(V(g)) %in% exposurevariables)
makepfade <- function(g, exposure, mode = "out"){
    # shortest paths
  # browser()
    ash <- all_shortest_paths(g, from = V(g)[exposure], mode = mode)[[1]]
    # simpliest paths
    asp <- all_simple_paths(g, from = V(g)[exposure], mode = mode)
    sp <- which(asp %in% ash)
    asp <- lapply(asp, function(x) names(V(g))[as.numeric(x)])
    asp <- lapply(asp, function(x) toString(x))
    if(length(asp) > 0){
     data.frame(SP = c(1:length(asp)) %in% sp, pfad = do.call(c,asp)) %>% gt
    } else {
      cat("no simple path found")
    }
}
avm   <- makepfade(g, whichexposure[1])
pa    <- makepfade(g, whichexposure[2])
sleep <- makepfade(g, whichexposure[3])
wb    <- makepfade(g, whichexposure[4])
yhei  <- makepfade(g, whichexposure[5])
age   <- all_shortest_paths(g, from = "Age.FU1", to = "BMI.FU2")

table.sp <- rbind(avm$`_data`, pa$`_data`, sleep$`_data`,
                  wb$`_data`, yhei$`_data`) %>% gt


### save table ---------------------------------------------------------------
table.sp %>% gtsave("table-sp.rtf", path = "results/")
# ----------------------------------------------------------------------------



### Shortest and simple path incoming to BMI.FU2 and WB.FU2 -------------------
exposurevariables <- c("BMI.FU2", "Well-being.FU2")
whichout <- which(names(V(g)) %in% exposurevariables)
bmi <- makepfade(g, whichout[1], mode = "in")
wb  <- makepfade(g, whichout[2], mode = "in")

table.sp.out <- rbind(bmi$`_data`, wb$`_data`) %>% gt

### save table ---------------------------------------------------------------
table.sp.out %>% gtsave("table-sp-out.rtf", path = "results/")
# ----------------------------------------------------------------------------


### Shortest and simple path from cultural, social and familial variables -----
exposurevariables <- c("Country", "Migrant", "Income", "ISCED", "Age_at_birth", "Smoking_pregnancy")
whichbasic <- which(names(V(g)) %in% exposurevariables)
country <- makepfade(g, whichbasic[1]) #1593
migrant <- makepfade(g, whichbasic[2]) #749
income  <- makepfade(g, whichbasic[3]) #527
isced   <- makepfade(g, whichbasic[4]) #527
age     <- makepfade(g, whichbasic[5]) #287
smok    <- makepfade(g, whichbasic[6]) #527

table.sp.basic <- rbind(country$`_data`, migrant$`_data`, age$`_data`,
                        income$`_data`, isced$`_data`) %>% gt

### save table ---------------------------------------------------------------
table.sp.basic %>% gtsave("table-sp-basic.rtf", path = "results/")
# ----------------------------------------------------------------------------
distanz <- igraph::distances(g, mode = "out")
dist.tab <- t(distanz[whichbasic,])
table.dist <- data.frame(nodes = rownames(dist.tab), dist.tab) %>%
  gt(rowname_col = "nodes")







## Find possible ascendents of BMI  ------------------------------------------
which(names(V(g)) %in% bmivariables)
amat <- as_adj(g, sparse = FALSE)
isValidGraph(t(amat), type = "cpdag", verbose = TRUE)

## possible ancestors of BMI.FU2
vorfahren2 <- pcalg::possAn(m = t(amat), x = 39, type = "cpdag")
vf2 <- rownames(amat)[setdiff(vorfahren2,39)]
nvf2 <- length(vf2)
vf2 <- data.frame(vorfahren = setdiff(vorfahren2,39), vf = vf2)

## possible ancestors of BMI.FU1
vorfahren1 <- pcalg::possAn(m = t(amat), x = 27, type = "cpdag")
vf1 <- rownames(amat)[setdiff(vorfahren1, 27)]
vf1 <- data.frame(vorfahren = setdiff(vorfahren1, 27),
                  vf = vf1)

## possible ancestors of BMI
vorfahren <- pcalg::possAn(m = t(amat), x = 16, type = "cpdag")
vf <- rownames(amat)[setdiff(vorfahren,16)]
vf <- data.frame(vf = vf,
                 vorfahren = setdiff(vorfahren,16))


a <- full_join(vf2, vf1, by = "vorfahren", suffix = c("2","1"))
posAncestors <- full_join(a,vf, by = "vorfahren")[,c(4:2)]

### save table ---------------------------------------------------------------
posAncestors %>% gt() %>% gtsave("table-posAn-bmi-pag.rtf", path = "results/")
# ----------------------------------------------------------------------------

## Find possible descendents of exposures  -------------------------------------
which(names(V(g)) %in% exposurevariables)

## AVM
des_avm <- pcalg::possDe(m = t(amat), x = 15, type = "cpdag")
des_avm <- data.frame(avm = rownames(amat)[setdiff(des_avm,15)],
                      nr = setdiff(des_avm,15))

## Physical activity
des_pa <- pcalg::possDe(m = t(amat), x = 19, type = "cpdag")
des_pa <- data.frame(pa = rownames(amat)[setdiff(des_pa,19)],
                      nr = setdiff(des_pa,19))

## Sleep
des_sl <- pcalg::possDe(m = t(amat), x = 20, type = "cpdag")
des_sl <- data.frame(sl = rownames(amat)[setdiff(des_sl, 20)],
                     nr = setdiff(des_sl, 20))

## Well-being
des_wb <- pcalg::possDe(m = t(amat), x = 21, type = "cpdag")
des_wb <- data.frame(wb = rownames(amat)[setdiff(des_wb, 21)],
                     nr = setdiff(des_wb, 21))

##YHEI
des_yh <- pcalg::possDe(m = t(amat), x = 22, type = "cpdag")
des_yh <- data.frame(yh = rownames(amat)[setdiff(des_yh, 22)],
                     nr = setdiff(des_yh, 22))



a1 <- full_join(des_avm, des_pa, by = "nr", suffix = c("avm","pa"))
a2 <- full_join(des_sl, des_wb, by = "nr", suffix = c("sl","wb"))
a3 <- full_join(a1, a2, by = "nr")
# a4 <- full_join(a3, des_yh, by = "nr")
posDescen <- full_join(a3, des_yh, by = "nr")[, -c(2)]


### save table ---------------------------------------------------------------
posDescen %>% gt() %>% gtsave("table-posDes-exposure.rtf", path = "results/")
# ----------------------------------------------------------------------------


