# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Pathway structures between repeated measurements, bootstrap vs main graph
#
# ------------------------------------------------------------------------------
library(gtsummary)
library(gt)


boot <- readRDS("data/boot100_mi1-graph.RDS")     # bootstrap, imputation (M = 1)
load("data/graph-10trees.RData")                   # CCG: MI, 0.05

# Build adjacency matrices ---------------------------------------------------------------
# adjacency matrix for the main graph
makeamat <- function(x){
  amat <- wgtMatrix(getGraph(x), transpose = FALSE)
  amat <- amat + t(amat)
  amat[which(amat > 1)] <- 1
  list(amat)
}
# make adjacency matrices for the bootstraps
makebootamat <- function(boot){
  amat <- lapply(boot, function(x){
    tmp <- wgtMatrix(getGraph(x), transpose = FALSE)
    wm2 <- (tmp + t(tmp))
    tmp[which(wm2 > 1)] <- 0.5
    tmp
  })
}

### Adjacency matrices
g.amat <- makeamat(g.pa)
boot.amat <- makebootamat(boot2)



# make table ---------------------------------------------------------------------------
myfunc <-  function(mat, n0, n1, n2){
  # mat:  adjacency matrix
  # n0:   at T0, numeric
  # n1:   at T1, numeric
  # n2:   at T3, numeric
  out <-  t(sapply(
            mat,
            function(x, v0 = n0, v1 = n1, v2 = n2){
              x[c(v0,v1), c(v1,v2)][upper.tri(x[c(v0,v1), c(v1,v2)], diag = TRUE)]})
  )
  out <- data.frame(out)
  out$X1 <- out$X1*100
  out$X2 <- out$X2*10
  out <- out %>% mutate(B = X1 + X2 + X3,
                        P = case_when(B == 0 ~ 'none',
                                      B == 1 ~ 'FU1-FU2',
                                      B == 10 ~ 'B-FU2',
                                      B == 11 ~ 'B-FU2, FU1-FU2',
                                      B == 100 ~ 'B-FU1',
                                      B == 101 ~ 'B-FU1-FU2',
                                      B == 110 ~ 'B-FU1, B-FU2',
                                      B == 111 ~ 'B-FU1-FU2-B',
                                      TRUE ~ as.character(B)
                                      ))

  out
}

mpt <- function(n0, n1, n2, var){
  # n0:   at T0, numeric
  # n1:   at T1, numeric
  # n2:   at T3, numeric
  # var:  variable, character

  b <- round(prop.table(table(myfunc(boot.amat, n0, n1, n2)$P)),3)*100
  o <- round(prop.table(table(myfunc(g.amat, n0, n1, n2)$P)),3)*100

  out <-  dplyr::full_join(as.data.frame(o), as.data.frame(b), by = "Var1")
  names(out) <- c(var, "original", 'Boot')
  return(out)
}

relevelandsort <- function(daten){
  daten[,1] <- fct_relevel(daten[,1],
                           c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                             "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))
  daten[order(daten[,1]),]
}
# ---------------------------------------------------------------------------------------


(p.bmi <- relevelandsort(mpt(16, 27, 39, "BMI")))
(p.avm <- relevelandsort(mpt(15, 26, 38, "AVM")))
(p.pa <- relevelandsort(mpt(19, 32, 45, "PA")))
(p.sleep <- relevelandsort(mpt(20, 33, 47, "Sleep")))
(p.yhei <- relevelandsort(mpt(22, 35, 50, "YHEI")))
(p.wb <- relevelandsort(mpt(21, 34, 49, "Well-being")))
p.homa <- relevelandsort(mpt(23, 36, 51, "HOMA-IR"))
p.bmim <- relevelandsort(mpt(17, 28, 39, "BMI_mother"))
p.income <- relevelandsort(mpt(4, 30, 43, "Income"))
p.isced <- relevelandsort(mpt(5, 31, 44, "ISCED"))
p.fm <- relevelandsort(mpt(18, 29, 42, "familymeal"))
(p.age <- relevelandsort(mpt(13, 24, 37, "Age")))




### Bild dataset -------------------------------------------------------------------------
#BMI: variable number 16, 27, 39
dat <- data.frame(group = c("Main", 
                            rep("Boot", length(boot))),
                  bmi = c(myfunc(g.amat, 16, 27, 39)$P,
                          myfunc(boot.amat, 16, 27, 39)$P))
dat$bmi <- fct_relevel(dat$bmi,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#AVM
dat$avm <- c(myfunc(g.amat, 15, 26, 38)$P,
             myfunc(boot.amat, 15, 26, 38)$P)
dat$avm <- fct_relevel(dat$avm,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#pa
dat$pa <- c(myfunc(g.amat, 19, 32, 45)$P,
            myfunc(boot.amat, 19, 32, 45)$P)
dat$pa <- fct_relevel(dat$pa,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#sleep
dat$sleep <- c(myfunc(g.amat, 20, 33, 47)$P,
               myfunc(boot.amat, 20, 33, 47)$P)
dat$sleep <- fct_relevel(dat$sleep,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#well/being
dat$wb <- c(myfunc(g.amat, 21, 34, 49)$P,
            myfunc(boot.amat, 21, 34, 49)$P)
dat$wb <- fct_relevel(dat$wb,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#yhei
dat$yhei <- c(myfunc(g.amat, 22, 35, 50)$P,
              myfunc(boot.amat, 22, 35, 50)$P)
dat$yhei <- fct_relevel(dat$yhei,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#homa
dat$homa <- c(myfunc(g.amat, 23, 36, 51)$P,
              myfunc(boot.amat, 23, 36, 51)$P)
dat$homa <- fct_relevel(dat$homa,
                       c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                         "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#Daily familymeals
dat$fm <- c(myfunc(g.amat, 18, 29, 42)$P,
              myfunc(boot.amat, 18, 29, 42)$P)
dat$fm <- fct_relevel(dat$fm,
                      c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                        "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#income
dat$income <- c(myfunc(g.amat, 4, 30, 43)$P,
                myfunc(boot.amat, 4, 30, 43)$P)
dat$income <- fct_relevel(dat$income,
                      c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                        "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))
#isced
dat$isced <- c(myfunc(g.amat, 5, 31, 44)$P,
               myfunc(boot.amat, 5, 31, 44)$P)
dat$isced <- fct_relevel(dat$isced,
                          c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                            "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))
#bmi mother
dat$bmi_m <- c(myfunc(g.amat, 17, 28, 39)$P,
               myfunc(boot.amat, 17, 28, 39)$P)
dat$bmi_m <- fct_relevel(dat$bmi_m,
                         c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                           "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))

#age
#bmi mother
dat$age <- c(myfunc(g.amat, 13, 24, 37)$P,
               myfunc(boot.amat, 13, 24, 37)$P)
dat$age <- fct_relevel(dat$age,
                         c("none", "B-FU1", "B-FU2", "B-FU1, B-FU2", "FU1-FU2",
                           "B-FU2, FU1-FU2", "B-FU1-FU2", "B-FU1-FU2-B"))



# Table ----------------------------------------------------------------------------------
tbl.main <-
  dat %>% filter(group == "Main") %>% select(-c(group)) %>%
  pivot_longer(cols = 1:12, names_to = "var", values_to = "path") %>%
  tbl_summary(
    by = var,
    statistic = list(all_categorical() ~ "{n}")) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_footnote(everything() ~ NA)

tbl.boot_m1 <-
  dat %>% filter(group == "Boot") %>% select(-c(group)) %>%
             pivot_longer(cols = 1:12, names_to = "var", values_to = "path") %>%
             tbl_summary(
               by = var,
               statistic = list(all_categorical() ~ "{n}")) %>%
             modify_header(all_stat_cols() ~ "**{level}**") %>%
             modify_footnote(everything() ~ NA)


# --- save ---------------------------------------------------------------------
tbl.main %>% as_gt() %>%
  gtsave("table-repeatedM-main.rtf", path = "results/")
tbl.boot_m1 %>% as_gt() %>%
    gtsave("table-repeatedM-boot-m1.rtf", path = "results/")

