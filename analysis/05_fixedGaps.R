# ----------------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Fixed Gaps Matrix and smaller dataset
#
# ----------------------------------------------------------------------------------------
### RF mit 10 trees
daten.mids <- readRDS("data/MI-10-scores-mids.rds")
imp10 <- mice::complete(daten.mids, action = "long", include = TRUE)

datenmanagement <- function(imp){
   daten <- imp %>%
      dplyr::select(.,c(.imp, .id,
                        sex, country3, language_t0, income_t0, isced_max_t0,
                        age_birth_el, bf_total_el, birth_w_el, compl_week_preg_el,
                        formula_milk_el, hd_month_el, preg_smoke_el,
                        age_t0, school_t0,
                        media_t0, bmi_t0, bmi_m_t0, club_mbr_t0, familymeal_t0, mvpa_t0,
                        phys_activ_t0, sed_cor_t0, sleep_total_t0,
                        wb_score_t0, yhei_100_t0, z_homa_t0,
                        age_t1, school_t1,
                        media_t1, bmi_t1, bmi_m_t1, club_mbr_t1, familymeal_t1, income_t1,
                        isced_max_t1, mvpa_t1, phys_activ_t1, sed_cor_t1, sleep_total_t1,
                        wb_score_t1, yhei_100_t1, z_homa_t1,
                        age_t3,
                        media_t3, bmi_t3, bmi_m_t3, club_mbr_t3, ever_alcohol_t3,
                        familymeal_t3, income_t3, isced_max_t3, mvpa_t3, phys_activ_t3,
                        pub_t3, sed_cor_t3, sleep_total_t3, smoke_teen_t3,
                        wb_score_t3, yhei_100_t3, z_homa_t3)) %>%
      rename(., "migrant" = "language_t0",
             "bage_el" = "age_birth_el",
             "bf_el" = "bf_total_el",
             "bweight_el" = "birth_w_el",
             "preg_week_el" = "compl_week_preg_el",
             "alc_t3" = "ever_alcohol_t3",
             "formula_el" = "formula_milk_el",
             "hdiet_el" = "hd_month_el",
             "income" = "income_t0",
             "isced" = "isced_max_t0",
             "isced_t1" = "isced_max_t1",
             "isced_t3" = "isced_max_t3",
             "pa_t0" = "phys_activ_t0",
             "pa_t1" = "phys_activ_t1",
             "pa_t3" = "phys_activ_t3",
             "sed_t0" = "sed_cor_t0",
             "sed_t1" = "sed_cor_t1",
             "sed_t3" = "sed_cor_t3",
             "sleep_t0" = "sleep_total_t0",
             "sleep_t1" = "sleep_total_t1",
             "sleep_t3" = "sleep_total_t3",
             "smoke_t3" = "smoke_teen_t3",
             "yhei_t0" = "yhei_100_t0",
             "yhei_t1" = "yhei_100_t1",
             "yhei_t3" = "yhei_100_t3",
             "wb_t0" = "wb_score_t0",
             "wb_t1" = "wb_score_t1",
             "wb_t3" = "wb_score_t3",
             "homa_t0" = "z_homa_t0",
             "homa_t1" = "z_homa_t1",
             "homa_t3" = "z_homa_t3")

   daten$bf_el    <- as.numeric(daten$bf_el)
   daten$preg_week_el <- as.numeric(daten$preg_week_el)
   daten$hdiet_el <- as.numeric(daten$hdiet_el)
   daten$wb_t3  <- as.numeric(daten$wb_t3)

   daten
}

daten    <- datenmanagement(imp10)
daten100 <- datenmanagement(imp100)


el <- grep("_el", names(daten))
t0 <- grep("_t0", names(daten))
t1 <- grep("_t1", names(daten))
t3 <- grep("_t3", names(daten))

names(daten) <- gsub("_t0", ".0", names(daten))
names(daten) <- gsub("_t1", ".1", names(daten))
names(daten) <- gsub("_t3", ".2", names(daten))
names(daten) <- gsub("_el", "", names(daten))


### ---   Build tiers   ------------------------------------------------------------------
# tier 1 : sex, country3, migrant, isced, income
# tier 2 : el
# tier 3 : t0
# tier 4 : t1
# tier 5 : t3

ebenen <- c(rep(1, 3), #tier 1
            rep(2, 2), #income, isced
            rep(3, length(el)),
            rep(4, length(t0)),
            rep(5, length(t1)),
            rep(6, length(t3)))


### ---   Fixed Gaps Matrix   ------------------------------------------------------------
nodes <- ncol(daten) - 2
fg <- matrix(0, ncol = nodes, nrow = nodes)
colnames(fg) <- row.names(fg) <- names(daten)[-c(1:2)]

t0 <- grep(".0", colnames(fg))
t1 <- grep(".1", colnames(fg))
t3 <- grep(".2", colnames(fg))
el <- 6:(min(t0) - 1)

### Sex, country, migrant
fg[ , 1:3] <- 1

# context: Income * Isced
fg[setdiff(1:nodes, which(colnames(fg) %in% c("country3","migrant","income", "isced"))),
   which(colnames(fg) %in% c("income", "isced"))] <- 1


### EL
fg[(max(el) + 1):nodes, el] <- 1
fg[1:nodes,  which(colnames(fg) %in% c("bage"))] <- 1
fg[which(colnames(fg) %in% c("country3", "migrant", "income", "isced")),
   which(colnames(fg) %in% c("bage"))] <- 0
fg[which(colnames(fg) %in% c("bf", "formula", "hdiet")),
   which(colnames(fg) %in% c("bweight", "preg_week", "preg_smoke"))] <- 1
fg[which(colnames(fg) %in% c("sex", "bf", "formula", "hdiet")),
   which(colnames(fg) %in% c("preg_smoke"))] <- 1


### T0
fg[(max(t0) + 1):nodes, t0] <- 1
fg[c(1:nodes), which(colnames(fg) == "age.0")] <- 1
fg[setdiff(1:nodes, which(colnames(fg) %in% c("country3", "migrant", "income", "isced",
            "bage", "bf", "bweight", "preg_week", "preg_smoke", "familymeal.0", "wb.0",
            "yhei.0"))),
   which(colnames(fg) == "bmi_m.0")] <- 1
fg[setdiff(1:nodes,
           which(colnames(fg) %in% c("country3", "migrant", "isced", "income", "age.0"))),
   which(colnames(fg) == "school.0")] <- 1


### T1
fg[(max(t1) + 1):nodes, t1] <- 1
fg[c(1:nodes), which(colnames(fg) == "age.1")] <- 1
fg[setdiff(1:nodes, which(colnames(fg) %in% c("country3", "migrant", "income", "isced",
            "bage", "bf", "bweight", "preg_week", "preg_smoke", "familymeal.0", "wb.0",
            "yhei.0", "isced.1", "income.1", "familymeal.1", "wb.1", "yhei.1"))),
   which(colnames(fg) == "bmi_m.1")] <- 1
fg[setdiff(1:nodes,
           which(colnames(fg) %in% c("country3", "migrant", "isced", "income",
                                     "age.1", "school.0"))),
   which(colnames(fg) == "school.1")] <- 1
fg[setdiff(1:nodes,
           which(colnames(fg) %in% c("country3","migrant", "income", "isced",
                                     "income.1", "isced.1"))),
   which(colnames(fg) %in% c("income.1", "isced.1"))] <- 1


### T3
fg[c(1:nodes), which(colnames(fg) == "age.2")] <- 1
fg[setdiff(1:nodes, which(colnames(fg) %in% c("country3", "migrant", "income", "isced",
           "bage", "bf", "bweight", "preg_week", "preg_smoke", "familymeal.0", "wb.0",
           "yhei.0",  "isced.1", "income.1", "familymeal.1", "wb.1", "yhei.1",
           "isced.2", "income.2", "familymeal.2", "wb.2", "yhei.2"))),
   which(colnames(fg) == "bmi_m.2")] <- 1
fg[setdiff(1:nodes,
           which(colnames(fg) %in% c("country3","migrant","income", "isced",
                                     "income.1", "isced.1","income.2", "isced.2"))),
   which(colnames(fg) %in% c("income.2", "isced.2"))] <- 1
fg[which(colnames(fg) %in% c("isced", "income", "media.0",
                             "isced.1", "income.1", "media.1",
                             "isced.2", "income.2", "media.2")),
   which(colnames(fg) %in% "pub.2")] <- 1

diag(fg) <- 1

### make mids model
daten.mids <- mice::as.mids(daten)
daten100.mids <- mice::as.mids(daten100)
V <- colnames(fg)



daten.pa <-  daten %>%
               dplyr::select(.,-c(club_mbr.0, club_mbr.1, club_mbr.2,
                                  mvpa.0, mvpa.1, mvpa.2,
                                  sed.0, sed.1, sed.2))
daten.pa.mids <- mice::as.mids(daten.pa)
fg.pa <- fg[-which(colnames(fg) %in% c("club_mbr.0", "club_mbr.1", "club_mbr.2",
                                       "mvpa.0", "mvpa.1", "mvpa.2",
                                       "sed.0", "sed.1", "sed.2")),
            -which(colnames(fg) %in% c("club_mbr.0", "club_mbr.1", "club_mbr.2",
                                       "mvpa.0", "mvpa.1", "mvpa.2",
                                       "sed.0", "sed.1", "sed.2"))]
V.pa <- colnames(fg.pa)
ebenen.pa <- c(rep(1, 3), rep(2, 2), rep(3, length(el)),
               rep(4, length(t0) - 3), rep(5, length(t1) - 3), rep(6, length(t3) - 3))



# --- Save data --------------------------------------------------------------------------
save(V.pa, ebenen.pa, fg.pa, daten.pa.mids, file = "data/graph.RData")
original.data <- daten.pa.mids$data
save(original.data, file = "data_not_load/original-data.RData")


# ----------------------------------------------------------------------------------------






