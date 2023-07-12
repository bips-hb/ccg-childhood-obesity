# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Recover graph using test-wise deletion instead of MI
#
# ------------------------------------------------------------------------------
daten <- readRDS("data/data2impute.rds")

#   Datamanagement  ------------------------------------------------------------
daten$bf_excl_el   <- log(daten$bf_excl_el + 1)
daten$bf_total_el  <- log(daten$bf_total_el + 1)
daten$hd_month_el  <- log(daten$hd_month_el + 1)
daten$mvpa_t0 <- log(daten$mvpa_t0 + 1)
daten$mvpa_t1 <- log(daten$mvpa_t1 + 1)
daten$mvpa_t3 <- log(daten$mvpa_t3 + 1)
daten$preg_w_up_el <- log(daten$preg_w_up_el + 1)
daten$phys_activ_t0 <- log(daten$phys_activ_t0 + 1)
daten$phys_activ_t1 <- log(daten$phys_activ_t1 + 1)
daten$phys_activ_t3 <- log(daten$phys_activ_t3 + 1)


# Country: South, Central, North countries
daten$country3 <- NULL
daten$country3[daten$country %in% c(1,3,9)] <- "South"
daten$country3[daten$country %in% c(4,7,8)] <- "Central"
daten$country3[daten$country %in% c(2,6)] <- "North"
daten$country3 <- as.factor(daten$country3)
levels(daten$country) <- c("ITA", "EST", "CYP", "BEL", "SWE", "GER", "HUN", "ESP")

## Income
daten$income_t0 <- NULL
daten$income_t0[daten$income_cat_t0 <= 2] <- 1
daten$income_t0[daten$income_cat_t0 == 3] <- 2
daten$income_t0[daten$income_cat_t0 >= 4] <- 3
daten$income_t0 <- as.ordered(daten$income_t0)

daten$income_t1 <- NULL
daten$income_t1[daten$income_cat_t1 <= 2] <- 1
daten$income_t1[daten$income_cat_t1 == 3] <- 2
daten$income_t1[daten$income_cat_t1 >= 4] <- 3
daten$income_t1 <- as.ordered(daten$income_t1)

daten$income_t3 <- NULL
daten$income_t3[daten$income_cat_t3 <= 2] <- 1
daten$income_t3[daten$income_cat_t3 == 3] <- 2
daten$income_t3[daten$income_cat_t3 >= 4] <- 3
daten$income_t3 <- as.ordered(daten$income_t3)

# Homemeal
daten$familymeal_t0 <- NULL
daten$familymeal_t0[daten$homebreakf_t0 == 7] <- 1
daten$familymeal_t0[daten$homebreakf_t0 < 7 ] <- 0
daten$familymeal_t0 <- as.factor(daten$familymeal_t0)
levels(daten$familymeal_t0) <- c("less", "at least once a day")

daten$familymeal_t1 <- NULL
daten$familymeal_t1[daten$homebreakf_t1 == 7] <- 1
daten$familymeal_t1[daten$homebreakf_t1 < 7 ] <- 0
daten$familymeal_t1 <- as.factor(daten$familymeal_t1)
levels(daten$familymeal_t1) <- c("less", "at least once a day")

daten$familymeal_t3 <- NULL
daten$familymeal_t3[daten$meal_togeth_t3 >= 7] <- 1
daten$familymeal_t3[daten$meal_togeth_t3 < 7] <- 0
daten$familymeal_t3 <- as.factor(daten$familymeal_t3)
levels(daten$familymeal_t3) <- c("less", "at least once a day")

# bedroom media
daten$bedrmedia_t0 <- NULL
daten$bedrmedia_t0[daten$bedrmedia_number_t0 == 0] <- 0
daten$bedrmedia_t0[daten$bedrmedia_number_t0 == 1 | daten$bedrmedia_number_t0 == 2] <- 1
daten$bedrmedia_t0[daten$bedrmedia_number_t0 >= 3] <- 2
daten$bedrmedia_t0 <- as.factor(daten$bedrmedia_t0)
levels(daten$bedrmedia_t0) <- c("0", "1-2", ">=3")

daten$bedrmedia_t1 <- NULL
daten$bedrmedia_t1[daten$bedrmedia_number_t1 == 0] <- 0
daten$bedrmedia_t1[daten$bedrmedia_number_t1 == 1 | daten$bedrmedia_number_t1 == 2] <- 1
daten$bedrmedia_t1[daten$bedrmedia_number_t1 >= 3] <- 2
daten$bedrmedia_t1 <- as.factor(daten$bedrmedia_t1)
levels(daten$bedrmedia_t1) <- c("0", "1-2", ">=3")

daten$bedrmedia_t3 <- NULL
daten$bedrmedia_t3[daten$bedrmedia_number_t3 == 0] <- 0
daten$bedrmedia_t3[daten$bedrmedia_number_t3 == 1 | daten$bedrmedia_number_t3 == 2] <- 1
daten$bedrmedia_t3[daten$bedrmedia_number_t3 >= 3] <- 2
daten$bedrmedia_t3 <- as.factor(daten$bedrmedia_t3)
levels(daten$bedrmedia_t3) <- c("0", "1-2", ">=3")

## school included 18.07.22
daten[which(daten$school_t0 == "2" & daten$school_t1 == "3"), 'school_t1'] <- 2
daten <- daten %>%
   mutate(
     ## school included 18.07.22
      school_t0 = forcats::fct_collapse(school_t0, "1" = c("1", "3")),
      school_t1 = forcats::fct_collapse(school_t1, "1" = c("1", "3")),

      bmi_m_t0 = weight_m_t0 / ((height_m_t0 / 100)**2),
      bmi_m_t1 = weight_m_t1 / ((height_m_t1 / 100)**2),
      bmi_m_t3 = weight_m_t3 / ((height_m_t3 / 100)**2),

      media_t0 = 5 * (tv_dur_wd_t0 + pc_dur_wd_t0) + 2 * (tv_dur_we_t0 + pc_dur_we_t0),
      media_t1 = 5 * (tv_dur_wd_t1 + pc_dur_wd_t1) + 2 * (tv_dur_we_t1 + pc_dur_we_t1),
      media_t3 = 5 * (tv_dur_wd_t3 + pc_dur_wd_t3) + 2 * (tv_dur_we_t3 + pc_dur_we_t3) +
                 5 * web_dur_wd_t3 + 2 * web_dur_we_t3
      )



daten <- daten %>%
  dplyr::select(.,c(id_no, sex, country3, language_t0, income_t0, isced_max_t0,
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

el <- grep("_el", names(daten))
t0 <- grep("_t0", names(daten))
t1 <- grep("_t1", names(daten))
t3 <- grep("_t3", names(daten))

names(daten) <- gsub("_t0", ".0", names(daten))
names(daten) <- gsub("_t1", ".1", names(daten))
names(daten) <- gsub("_t3", ".2", names(daten))
names(daten) <- gsub("_el", "", names(daten))

dat.tw <- daten




### use fixed gaps from 05_fixedGaps.R 
load("data/graph.RData")
rm(daten, daten.pa.mids, daten.mids, daten100.mids)
ebenen <- ebenen.pa
fg <- fg.pa


# -------   Data   ----------------------------------------------------------------------
tw.data <- dat.tw %>% select(., -c(id_no, club_mbr.0, club_mbr.1, club_mbr.2,
                                   mvpa.0, mvpa.1, mvpa.2,
                                   sed.0, sed.1, sed.2))


# -------   Graph   ----------------------------------------------------------------------
library(parallel)
g.tw <- tpc(suffStat = tw.data,
            indepTest = flexCItwd,
            skel.method = "stable",
            alpha = 0.05,
            labels = colnames(fg),
            forbEdges = fg,
            verbose = FALSE,
            tiers = ebenen)
save(g.tw, file = "data/graph-twd.RData")



