# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Variables are selected and stored into one data set
#
# ------------------------------------------------------------------------------

### load data
load("data/00.RData")


# ------------------------------------------------------------------------------


### Core T0 ---
idefics$core_t0 <-
           subset(idefics$core_t0,
            select = c(id_no, doe, dob, country, sex_child, age,
                       income_cat, isced_cat2011, bf_1, bf_2, bmi_score_cole_12,
                       contr_interv, bodyfat, ffmi, phys_activ, bedrmedia_number,
                       att_eo_1, leis_activ, att_sleep_sac_corr, wb_score,
                       sweet_prop, fat_prop, waist, height, avm_1_week,
                       waist_score_cole, evenson_mvpa_1, val_time))

names(idefics$core_t0)[which(names(idefics$core_t0)=="isced_cat2011")] <- "isced_max"
names(idefics$core_t0)[which(names(idefics$core_t0)=="att_eo_1")] <- "homemeal"
names(idefics$core_t0)[which(names(idefics$core_t0)=="bmi_score_cole_12")] <- "bmi"
names(idefics$core_t0)[which(names(idefics$core_t0)=="evenson_mvpa_1")] <- "mvpa"
idefics$core_t0$country <- idefics$core_t0$country - 10
idefics$core_t0$mvpa <- 100 * idefics$core_t0$mvpa
idefics$core_t0$birthyear <- format(as.Date(idefics$core_t0$dob, format="%Y-%m-%d"),"%Y")
idefics$core_t0$year <-format(as.Date(idefics$core_t0$doe, format="%Y-%m-%d"),"%Y")
idefics$core_t0$survey <- 0
tmp <- cpmerge(idefics$core_t0, id = "id_no", mergeby = "year")
idefics$core_t0 <- merge(idefics$core_t0, tmp, by = "id_no", all.x = TRUE)
names(idefics$core_t0)[6:ncol(idefics$core_t0)] <- paste0(names(idefics$core_t0)[6:ncol(idefics$core_t0)], "_t0")

idefics$core_t0 <- merge(data_id,
                         subset(idefics$core_t0, select = -c(doe, dob)),
                         by = "id_no")


### Core T1 ---
idefics$core_t1 <-
  subset(idefics$core_t1,
         select =  c(keep = id_no, doe_t1, dob_t1, country_t1,
                     bodyfat_t1, age_t1, income_cat_t1,
                     isced_cat2011_t1, contr_interv_t1, att_eo_1_t1,  bf_1_t1,
                     bf_2_t1, bmi_score_cole_12_t1, ffmi_t1, leis_activ_t1,
                     phys_activ_t1,  bedrmedia_number_t1, wb_score_t1,
                     sweet_prop_t1, fat_prop_t1, sleep_night_t1, sleep_total_t1,
                     waist_t1, height_t1, avm_1_week_t1, waist_score_cole_t1,
                     evenson_mvpa_1_t1, val_time_t1))

names(idefics$core_t1)[which(names(idefics$core_t1)=="family_id_t1")] <- "family_id"
names(idefics$core_t1)[which(names(idefics$core_t1)=="isced_cat2011_t1")] <- "isced_max_t1"
names(idefics$core_t1)[which(names(idefics$core_t1)=="att_eo_1_t1")] <- "homemeal_t1"
names(idefics$core_t1)[which(names(idefics$core_t1)=="bmi_score_cole_12_t1")] <- "bmi_t1"
names(idefics$core_t1)[which(names(idefics$core_t1)=="evenson_mvpa_1_t1")] <- "mvpa_t1"
idefics$core_t1$mvpa_t1 <- 100 * idefics$core_t1$mvpa_t1
idefics$core_t1$birthyear_t1 <- format(as.Date(idefics$core_t1$dob_t1, format="%Y-%m-%d"),"%Y")
idefics$core_t1$year_t1 <-format(as.Date(idefics$core_t1$doe_t1, format="%Y-%m-%d"),"%Y")
idefics$core_t1$survey_t1 <- 1
tmp <- cpmerge(idefics$core_t1, id = "id_no", mergeby = "year_t1", cntry = "country_t1")
idefics$core_t1 <- merge(idefics$core_t1, tmp, by = "id_no", all.x = TRUE)
names(idefics$core_t1)[which(names(idefics$core_t1)=="cpscore")] <- "cpscore_t1"

idefics$core_t1 <- merge(data_id,
                         subset(idefics$core_t1, select = -c(country_t1, doe_t1, dob_t1)),
                         by = "id_no")


### Core T3 ---
idefics$core_t3 <-
  subset(core_t3,
         select = c(ifam_no, idefics_id, doe_t3, dob_t3, country,
                    status_t3, contr_interv_t3, age_t3, income_cat_t3,
                    bmi_score_cole_12_t3, isced_cat2011_t3, waist_to_height_t3,
                    ffmi_t3, phys_activ_t3, bedrmedia_number_t3, sweet_prop_t3,
                    fat_prop_t3, sleep_night_t3, sleep_total_t3, wb_score_t3,
                    rel_meal_freq_pre_t3, pub_t3, smoke_teen_t3, ever_alcohol_t3,
                    body_fat_t3, bf_1_t3, bf_2_t3, avm_1_week_t3,
                    waist_score_cole_t3, avg_weartime_t3, avg_mvpa_t3))

idefics$core_t3$birthyear_t3 <- format(as.Date(idefics$core_t3$dob_t3, format="%Y-%m-%d"),"%Y")
idefics$core_t3$year_t3 <-format(as.Date(idefics$core_t3$doe_t3, format="%Y-%m-%d"),"%Y")
idefics$core_t3$survey_t3 <- 2
idefics$core_t3$homemeal_t3 <- idefics$core_t3$rel_meal_freq_pre_t3 * 100
idefics$core_t3$mvpa_t3 <- 100 * (core_t3$avg_mvpa_t3 / core_t3$avg_weartime_t3)
tmp <- cpmerge(idefics$core_t3, id = "ifam_no", mergeby = "year_t3")
idefics$core_t3 <- merge(idefics$core_t3, tmp, by = "ifam_no", all.x = TRUE)
names(idefics$core_t3)[which(names(idefics$core_t3)=="cpscore")] <- "cpscore_t3"
names(idefics$core_t3)[which(names(idefics$core_t3)=="idefics_id")] <- "id_no"
names(idefics$core_t3)[which(names(idefics$core_t3)=="family_id_t3")] <- "family_id"
names(idefics$core_t3)[which(names(idefics$core_t3)=="isced_cat2011_t3")] <- "isced_max_t3"
names(idefics$core_t3)[which(names(idefics$core_t3)=="body_fat_t3")] <- "bodyfat_t3"
names(idefics$core_t3)[which(names(idefics$core_t3)=="bmi_score_cole_12_t3")] <- "bmi_t3"

idefics$core_t3 <- merge(data_id,
                         subset(idefics$core_t3,
                                select = -c(country, doe_t3, dob_t3,
                                            avg_mvpa_t3,
                                            rel_meal_freq_pre_t3)),
                         by = c("id_no", "ifam_no"), all.x = TRUE)


### MERGE: core t0-t3 ----------------------------------------------------------
analysis_dataset <- merge(idefics$core_t0,
                          idefics$core_t1,
                          by = c("id_no", "ifam_no", "family_id"))
analysis_dataset <- merge(analysis_dataset,
                          idefics$core_t3,
                          by = c("id_no", "ifam_no", "family_id"))
# ------------------------------------------------------------------------------





# Accelerometer  ---------------------------------------------------------------
tmp <- subset(acc_long,
              select = c(id_cohort, id_no, survey, val_time, sed, mvpa))
tmp <- tmp[which(tmp$id_cohort %in% c(data_id$id_no, data_id$ifam_no)),]

tmp$survey[tmp$survey == 0] <- "t0"
tmp$survey[tmp$survey == 1] <- "t1"
tmp$survey[tmp$survey == 3] <- "t3"
tmp$sed_cor  <- 100 * tmp$sed / tmp$val_time
tmp$mvpa_cor <- 100 * tmp$mvpa / tmp$val_time
tmp <- tmp[order(tmp$id_no), -c(1,4:6)]
#tmp <- tmp[order(tmp$id_no), -c(1,5:6)]
row.names(tmp) <- 1:nrow(tmp)

tmp_1 <- spread_with_many_values(tmp, survey, c(sed_cor, mvpa_cor))



### MERGE: usual intakes   -----------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          tmp_1,
                          by = c("id_no"),
                          all.x = TRUE)
# ------------------------------------------------------------------------------
rm(tmp, tmp_1)


### pq: Parental Questionnaire -------------------------------------------------
idefics$pq_t0 <-  subset(idefics$pq_t0,
              select = c(id_no, age_birth, school, language, househ, birth_w,
                         hd_month, birth_term, preg_w_up, breastf, breastc,
                         breastf_st, breastc_st, breastf_en, breastc_en,
                         formula, formula_st, otfeed, otfeed_st, event1, event2,
                         event3, event4, event5, event6, event7, event8,
                         smoke_cig, smoke_tob, weight_m, height_m, weight_f,
                         height_f, tv_dur_wd, tv_dur_we, pc_dur_wd, pc_dur_we,
                         cons_beer, cons_liq, cons_whis, club_mbr))
names(idefics$pq_t0)[2:ncol(idefics$pq_t0)] <- paste0(names(idefics$pq_t0)[2:ncol(idefics$pq_t0)], "_t0")
idefics$pq_t0 <- merge(data_id, idefics$pq_t0, by = "id_no")
idefics$pq_t0$tv_dur_wd_t0 <- recodeMedia(idefics$pq_t0$tv_dur_wd_t0)
idefics$pq_t0$tv_dur_we_t0 <- recodeMedia(idefics$pq_t0$tv_dur_we_t0)
idefics$pq_t0$pc_dur_wd_t0 <- recodeMedia(idefics$pq_t0$pc_dur_wd_t0)
idefics$pq_t0$pc_dur_we_t0 <- recodeMedia(idefics$pq_t0$pc_dur_we_t0)
idefics$pq_t0 <- idefics$pq_t0 %>%  replace_na(
                               list(event1_t0 = 0, event2_t0 = 0, event3_t0 = 0,
                                    event4_t0 = 0, event5_t0 = 0, event6_t0 = 0,
                                    event7_t0 = 0, event8_t0 = 0, breastc_t0 = 0,
                                    breastf_t0 = 0, formula_t0 = 0, otfeed_t0 = 0))

idefics$pq_t1 <-  subset(idefics$pq_t1,
              select = c(id_no, age_birth_t1, school_t1, language_t1,
                         househ_t1, birth_w_t1, hd_month_t1, preg_w_up_t1,
                         birth_term_t1, breastf_t1, cons_beer_t1, cons_liq_t1,
                         cons_whis_t1, breastc_t1, breastf_st_t1, breastc_st_t1,
                         breastf_en_t1, breastc_en_t1, formula_t1, formula_st_t1,
                         otfeed_t1, otfeed_st_t1, event1_t1, event2_t1,
                         event3_t1, event4_t1, event5_t1, event6_t1, event7_t1,
                         event8_t1, smoke_cig_t1, smoke_tob_t1, weight_m_t1,
                         height_m_t1, weight_f_t1, height_f_t1, tv_dur_wd_t1,
                         tv_dur_we_t1, pc_dur_wd_t1, pc_dur_we_t1,
                         club_mbr_t1))

idefics$pq_t1 <- merge(data_id, idefics$pq_t1, by = "id_no")
idefics$pq_t1$tv_dur_wd_t1 <- recodeMedia(idefics$pq_t1$tv_dur_wd_t1)
idefics$pq_t1$tv_dur_we_t1 <- recodeMedia(idefics$pq_t1$tv_dur_we_t1)
idefics$pq_t1$pc_dur_wd_t1 <- recodeMedia(idefics$pq_t1$pc_dur_wd_t1)
idefics$pq_t1$pc_dur_we_t1 <- recodeMedia(idefics$pq_t1$pc_dur_we_t1)
idefics$pq_t1 <- idefics$pq_t1 %>%  replace_na(
  list(event1_t1 = 0, event2_t1 = 0, event3_t1 = 0,
       event4_t1 = 0, event5_t1 = 0, event6_t1 = 0,
       event7_t1 = 0, event8_t1 = 0, breastc_t1 = 0,
       breastf_t1 = 0, formula_t1 = 0, otfeed_t1 = 0))

idefics$pq_t3 <-  subset(early_t3,
                  select = c(ifam_no, preterm_t3, hd_month_t3, birth_w_t3,
                            smoke_tob_t3, smoke_cig_t3, preg_w_up_t3,
                            cons_beer_t3, cons_liq_t3, cons_whis_t3,
                            breastf_st_t3, breastf_en_t3, breastsc_st_t3,
                            breastsc_en_t3, breastf_t3, breastsc_t3, formula_t3,
                            otfeed_t3, otfeed_st_t3, formula_st_t3))
idefics$pq_t3 <- merge(data_id, idefics$pq_t3, by = "ifam_no")

### MERGE: pq t0-t3   ----------------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          idefics$pq_t0,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$pq_t1,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$pq_t3,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)

# ------------------------------------------------------------------------------



# FFQ    -----------------------------------------------------------------------
ffq_t0 <- subset(idefics$pqd_t0,
                 select = c(id_no, fruit_1,
                            fruit_2,  drink_1, drink_2, drink_3,
                            drink_4, breakf_1, breakf_2, milk_1, milk_2, yoghurt_1,
                            yoghurt_2, fish_1, fish_2, meat_1, att_meat_2,
                            att_meat_3, egg_1, egg_2, egg_3, replacem,
                            att_cheese_1, att_cheese_2, spread_1, spread_2,
                            spread_3, spread_4, spread_5, cereal_1, cereal_2,
                            cereal_3, att_cereal_4, cereal_5, cereal_6,
                            snack_1, snack_2, snack_3, snack_4, snack_5,
                            snack_6, snack_7,
                            att_vegetabl_1, att_vegetabl_2, att_vegetabl_4,
                            vegetabl_3))

names(ffq_t0)[2:ncol(ffq_t0)] <- paste0(names(ffq_t0)[2:ncol(ffq_t0)], "_t0")

# recode food items
idefics$ffq_t0 <- data.frame("id_no" = ffq_t0$id_no, apply(ffq_t0[,-1], 2, recodeFood))



ffq_t1 <- subset(idefics$pqd_t1,
                 select = c(id_no, breakf_1_t1, breakf_2_t1,
                            fruit_1_t1, fruit_2_t1,
                            drink_1_t1, drink_2_t1, drink_3_t1, drink_4_t1,
                            milk_1_t1, milk_2_t1, fish_1_t1, fish_2_t1,
                            meat_1_t1, meat_2_t1,
                            meat_3_t1, egg_1_t1, egg_2_t1, egg_3_t1, replacem_t1,
                            cheese_1_t1, cheese_2_t1, spread_1_t1, spread_2_t1,
                            spread_3_t1, spread_4_t1, spread_5_t1, cereal_1_t1,
                            cereal_2_t1, cereal_3_t1, cereal_4_t1, cereal_5_t1,
                            cereal_6_t1, snack_1_t1, snack_2_t1, snack_3_t1,
                            snack_4_t1, snack_5_t1, snack_6_t1, snack_7_t1,
                            vegetabl_1_t1, vegetabl_2_t1, vegetabl_3_t1,
                            vegetabl_4_t1, yoghurt_1_t1, yoghurt_2_t1))

# recode food items
idefics$ffq_t1 <- data.frame("id_no" = ffq_t1$id_no, apply(ffq_t1[,-1], 2, recodeFood))


ffq_ch <- subset(ch_t3,
                 select = c(ifam_no, vegetabl_1a_t3, vegetabl_1b_t3, vegetabl_2_t3,
                            vegetabl_4_t3, fruit_1_t3, fruit_2_t3, vegetabl_3_t3,
                            drink_1_t3, drink_2_t3, drink_3_t3, drink_4_t3,
                            drink_5_t3, drink_6_t3, drink_7a_t3, drink_7b_t3,
                            drink_8a_t3, drink_8b_t3, breakf_1_t3, breakf_2_t3,
                            milk_1_t3, milk_2_t3, yoghurt_1_t3,
                            yoghurt_2_t3, fish_1_t3, fish_2_t3, fish_3_t3,
                            meat_1_t3, meat_2_t3, meat_3_t3, meat_4_t3, meat_5_t3,
                            egg_1_t3, egg_2_t3, egg_3_t3, replacem_t3,
                            cheese_1_t3, cheese_2_t3, cheese_3_t3, spread_1_t3,
                            spread_2_t3, spread_3_t3, spread_4_t3, spread_5_t3,
                            oil_t3, cereal_1_t3, cereal_2_t3, cereal_3_t3,
                            cereal_4_t3, cereal_5_t3, cereal_6_t3, cereal_7_t3,
                            snack_1_t3, snack_2_t3, snack_3_t3, snack_4_t3,
                            snack_5_t3, snack_6_t3, snack_7_t3, snack_8_t3))

# recode food items
ffq_ch <- data.frame("ifam_no" = ffq_ch$ifam_no, apply(ffq_ch[,-1], 2, recodeFood))



ffq_te <- subset(te_t3,
                 select = c(ifam_no, vegetabl_1a_t3, vegetabl_1b_t3, vegetabl_2_t3,
                            vegetabl_4_t3, fruit_1_t3, fruit_2_t3,
                            vegetabl_3_t3, drink_1_t3, drink_2_t3, drink_3_t3,
                            drink_4_t3, drink_5_t3, drink_6_t3, drink_7a_t3,
                            drink_7b_t3, drink_8a_t3, drink_8b_t3, drink_9_t3,
                            breakf_1_t3,
                            breakf_2_t3, milk_1_t3, milk_2_t3, yoghurt_1_t3,
                            yoghurt_2_t3, fish_1_t3, fish_2_t3, fish_3_t3,
                            meat_1_t3, meat_2_t3, meat_3_t3, meat_4_t3, meat_5_t3,
                            egg_1_t3, egg_2_t3, egg_3_t3, replacem_t3,
                            cheese_1_t3, cheese_2_t3, cheese_3_t3, spread_1_t3,
                            spread_2_t3, spread_3_t3, spread_4_t3, spread_5_t3,
                            oil_t3, cereal_1_t3, cereal_2_t3, cereal_3_t3,
                            cereal_4_t3, cereal_5_t3, cereal_6_t3, cereal_7_t3,
                            snack_1_t3, snack_2_t3, snack_3_t3, snack_4_t3,
                            snack_5_t3, snack_6_t3, snack_7_t3, snack_8_t3,
                            drink_9_t3))

# recode food items
ffq_te <- data.frame("ifam_no" = ffq_te$ifam_no, apply(ffq_te[,-1], 2, recodeFood))


ffq_t3 <- ffq_ch %>% full_join(ffq_te)
idefics$ffq_t3 <- merge(data_id, ffq_t3, by = "ifam_no", all.x = TRUE)

rm(ffq_t0, ffq_t1, ffq_ch, ffq_te, ffq_t3)

### MERGE: FFQ   ---------------------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          idefics$ffq_t0,
                          by = c("id_no"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$ffq_t1,
                          by = c("id_no"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$ffq_t3,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)
# ------------------------------------------------------------------------------



### pqd: Snacks ----------------------------------------------------------------
idefics$pqd_t0 <- subset(idefics$pqd_t0,
                         select = c(id_no, homebreakf, fastmeal, fastsnack))
names(idefics$pqd_t0)[2:4] <- paste0(names(idefics$pqd_t0)[2:4], "_t0")
idefics$pqd_t0$homebreakf_t0 <- recode71(idefics$pqd_t0$homebreakf_t0)
idefics$pqd_t0$fastmeal_t0   <- recodeFastsnack(idefics$pqd_t0$fastmeal_t0)
idefics$pqd_t0$fastsnack_t0  <- recodeFastsnack(idefics$pqd_t0$fastsnack_t0)


idefics$pqd_t1 <- subset(idefics$pqd_t1,
                         select = c(id_no, homebreakf_t1, fastmeal_t1, fastsnack_t1))
idefics$pqd_t1$homebreakf_t1 <- recode71(idefics$pqd_t1$homebreakf_t1)
idefics$pqd_t1$fastmeal_t1   <- recodeFastsnack(idefics$pqd_t1$fastmeal_t1)
idefics$pqd_t1$fastsnack_t1  <- recodeFastsnack(idefics$pqd_t1$fastsnack_t1)


tmp_ch <- subset(ch_t3,
                 select = c(ifam_no, breakfast_t3, fastmeal_t3, fastsnack_t3,
                            tv_dur_wd_t3, tv_dur_we_t3, web_dur_wd_t3,
                            web_dur_we_t3, pc_dur_wd_t3, pc_dur_we_t3,
                            event1_t3, event2_t3, event3_t3, event4_t3,
                            event5_t3, event6a_t3, event6b_t3, event7_t3,
                            event8_t3, event9_t3, event10_t3, event11_t3,
                            event12_t3, club_mbr_t3))

tmp_ch$breakfast_t3  <- recode71(tmp_ch$breakfast_t3)
tmp_ch$fastmeal_t3   <- recodeFastsnack(tmp_ch$fastmeal_t3)
tmp_ch$fastsnack_t3  <- recodeFastsnack(tmp_ch$fastsnack_t3)
tmp_ch$tv_dur_wd_t3  <- recodeMedia(tmp_ch$tv_dur_wd_t3)
tmp_ch$tv_dur_we_t3  <- recodeMedia(tmp_ch$tv_dur_we_t3)
tmp_ch$pc_dur_wd_t3  <- recodeMedia(tmp_ch$pc_dur_wd_t3)
tmp_ch$pc_dur_we_t3  <- recodeMedia(tmp_ch$pc_dur_we_t3)
tmp_ch$web_dur_wd_t3 <- recodeMedia(tmp_ch$web_dur_wd_t3)
tmp_ch$web_dur_we_t3 <- recodeMedia(tmp_ch$web_dur_we_t3)
tmp_ch <- tmp_ch %>%  replace_na(
  list(event1_t3 = 0, event2_t3 = 0, event3_t3 = 0,
       event4_t3 = 0, event5_t3 = 0, event6a_t3 = 0, event6b_t3 = 0,
       event7_t3 = 0, event8_t3 = 0, event9_t3 = 0,
       event10_t3 = 0, event11_t3 = 0, event12_t3 = 0))





tmp_te <- subset(te_t3,
                 select = c(ifam_no, breakfast_t3, fastmeal_t3,
                           fastsnack_t3,
                           tv_dur_wd_t3, tv_dur_we_t3, web_dur_wd_t3,
                           web_dur_we_t3, pc_dur_wd_t3, pc_dur_we_t3,
                           event1_t3, event2_t3, event3_t3, event4_t3,
                           event5_t3, event6a_t3, event6b_t3,
                           event7_t3, event8_t3, event9_t3, event10_t3,
                           event11_t3, event12_t3, club_mbr_t3))

tmp_te$breakfast_t3  <- recode71(tmp_te$breakfast_t3)
tmp_te$fastmeal_t3   <- recodeFastsnack(tmp_te$fastmeal_t3)
tmp_te$fastsnack_t3  <- recodeFastsnack(tmp_te$fastsnack_t3)
tmp_te$tv_dur_wd_t3  <- recodeMedia(tmp_te$tv_dur_wd_t3)
tmp_te$tv_dur_we_t3  <- recodeMedia(tmp_te$tv_dur_we_t3)
tmp_te$pc_dur_wd_t3  <- recodeMedia(tmp_te$pc_dur_wd_t3)
tmp_te$pc_dur_we_t3  <- recodeMedia(tmp_te$pc_dur_we_t3)
tmp_te$web_dur_wd_t3 <- recodeMedia(tmp_te$web_dur_wd_t3)
tmp_te$web_dur_we_t3 <- recodeMedia(tmp_te$web_dur_we_t3)
tmp_te <- tmp_te %>%  replace_na(
  list(event1_t3 = 0, event2_t3 = 0, event3_t3 = 0,
       event4_t3 = 0, event5_t3 = 0, event6a_t3 = 0, event6b_t3 = 0,
       event7_t3 = 0, event8_t3 = 0, event9_t3 = 0,
       event10_t3 = 0, event11_t3 = 0, event12_t3 = 0))

tmp <- tmp_ch %>% full_join(tmp_te)

idefics$ch_te <- merge(data_id, tmp, by = "ifam_no", all.x = TRUE)
rm(tmp, tmp_ch, tmp_te)



### MERGE: pqd t0-t3   ----------------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          idefics$pqd_t0,
                          by = c("id_no"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$pqd_t1,
                          by = c("id_no"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$ch_te,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)
# ------------------------------------------------------------------------------





### YHEI -----------------------------------------------------------------------
idefics$yheit0 <- merge(data_id,
                        subset(idefics$yheit0, select = c(id_no, yhei_100)),
                        by = "id_no", all.x = TRUE)
idefics$yheit1 <- merge(data_id,
                        subset(idefics$yheit1, select = c(id_no, yhei_100)),
                        by = "id_no", all.x = TRUE)
idefics$yheit3 <- merge(data_id,
                        subset(idefics$yheit3, select = c(ifam_no, yhei_100)),
                        by = "ifam_no", all.x = TRUE)

names(idefics$yheit0)[which(names(idefics$yheit0) == "yhei_100")] <- "yhei_100_t0"
names(idefics$yheit1)[which(names(idefics$yheit1) == "yhei_100")] <- "yhei_100_t1"
names(idefics$yheit3)[which(names(idefics$yheit3) == "yhei_100")] <- "yhei_100_t3"

### MERGE: yhei t0-t3   ----------------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          idefics$yheit0,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$yheit1,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)
analysis_dataset <- merge(analysis_dataset,
                          idefics$yheit3,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)

# ------------------------------------------------------------------------------






### Family ---------------------------------------------------------------------
tmp_1 <- subset(hmh_t3,
                select = c(family_id, weight_m_t3, height_m_t3, weight_f_t3, height_f_t3))

tmp_2 <- subset(kinship_t3,
                select = c(family_id, househ_t3))

tmp_3 <- subset(family_t3,
                select = c(family_id, meal_togeth_t3))
tmp_3$meal_togeth_t3[tmp_3$meal_togeth_t3 == 5] <- 14
tmp_3$meal_togeth_t3[tmp_3$meal_togeth_t3 == 4] <- 7
tmp_3$meal_togeth_t3[tmp_3$meal_togeth_t3 == 3] <- 5
tmp_3$meal_togeth_t3[tmp_3$meal_togeth_t3 == 1] <- 0.5

tmp_4 <- merge(subset(core_t3, status_t3 == 3,
                      select = c(ifam_no, family_id, sex_t3, age_t3,
                                 smoker_t3, status_t3, packyear_t3)),
               subset(parents_t3, select = c(ifam_no, smoke_wakeup_t3,
                                             smoke_cig_we_t3, smoke_cigars_we_t3,
                                             club_mbr_t3)),
               by = "ifam_no", all = TRUE)
tmp_4$smok_t3 <- tmp_4$smoker_t3
tmp_4$smok_t3[tmp_4$smoker_t3 == 1] <- 2 #current
tmp_4$smok_t3[tmp_4$smoker_t3 > 2] <- 1  #former
tmp_4 <- tmp_4[-which(tmp_4$family_id == 20288 & is.na(tmp_4$smoke_wakeup_t3)),]

tmp <- subset(tmp_4, select = c(family_id, sex_t3, smok_t3))
tmp <- tmp[!is.na(tmp$family_id),]
tmp_5 <- spread(tmp, sex_t3, smok_t3)
names(tmp_5) <- c("family_id", "smok_m_t3", "smok_f_t3")
tmp_5$smok_max_t3 <- apply(cbind(tmp_5$smok_m_t3, tmp_5$smok_f_t3), 1, max, na.rm = TRUE)
tmp_5$smok_max_t3[which(tmp_5$smok_max_t3 == -Inf)] <- NA

tmp <- subset(tmp_4, select = c(family_id, sex_t3, packyear_t3))
tmp <- tmp[!is.na(tmp$family_id),]
tmp_6 <- spread(tmp, sex_t3, packyear_t3)
names(tmp_6) <- c("family_id", "py_m_t3", "py_f_t3")
tmp_6$packyear_max_t3 <- apply(cbind(tmp_6$py_m_t3, tmp_6$py_f_t3), 1, sum, na.rm = TRUE)

tmp <- subset(tmp_4, select = c(family_id, sex_t3, smoke_wakeup_t3))
tmp <- tmp[!is.na(tmp$family_id),]
tmp_7 <- spread(tmp, sex_t3, smoke_wakeup_t3)
names(tmp_7) <- c("family_id", "sw_m_t3", "sw_f_t3")
tmp_7$smoke_wakeup_max_t3 <- apply(cbind(tmp_7$sw_m_t3, tmp_7$sw_f_t3), 1, min, na.rm = TRUE)
tmp_7$smoke_wakeup_max_t3[which(tmp_7$smoke_wakeup_max_t3 == Inf)] <- NA

tmp <- subset(tmp_4, select = c(family_id, sex_t3, club_mbr_t3))
tmp$club_mbr_t3[tmp$club_mbr_t3 == 2] <- 0
tmp <- tmp[!is.na(tmp$family_id),]
tmp_8 <- spread(tmp, sex_t3, club_mbr_t3)
names(tmp_8) <- c("family_id", "club_m_t3", "club_f_t3")
tmp_8$club_par_t3 <- apply(cbind(tmp_8$club_m_t3, tmp_8$club_f_t3), 1, max, na.rm = TRUE)
tmp_8$club_par_t3[which(tmp_8$club_par_t3 == -Inf)] <- NA



parents <- tmp_5[,c("family_id", "smok_max_t3")]
parents <- merge(parents, tmp_6[,c("family_id", "packyear_max_t3")],
                 by = "family_id", all.x = TRUE)
parents <- merge(parents, tmp_7[,c("family_id", "smoke_wakeup_max_t3")],
                 by = "family_id", all.x = TRUE)
parents <- merge(parents, tmp_8[,c("family_id", "club_par_t3")],
                 by = "family_id", all.x = TRUE)
parents <- merge(parents, tmp_1, by = "family_id", all.x = TRUE)
parents <- merge(parents, tmp_2, by = "family_id", all.x = TRUE)
parents <- merge(parents, tmp_3, by = "family_id", all.x = TRUE)
parents <- merge(data_id, parents, by = "family_id", all.x = TRUE)

rm(tmp, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7, tmp_8)


### MERGE: parents   -----------------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          parents,
                          by = c("id_no", "ifam_no", "family_id"),
                          all.x = TRUE)

# ------------------------------------------------------------------------------



# UI   -------------------------------------------------------------------------
tmp <- subset(ui, survey < 4,
              select = c(id_no, survey,energy_day_exmr, prot_day_exmr,
                         carb_day_exmr, fiber_day_exmr, intake_day_exmr,
                         na_day_exmr))
tmp$survey[tmp$survey == 0] <- "t0"
tmp$survey[tmp$survey == 1] <- "t1"
tmp$survey[tmp$survey == 3] <- "t3"

tmp_1 <- spread_with_many_values(tmp, survey, c(energy_day_exmr, prot_day_exmr,
                                                carb_day_exmr, fiber_day_exmr,
                                                intake_day_exmr, na_day_exmr))

### MERGE: usual intakes   -----------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          tmp_1,
                          by = c("id_no"),
                          all.x = TRUE)
# ------------------------------------------------------------------------------
rm(tmp, tmp_1)

# z-scores   -------------------------------------------------------------------
tmp <- subset(zscores, survey < 4,
              select = c(id_no, ifam_no, survey, z_mets, z_dbp, z_sbp, z_hdl,
                         z_insulin, z_ffmi, z_homa))
tmp$survey[tmp$survey == 0] <- "t0"
tmp$survey[tmp$survey == 1] <- "t1"
tmp$survey[tmp$survey == 3] <- "t3"

tmp_zs <- spread_with_many_values(tmp, survey, c(z_mets, z_dbp, z_sbp, z_hdl,
                                                 z_insulin, z_ffmi, z_homa))

### MERGE: usual intakes   -----------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          tmp_zs,
                          by = c("id_no", "ifam_no"),
                          all.x = TRUE)
rm(tmp, tmp_zs)
# ------------------------------------------------------------------------------





### U-Heft   -------------------------------------------------------------------
pepo <- subset(pepo_t3, select = c(id_cohort, weight_birth_u, compl_week_preg))
pepo <- pepo[order(pepo$id_cohort),]
idefix <- which(substr(pepo$id_cohort,1,1) < 5)
obelix <- which(substr(pepo$id_cohort,1,1) > 4)
pepo$id_no <- pepo$ifam_no <- NA
pepo$id_no[idefix] <- pepo$id_cohort[idefix]
pepo$ifam_no[obelix] <- pepo$id_cohort[obelix]

pepo <- merge(data_id[,1:2],
              subset(pepo, select = -c(ifam_no, id_cohort)), by = "id_no",
              all.x = TRUE)


birth <- subset(analysis_dataset,
                select = c(id_no, birth_w_t0, birth_w_t1, birth_w_t3))

birth <- merge(birth,
               subset(pepo, select = c(id_no, weight_birth_u, compl_week_preg)),
                      by = c("id_no"),
                      all.x = TRUE)
birth$birth_w <- birth$weight_birth_u
birth$birth_w[is.na(birth$birth_w)] <- birth$birth_w_t0[is.na(birth$birth_w)]
birth$birth_w[is.na(birth$birth_w)] <- birth$birth_w_t1[is.na(birth$birth_w)]
birth$birth_w[is.na(birth$birth_w)] <- birth$birth_w_t3[is.na(birth$birth_w)]



### MERGE: usual intakes   -----------------------------------------------------
analysis_dataset <- merge(analysis_dataset,
                          subset(birth, select = c(id_no, birth_w, compl_week_preg)),
                          by = c("id_no"),
                          all.x = TRUE)
rm(pepo, birth, idefix, obelix)

analysis_dataset <- subset(analysis_dataset,
                           select = -c(birth_w_t0, birth_w_t1, birth_w_t3))

ad <- subset(analysis_dataset,
             select = -c(survey_t0, survey_t1, survey_t3, status_t3,
                         birthyear_t1, birthyear_t3))

## recode to harmonise between surveys -----------------------------------------
ad$birth_term_t0[ad$birth_term_t0 == 2] <- 0
ad$birth_term_t1[ad$birth_term_t1 == 2] <- 0
ad$breastf_t3[ad$breastf_t3 == 2] <-  0
ad$breastsc_t3[ad$breastsc_t3 == 2] <- 0
ad$club_mbr_t0[ad$club_mbr_t0 == 2] <- 0
ad$club_mbr_t1[ad$club_mbr_t1 == 2] <- 0
ad$club_mbr_t3[ad$club_mbr_t3 == 2] <- 0
ad$club_par_t3[ad$club_par_t3 == 2] <- 0
ad$cons_beer_t3[ad$cons_beer_t3 >= 7] <- NA
ad$cons_whis_t3[ad$cons_whis_t3 >= 7] <- NA
ad$cons_liq_t3[ad$cons_liq_t3 >= 7] <-  NA
ad$contr_interv_t0[ad$contr_interv_t0 == 2] <- 0
ad$contr_interv_t1[ad$contr_interv_t1 == 2] <- 0
ad$contr_interv_t3[ad$contr_interv_t3 == 2] <- 0
ad$language_t0[ad$language_t0 == 2] <- 0
ad$language_t1[ad$language_t1 == 2] <- 0
ad$preterm_t3[ad$preterm_t3 == 7] <-  NA
ad$otfeed_t3[ad$otfeed_t3 == 2] <-  0
ad$formula_t3[ad$formula_t3 == 2] <- 0 # klappt nicht
ad$preterm_t3[ad$preterm_t3 == 2] <- 0
ad$preterm_t3[ad$preterm_t3 == 7] <- NA
ad$pub_t3[ad$pub_t3 == 2] <-  0



ad$preg_drink <- apply(subset(ad,
                              select = c(cons_beer_t0, cons_whis_t0, cons_liq_t0,
                                         cons_beer_t1, cons_whis_t1, cons_liq_t1,
                                         cons_beer_t3, cons_whis_t3, cons_liq_t3)),
                       1, max, na.rm = TRUE)
ad$preg_drink[ad$preg_drink == -Inf] <- NA

ad$preg_smoke <- apply(subset(ad,
                              select = c(smoke_cig_t0, smoke_cig_t1, smoke_cig_t3,
                                         smoke_tob_t0, smoke_tob_t1, smoke_tob_t3
                              )),
                       1, max, na.rm = TRUE)
ad$preg_smoke[ad$preg_smoke == -Inf] <- NA


### Preterm
ad$preterm <- ad$preterm_t3
ad$preterm[is.na(ad$preterm)] <- ad$birth_term_t0[is.na(ad$preterm)]
ad$preterm[is.na(ad$preterm)] <- ad$birth_term_t1[is.na(ad$preterm)]

### Breastfeeding
ad$bf_total <- ad$bf_1_t3
ad$bf_total[is.na(ad$bf_total)] <- ad$bf_1_t0[is.na(ad$bf_total)]
ad$bf_total[is.na(ad$bf_total)] <- ad$bf_1_t1[is.na(ad$bf_total)]

ad$bf_excl  <- ad$bf_2_t3
ad$bf_excl[is.na(ad$bf_excl)] <- ad$bf_2_t0[is.na(ad$bf_excl)]
ad$bf_excl[is.na(ad$bf_excl)] <- ad$bf_2_t1[is.na(ad$bf_excl)]
ad$bf_excl[ad$bf_excl < 0] <- NA

ad$formula_milk <- ad$formula_t0
ad$formula_milk[is.na(ad$formula_milk)] <- ad$formula_t1[is.na(ad$formula_milk)]
ad$formula_milk[is.na(ad$formula_milk)] <- ad$formula_t3[is.na(ad$formula_milk)]

ad$hd_month <- ad$hd_month_t3
ad$hd_month[is.na(ad$hd_month)] <- ad$hd_month_t0[is.na(ad$hd_month)]
ad$hd_month[is.na(ad$hd_month)] <- ad$hd_month_t1[is.na(ad$hd_month)]

## Pregnancy weight gain
ad$preg_w_up <- ad$preg_w_up_t0
ad$preg_w_up[is.na(ad$preg_w_up)] <- ad$preg_w_up_t1[is.na(ad$preg_w_up)]
ad$preg_w_up[is.na(ad$preg_w_up)] <- ad$preg_w_up_t3[is.na(ad$preg_w_up)]

### Age at birht
ad$age_birth <- ad$age_birth_t0
ad$age_birth[is.na(ad$age_birth)] <- ad$age_birth_t1[is.na(ad$age_birth)]

### Language
ad$language <- ad$language_t0
ad$language[is.na(ad$language)] <- ad$language_t1[is.na(ad$language)]


### Intervention
ad$intervention <- ad$contr_interv_t0


### delete variables that are not needed ---------------------------------------
anad <- subset(ad, select =
                 -c(age_birth_t0, age_birth_t1,
                    birth_term_t0, birth_term_t1, preterm_t3,
                    cons_beer_t0, cons_whis_t0, cons_liq_t0,
                    cons_beer_t1, cons_whis_t1, cons_liq_t1,
                    cons_beer_t3, cons_whis_t3, cons_liq_t3,
                    smoke_cig_t0, smoke_cig_t1, smoke_cig_t3,
                    smoke_tob_t0, smoke_tob_t1, smoke_tob_t3,
                    bf_1_t0, bf_1_t1, bf_1_t3,
                    bf_2_t0, bf_2_t1, bf_2_t3,
                    formula_t0, formula_t1, formula_t3,
                    otfeed_t0, otfeed_t1, otfeed_t3,
                    breastf_st_t0, breastf_st_t1, breastf_st_t3,
                    breastf_en_t0, breastf_en_t1, breastf_en_t3,
                    breastc_st_t0, breastc_st_t1, breastsc_st_t3,
                    breastc_en_t0, breastc_en_t1, breastsc_en_t3,
                    breastf_t0, breastf_t1, breastf_t3,
                    breastc_t0, breastc_t1, breastsc_t3,
                    formula_st_t0, formula_st_t1, formula_st_t3,
                    otfeed_st_t0, otfeed_st_t1, otfeed_st_t3,
                    hd_month_t0, hd_month_t1, hd_month_t3,
                    contr_interv_t0, contr_interv_t1, contr_interv_t3,
                    language_t0, language_t1,
                    preg_w_up_t0, preg_w_up_t1, preg_w_up_t3,
                    avg_weartime_t3, val_time_t0, val_time_t1))




### Specify variable type ------------------------------------------------------
analysisdata <- anad %>%
  mutate_at(
    vars('country', 'sex_child',
         #'birth_term_t0', 'birth_term_t1',
         #'breastc_t0', 'breastc_t1', 'breastsc_t3',
         #'breastf_t0', 'breastf_t1', 'breastf_t3',
         'club_mbr_t0', 'club_mbr_t1', 'club_mbr_t3', 'club_par_t3',
         #'cons_beer_t0', 'cons_beer_t1', 'cons_beer_t3',
         #"cons_liq_t0", "cons_liq_t1", "cons_liq_t3",
         #"cons_whis_t0", "cons_whis_t1", "cons_whis_t3",
         #"contr_interv_t0", "contr_interv_t1", "contr_interv_t3",
         "intervention",
         "event1_t0", "event1_t1", "event1_t3", "event10_t3",
         "event11_t3", "event12_t3",
         "event2_t0", "event2_t1", "event2_t3",
         "event3_t0", "event3_t1", "event3_t3",
         "event4_t0", "event4_t1", "event4_t3",
         "event5_t0", "event5_t1", "event5_t3",
         "event6_t0", "event6_t1", "event6a_t3", "event6b_t3",
         "event7_t0", "event7_t1", "event7_t3",
         "event8_t0", "event8_t1", "event8_t3", "event9_t3",
         "ever_alcohol_t3",
         #"fastmeal_t0", "fastmeal_t1", "fastmeal_t3",
         #"fastsnack_t0", "fastsnack_t1", "fastsnack_t3",
         #"formula_t0", "formula_t1", "formula_t3",
         "formula_milk",
         #"homemeal_t0", "homemeal_t1", "homemeal_t3",
         #'language_t0', 'language_t1',
         'language',
         #"otfeed_t0", "otfeed_t1", "otfeed_t3",
         'preterm',
         'preg_drink', 'preg_smoke',
         'pub_t3',
         "school_t0", "school_t1",
         "smok_max_t3",
         #"smoke_cig_t0", "smoke_cig_t1","smoke_cig_t3",
         "smoke_teen_t3",
         #"smoke_tob_t0","smoke_tob_t1", "smoke_tob_t3",
         "smoke_wakeup_max_t3"
    ),
    ~as_factor(.)
  )


analysisdata$meal_togeth_t3 <- as.ordered(analysisdata$meal_togeth_t3)
analysisdata$income_cat_t0 <- as.ordered(analysisdata$income_cat_t0)
analysisdata$income_cat_t1 <- as.ordered(analysisdata$income_cat_t1)
analysisdata$income_cat_t3 <- as.ordered(analysisdata$income_cat_t3)
analysisdata$isced_max_t0 <- as.ordered(analysisdata$isced_max_t0)
analysisdata$isced_max_t1 <- as.ordered(analysisdata$isced_max_t1)
analysisdata$isced_max_t3 <- as.ordered(analysisdata$isced_max_t3)


analysisdata <- analysisdata %>%
  mutate_at(
    vars('id_no', 'ifam_no','family_id',
         'bedrmedia_number_t0', 'bedrmedia_number_t1', 'bedrmedia_number_t3',
         #"bf_1_t0", "bf_1_t1", "bf_2_t0", "bf_2_t1",
         'bf_total', 'bf_excl',
         #'breastf_en_t0', "breastf_en_t1",
         #'breastf_st_t0', 'breastf_st_t1',
         "birthyear_t0",
         'breakf_1_t0', 'breakf_1_t1', 'breakf_1_t3',
         'breakf_2_t0', 'breakf_2_t1', 'breakf_2_t3', 'breakfast_t3',
         'cereal_1_t0', 'cereal_1_t1', 'cereal_1_t3',
         'cereal_2_t0', 'cereal_2_t1', 'cereal_2_t3',
         'cereal_3_t0', 'cereal_3_t1', 'cereal_3_t3',
         'att_cereal_4_t0', 'cereal_4_t1', 'cereal_4_t3',
         'cereal_5_t0', 'cereal_5_t1', 'cereal_5_t3',
         'cereal_6_t0', 'cereal_6_t1', 'cereal_6_t3', 'cereal_7_t3',
         'att_cheese_1_t0', 'cheese_1_t1', 'cheese_1_t3',
         'att_cheese_2_t0', 'cheese_2_t1', 'cheese_2_t3',
         'cheese_3_t3',
         "drink_1_t0", "drink_1_t1","drink_1_t3",
         "drink_2_t0", "drink_2_t1", "drink_2_t3",
         "drink_3_t0", "drink_3_t1", "drink_3_t3",
         "drink_4_t0", "drink_4_t1", "drink_4_t3",
         "drink_5_t3", "drink_6_t3", "drink_7a_t3", "drink_7b_t3", "drink_8a_t3",
         "drink_8b_t3", "drink_9_t3",
         "egg_1_t0", "egg_1_t1", "egg_1_t3",
         "egg_2_t0", "egg_2_t1", "egg_2_t3",
         "egg_3_t0", "egg_3_t1", "egg_3_t3",
         #"fastmeal_t0", "fastmeal_t1", "fastmeal_t3",
         #"fastsnack_t0", "fastsnack_t1", "fastsnack_t3",
         #'formula_st_t0', 'formula_st_t1',
         "fish_1_t0", "fish_1_t1", "fish_1_t3",
         "fish_2_t0", "fish_2_t1", "fish_2_t3", "fish_3_t3",
         "fruit_1_t0", "fruit_1_t1", "fruit_1_t3",
         "fruit_2_t0", "fruit_2_t1", "fruit_2_t3",
         'height_f_t0', 'height_f_t1',
         'height_m_t0', 'height_m_t1',
         'hd_month',
         "homebreakf_t0", "homebreakf_t1",
         'househ_t0', 'househ_t1',
         "meat_1_t0", "meat_1_t1", "meat_1_t3",
         'att_meat_2_t0', "meat_2_t1", "meat_2_t3",
         'att_meat_3_t0', "meat_3_t1", "meat_3_t3", "meat_4_t3", "meat_5_t3",
         "milk_1_t0", "milk_1_t1", "milk_1_t3",
         "milk_2_t0", "milk_2_t1", "milk_2_t3",
         'oil_t3',
         #'otfeed_st_t0', 'otfeed_st_t1',
         'preg_w_up',
         "replacem_t0", "replacem_t1", "replacem_t3",
         "snack_1_t0", "snack_1_t1","snack_1_t3",
         "snack_2_t0", "snack_2_t1", "snack_2_t3",
         "snack_3_t0", "snack_3_t1", "snack_3_t3",
         "snack_4_t0", "snack_4_t1", "snack_4_t3",
         "snack_5_t0", "snack_5_t1", "snack_5_t3",
         "snack_6_t0", "snack_6_t1", "snack_6_t3",
         "snack_7_t0", "snack_7_t1", "snack_7_t3", "snack_8_t3",
         "spread_1_t0", "spread_1_t1", "spread_1_t3",
         "spread_2_t0", "spread_2_t1", "spread_2_t3",
         "spread_3_t0", "spread_3_t1", "spread_3_t3",
         "spread_4_t0", "spread_4_t1", "spread_4_t3",
         "spread_5_t0", "spread_5_t1", "spread_5_t3",
         'weight_f_t0', 'weight_f_t1',
         'weight_m_t0', 'weight_m_t1',
         'att_vegetabl_1_t0', "vegetabl_1_t1", "vegetabl_1a_t3", "vegetabl_1b_t3",
         'att_vegetabl_2_t0', 'vegetabl_2_t1', "vegetabl_2_t3",
         "vegetabl_3_t0", "vegetabl_3_t1", "vegetabl_3_t3",
         "att_vegetabl_4_t0",  "vegetabl_4_t1", "vegetabl_4_t3",
         "web_dur_wd_t3", "web_dur_we_t3",
         "year_t0", "year_t1", "year_t3",
         "yoghurt_1_t0", "yoghurt_1_t1", "yoghurt_1_t3",
         "yoghurt_2_t0", "yoghurt_2_t1", "yoghurt_2_t3"
    ),
    ~as.integer(.)
  )


### rename
analysisdata <- rename(analysisdata,
                       birthyear = birthyear_t0,
                       sex = sex_child,
                       sleep_total_t0 = att_sleep_sac_corr_t0,
                       age_birth_el = age_birth,
                       bf_excl_el = bf_excl,
                       bf_total_el = bf_total,
                       birth_w_el = birth_w,
                       compl_week_preg_el = compl_week_preg,
                       formula_milk_el = formula_milk,
                       hd_month_el = hd_month,
                       language_t0 = language,
                       preg_drink_el = preg_drink,
                       preg_smoke_el = preg_smoke,
                       preg_w_up_el = preg_w_up,
                       preterm_el = preterm
                       )

### Create teen and child variable
teen <- which(analysisdata$ifam_no %in% te_t3$ifam_no)
child <- which(analysisdata$ifam_no %in% ch_t3$ifam_no)
analysisdata$teen <- NA
analysisdata$teen[teen] <- 1
analysisdata$teen[child] <- 0
analysisdata$teen[is.na(analysisdata$teen) & analysisdata$age_t3 >= 12 ] <- 1
analysisdata$teen[is.na(analysisdata$teen) & analysisdata$age_t3 < 12 ] <- 0

analysisdata <- analysisdata[, c(1:5, order(names(analysisdata)[-c(1:5)]) + 5)]
### ! SAVE ---------------------------------------------------------------------
saveRDS(analysisdata, "data/data2impute.rds")
# ------------------------------------------------------------------------------

















