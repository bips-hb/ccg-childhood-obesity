makescores <- function(daten){
  library(dplyr)
  ### Well-being events ----------------------------------------------------------
  daten <- daten %>%
    mutate(any_event_t0 =
             select(.,event1_t0 , event2_t0 , event3_t0 , event4_t0,
                    event5_t0, event6_t0, event7_t0, event8_t0) %>%
             apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)))

  daten <- daten %>%
    mutate(any_event_t1 =
             select(.,event1_t1 , event2_t1 , event3_t1 , event4_t1,
                    event5_t1, event6_t1, event7_t1, event8_t1) %>%
             apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)))

  daten <- daten %>%
    mutate(any_event_t1 =
             if_else(any_event_t0 > any_event_t1, any_event_t0, any_event_t1))

  daten <- daten %>%
    mutate(any_event_t3 =
             select(.,event1_t3 , event2_t3 , event3_t3 , event4_t3,
                    event5_t3, event6a_t3, event6b_t3, event7_t3, event8_t3,
                    event9_t3, event10_t3, event11_t3, event12_t3) %>%
             apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)))

  daten <- daten %>%
    mutate(any_event_t3 =
             if_else(any_event_t1 > any_event_t3, any_event_t1, any_event_t3))


  ### veggi score ----------------------------------------------------------------
  daten <- daten %>%
    mutate(
      ffq_total_t0 =
        select(.,breakf_1_t0, breakf_2_t0,
               cereal_1_t0, cereal_2_t0, cereal_3_t0, att_cereal_4_t0,
               cereal_5_t0, cereal_6_t0,
               att_cheese_1_t0, att_cheese_2_t0,
               drink_1_t0, drink_2_t0, drink_3_t0, drink_4_t0,
               egg_1_t0, egg_2_t0, egg_3_t0,
               fish_1_t0, fish_2_t0,
               fruit_1_t0, fruit_2_t0,
               meat_1_t0, att_meat_2_t0, att_meat_3_t0,
               milk_1_t0, milk_2_t0, replacem_t0,
               spread_1_t0, spread_2_t0, spread_3_t0, spread_4_t0, spread_5_t0,
               snack_1_t0, snack_2_t0, snack_3_t0, snack_4_t0,
               snack_5_t0, snack_6_t0, snack_7_t0,
               att_vegetabl_1_t0, vegetabl_3_t0, att_vegetabl_4_t0,
               yoghurt_1_t0, yoghurt_2_t0) %>%
        apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

      veg_total_t0 =
        select(., fruit_1_t0, fruit_2_t0,
               att_vegetabl_1_t0, vegetabl_3_t0, att_vegetabl_4_t0) %>%
        apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

      veggiscore_t0 = 100 * veg_total_t0 / ffq_total_t0)


  daten <- daten %>%
    mutate(ffq_total_t1 =
             select(.,breakf_1_t1, breakf_2_t1,
                    cereal_1_t1, cereal_2_t1, cereal_3_t1, cereal_4_t1,
                    cereal_5_t1, cereal_6_t1,
                    cheese_1_t1, cheese_2_t1,
                    drink_1_t1, drink_2_t1, drink_3_t1, drink_4_t1,
                    egg_1_t1, egg_2_t1, egg_3_t1,
                    fish_1_t1, fish_2_t1,
                    fruit_1_t1, fruit_2_t1,
                    meat_1_t1, meat_2_t1, meat_3_t1,
                    milk_1_t1, milk_2_t1, replacem_t1,
                    spread_1_t1, spread_2_t1, spread_3_t1, spread_4_t1, spread_5_t1,
                    snack_1_t1, snack_2_t1, snack_3_t1, snack_4_t1,
                    snack_5_t1, snack_6_t1, snack_7_t1,
                    vegetabl_1_t1, vegetabl_3_t1, vegetabl_4_t1,
                    yoghurt_1_t1, yoghurt_2_t1) %>%
             apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

           veg_total_t1 =
             select(., fruit_1_t1, fruit_2_t1,
                    vegetabl_1_t1, vegetabl_3_t1, vegetabl_4_t1) %>%
             apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

           veggiscore_t1 = 100 * veg_total_t1 / ffq_total_t1)

  daten <- daten %>%
    mutate(
      ffq_total_ch_t3 =
        select(.,breakf_1_t3, breakf_2_t3,
               cereal_1_t3, cereal_2_t3, cereal_3_t3, cereal_4_t3,
               cereal_5_t3, cereal_6_t3, cereal_7_t3,
               cheese_1_t3, cheese_2_t3, cheese_3_t3,
               drink_1_t3, drink_2_t3, drink_3_t3, drink_4_t3, drink_5_t3,
               drink_6_t3, drink_7a_t3, drink_7b_t3, drink_8a_t3, drink_8b_t3,
               egg_1_t3, egg_2_t3, egg_3_t3,
               fish_1_t3, fish_2_t3, fish_3_t3,
               fruit_1_t3, fruit_2_t3,
               meat_1_t3, meat_2_t3, meat_3_t3, meat_4_t3, meat_5_t3,
               milk_1_t3, milk_2_t3, replacem_t3,
               oil_t3,
               spread_1_t3, spread_2_t3, spread_3_t3, spread_4_t3, spread_5_t3,
               snack_1_t3, snack_2_t3, snack_3_t3, snack_4_t3,
               snack_5_t3, snack_6_t3, snack_7_t3, snack_8_t3,
               vegetabl_1a_t3, vegetabl_1b_t3, vegetabl_2_t3,
               vegetabl_3_t3, vegetabl_4_t3,
               yoghurt_1_t3, yoghurt_2_t3) %>%
        apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

      ffq_total_te_t3 =
        select(.,breakf_1_t3, breakf_2_t3,
               cereal_1_t3, cereal_2_t3, cereal_3_t3, cereal_4_t3,
               cereal_5_t3, cereal_6_t3, cereal_7_t3,
               cheese_1_t3, cheese_2_t3, cheese_3_t3,
               drink_1_t3, drink_2_t3, drink_3_t3, drink_4_t3, drink_5_t3,
               drink_6_t3, drink_7a_t3, drink_7b_t3, drink_8a_t3,
               drink_8b_t3,
               drink_9_t3,
               egg_1_t3, egg_2_t3, egg_3_t3,
               fish_1_t3, fish_2_t3, fish_3_t3,
               fruit_1_t3, fruit_2_t3,
               meat_1_t3, meat_2_t3, meat_3_t3, meat_4_t3, meat_5_t3,
               milk_1_t3, milk_2_t3, replacem_t3,
               oil_t3,
               spread_1_t3, spread_2_t3, spread_3_t3, spread_4_t3, spread_5_t3,
               snack_1_t3, snack_2_t3, snack_3_t3, snack_4_t3,
               snack_5_t3, snack_6_t3, snack_7_t3, snack_8_t3,
               vegetabl_1a_t3, vegetabl_1b_t3, vegetabl_2_t3,
               vegetabl_3_t3, vegetabl_4_t3,
               yoghurt_1_t3, yoghurt_2_t3) %>%
        apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

      veg_total_t3 =
        select(., fruit_1_t3, fruit_2_t3,
               vegetabl_1a_t3, vegetabl_1b_t3, vegetabl_2_t3,
               vegetabl_3_t3, vegetabl_4_t3) %>%
        apply(1, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),

      veggiscore_t3 = case_when(
        age_t3 < 12 ~ 100 * veg_total_t3 / ffq_total_ch_t3,
        age_t3 >=12 ~ 100 * veg_total_t3 / ffq_total_te_t3))


  daten <- daten %>%
    mutate(
      waist_to_height_t0 = waist_t0 / height_t0,
      waist_to_height_t1 = waist_t1 / height_t1,

      bmi_m_t0 = weight_m_t0 / ((height_m_t0 / 100)**2),
      bmi_m_t1 = weight_m_t1 / ((height_m_t1 / 100)**2),
      bmi_m_t3 = weight_m_t3 / ((height_m_t3 / 100)**2),

      media_t0 = 5 * (tv_dur_wd_t0 + pc_dur_wd_t0) +
        2 * (tv_dur_we_t0 + pc_dur_we_t0),
      media_t1 = 5 * (tv_dur_wd_t1 + pc_dur_wd_t1) +
        2 * (tv_dur_we_t1 + pc_dur_we_t1),
      media_t3 = 5 * (tv_dur_wd_t3 + pc_dur_wd_t3) +
        2 * (tv_dur_we_t3 + pc_dur_we_t3) +
        5 * web_dur_wd_t3 + 2 * web_dur_we_t3,

      protein_t0 = 1000 * prot_day_exmr_t0 / energy_day_exmr_t0,
      protein_t1 = 1000 * prot_day_exmr_t1 / energy_day_exmr_t1,
      protein_t3 = 1000 * prot_day_exmr_t3 / energy_day_exmr_t3,
      fibre_t0 = 1000 * fiber_day_exmr_t0 / energy_day_exmr_t0,
      fibre_t1 = 1000 * fiber_day_exmr_t1 / energy_day_exmr_t1,
      fibre_t3 = 1000 * fiber_day_exmr_t3 / energy_day_exmr_t3,
      carb_t0 = 1000 * carb_day_exmr_t0 / energy_day_exmr_t0,
      carb_t1 = 1000 * carb_day_exmr_t1 / energy_day_exmr_t1,
      carb_t3 = 1000 * carb_day_exmr_t3 / energy_day_exmr_t3,

      fastfood_t0 = 0.5 * (fastmeal_t0 + fastsnack_t0),
      fastfood_t1 = 0.5 * (fastmeal_t1 + fastsnack_t1),
      fastfood_t3 = 0.5 * (fastmeal_t3 + fastsnack_t3),

      bp_t0 = 0.5 * (z_sbp_t0 + z_dbp_t0),
      bp_t1 = 0.5 * (z_sbp_t1 + z_dbp_t1),
      bp_t3 = 0.5 * (z_sbp_t3 + z_dbp_t3)
    )


  ### Drop unnecessary variables -------------------------------------------------
  daten <- daten %>%
    select(
      -c(att_cereal_4_t0, att_cheese_1_t0, att_cheese_2_t0,
         att_meat_2_t0, att_meat_3_t0,
         att_vegetabl_1_t0, att_vegetabl_4_t0,
         avm_1_week_t0, avm_1_week_t1, avm_1_week_t3,
         birthyear,
         breakf_1_t0, breakf_1_t1, breakf_1_t3,
         breakf_2_t0, breakf_2_t1, breakf_2_t3, breakfast_t3,
         carb_day_exmr_t0, carb_day_exmr_t1, carb_day_exmr_t3,
         cereal_1_t0, cereal_1_t1, cereal_1_t3, cereal_2_t0, cereal_2_t1,
         cereal_2_t3, cereal_3_t0, cereal_3_t1, cereal_3_t3, cereal_4_t1,
         cereal_4_t3, cereal_5_t0, cereal_5_t1, cereal_5_t3, cereal_6_t0,
         cereal_6_t1, cereal_6_t3, cereal_7_t3, cheese_1_t1, cheese_1_t3,
         cheese_2_t1, cheese_2_t3, cheese_3_t3,
         drink_1_t0, drink_1_t1, drink_1_t3,
         drink_2_t0, drink_2_t1, drink_2_t3, drink_3_t0, drink_3_t1,
         drink_3_t3, drink_4_t0, drink_4_t1, drink_4_t3, drink_5_t3,
         drink_6_t3, drink_7a_t3, drink_7b_t3, drink_8a_t3, drink_8b_t3,
         drink_9_t3,
         egg_1_t0, egg_1_t1, egg_1_t3, egg_2_t0,
         egg_2_t1, egg_2_t3, egg_3_t0, egg_3_t1, egg_3_t3,
         energy_day_exmr_t0, energy_day_exmr_t1, energy_day_exmr_t3,
         event1_t0, event1_t1,
         event1_t3, event10_t3, event11_t3, event12_t3, event2_t0,
         event2_t1, event2_t3, event3_t0, event3_t1, event3_t3,
         event4_t0, event4_t1, event4_t3, event5_t0, event5_t1,
         event5_t3, event6_t0, event6_t1, event6a_t3, event6b_t3,
         event7_t0, event7_t1, event7_t3, event8_t0, event8_t1,
         event8_t3, event9_t3,
         # family_id,
         fastmeal_t0, fastmeal_t1, fastmeal_t3,
         fastsnack_t0, fastsnack_t1, fastsnack_t3,
         ffq_total_ch_t3, ffq_total_t0, ffq_total_t1, ffq_total_te_t3,
         fiber_day_exmr_t0, fiber_day_exmr_t1, fiber_day_exmr_t3,
         fish_1_t0, fish_1_t1, fish_1_t3, fish_2_t0,
         fish_2_t1, fish_2_t3, fish_3_t3,
         fruit_1_t0, fruit_1_t1, fruit_1_t3, fruit_2_t0, fruit_2_t1, fruit_2_t3,
         height_f_t0, height_f_t1, height_f_t3,
         height_m_t0, height_m_t1, height_m_t3,
         height_t0, height_t1,
         # ifam_no,
         intake_day_exmr_t0, intake_day_exmr_t1, intake_day_exmr_t3,
         intervention,
         meat_1_t0, meat_1_t1,
         meat_1_t3, meat_2_t1, meat_2_t3, meat_3_t1, meat_3_t3,
         meat_4_t3, meat_5_t3, milk_1_t0, milk_1_t1, milk_1_t3,
         milk_2_t0, milk_2_t1, milk_2_t3,
         na_day_exmr_t0, na_day_exmr_t1, na_day_exmr_t3, oil_t3,
         pc_dur_wd_t0, pc_dur_wd_t1, pc_dur_wd_t3,
         pc_dur_we_t0, pc_dur_we_t1, pc_dur_we_t3,
         preterm_el,
         prot_day_exmr_t0, prot_day_exmr_t1, prot_day_exmr_t3,
         replacem_t0, replacem_t1, replacem_t3,
         sleep_night_t1, sleep_night_t3,
         snack_1_t0, snack_1_t1, snack_1_t3, snack_2_t0, snack_2_t1, snack_2_t3,
         snack_3_t0, snack_3_t1, snack_3_t3, snack_4_t0, snack_4_t1,
         snack_4_t3, snack_5_t0, snack_5_t1, snack_5_t3, snack_6_t0,
         snack_6_t1, snack_6_t3, snack_7_t0, snack_7_t1, snack_7_t3,
         snack_8_t3, spread_1_t0, spread_1_t1, spread_1_t3, spread_2_t0,
         spread_2_t1, spread_2_t3, spread_3_t0, spread_3_t1, spread_3_t3,
         spread_4_t0, spread_4_t1, spread_4_t3, spread_5_t0, spread_5_t1,
         spread_5_t3, sweet_prop_t0, sweet_prop_t1, sweet_prop_t3, teen,
         tv_dur_wd_t0, tv_dur_wd_t1, tv_dur_wd_t3, tv_dur_we_t0, tv_dur_we_t1,
         tv_dur_we_t3,
         veg_total_t0, veg_total_t1, veg_total_t3, vegetabl_1_t1, vegetabl_1a_t3,
         vegetabl_1b_t3, vegetabl_2_t3, vegetabl_3_t0, vegetabl_3_t1,
         vegetabl_3_t3, vegetabl_4_t1, vegetabl_4_t3,
         waist_t0, waist_t1, web_dur_wd_t3, web_dur_we_t3,
         weight_f_t0, weight_f_t1, weight_f_t3, weight_m_t0, weight_m_t1, weight_m_t3,
         year_t0, year_t1, year_t3,
         yoghurt_1_t0, yoghurt_1_t1, yoghurt_1_t3,
         yoghurt_2_t0, yoghurt_2_t1, yoghurt_2_t3,
         z_sbp_t0, z_sbp_t1, z_sbp_t3, z_dbp_t0, z_dbp_t1, z_dbp_t3
      )
    )


  # ----------------------------------------------------------------------------------------
  #  1) Relocate
  # ----------------------------------------------------------------------------------------

  daten <- daten %>%
    relocate(c(country, sex, age_t0, cpscore_t0, school_t0, age_t1,
               cpscore_t1, school_t1, age_t3, cpscore_t3),
             .after = '.id')


  # ----------------------------------------------------------------------------------------
  #  2) Reduce number of categories
  # ----------------------------------------------------------------------------------------

  ## Country: South, Central, North countries
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




  # ----------------------------------------------------------------------------------------
  # 3) Look at integers
  # ----------------------------------------------------------------------------------------
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

  daten <- daten %>%
    dplyr::select(.,-c(country,
                       bedrmedia_number_t0, bedrmedia_number_t1,
                       bedrmedia_number_t3,
                       homebreakf_t0, homebreakf_t1,
                       meal_togeth_t3,
                       income_cat_t0, income_cat_t1, income_cat_t3
    ))


  ### Sort variables   ---------------------------------------------------------------------
  daten <- daten %>% select(order(colnames(.)))

  daten <- daten %>%
    relocate(c(country3, sex, age_t0, cpscore_t0, school_t0, age_t1,
               cpscore_t1, school_t1, age_t3, cpscore_t3),
             .after = .imp)

  daten <- daten %>%
    dplyr::select(.,c(.imp, .id, sex, country3),
                  ends_with(c("el", "_t0", "_t1", "_t3")))
}
