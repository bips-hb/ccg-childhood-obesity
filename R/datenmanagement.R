
datenmanagement <- function(imp){
  daten <- imp %>%
    dplyr::select(.,c(.imp, .id,
                      sex, country3, language_t0, income_t0, isced_max_t0,
                      age_birth_el, bf_total_el, birth_w_el, compl_week_preg_el,
                      formula_milk_el, hd_month_el, preg_smoke_el,
                      age_t0, school_t0,
                      media_t0, bmi_t0, bmi_m_t0, familymeal_t0,
                      phys_activ_t0, sleep_total_t0,
                      wb_score_t0, yhei_100_t0, z_homa_t0,
                      age_t1, school_t1,
                      media_t1, bmi_t1, bmi_m_t1, familymeal_t1, income_t1,
                      isced_max_t1, phys_activ_t1, sleep_total_t1,
                      wb_score_t1, yhei_100_t1, z_homa_t1,
                      age_t3,
                      media_t3, bmi_t3, bmi_m_t3, ever_alcohol_t3,
                      familymeal_t3, income_t3, isced_max_t3, phys_activ_t3,
                      pub_t3, sleep_total_t3, smoke_teen_t3,
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

  daten$wb_t3  <- as.numeric(daten$wb_t3)
  daten$preg_week_el <- as.numeric(daten$preg_week_el)

  daten
}
