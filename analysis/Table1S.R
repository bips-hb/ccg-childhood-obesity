# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
# Date:    JUL 2021
#
# Purpose: Create Table 1
#
# ------------------------------------------------------------------------------
library(gt)
library(gtsummary)

load("data_not_load/06_graph-pa.RData")
rm(fg.pa, ebenen.pa, V.pa)

df <- mice::complete(daten.pa.mids, include = FALSE, action = "stacked")


df1 <- df %>% rename(
         "sex.iv" = "sex",
         "country.iv" = "country3",
         "migrant.iv" = "migrant",
         "income.0" = "income",
         "isced.0" = "isced",
         "bage.el" = "bage",
         "bf.el" = "bf",
         "bweight.el" = "bweight",
         "preg_week.el" = "preg_week",
         "preg_smoke.el" = "preg_smoke",
         "formula.el" = "formula",
         "hdiet.el" = "hdiet") %>%
        mutate(id = 1:nrow(df))

levels(df1$country.iv) <-  c("1","2","3")   # Central   North   South
levels(df1$familymeal.0) <-  c("0","1")     # less at least once a day
levels(df1$familymeal.1) <-  c("0","1")     # less at least once a day
levels(df1$familymeal.2) <-  c("0","1")     # less at least once a day

df1 <- df1  %>%  mutate_if(., is.factor, ~ as.numeric(as.character(.x)))




# Review: include BMI categories --------------------------
analysis_data <- readRDS("data_not_load/02_data2impute.RDS")
# idnr <- analysis_data$id_no
# save(idnr, file = "data_not_load/review_id.rda")

# prepare the following dataset on the CDS
# C:\Users\foraita\Documents\Foraita\ccg\review
load("data_not_load/bmi_cat.rda")

# merge BMI categories to df
ad_sub <- subset(analysis_data, select = c(id_no, country, sex, age_t0, bmi_t0))
ad_sub <- merge(ad_sub, core_out,
                by.x = c("id_no", "country", "sex", "age_t0", "bmi_t0"),
                by.y = c("id_no", "country", "sex", "age.0", "bmi.0"),
                all.x = TRUE, sort = FALSE)
names(ad_sub)[c(4:5, 10:12)] <- c("age.0", "bmi.0", "age.2", "bmi.2", "bmicat.2")

# bind the dataset 10 times
ad_sub_mult <- data.frame(bmicat.0 = rep(ad_sub$bmicat.0, 10),
                          bmicat.1 = rep(ad_sub$bmicat.1, 10),
                          bmicat.2 = rep(ad_sub$bmicat.2, 10))

together <- data.frame(df1, ad_sub_mult)

# review end ----------------------------------------------




df2 <- together %>% pivot_longer(cols = c(1:51, 53:55), #starts_with("age"),
                            names_to = "group",
                            values_to = "val")


df3 <- df2 %>%
       separate(group, c("variable","group"), sep="\\.") %>%
       pivot_wider(names_from = variable,
                   values_from = val) %>%
       mutate(bf = exp(bf) - 1,
              pa = exp(pa) - 1,
              media = media / 7,
              hdiet = exp(hdiet) - 1,
              wb = wb / 48 * 100) %>%
       mutate_at(c("group", "sex", "migrant", "country", "school", "income", "isced",
                   "formula", "preg_smoke", "familymeal", "alc", "pub", "smoke", "bmicat"),
                 ~ as.factor(.x)) %>%
       relocate(c(id, group,
                  country, sex, migrant, preg_week,  preg_smoke, bage,
                  bweight, bf, formula, hdiet,
                  age, school, bmi, bmicat, wb, media, pa, sleep, yhei, familymeal,
                  homa, pub, alc, smoke, bmi_m, income, isced))

levels(df3$group) <-  c("Baseline", "FU1", "FU2", "EL", "IV")
levels(df3$sex) <- c("no", "yes")
levels(df3$migrant) <- c("yes", "no")
levels(df3$country) <- c("Central", "North", "South")
levels(df3$school) <- c("Kindergarden", "School", "neither")
levels(df3$familymeal) <- c("no", "yes")
levels(df3$income) <- c("low", "middle", "high")
levels(df3$isced) <- c("low", "middle", "high")
levels(df3$formula) <- c("no", "yes")
levels(df3$preg_smoke) <- c("never", "rarely", "several occasions a week", "daily")
levels(df3$alc) <- c("no", "yes")
levels(df3$smoke) <- c("no", "yes")
levels(df3$pub) <- c("no", "yes")
levels(df3$bmicat) <- c("underweight", "underweight", "underweight", "normal weight", "overweight", "obesity")



table1s <- df3 %>% select(.,-c(id)) %>%
  tbl_summary(
        by = group,
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        digits = list(all_categorical() ~ c(0, 1)),
        label = list(country ~ "Country",
                     sex ~ "Female",
                     migrant ~ "Migration background",
                     bweight ~ "Birthweight [g]",
                     bage ~ "Mother's age at birth",
                     preg_smoke ~ "Tobacco smoking during pregnancy",
                     formula ~ "Was fed with formula milk",
                     bf ~ "Total breastfeeding [months]",
                     preg_week ~ "Completed weeks of pregnancy",
                     hdiet ~ "Fully integrated into household's diet [month]",
                     age ~ "Age",
                     bmi ~ "BMI z-score",
                     bmicat ~ "BMI",
                     bmi_m ~ "Mother's BMI",
                     school ~ "School",
                     isced ~ "ISCED",
                     income ~ "Household's income",
                     media ~ "Audiovisual media consumption [h/day]",
                     familymeal ~ "Daily family meals",
                     pa ~ "Physical activity [h/week]",
                     sleep ~ "Total sleep [h/day]",
                     wb ~ "Well-being [%]",
                     yhei ~ "Youth healthy eating index [%]",
                     homa ~ "Homa index z-score",
                     alc ~ "Ever alcohol drinking",
                     smoke ~ "Ever tobacco smoking",
                     pub ~ "Pubertal"
                     ),
        missing_text = "Missing")


### save table ---------------------------------------------------------------
table1s %>% as_gt() %>% gtsave("table1s_revision.rtf", path = "results/")
# write_rds(df3, file = "data_not_load/table1s_review.RDS")


