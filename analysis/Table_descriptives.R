# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Create Table "Descriptives"
#
# ------------------------------------------------------------------------------
library(gt)
library(gtsummary)

load("data/graph.RData")

### Main descriptive table including missing values before imputation
df <- daten.pa.mids$data

### Supplemental descriptive table after imputation
# df <- mice::complete(daten.pa.mids, include = FALSE, action = "stacked")

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


df2 <- df1 %>% pivot_longer(cols = 1:51, #starts_with("age"),
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
                   "formula", "preg_smoke", "familymeal", "alc", "pub", "smoke"),
                 ~ as.factor(.x)) %>%
       relocate(c(id, group,
                  country, sex, migrant, preg_week,  preg_smoke, bage,
                  bweight, bf, formula, hdiet,
                  age, school, bmi, wb, media, pa, sleep, yhei, familymeal,
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


table1 <- df3 %>% select(.,-c(id)) %>%
  tbl_summary(
        by = group,
        statistic = list(all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"),
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
table1 %>% as_gt() %>% gtsave("table_desc_with_missing.rtf", path = "results/")
#table1 %>% as_gt() %>% gtsave("tableS_desc_after_imputation.rtf", path = "results/")


