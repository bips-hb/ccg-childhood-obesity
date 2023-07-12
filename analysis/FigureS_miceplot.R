# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Create Figure of imputed data vs observed data
#
# ------------------------------------------------------------------------------
library(gridExtra)
library(ggplot2)

load("data/graph.RData")

imp <- daten.pa.mids
names(imp$imp)[c(2,6,7,8,15,26,38)] <- c("region", "agebirth","breastfeed","birthweight",
                                         "avm.0","avm.1","avm.2")
names(imp$data)[c(2,6,7,8,15,26,38)] <- c("region", "agebirth","breastfeed","birthweight",
                                          "avm.0","avm.1","avm.2")

impC <- complete(imp,"long",include = T)

# For continuous variables ---------------------------------------------------------------
bw <- mice::bwplot(imp)
dp <- mice::densityplot(imp)

png(filename ="results/imp_boxplot.png", units = "cm", width = 16, height = 25, res = 120)
  bw
dev.off()
  png(filename ="results/imp_density.png", units = "cm", width = 16, height = 25, res = 120)
  dp
dev.off()

# For discrete variables -----------------------------------------------------------------
b1 <- micebarplot(impC, "migrant")
b2 <- micebarplot(impC, "formula")
b3 <- micebarplot(impC, "preg_smoke")
b4 <- micebarplot(impC, "income")
b5 <- micebarplot(impC, "isced")
b6 <- micebarplot(impC, "school.0")
b7 <- micebarplot(impC, "familymeal.0")
b8 <- micebarplot(impC, "income.1")
b9 <- micebarplot(impC, "isced.1")
b10<- micebarplot(impC, "school.1")
b11<- micebarplot(impC, "familymeal.1")
b12<- micebarplot(impC, "income.2")
b13<- micebarplot(impC, "isced.2")
b14<- micebarplot(impC, "familymeal.1")
b15<- micebarplot(impC, "pub.2")
b16<- micebarplot(impC, "alc.2")
b17<- micebarplot(impC, "smoke.2")

ga <- grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17, nrow = 5, ncol = 4)
ggsave("results/imp_bar.png",ga,  units = "cm", width = 16, height = 20, dpi = 120)




