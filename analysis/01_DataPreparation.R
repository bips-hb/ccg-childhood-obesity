# ------------------------------------------------------------------------------
#
# Project: Cohort Causal Graph
#
# Author:  R. Foraita
#
# Purpose: Prepare data and IDs
#
# ------------------------------------------------------------------------------

idefics <- load_IDEFICS(
  pfad = c("...",
           "...",
           "..."),
  folder = c("..", "..", "..",
             "..", "..", "..",
             rep("..", 3)),
  datensaetze = c("..", "..", "..",
                  "..", "..", "..",
                  "..", "..", ".."),
  version = c("..", "..","..",
              "..", "..", ".."),
  times = c(3,3,3), nott0t1 = 3
)


core_t3    <- fetch(dbSendQuery(db.i.family, "select * from core"), n=-1)
ch_t3      <- fetch(dbSendQuery(db.i.family, "select * from ch"), n=-1)
te_t3      <- fetch(dbSendQuery(db.i.family, "select * from te"), n=-1)
early_t3   <- fetch(dbSendQuery(db.i.family, "select * from paec"), n=-1)
sac_t3     <- fetch(dbSendQuery(db.i.family, "select * from sac_core"), n=-1)
perinatal_t3 <- fetch(dbSendQuery(db.i.family, "select * from pe_po"), n=-1)
parents_t3 <- fetch(dbSendQuery(db.i.family, "select * from ad"), n=-1)
hmh_t3     <- fetch(dbSendQuery(db.i.family, "select * from hmh"), n=-1)
kinship_t3 <- fetch(dbSendQuery(db.i.family, "select * from kh"), n=-1)
family_t3  <- fetch(dbSendQuery(db.i.family, "select * from fa"), n=-1)
pepo_t3    <- fetch(dbSendQuery(db.i.family, "select * from pe_po"), n=-1)
zscores <- fetch(dbSendQuery(db.i.cohort, "select * from zscores_long"), n=-1)
ui <- fetch(dbSendQuery(db.i.cohort, "select * from ui_long"), n=-1)
acc_long <- fetch(dbSendQuery(db.i.cohort, "select * from acc_long"), n=-1)

dbDisconnect(db.i.family)

names(core_t3)      <- tolower(names(core_t3))
names(ch_t3)        <- tolower(names(ch_t3))
names(te_t3)        <- tolower(names(te_t3))
names(early_t3)     <- tolower(names(early_t3))
names(sac_t3)       <- tolower(names(sac_t3))
names(perinatal_t3) <- tolower(names(perinatal_t3))
names(parents_t3)   <- tolower(names(parents_t3))
names(hmh_t3)       <- tolower(names(hmh_t3))
names(kinship_t3)   <- tolower(names(kinship_t3))
names(family_t3)    <- tolower(names(family_t3))
names(pepo_t3)      <- tolower(names(pepo_t3))
names(zscores)      <- tolower(names(zscores))
names(ui)           <- tolower(names(ui))
names(acc_long)     <- tolower(names(acc_long))

names(core_t3)[which(names(core_t3) == "id_no")]   <- "ifam_no"
names(ch_t3)[which(names(ch_t3) == "id_no")]       <- "ifam_no"
names(te_t3)[which(names(te_t3) == "id_no")]       <- "ifam_no"
names(early_t3)[which(names(early_t3) == "id_no")] <- "ifam_no"
names(sac_t3)[which(names(sac_t3) == "id_no")] <- "ifam_no"
names(perinatal_t3)[which(names(perinatal_t3) == "id_no")] <- "ifam_no"
names(parents_t3)[which(names(parents_t3) == "id_no")]     <- "ifam_no"
names(pepo_t3)[which(names(pepo_t3) == "id_no")]     <- "id_cohort"
names(zscores)[which(names(zscores) == "id_no_idefics")]   <- "id_no"
names(zscores)[which(names(zscores) == "id_no_ifamily")]   <- "ifam_no"
names(ui)[which(names(ui) == "id_no_idefics")] <- "id_no"
names(ui)[which(names(ui) == "id_no_ifamily")] <- "ifam_no"
names(acc_long)[which(names(acc_long) == "id_no_idefics")] <- "id_no"
names(acc_long)[which(names(acc_long) == "id_no_ifamily")] <- "ifam_no"

names(idefics$yheit3)[which(names(idefics$yheit3) == "id_no")] <- "ifam_no"


# ------------------------------------------------------------------------------
# INCLUSION: Only children that participated at T0, T1 and T3
# ------------------------------------------------------------------------------
data_id <- core_t3[which(core_t3$idefics_id %in%
                intersect(intersect(idefics$core_t0$id_no,
                                    idefics$core_t1$id_no), core_t3$idefics_id)),
               c("idefics_id", "ifam_no", "family_id")]
data_id <- data_id[order(data_id$idefics_id),]
data_id <- rename(data_id, id_no = idefics_id)
rownames(data_id) <- 1:nrow(data_id)



i1 <- lapply(idefics[c(1:8)], function(x) x[x$id_no %in% data_id$id_no,])
i2 <- idefics$yheit3[idefics$yheit3$ifam_no %in% data_id$ifam_no,]
idefics <- c(i1, "yheit3"=list(i2))


# id_cohort in zscores und ui stimmt nicht
zs1 <- merge(data_id, select(zscores, -c(id_cohort, ifam_no)),
            by = "id_no", all.x = TRUE)
zs2 <- merge(data_id, select(zscores, -c(id_cohort, id_no)),
             by = "ifam_no", all.x = TRUE)
zscores <- rbind(zs1, zs2)
zscores <- zscores[order(zscores$id_no),]
row.names(zscores) <- 1:nrow(zscores)
rm(zs1, zs2)

u1 <- merge(data_id, select(ui, -c(id_cohort, ifam_no, family_id)),
             by = "id_no", all.x = TRUE)
u2 <- merge(data_id, select(ui, -c(id_cohort, id_no, family_id)),
             by = "ifam_no", all.x = TRUE)
ui <- rbind(u1[u1$survey < 4,], u2[u2$survey < 4,])
ui <- ui[order(ui$id_no),]
row.names(ui) <- 1:nrow(ui)
rm(u1, u2)

# ------------------------------------------------------------------------------
# Either child or teen questionnaire: keep child if age < 12
# intersection between child and teens
# ------------------------------------------------------------------------------
child <- ch_t3$ifam_no
teen  <- te_t3$ifam_no
doppelte_ids <- intersect(child, teen)
core_t3[which(core_t3$ifam_no %in% doppelte_ids), c("ifam_no", "age_t3")]
# all < 12 years -> child questionaire
te_t3 <- te_t3[-which(te_t3$ifam_no %in% doppelte_ids), ]
rm(child, teen, doppelte_ids)


# Consumer prices --------------------------------------------------------------
cp <- read_sas("data_not_load/ConsumerPrices/cp.sas7bdat")
names(cp) <- tolower(names(cp))


### ! SAVE   -------------------------------------------------------------------
save.image("data/00.RData")
# ------------------------------------------------------------------------------

