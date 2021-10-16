# ----------------------- MODELISATION -----------

library(stargazer)
library(dplyr)
library(plm)
library(tibble)
library(haven)
load("~/Desktop/Dissertation/Recensement/MODÉLISATION/FusionDesBases.Rdata")

# chargement de la base CHIRPS
load(file = "~/Desktop/Dissertation/Recensement/MODÉLISATION/CHIRPS_DonnéesMétéo_15072021.RData")
tdata <- tibble(tdata)
names(tdata)[1] <- "stationCHIRPS"

load(file = "~/Desktop/Dissertation/Recensement/MODÉLISATION/FichierAssociationCleHH_CHIRPSdata.RData")

load("~/Desktop/Dissertation/Recensement/MODÉLISATION/databaseSPEI.RData")
SPEI1 <- tibble(SPEI1)
names(SPEI1)[1] <- "stationSPEI"

load("~/Desktop/Dissertation/Recensement/MODÉLISATION/AssociationSPEI.BData")
CHIRPS_association <- association
names(CHIRPS_association) <- c("clef","stationCHIRPS")
CHIRPS_association$clef <- as.integer(CHIRPS_association$clef)
association_bis <- unique(CHIRPS_association[,"clef"])
CHIRPS_association <- merge(association_bis[,c("clef")],CHIRPS_association, by = "clef", all.x = TRUE)

SPEI_association <- SPEI_association %>% select(latlong, clef)
names(SPEI_association) <- c("stationSPEI","clef")
SPEI_association <- unique(SPEI_association)
SPEI_association <- data.frame(SPEI_association)
names(SPEI_association) <- c("stationSPEI","clef")

bf_new <- bf_new %>% left_join(y = SPEI_association, on = "clef")
bf_new <- bf_new %>% left_join(y = CHIRPS_association, on = "clef")

bf_new <- bf_new %>% left_join(y = tdata, on = c("stationCHIRPS","year"))
SPEI1$year <- as.character(SPEI1$year)
attributes(SPEI1$year) <- NULL
attributes(bf_new$year) <- NULL
bf_new <- bf_new %>% left_join(y = SPEI1, on = c("stationSPEI","year"))

bf_new_bis <- bf_new

library(readr)
setwd("~/Desktop/Dissertation/Recensement/MODÉLISATION")
bf_new <- data.frame(bf_new)
write_dta(bf_new, path = "baseFinale26_07_2021.dta", version = 14)

library(psy)

bf_new$succCHIRPS_days_dicho   <- ifelse(bf_new$succCHIRPS_days == "0 succ",0,1)
bf_new$succCHIRPS_volume_dicho <- ifelse(bf_new$succCHIRPS_volume == "0 succ",0,1)
bf_new$succSPEI_dicho          <- ifelse(bf_new$succSPEI == "0 succ",0,1)

bf_new[bf_new == 0] <- -1 

bf_new$CHIRPSdays  <-bf_new$EC_nbJOURS*bf_new$EC_nbJOURS_choc
bf_new$CHIRPSdays_1<-bf_new$EC_nbJOURS_1*bf_new$EC_nbJOURS_choc_1
bf_new$CHIRPSdays_2<-bf_new$EC_nbJOURS_2*bf_new$EC_nbJOURS_choc_2
bf_new$CHIRPSdays_3<-bf_new$EC_nbJOURS_3*bf_new$EC_nbJOURS_choc_3
bf_new$CHIRPSdays_4<-bf_new$EC_nbJOURS_4*bf_new$EC_nbJOURS_choc_4
bf_new$CHIRPSdays_5<-bf_new$EC_nbJOURS_5*bf_new$EC_nbJOURS_choc_5

bf_new$CHIRPSvolume  <-bf_new$EC_nbVOLUME*bf_new$EC_nbVOLUME_choc
bf_new$CHIRPSvolume_1<-bf_new$EC_nbVOLUME_1*bf_new$EC_nbVOLUME_choc_1
bf_new$CHIRPSvolume_2<-bf_new$EC_nbVOLUME_2*bf_new$EC_nbVOLUME_choc_2
bf_new$CHIRPSvolume_3<-bf_new$EC_nbVOLUME_3*bf_new$EC_nbVOLUME_choc_3
bf_new$CHIRPSvolume_4<-bf_new$EC_nbVOLUME_4*bf_new$EC_nbVOLUME_choc_4
bf_new$CHIRPSvolume_5<-bf_new$EC_nbVOLUME_5*bf_new$EC_nbVOLUME_choc_5

bf_new$SPEIvalue  <-bf_new$drought*bf_new$SPEI_value
bf_new$SPEIvalue_1<-bf_new$drought_1*bf_new$SPEI_value_1
bf_new$SPEIvalue_2<-bf_new$drought_2*bf_new$SPEI_value_2
bf_new$SPEIvalue_3<-bf_new$drought_3*bf_new$SPEI_value_3
bf_new$SPEIvalue_4<-bf_new$drought_4*bf_new$SPEI_value_4
bf_new$SPEIvalue_5<-bf_new$drought_5*bf_new$SPEI_value_5


varY <- c("Chemical_per_acres","Organic_per_acres")

varCHIRPS_DAY <- c("EC_nbJOURS_choc","EC_nbJOURS_choc_1","EC_nbJOURS_choc_2","EC_nbJOURS_choc_3","EC_nbJOURS_choc_4","EC_nbJOURS_choc_5" ,"succCHIRPS_days_dicho")
varCHIRPS_volume <- c("EC_nbVOLUME_choc","EC_nbVOLUME_choc_1","EC_nbVOLUME_choc_2","EC_nbVOLUME_choc_3","EC_nbVOLUME_choc_4","EC_nbVOLUME_choc_5","succCHIRPS_volume_dicho")
varSPEI <- c('drought', 'drought_1', 'drought_2', 'drought_3', 'drought_4', 'drought_5',"succSPEI_dicho")



library(psy)

sphpca(bf_new[,c(varY,varCHIRPS_DAY)],    v=55, nbsphere = 1)
sphpca(bf_new[,c(varY,varCHIRPS_volume)], v=55, nbsphere = 1)
sphpca(bf_new[,c(varY,varSPEI)],          v=55, nbsphere = 1)

table(bf_new$succSPEI)
table(bf_new$succCHIRPS_days)
table(bf_new$succCHIRPS_volume)
barplot(table_succCHIRPS_day,xlab = "Bloublou", ylab = "Effectif")

fpca(data=bf_new,y=varY[1],x=varSPEI,partial="No")
fpca(data=bf_new,y=varY[2],x=varSPEI,partial="No")

fpca(data=bf_new,y=varY[1],x=varCHIRPS_volume,partial="No")
fpca(data=bf_new,y=varY[2],x=varCHIRPS_volume,partial="No")

fpca(data=bf_new,y=varY[1],x=varCHIRPS_DAY,partial="No")
fpca(data=bf_new,y=varY[2],x=varCHIRPS_DAY,partial="No")




















## OLS

bf_new$succSPEI_dicho <- ifelse(bf_new$succSPEI != "0 succ",1,0)
bf_new$Chemical_per_acres_dicho <- ifelse(bf_new$Chemical_per_acres > 0,1,0)
bf_new$Organic_per_acres_dicho <- ifelse(bf_new$Organic_per_acres > 0,1,0)
bf_new$Fertilizer_per_acres_dicho <- ifelse(bf_new$Organic_per_acres > 0 | bf_new$Chemical_per_acres > 0,1,0)


OLS_SPEI_1 <- lm(Chemical_per_acres ~ drought +drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                   SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 + 
                 I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                   I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                   I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI_dicho,
                 data = bf_new)

OLS_SPEI_2 <- lm(Organic_per_acres ~ drought +drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                   SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 + 
                   I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                   I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                   I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI,
                 data = bf_new)

OLS_SPEI_Chemical_dicho <- lm(Chemical_per_acres_dicho ~ drought + 
                   drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                   SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 + 
                   I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                   I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                   I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI_dicho,
                 data = bf_new)

OLS_SPEI_Organic_dicho <- lm(Organic_per_acres_dicho ~ drought + 
                         drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                         SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 +
                         I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                         I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                         I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI_dicho,
                       data = bf_new)

OLS_SPEI_Any_dicho <- lm(Fertilizer_per_acres_dicho ~ drought + 
                               drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                               SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 +
                               I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                               I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                               I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI_dicho,
                             data = bf_new)

summary(OLS_SPEI_1)


OLS_CHIRPS_organic_dicho <- lm(Chemical_per_acres_dicho ~ EC_nbJOURS_choc + 
                           EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5 +
                           EC_nbJOURS + EC_nbJOURS_1 + EC_nbJOURS_2 + EC_nbJOURS_3 + EC_nbJOURS_4 + EC_nbJOURS_5 +
                           I(EC_nbJOURS*EC_nbJOURS_choc) + I(EC_nbJOURS_1*EC_nbJOURS_choc_1) + 
                           I(EC_nbJOURS_2*EC_nbJOURS_choc_2) + I(EC_nbJOURS_3*EC_nbJOURS_choc_3) + 
                           I(EC_nbJOURS_4*EC_nbJOURS_choc_4) + I(EC_nbJOURS_5*EC_nbJOURS_choc_5) + succCHIRPS_days,
                         data = bf_new)

OLS_CHIRPS_organic <- lm(Organic_per_acres_dicho ~ EC_nbJOURS_choc + 
                           EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5 +
                           EC_nbJOURS + EC_nbJOURS_1 + EC_nbJOURS_2 + EC_nbJOURS_3 + EC_nbJOURS_4 + EC_nbJOURS_5 +
                           I(EC_nbJOURS*EC_nbJOURS_choc) + I(EC_nbJOURS_1*EC_nbJOURS_choc_1) + 
                           I(EC_nbJOURS_2*EC_nbJOURS_choc_2) + I(EC_nbJOURS_3*EC_nbJOURS_choc_3) + 
                           I(EC_nbJOURS_4*EC_nbJOURS_choc_4) + I(EC_nbJOURS_5*EC_nbJOURS_choc_5) + succCHIRPS_days,
                         data = bf_new)
summary(OLS_CHIRPS_organic)

OLS_fertilizer_organic <- lm(Fertilizer_per_acres_dicho ~ EC_nbJOURS_choc + 
                           EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5 +
                           EC_nbJOURS + EC_nbJOURS_1 + EC_nbJOURS_2 + EC_nbJOURS_3 + EC_nbJOURS_4 + EC_nbJOURS_5 +
                           I(EC_nbJOURS*EC_nbJOURS_choc) + I(EC_nbJOURS_1*EC_nbJOURS_choc_1) + 
                           I(EC_nbJOURS_2*EC_nbJOURS_choc_2) + I(EC_nbJOURS_3*EC_nbJOURS_choc_3) + 
                           I(EC_nbJOURS_4*EC_nbJOURS_choc_4) + I(EC_nbJOURS_5*EC_nbJOURS_choc_5) + succCHIRPS_days,
                         data = bf_new)
summary(OLS_fertilizer_organic)

## MODÈLE À EFFETS FIXES

OLS_CHIRPS_chemical_dicho_FE <- lm(Chemical_per_acres_dicho ~ EC_nbJOURS_choc + 
                           EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5 +
                           EC_nbJOURS + EC_nbJOURS_1 + EC_nbJOURS_2 + EC_nbJOURS_3 + EC_nbJOURS_4 + EC_nbJOURS_5 +
                           I(EC_nbJOURS*EC_nbJOURS_choc) + I(EC_nbJOURS_1*EC_nbJOURS_choc_1) + 
                           I(EC_nbJOURS_2*EC_nbJOURS_choc_2) + I(EC_nbJOURS_3*EC_nbJOURS_choc_3) + 
                           I(EC_nbJOURS_4*EC_nbJOURS_choc_4) + I(EC_nbJOURS_5*EC_nbJOURS_choc_5) + succCHIRPS_days + 
                           year + Parish,
                         data = bf_new)

OLS_CHIRPS_organic_binaire_FE <- lm(Organic_per_acres_dicho ~ EC_nbJOURS_choc + 
                                  EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5 +
                                  EC_nbJOURS + EC_nbJOURS_1 + EC_nbJOURS_2 + EC_nbJOURS_3 + EC_nbJOURS_4 + EC_nbJOURS_5 +
                                  I(EC_nbJOURS*EC_nbJOURS_choc) + I(EC_nbJOURS_1*EC_nbJOURS_choc_1) + 
                                  I(EC_nbJOURS_2*EC_nbJOURS_choc_2) + I(EC_nbJOURS_3*EC_nbJOURS_choc_3) + 
                                  I(EC_nbJOURS_4*EC_nbJOURS_choc_4) + I(EC_nbJOURS_5*EC_nbJOURS_choc_5) + succCHIRPS_days + 
                                  year + Parish,
                                data = bf_new)

## MODÈLE DE DONNÉES DE PANEL UTILISATION 

bf_new_panel <- pdata.frame(bf_new, index = c("clef","year"))

PLM_SPEI_Any <- plm(Fertilizer_per_acres_dicho ~ drought + 
                      drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                      SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 +
                      I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                      I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                      I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI_dicho,
                    data = bf_new,
                    model = "within",
                    index = c("clef","year"))

PLM_CHIRPS_organic <- plm(Chemical_per_acres_dicho ~ EC_nbJOURS_choc + 
    EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5 +
    EC_nbJOURS + EC_nbJOURS_1 + EC_nbJOURS_2 + EC_nbJOURS_3 + EC_nbJOURS_4 + EC_nbJOURS_5 +
    I(EC_nbJOURS*EC_nbJOURS_choc) + I(EC_nbJOURS_1*EC_nbJOURS_choc_1) + 
    I(EC_nbJOURS_2*EC_nbJOURS_choc_2) + I(EC_nbJOURS_3*EC_nbJOURS_choc_3) + 
    I(EC_nbJOURS_4*EC_nbJOURS_choc_4) + I(EC_nbJOURS_5*EC_nbJOURS_choc_5) + succCHIRPS_days,
    data = bf_new_panel,
    model = c("within"),
    index = c("clef","year")     )
  
PLM_CHIRPS_organic <- plm(  Fertilizer_per_acres_dicho ~ drought + 
                      drought_1 + drought_2 + drought_3 + drought_4 + drought_5 +
                      SPEI_value + SPEI_value_1 + SPEI_value_2 + SPEI_value_3 + SPEI_value_4 + SPEI_value_5 +
                      I(SPEI_value*drought) + I(SPEI_value_1*drought_1) + 
                      I(SPEI_value_2*drought_2) + I(SPEI_value_3*drought_3) + 
                      I(SPEI_value_4*drought_4) + I(SPEI_value_5*drought_5) + succSPEI_dicho,
                    data = bf_new,
                    model = c("within"), 
                    index = c("clef","year")    )













## Organic 

stargazer(OLS_CHIRPS_organic_dicho,
          OLS_CHIRPS_organic_binaire_FE,
          type = "text", 
          column.labels = c("OLS","OLS_FE"))

## Chimique 

stargazer(OLS_CHIRPS_chemical_dicho,
          OLS_CHIRPS_chemical_dicho_FE,
          type = "text", 
          column.labels = c("OLS","OLS_FE"))

## TOBIT

duplicated <- unique(data[,c("clef","year",in)])
duplicated_bd <- bf_new %>% filter(duplicated == TRUE)
bf_new_duplicate <- duplicated_bd %>% distinct(x = clef, y = year)

bf_new$duplicated <- bf_new %>%  select(clef,year) %>%  duplicated()
duplicated_bd <- bf_new %>% filter(duplicated == TRUE)
duplicated_bd <- duplicated_bd  %>% unique()