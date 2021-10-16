# Base Météo
# traitement de la hbase CHIRPS

load("~/Desktop/Dissertation/Recensement/DonnéesMétéo/CHIRPS_ProjectionFinale.RData")

tdata <- bdata_pt

# chargement base  

bdata_pt <- tdata %>% filter(`precipitation(mm)` >= 0)

tdata$rainDay <- ifelse(tdata$`precipitation(mm)` > 0,1,0)

rain <- tdata %>% group_by(station,year) %>% 
  summarise(NbRainyDays = sum(rainDay, na.rm = TRUE),
            Precipitations = sum(`precipitation(mm)`,na.rm = TRUE))

rain <- rain %>% group_by(station) %>% 
    mutate(NbRainyDaysMean    = mean(NbRainyDays, na.rm = TRUE),
              PrecipitationsMean = mean(Precipitations,na.rm = TRUE),
              NbRainyDaysSD      = sd(NbRainyDays, na.rm = TRUE),
              PrecipitationsSD   = sd(Precipitations,na.rm = TRUE))

rain$EC_nbJOURS  <- abs(rain$NbRainyDays - rain$NbRainyDaysMean)       / rain$NbRainyDaysSD
rain$EC_nbVOLUME <- abs(rain$Precipitations - rain$PrecipitationsMean) / rain$PrecipitationsSD

rain$EC_nbJOURS_choc  <- ifelse(rain$NbRainyDays - rain$NbRainyDaysMean       < 0, 1,0)
rain$EC_nbVOLUME_choc <- ifelse(rain$Precipitations - rain$PrecipitationsMean < 0, 1,0)

# ensemble de l'année JOURS  ------
library(plm)

tdata <- pdata.frame(rain, index = c("station","year"))

tdata$EC_nbJOURS_choc_1    <- lag(x = tdata$EC_nbJOURS_choc,k = 1)
tdata$EC_nbJOURS_choc_2    <- lag(x = tdata$EC_nbJOURS_choc,k = 2)
tdata$EC_nbJOURS_choc_3    <- lag(x = tdata$EC_nbJOURS_choc,k = 3)
tdata$EC_nbJOURS_choc_4    <- lag(x = tdata$EC_nbJOURS_choc,k = 4)
tdata$EC_nbJOURS_choc_5    <- lag(x = tdata$EC_nbJOURS_choc,k = 5)

tdata$EC_nbJOURS_1    <- lag(x = tdata$EC_nbJOURS,k = 1)
tdata$EC_nbJOURS_2    <- lag(x = tdata$EC_nbJOURS,k = 2)
tdata$EC_nbJOURS_3    <- lag(x = tdata$EC_nbJOURS,k = 3)
tdata$EC_nbJOURS_4    <- lag(x = tdata$EC_nbJOURS,k = 4)
tdata$EC_nbJOURS_5    <- lag(x = tdata$EC_nbJOURS,k = 5)

tdata$EC_nbVOLUME_choc_1    <- lag(x = tdata$EC_nbVOLUME_choc,k = 1)
tdata$EC_nbVOLUME_choc_2    <- lag(x = tdata$EC_nbVOLUME_choc,k = 2)
tdata$EC_nbVOLUME_choc_3    <- lag(x = tdata$EC_nbVOLUME_choc,k = 3)
tdata$EC_nbVOLUME_choc_4    <- lag(x = tdata$EC_nbVOLUME_choc,k = 4)
tdata$EC_nbVOLUME_choc_5    <- lag(x = tdata$EC_nbVOLUME_choc,k = 5)

tdata$EC_nbVOLUME_1    <- lag(x = tdata$EC_nbVOLUME,k = 1)
tdata$EC_nbVOLUME_2    <- lag(x = tdata$EC_nbVOLUME,k = 2)
tdata$EC_nbVOLUME_3    <- lag(x = tdata$EC_nbVOLUME,k = 3)
tdata$EC_nbVOLUME_4    <- lag(x = tdata$EC_nbVOLUME,k = 4)
tdata$EC_nbVOLUME_5    <- lag(x = tdata$EC_nbVOLUME,k = 5)








attach(tdata)

tdata$succCHIRPS_days <- 0

tdata$succCHIRPS_days[
             (EC_nbJOURS_choc   > 0 & EC_nbJOURS_choc_1 > 0) |
             (EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0) |
             (EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0) |
             (EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0) |
             (EC_nbJOURS_choc_4 > 0 & EC_nbJOURS_choc_5 > 0)] <- 1 

tdata$succCHIRPS_days[
    ((EC_nbJOURS_choc > 0 & EC_nbJOURS_choc_1 > 0) & (EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0)) | 
    ((EC_nbJOURS_choc > 0 & EC_nbJOURS_choc_1 > 0) & (EC_nbJOURS_choc_4 > 0 & EC_nbJOURS_choc_5 > 0)) | 
    ((EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0) & (EC_nbJOURS_choc_4 > 0 & EC_nbJOURS_choc_5 > 0)) ] <- 2

tdata$succCHIRPS_days[
  (EC_nbJOURS_choc > 0 & EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0) | 
    (EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0) | 
    (EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0) | 
    (EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0 & EC_nbJOURS_choc_5 > 0) > 0] <- 3

tdata$succCHIRPS_days[
  (EC_nbJOURS_choc > 0 & EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0) |
    (EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0) | 
    (EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0 & EC_nbJOURS_choc_5 > 0) ] <- 4

tdata$succCHIRPS_days[
  (EC_nbJOURS_choc > 0 & EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0) |
    (EC_nbJOURS_choc_5 > 0 & EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0)] <- 5

tdata$succCHIRPS_days[
  EC_nbJOURS_choc > 0 & EC_nbJOURS_choc_1 > 0 & EC_nbJOURS_choc_2 > 0 & EC_nbJOURS_choc_3 > 0 & EC_nbJOURS_choc_4 > 0 & EC_nbJOURS_choc_5 > 0] <- 6

detach(tdata)

tdata$succCHIRPS_days <- factor(tdata$succCHIRPS_days, levels=seq(0,6), 
                     labels=c("0 succ", "2 ans", "2 ans *2", "3 ans", "4 ans", "5 ans", "6 ans"))


# ensemble de l'année Volume (EC_nbVOLUME_choc)  ------

attach(tdata)

tdata$succCHIRPS_volume <- 0

tdata$succCHIRPS_volume[(EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0) |
             (EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0) |
             (EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0) |
             (EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0) |
             (EC_nbVOLUME_choc_4 > 0 & EC_nbVOLUME_choc_5 > 0)] <- 1 

tdata$succCHIRPS_volume[
  ((EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0) & (EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0)) | 
    ((EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0) & (EC_nbVOLUME_choc_4 > 0 & EC_nbVOLUME_choc_5 > 0)) | 
    ((EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0) & (EC_nbVOLUME_choc_4 > 0 & EC_nbVOLUME_choc_5 > 0)) ] <- 2

tdata$succCHIRPS_volume[
  (EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0) | 
    (EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0) | 
    (EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0) | 
    (EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0 & EC_nbVOLUME_choc_5 > 0) > 0] <- 3

tdata$succCHIRPS_volume[
  (EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0) |
    (EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0) | 
    (EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0 & EC_nbVOLUME_choc_5 > 0)] <- 4  

tdata$succCHIRPS_volume[
  (EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0) |
    (EC_nbVOLUME_choc_5 > 0 & EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0)] <- 5

tdata$succCHIRPS_volume[
  EC_nbVOLUME_choc > 0 & EC_nbVOLUME_choc_1 > 0 & EC_nbVOLUME_choc_2 > 0 & EC_nbVOLUME_choc_3 > 0 & EC_nbVOLUME_choc_4 > 0 & EC_nbVOLUME_choc_5 > 0] <- 6

detach(tdata)

tdata$succCHIRPS_volume <- factor(tdata$succCHIRPS_volume, levels=seq(0,6), 
                     labels=c("0 succ", "2 ans", "2 ans *2", "3 ans", "4 ans", "5 ans", "6 ans"))

setwd("~/Desktop/Dissertation/Recensement/MODÉLISATION")

save(tdata, file = "CHIRPS_DonnéesMétéo_15072021.RData")








library(readr)
library(plm)

SPEI1 <- read_delim("SPEI1_modelisation.csv",";", escape_double = FALSE, trim_ws = TRUE)
SPEI1 <- SPEI1 %>% select(latlong,lat,long,date,year,spei01)

SPEI1 <- SPEI1 %>%  group_by(latlong,year) %>% summarise(SPEI_value = mean(spei01, na.rm = TRUE))

SPEI1 <- pdata.frame(SPEI1, index = c("latlong","year"))

valeurSecheresse <- -0.5 

SPEI1$drought <- ifelse(SPEI1$SPEI_value < valeurSecheresse,1,0)

SPEI1$drought_1    <- lag(x = SPEI1$drought,k = 1)
SPEI1$drought_2    <- lag(x = SPEI1$drought,k = 2)
SPEI1$drought_3    <- lag(x = SPEI1$drought,k = 3)
SPEI1$drought_4    <- lag(x = SPEI1$drought,k = 4)
SPEI1$drought_5    <- lag(x = SPEI1$drought,k = 5)

SPEI1$SPEI_value_1    <- lag(x = SPEI1$SPEI_value,k = 1)
SPEI1$SPEI_value_2    <- lag(x = SPEI1$SPEI_value,k = 2)
SPEI1$SPEI_value_3    <- lag(x = SPEI1$SPEI_value,k = 3)
SPEI1$SPEI_value_4    <- lag(x = SPEI1$SPEI_value,k = 4)
SPEI1$SPEI_value_5    <- lag(x = SPEI1$SPEI_value,k = 5)

attach(SPEI1)

SPEI1$succSPEI <- 0

SPEI1$succSPEI[(drought > 0 & drought_1 > 0) |
               (drought_1 > 0 & drought_2 > 0) |
               (drought_2 > 0 & drought_3 > 0) |
               (drought_3 > 0 & drought_4 > 0) |
               (drought_4 > 0 & drought_5 > 0)] <- 1 


SPEI1$succSPEI[
  ((drought > 0 & drought_1 > 0) & (drought_3 > 0 & drought_4 > 0)) | 
    ((drought > 0 & drought_1 > 0) & (drought_4 > 0 & drought_5 > 0)) | 
    ((drought_1 > 0 & drought_2 > 0) & (drought_4 > 0 & drought_5 > 0)) ] <- 2

SPEI1$succSPEI[
  (drought > 0 & drought_1 > 0 & drought_2 > 0) | 
    (drought_1 > 0 & drought_2 > 0 & drought_3 > 0) | 
    (drought_2 > 0 & drought_3 > 0 & drought_4 > 0) | 
    (drought_3 > 0 & drought_4 > 0 & drought_5 > 0) > 0] <- 3

SPEI1$succSPEI[
  (drought > 0 & drought_1 > 0 & drought_2 > 0 & drought_3 > 0) |
    (drought_1 > 0 & drought_2 > 0 & drought_3 > 0 & drought_4 > 0) | 
    (drought_2 > 0 & drought_3 > 0 & drought_4 > 0 & drought_5 > 0)] <- 4  

SPEI1$succSPEI[
  (drought > 0 & drought_1 > 0 & drought_2 > 0 & drought_3 > 0 & drought_4 > 0) |
    (drought_5 > 0 & drought_1 > 0 & drought_2 > 0 & drought_3 > 0 & drought_4 > 0)] <- 5

SPEI1$succSPEI[
  drought > 0 & drought_1 > 0 & drought_2 > 0 & drought_3 > 0 & drought_4 > 0 & drought_5 > 0] <- 6

detach(SPEI1)

SPEI1$succSPEI <- factor(SPEI1$succSPEI, levels=seq(0,6), 
                                  labels=c("0 succ", "2 ans", "2 ans *2", "3 ans", "4 ans", "5 ans", "6 ans"))

save(SPEI1, file = "databaseSPEI.RData")





