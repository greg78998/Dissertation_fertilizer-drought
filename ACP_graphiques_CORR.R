### Programme pour réaliser les graphiques du rapport + ACP + étude des corrélations

installationPackage<- FALSE 

if (installationPackage == TRUE) {
  install.packages('Factoshiny')
  install.packages("readxl")
  install.packages("dplyr")
  install.packages("ggplot2")
  install.packages("scales")
  install.packages("Hmisc")
  install.packages("VennDiagram")
  install.packages("ggvenn")
}

library(VennDiagram)
library(ggvenn)
library(Factoshiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(Hmisc)


BasePourR <- read_excel("/Users/gregoirehaniquaut/Desktop/Dissertation/Recensement/MODÉLISATION/BasePourR.xlsx")

BaseCylindrage <- BasePourR %>% group_by(clef) %>%  summarise(NB_OCC = n()) %>% mutate(perNB_OCC = NB_OCC/trz)

trz <- sum(BaseCylindrage$NB_OCC)


ggplot(BaseCylindrage,aes(x = NB_OCC)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = NB_OCC) )+   
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  ylab("Fréquences relatives.") + xlab("Nombre d'occurences de l'individu.") + 
  labs(title = "Étude du cylindrage.") + 
  scale_x_continuous(breaks = seq(1, 6, 1))

BaseACP <- BasePourR[,c(15:70)]

BaseACP <- BaseACP %>% select(-one_of("NbRainyDays","Precipitations","NbRainyDaysMean","PrecipitationsMean","NbRainyDaysSD","PrecipitationsSD","organic_dicho","chemical_dicho","succSPEI","succCHIRPS_days","succCHIRPS_volume"))

BaseACP_SPEI <- BaseACP %>% select("Chemical_per_acres_dicho","Organic_per_acres_dicho",
                                   "drought","drought_1","drought_2","drought_3","drought_4","drought_5" ,
                                   "SPEI_value","SPEI_value_1","SPEI_value_2","SPEI_value_3","SPEI_value_4","SPEI_value_5" )

BaseACP_CHIRPS_days <- BaseACP %>% select("Chemical_per_acres_dicho","Organic_per_acres_dicho",
                                          "EC_nbJOURS_choc","EC_nbJOURS_choc_1","EC_nbJOURS_choc_2","EC_nbJOURS_choc_3","EC_nbJOURS_choc_4" ,"EC_nbJOURS_choc_5",
                                          "EC_nbJOURS","EC_nbJOURS_1","EC_nbJOURS_2","EC_nbJOURS_3","EC_nbJOURS_4","EC_nbJOURS_5")
BaseACP_CHIRPS_volume <- BaseACP %>% select("Chemical_per_acres_dicho","Organic_per_acres_dicho",
                                            "EC_nbVOLUME","EC_nbVOLUME_1","EC_nbVOLUME_2","EC_nbVOLUME_3","EC_nbVOLUME_4","EC_nbVOLUME_5",
                                            "EC_nbVOLUME_choc","EC_nbVOLUME_choc_1","EC_nbVOLUME_choc_2","EC_nbVOLUME_choc_3","EC_nbVOLUME_choc_4" , "EC_nbVOLUME_choc_5")
                                            
names(BaseACP_SPEI) <- c("Uti. chimique","Uti. organique",
              "Sécheresse","Sécheresse N-1","Sécheresse N-2","Sécheresse N-3","Sécheresse N-4","Sécheresse N-5",
              "IntensitéSPEI","IntensitéSPEI N-1","IntensitéSPEI N-2","IntensitéSPEI N-3","IntensitéSPEI N-4","IntensitéSPEI N-5" )

names(BaseACP_CHIRPS_days) <- c("Uti. chimique","Uti. organique",
                         "IntensitéDAY","IntensitéDAY N-1","IntensitéDAY N-2","IntensitéDAY N-3","IntensitéDAY N-4","IntensitéDAY N-5",
                         "Choc Pluie","Choc Pluie N-1","Choc Pluie N-2","Choc Pluie N-3","Choc Pluie N-4","Choc Pluie N-5")

names(BaseACP_CHIRPS_volume) <- c("Uti. chimique","Uti. organique",
                                  "IntensitéVOL","IntensitéVOL N-1","IntensitéVOL N-2","IntensitéVOL N-3","IntensitéVOL N-4","IntensitéVOL N-5","Choc Pluie","Choc Pluie N-1","Choc Pluie N-2","Choc Pluie N-3","Choc Pluie N-4","Choc Pluie N-5" )
  
corrplot(rcorr(as.matrix(BaseACP_SPEI))[[1]], 
         type="upper", order="hclust", tl.col="black")

corrplot(rcorr(as.matrix(BaseACP_CHIRPS_days))[[1]], 
         type="upper", order="hclust", tl.col="black")

corrplot(rcorr(as.matrix(BaseACP_CHIRPS_volume))[[1]], 
         type="upper", order="hclust", tl.col="black")




write(matrice_1, file = "matrice.csv")


PCAshiny(BaseACP)


Engrais_variable <- c("Chemical_per_acres_dicho","Organic_per_acres_dicho")
BaseACP_SPEI <- BaseACP %>% select(Engrais_variable,"SPEI_value","drought","drought_1","drought_2","drought_3",              "drought_4","drought_5" ,"SPEI_value_1","SPEI_value_2","SPEI_value_3","SPEI_value_4","SPEI_value_5" )
BaseACP_CHIRPS_days <- BaseACP %>% select(Engrais_variable,"NbRainyDays", "EC_nbJOURS","EC_nbVOLUME","EC_nbJOURS_choc",    "EC_nbVOLUME_choc","EC_nbJOURS_choc_1","EC_nbJOURS_choc_2","EC_nbJOURS_choc_3","EC_nbJOURS_choc_4" ,       "EC_nbJOURS_choc_5","EC_nbJOURS_1","EC_nbJOURS_2","EC_nbJOURS_3","EC_nbJOURS_4","EC_nbJOURS_5" )
BaseACP_CHIRPS_volume <- BaseACP %>% select(Engrais_variable,"Precipitations", "EC_nbVOLUME","EC_nbVOLUME_1","EC_nbVOLUME_2","EC_nbVOLUME_3","EC_nbVOLUME_4","EC_nbVOLUME_5",
   
# on va créer une fonction qui crée une nouvelle variable d'intéraction                             

CHIRPS_days <- c("EC_nbJOURS")

baseDonnées <- BaseACP

CreateInteractionVariable <- function(baseDeDonnées,ListeDeVariable_Value,ListeDeVariable_Choc) {
  if (baseDeDonnées == BaseACP_SPEI) {
    nomVar <- "SPEI" }
  else if (baseDeDonnées == BaseACP_CHIRPS_days) {
    nomVar <- "CHIRPS_days" }
  else if (baseDeDonnées == BaseACP_CHIRPS_volume) {
    nomVar <- "CHIRPS_volume" }
  else {
    print("error")}
  for (compteur in seq(1,6)) {
    NomVar <- paste("IntensityChoc",nomVar,compteur, sep = "_")
    value <- paste(ListeDeVariable_Value,
    baseDeDonnées[,NomVar] <- baseDeDonnées[ListeDeVariable_Value]
    }
}




for (Variable in seq(1,1)) {
  baseDonnées[, CHIRPS_days_ + "interaction"] <- rnorm(10)
}
      
baseDonnées[,"Blou"] <- 1                                      
                                            
                                           
PCAshiny(BaseACP_CHIRPS_days)

PCAshiny(BaseACP_SPEI)



BaseCylindrage_bis <- BasePourR %>% group_by(clef) %>% mutate(perNB_OCC = n())

cor(x = BaseCylindrage_bis[,c("perNB_OCC","Precipitations")], use = "complete.obs", method = "kendall")



cor(x = BaseCylindrage_bis[,c("perNB_OCC","Precipitations")], use = "complete.obs", method = "kendall")


cor(x = BaseCylindrage_bis[,c("perNB_OCC","Organic_per_acres","Chemical_per_acres")], use = "complete.obs", method = "kendall")


cor.test(x = BaseCylindrage_bis$Organic_per_acres[BaseCylindrage_bis$Any_Fertilizer],
         y = BaseCylindrage_bis$Chemical_per_acres[BaseCylindrage_bis$Any_Fertilizer != NA])



myplot <- ggplot(tips, aes(day)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")


ggplot(BasePourR %>% filter(Organic_per_acres != . & Chemical_per_acres != .,
                            Organic_per_acres < 1000 & Chemical_per_acres < 1000),
       aes(x = year, y = Chemical_per_acres, group = "clef")) + 
    geom_line() 

borneSup <- 100

ggplot(BasePourR %>% filter(Organic_per_acres != . & Chemical_per_acres != .,
                            Organic_per_acres < borneSup & Chemical_per_acres < borneSup,
                            Organic_per_acres > 0 & Chemical_per_acres > 0,
                            year != 2012),
       aes(x = factor(year), y = Organic_per_acres, color = factor(year))) + 
  geom_boxplot(show.legend = FALSE) + 
  xlab("") + ylab("Qté engrais organiques (kg/acres).") + 
  labs(title = "Distribution de la quantité d'engrais organiques.",
       caption = "Ce graphique considère les années d'enquêtes et non les vagues.
                  Pour prendre en compte le fait que beaucoup d'agriculteur n'utilisent pas d'engrais 
       et les valeurs abérrantes, plusieurs filtres sont utilisés (sur les quantités des engrais).") 

ggplot(BasePourR %>% filter(Organic_per_acres != . & Chemical_per_acres != .,
                            Organic_per_acres < borneSup & Chemical_per_acres < borneSup,
                            Organic_per_acres > 0 & Chemical_per_acres > 0,
                            year != 2012),
       aes(x = factor(year), y = Chemical_per_acres, color = factor(year))) + 
  geom_boxplot(show.legend = FALSE) + 
  xlab("") + ylab("Qté engrais chimiques (kg/acres).") + 
  labs(title = "Distribution de la quantité d'engrais chimiques",
       caption = "Ce graphique considère les années d'enquêtes et non les vagues.
                  Pour prendre en compte le fait que beaucoup d'agriculteur n'utilisent pas d'engrais 
       et les valeurs abérrantes, plusieurs filtres sont utilisés (sur les quantités des engrais).")




BasePourR %>% filter(Organic_per_acres != . & Chemical_per_acres != .,
                     Organic_per_acres < 1000 & Chemical_per_acres < 1000) %>% 
   mutate(blou = is.na(Organic_per_acres)) %>% filter(blou == FALSE) %>% 
   select(blou,year)%>% table()



c <- BasePourR %>% select(succSPEI,succCHIRPS_days,succCHIRPS_volume) 
ggvenn(BaseGraphiqueVenn)


succSPEI          <- list(BasePourR %>% select(succSPEI) %>% filter(succSPEI != "0 succ"))
succCHIRPS_days   <- list(BasePourR %>% select(succCHIRPS_days) %>% filter(succCHIRPS_days != "0 succ"))
succCHIRPS_volume <- list(BasePourR %>% select(succCHIRPS_volume) %>% filter(succCHIRPS_volume != "0 succ"))


