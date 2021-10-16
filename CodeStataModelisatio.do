// Mémoire

// date : 26 juillet 2021 

// dans ce document l'enjeu sera de travailler sur la modélisation. 

clear 

cd "/Users/gregoirehaniquaut/Desktop/Dissertation/Recensement/MODÉLISATION"

use "baseFinale26_07_2021.dta", clear 

drop if missing(year)
destring year, replace

capture drop Any_Fertilizer
capture drop Chemical_per_acres_dicho
capture drop Organic_per_acres_dicho

gen Any_Fertilizer = (Chemical_per_acres > 0  | Organic_per_acres > 0)
	replace Any_Fertilizer = . if Chemical_per_acres == . & Organic_per_acres == . 
gen Chemical_per_acres_dicho = (Chemical_per_acres > 0)
	replace Chemical_per_acres_dicho = . if Chemical_per_acres == .
gen Organic_per_acres_dicho  = (Organic_per_acres > 0)
	replace Organic_per_acres_dicho = . if Organic_per_acres == .

capture drop SUCC_SPEI_dicho
gen SUCC_SPEI_dicho = (succSPEI != 1)

capture drop SUCC_CHIRPSvolume_dicho
gen SUCC_CHIRPSvolume_dicho = (succCHIRPS_volume != 1)

capture drop SUCC_CHIRPSjours_dicho
gen SUCC_CHIRPSjours_dicho = (succCHIRPS_day != 1)



capture drop chemical_dicho organic_dicho

gen chemical_dicho = Chemical_per_acres_dicho
gen organic_dicho = Organic_per_acres_dicho

pwcorr Chemical_per_acres Organic_per_acres, star(0.05)


capture drop NB_Drought_SPEI 
capture drop NB_drought_CHIRPS_d 
capture drop NB_drought_CHIRPS_vol

egen NB_Drought_SPEI       = rowtotal(drought drought_*)
egen NB_drought_CHIRPS_d   = rowtotal(EC_nbJOURS_choc*)
egen NB_drought_CHIRPS_vol = rowtotal(EC_nbVOLUME_choc*)

// portion qui permet de réaliser le graphique avec les moyennes
preserve
	drop if Chemical_per_acres == . & Organic_per_acres == . 
	tabulate year , summarize(Chemical_per_acres_dicho)
	tabulate year , summarize(Organic_per_acres_dicho)
	tabulate year , summarize(Any_Fertilizer)
	tabulate year , summarize(Chemical_per_acres)
	tabulate year , summarize(Organic_per_acres)
restore 




// base preparation 

duplicates drop clef year, force


capture drop county parish subcounty
encode County , gen(county)
encode Parish , gen(parish)
encode SubCounty, gen(subcounty)

gen InteractionCHIR_VOL   = EC_nbVOLUME_choc   * EC_nbVOLUME
gen InteractionCHIR_VOL_1 = EC_nbVOLUME_choc_1 * EC_nbVOLUME_1
gen InteractionCHIR_VOL_2 = EC_nbVOLUME_choc_2 * EC_nbVOLUME_2
gen InteractionCHIR_VOL_3 = EC_nbVOLUME_choc_3 * EC_nbVOLUME_3
gen InteractionCHIR_VOL_4 = EC_nbVOLUME_choc_4 * EC_nbVOLUME_4
gen InteractionCHIR_VOL_5 = EC_nbVOLUME_choc_5 * EC_nbVOLUME_5

gen InteractionCHIR_DAYS   = EC_nbJOURS_choc   * EC_nbJOURS
gen InteractionCHIR_DAYS_1 = EC_nbJOURS_choc_1 * EC_nbJOURS_1
gen InteractionCHIR_DAYS_2 = EC_nbJOURS_choc_2 * EC_nbJOURS_2
gen InteractionCHIR_DAYS_3 = EC_nbJOURS_choc_3 * EC_nbJOURS_3
gen InteractionCHIR_DAYS_4 = EC_nbJOURS_choc_4 * EC_nbJOURS_4
gen InteractionCHIR_DAYS_5 = EC_nbJOURS_choc_5 * EC_nbJOURS_5

gen InteractionSPEI   = drought   * SPEI_value
gen InteractionSPEI_1 = drought_1 * SPEI_value_1
gen InteractionSPEI_2 = drought_2 * SPEI_value_2
gen InteractionSPEI_3 = drought_3 * SPEI_value_3
gen InteractionSPEI_4 = drought_4 * SPEI_value_4
gen InteractionSPEI_5 = drought_5 * SPEI_value_5



// group de variables 
global CHIRPS_DAY_variables EC_nbJOURS_choc* EC_nbJOURS EC_nbJOURS_1 EC_nbJOURS_2 EC_nbJOURS_3 EC_nbJOURS_4 EC_nbJOURS_5 InteractionCHIR_DAYS* i.succCHIRPS_day

global CHIRPS_VOLUME_variables EC_nbVOLUME_choc* EC_nbVOLUME EC_nbVOLUME_1 EC_nbVOLUME_2 EC_nbVOLUME_3 EC_nbVOLUME_4 EC_nbVOLUME_5 InteractionCHIR_VOL* i.succCHIRPS_volume

global SPEI_variables drought* SPEI_value* InteractionSPEI* i.succSPEI


global VariablesÀExpliquer Any_Fertilizer chemical_dicho organic_dicho
global VariablesÀExpliquer2 chemical_dicho organic_dicho

capture drop organicPC
capture drop chemicalPC
	gen organicPC  = Organic_per_acres
	gen chemicalPC = Chemical_per_acres

global VariableExpliContinue organicPC chemicalPC

scalar lowerBound = 0
scalar upperBound =100


xtset clef year 
capture drop occurence
egen occurence = count(year), by (clef)


// equation d'attrition 

gen ObsSupp = (occurence == 1)

reg ObsSupp $SPEI_variables $CHIRPS_DAY_variables $CHIRPS_VOLUME_variables , vce(cluster parish)


drop if occurence == 1 




// Test sur les modèles 

gen NBchoc = EC_nbJOURS_choc + EC_nbJOURS_choc_1 + EC_nbJOURS_choc_2 + EC_nbJOURS_choc_3 + EC_nbJOURS_choc_4 + EC_nbJOURS_choc_5
gen succCHOC = (succCHIRPS_day != 1)

global CHIRPS_DAY_variables1 EC_nbJOURS_choc* EC_nbJOURS EC_nbJOURS_1 EC_nbJOURS_2 EC_nbJOURS_3 EC_nbJOURS_4 EC_nbJOURS_5 

global CHIRPS_DAY_variables2 EC_nbJOURS_choc* EC_nbJOURS EC_nbJOURS_1 EC_nbJOURS_2 EC_nbJOURS_3 EC_nbJOURS_4 EC_nbJOURS_5 InteractionCHIR_DAYS* 


reg Any_Fertilizer $CHIRPS_DAY_variables1 i.year 
	est store mod1

reg Any_Fertilizer $CHIRPS_DAY_variables1 i.year InteractionCHIR_DAYS* 
	est store mod1

reg Any_Fertilizer $CHIRPS_DAY_variables1 i.year i.NBchoc
	est store mod2
	
reg Any_Fertilizer $CHIRPS_DAY_variables2 i.year i.NBchoc succCHOC
	est store mod3
	
reg Any_Fertilizer $CHIRPS_DAY_variables2 i.year i.NBchoc succCHOC
	est store mod4
	
reg Any_Fertilizer $CHIRPS_DAY_variables2 i.year succCHOC
	est store mod5
	
esttab mod1 mod2 mod3 mod4 mod5 using ComparaisonModel.csv, ar2 aic bic nostar p

//------------------------------------------------------------------------------
//// MODÈLE DICHOTOMIQUE
//------------------------------------------------------------------------------







//  SPEI
foreach Explicative in $VariablesÀExpliquer {	
	reg `Explicative' $SPEI_variables , vce(cluster parish)
		est store OLS_`Explicative'SP
	reg `Explicative' $SPEI_variables i.year , vce(cluster parish)
		est store reg2_`Explicative'ESP
	logit `Explicative' $SPEI_variables i.year , vce(cluster parish) or
		est store lo`Explicative'ESP
	logit `Explicative' $SPEI_variables, vce(cluster parish) or
		est store lo`Explicative'SP	
	probit `Explicative' $SPEI_variables , vce(cluster parish)
		est store pro`Explicative'SP
	probit `Explicative' $SPEI_variables i.year , vce(cluster parish)
		est store pro`Explicative'ESP
	xtlogit `Explicative' $SPEI_variables, fe 
		est store xtL_`Explicative'_SP
	  }


////////// -------------------------------------------------

//// CHIRPS days

foreach Explicative in $VariablesÀExpliquer {	
	reg `Explicative' $CHIRPS_DAY_variables , vce(cluster parish)
		est store OLS_`Explicative'CD
	reg `Explicative' $CHIRPS_DAY_variables i.year , vce(cluster parish)
		est store OLS_`Explicative'ECD
	logit `Explicative' $CHIRPS_DAY_variables i.year , vce(cluster parish) or
		est store log`Explicative'ECD
	logit `Explicative' $CHIRPS_DAY_variables, vce(cluster parish) or
		est store log`Explicative'CD
	probit `Explicative' $CHIRPS_DAY_variables , vce(cluster parish)
		est store pro`Explicative'CD
	probit `Explicative' $CHIRPS_DAY_variables i.year, vce(cluster parish)
		est store pro`Explicative'ECD
	xtlogit `Explicative' $CHIRPS_DAY_variables, fe 
		est store xtL_`Explicative'ECD
	  }
	
xtlogit Any_Fertilizer $CHIRPS_DAY_variables i.year, offset(year) fe 
testparm i.year
	est store xtL_`Explicative'ECD
	
xtlogit chemical_dicho $CHIRPS_DAY_variables , offset(year) fe 
testparm i.year
	est store xtL_`Explicative'ECD
	
xtlogit chemical_dicho $CHIRPS_DAY_variables , offset(year) fe 

	
////////// -------------------------------------------------

/// CHIRPS volume
	
foreach Explicative in $VariablesÀExpliquer {	
	reg `Explicative' $CHIRPS_VOLUME_variables , vce(cluster parish)
		est store OLS_`Explicative'CV
	reg `Explicative' $CHIRPS_VOLUME_variables i.year, vce(cluster parish)
		est store OLS_`Explicative'ECV
	logit `Explicative' $CHIRPS_VOLUME_variables i.year , vce(cluster parish) or
		est store log`Explicative'ECV
	logit `Explicative' $CHIRPS_VOLUME_variables, vce(cluster parish) or
		est store log`Explicative'CV
	probit `Explicative' $CHIRPS_VOLUME_variables , vce(cluster parish)
		est store pro`Explicative'CV
	probit `Explicative' $CHIRPS_VOLUME_variables i.year, vce(cluster parish)
		est store pro`Explicative'ECV
	xtlogit `Explicative' $CHIRPS_VOLUME_variables, fe 
		est store xtL_`Explicative'ECV
	  }

xtlogit chemical_dicho $CHIRPS_DAY_variables, fe 
margins, dydx(*)
predict resid

logit Any_Fertilizer $CHIRPS_DAY_variables i.year i.county
estat classification

//------------------------------------------------------------------------------
//// MODÈLE CONTINUE
//------------------------------------------------------------------------------

//  SPEI
foreach Explicative in $VariableExpliContinue {	
	reg `Explicative' $SPEI_variables , vce(cluster parish)
		est store OLS_`Explicative'SP
	reg `Explicative' $SPEI_variables i.year, vce(cluster parish)
		est store OLS_`Explicative'ESP
	tobit `Explicative' $SPEI_variables i.year, vce(cluster parish) ll(0) ul(100)
		est store T_`Explicative'ESP
	xtreg `Explicative' $SPEI_variables , fe
		est store xtreg_`Explicative'SP
	  }

////////// -------------------------------------------------

//// CHIRPS days

foreach Explicative in $VariableExpliContinue {	
	reg `Explicative' $CHIRPS_DAY_variables , vce(cluster parish)
		est store OLS_`Explicative'CD
	reg `Explicative' $CHIRPS_DAY_variables i.year , vce(cluster parish)
		est store OLS_`Explicative'ECD
	tobit `Explicative' $CHIRPS_DAY_variables i.year , vce(cluster parish)  ll(0) ul(100)
		est store T_`Explicative'ECD
	xtreg `Explicative' $CHIRPS_DAY_variables , fe
		est store xtreg_`Explicative'CD
	  }
	  
////////// -------------------------------------------------

/// CHIRPS volume
	
global MtitleContinue "MCO" "MCO_FE" "TOBIT" "xtreg"

foreach Explicative in $VariableExpliContinue {	
	reg `Explicative' $CHIRPS_VOLUME_variables , vce(cluster parish)
		est store OLS_`Explicative'CV
	reg `Explicative' $CHIRPS_VOLUME_variables i.year , vce(cluster parish)
		est store OLS_`Explicative'ECV
	tobit `Explicative' $CHIRPS_VOLUME_variables i.year , vce(cluster parish) ll(0) ul(100)
		est store T_`Explicative'ECV
	xtreg `Explicative' $CHIRPS_VOLUME_variables , fe
		est store xtreg_`Explicative'CV
	  }	  

foreach Explicative in $VariableExpliContinue {
	xtreg `Explicative' $SPEI_variables , fe
		est store xtreg_`Explicative'SP
	xtreg `Explicative' $CHIRPS_DAY_variables , fe
		est store xtreg_`Explicative'CD	
	xtreg `Explicative' $CHIRPS_VOLUME_variables , fe
		est store xtreg_`Explicative'CV
	  }	  
	  
	  
esttab OLS_organicPCSP OLS_organicPCESP T_organicPCESP xtreg_organicPCSP  OLS_organicPCCD OLS_organicPCECD T_organicPCECD xtreg_organicPCCD  OLS_organicPCCV OLS_organicPCECV T_organicPCECV xtreg_organicPCCV using regContinueOrganic.csv,$statistiquesDemandees  nostar replace



esttab OLS_chemicalPCSP OLS_chemicalPCESP T_chemicalPCESP xtreg_chemicalPCSP  OLS_chemicalPCCD OLS_chemicalPCECD T_chemicalPCECD xtreg_chemicalPCCD  OLS_chemicalPCCV OLS_chemicalPCECV T_chemicalPCECV xtreg_chemicalPCCV using "regContinueChemical.csv" , $statistiquesDemandees  nostar replace 
	


tobit chemicalPC $CHIRPS_DAY_variables i.year i.county, vce(cluster parish) ll(0) ul(100)
est store T_chemicalPCECD	

xttobit chemicalPC $CHIRPS_DAY_variables , ll(0) ul(100)
	
	
	
//------------------------------------------------------------------------------
// ----------------------------- TABLE DICHO -----------------------------------
//------------------------------------------------------------------------------


global statistiquesDemandees p r2 pr2 aic bic
global Mtitle "MCO" "MCO_FE" "logit" "logit_FE" "probit" "probit_FE" "xtlogit"

//  any fertilizer 
	
	esttab OLS_Any_FertilizerSP reg2_Any_FertilizerESP loAny_FertilizerSP loAny_FertilizerESP proAny_FertilizerSP proAny_FertilizerESP xtL_Any_Fertilizer_SP using AnyFertillizerSPEI.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 

	esttab OLS_Any_FertilizerCD OLS_Any_FertilizerECD logAny_FertilizerCD  logAny_FertilizerECD proAny_FertilizerCD proAny_FertilizerECD xtL_Any_FertilizerECD using AnyFertillizerCD.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 
	
	esttab OLS_Any_FertilizerCV OLS_Any_FertilizerECV logAny_FertilizerCV  logAny_FertilizerECV proAny_FertilizerCV proAny_FertilizerECV xtL_Any_FertilizerECV using AnyFertillizerCV.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 

// organic_dicho

	esttab OLS_organic_dichoSP reg2_organic_dichoESP loorganic_dichoSP loorganic_dichoESP proorganic_dichoSP proorganic_dichoESP xtL_organic_dicho_SP using organicSPEI.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 

	esttab OLS_organic_dichoCD OLS_organic_dichoECD logorganic_dichoCD  logorganic_dichoECD proorganic_dichoCD proorganic_dichoECD xtL_organic_dichoECD using organicCD.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 

	esttab OLS_organic_dichoCV OLS_organic_dichoECV logorganic_dichoCV  logorganic_dichoECV proorganic_dichoCV proorganic_dichoECV xtL_organic_dichoECV using organicCV.csv , $statistiquesDemandees mtitle($Mtitle) nostar replace 


// chemical_dicho

	esttab OLS_chemical_dichoSP reg2_chemical_dichoESP lochemical_dichoSP lochemical_dichoESP prochemical_dichoSP prochemical_dichoESP xtL_chemical_dicho_SP using chemicalSPEI.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 

	esttab OLS_chemical_dichoCD OLS_chemical_dichoECD logchemical_dichoCD  logchemical_dichoECD prochemical_dichoCD prochemical_dichoECD xtL_chemical_dichoECD using chemicalCD.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 

	esttab OLS_chemical_dichoCV OLS_chemical_dichoECV logchemical_dichoCV  logchemical_dichoECV prochemical_dichoCV prochemical_dichoECV xtL_chemical_dichoECV using chemicalCV.csv, $statistiquesDemandees mtitle($Mtitle) nostar replace 



//------------------------------------------------------------------------------
// ----------------------------- VARIABLE CONTINUE -----------------------------
//------------------------------------------------------------------------------

global Mtitle "MCO" "Tobit" "MCO_FE" "TOBIT_FE" "xtReg" "xtTobit"


global QuantiTitle "MCO" "MCO_FE" "TOBIT_FE" "xtreg"



esttab OLS_organicPCSP OLS_organicPCESP T_organicPCESP xtreg_organicPCSP

esttab OLS_organicPCCD OLS_organicPCECD T_organicPCECD xtreg_organicPCCD 

esttab OLS_organicPCCV OLS_organicPCECV T_organicPCECV xtreg_organicPCCV 

, $statistiquesDemandees mtitle($QuantiTitle $QuantiTitle $QuantiTitle) nostar replace

  
esttab OLS_chemicalPCSP OLS_chemicalPCESP T_chemicalPCESP xtreg_chemicalPCSP OLS_chemicalPCCD OLS_chemicalPCECD T_chemicalPCECD xtreg_chemicalPCCD OLS_chemicalPCCV OLS_chemicalPCECV T_chemicalPCECV xtreg_chemicalPCCV using chemicalPC.csv , $statistiquesDemandees mtitle($QuantiTitle $QuantiTitle $QuantiTitle) nostar replace




reg organicPC $CHIRPS_VOLUME_variables i.year , vce(cluster parish)
	
	
	
	
	
	
	
	
	
	
	  
	  



esttab OLS_SPEI_organic_dicho reg2_organic_dichoEF logitSPEIorganic_dicho logitSPEIorganic_dichoEF probitSPEIorganic_dicho proSPEIorganic_dichoEF xtregorganic_dicho_SPEI xtP_organic_dicho_SPEI, r2 pr2 aic bic  title("Variable à expliquer : utilisation d'engrais organics.") nonumbers mtitles($Mtitle "xtreg" "xtlogit") 

esttab OLS_Any_Fertilizer_CD OLS_Any_FertilizerEF_CD logAny_Fertilizer_CD  logAny_FertilizerEF_CD proAny_Fertilizer_CD pAny_FertilizerECD , $statistiquesDemandees mtitles($Mtitle)


OLS_
, r2 pr2 aic bic  title("Variable à expliquer : quantité d'engrais organic utilisé.") nonumbers mtitles("MCO" "MCO_FE" "Tobit_FE" "xtReg" "xtTobit")

/// CHEMICAL 

global Mtitle "MCO" "Tobit" "MCO_FE" "TOBIT_FE" "xtReg" "xtTobit"


global VariablesÀExpliquer Any_Fertilizer chemical_dicho organic_dicho
global VariablesÀExpliquer2 chemical_dicho organic_dicho
global VariableExpliContinue Organic_per_acres Chemical_per_acres_dicho



esttab
	  
	  
	  
	  
	  
	  
	  
foreach Explicative in $VariablesÀExpliquer {
		
	reg `Explicative' $CHIRPS_DAY_variables
		est store OLS_CHIday_`Explicative'
		
	reg `Explicative' $CHIRPS_DAY_variables i.year i.county, vce(cluster parish)
		est store OLS_CHIday`Explicative'_EF

	tobit `Explicative' $CHIRPS_DAY_variables i.year i.county, vce(cluster parish) ll(0) ul(20)
		est store tobCHIRPSday_`Explicative'
	  }

foreach Explicative in $VariablesÀExpliquer {
		
	reg `Explicative' $CHIRPS_VOLUME_variables
		est store OLS_CHIvol_`Explicative'
		
	reg `Explicative' $CHIRPS_VOLUME_variables i.year i.county, vce(cluster parish)
		est store OLS_CHIvol`Explicative'_EF

	tobit `Explicative' $CHIRPS_VOLUME_variables i.year i.county, vce(cluster parish) ll(0) ul(20)
		est store tobCHIRPSvol_`Explicative'
	  }
	  
xtset clef year 
capture drop occurence
egen occurence = count(year), by (clef)
drop if occurence == 1 

// stat desc dans le temps et dans l'espace
global VariableMeteo NbRainyDays Precipitations EC_nbJOURS EC_nbVOLUME EC_nbJOURS_choc EC_nbVOLUME_choc drought SPEI_value
global VariableFertilizer Chemical_per_acres Organic_per_acres Chemical_per_acres_dicho Organic_per_acres_dicho Any_Fertilizer

xtsum $VariableMeteo $VariableFertilizer

foreach Explicative in Any_Fertilizer {
	 xtreg `Explicative' $SPEI_variables, fe 
		est store xtreg`Explicative'_SPEI
	 xtlogit `Explicative' $SPEI_variables, fe 
		est store xtP_`Explicative'_SPEI 
	}

	
foreach Explicative in $VariablesÀExpliquer2 {
	xtprobit `Explicative' $CHIRPS_DAY_variables , or
	est store xtP_`Explicative'_CHd 
	}

foreach Explicative in $VariablesÀExpliquer2 {
	xtprobit `Explicative' $CHIRPS_VOLUME_variables , or
	est store xtP_`Explicative'_CHv 
	}

pwcorr $VariableExpliContinue
	
esttab OLS_SPEI_Any_Fertilizer reg2_Any_FertilizerEF logitSPEIAny_Fertilizer logitSPEIAny_FertilizerEF probitAny_Fertilizer probitAny_FertilizerEF xtregAny_Fertilizer_SPEI xtP_Any_Fertilizer_SPEI, r2 pr2 aic bic  title("Variable à expliquer : utilisation d'engrais chimiques ou organic.") nonumbers mtitles("MCO" "MCO_FE" "logit" "logit_FE" "probit" "probit_FE" "xtreg" "xtlogit") 





// pour les any fertilizer

esttab OLS_SPEI_Any_Fertilizer reg2_Any_FertilizerEF tobCHIRPSvol_Any_Fertilizer xtP_Any_Fertilizer_SPEI using AnyFertillizer.csv, wide title("Variable à expliquer : utilisation d'un engrais chimique ou organic dichotomique") nonumbers mtitles("MCO" "MCO_FE" "TOBIT_FE" "LOGIT panel") 

esttab OLS_SPEI_Chemical_per_acres reg2_Chemical_per_acres tobCHIRPSvol_Any_Fertilizer xtP_Any_Fertilizer_SPEI using AnyFertillizer.csv, wide title("Variable à expliquer : utilisation d'un engrais chimique ou organic dichotomique") nonumbers mtitles("MCO" "MCO_FE" "TOBIT_FE" "LOGIT panel") 

esttab OLS_SPEI_Any_Fertilizer OLS_SPEI_Any_Fertilizer_FE OLS_CHIday_Any_Fertilizer OLS_CHIday_Any_Fertilizer_FE OLS_CHIvol_Any_Fertilizer OLS_CHIvol_Any_Fertilizer_FE 






