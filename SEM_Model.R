library(brms)
library(ggplot2)
library(lme4)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(sp)
library(plm)
library(spdep)
library(haven) # to load the SPSS .sav file
library(dplyr)
library(MASS)
library("dplyr")
library("bayesplot")
library(scales)
library(tidyverse)
library(RColorBrewer)
library(rsem)
library(tidySEM)
library( stats)
library( semPlot)
library( seminr)
library( lmerTest)
library(reshape2)
library(reshape)
library(lavaan)
library(lavaanPlot)
library("olsrr")
#install.packages( c( "rsem", "tidySEM", "bnlearn", "stats", "semPlot", "seminr", "lmerTest" ) )
########################################################################################################
pth0 = "../ProcessedInputs.csv"
MyData0 <- read.csv(file= pth0, header=TRUE)


myvars <- names(MyData0) %in% c( "STATE_x", "COUNTY_x", "PopTot", "MedHHInc", 
	"TotAge65", "Unempolyme", "BelPoverty", "Minority", 
	"FIPS", "Net_Migration", "NETMIG", "Year", 

"UNCLASSIFIED", "OPEN_WATER_x", "DEVELOPED__OPEN_", "DEVELOPED__LOW_I", 
"DEVELOPED__MEDIU", "DEVELOPED__HIGH_", "CULTIVATED_CROPS", 
"policyCost", "policyCount", "crsClass", "elevationN", "BuildCoverage", "floodZoneN",

"BuildPaid", "Claimcount", "ContentPaid", "ClaimBcover", "ClaimCcover", 
"ClaimFZone", "ClaimPosFirmN", "NPMDProperty", "NPMDProject", "NPMDamount", 

"PMDProperty", "PMDProject", "PMDamount", "CountHAR", "HARAproved", "CountHAO", 
"HAOAproved", "PAAmount", "PAfObl",  "PAtotalObl",  "Count_h", "ApprovedLoan_h", 
"SOVI_VALUE", "RESL_VALUE",  "RFLD_EVNTS", 
#"cbsa", "Category"
"CropDmg", "PropertyDmg", "Injuries", "Fatalities", "netflow", "period", "fips")

MyNewData <- MyData0[myvars]
#############################################################################################################
MyNewData2 <- MyNewData
new_colnames <- c( "STATE", "COUNTY", "PopTot", "MedHHInc", 
	"TotAge65", "Unempolyed", "BelPoverty", "Minority", 
	"FIPS", "Net_Migration", "NETMIG", "Year", 

	"UNCLASSIFIED", "OPEN_WATER", "DEVELOPED", "DEVELOW", 
	"DEVEMED", "DEVEHIGH", "CULTI_CROPS", 
	"policyCost", "policyCount", "crsClass", "elevationN", "BuildCoverage", 	"floodZoneN",

	"BuildPaid", "Claimcount", "ContentPaid", "ClaimBcover", "ClaimCcover", 
	"ClaimFZone", "ClaimPosFirmN", "NPMDProperty", "NPMDProject", 	"NPMDamount", 

	"PMDProperty", "PMDProject", "PMDamount", "CountHAR", "HARAproved", 	"CountHAO", 
	"HAOAproved", "PAAmount", "PAfObl",  "PAtotalObl",  "SBAN", "SBALoan", 
	"SOVI_VALUE", "RESL_VALUE",  
	#"cbsa", "Category"
	"CropDmg", "PropertyDmg", "Injuries", "Fatalities", "netflow", "period", 	"fips")

# Assign new column names
colnames(MyNewData2) <- new_colnames
#############################################################################################################
# Select specific columns
selected_columns <- MyNewData [, c("Net_Migration", "NETMIG", "netflow", "period", "fips")]
# Print the selected columns
print( head( selected_columns) )
#############################################################################################################
MyNewData[is.na(MyNewData) | MyNewData == "Inf"] <- 0
MyNewData2 $CropDmglog 			<- log( MyNewData2 $CropDmg + 1 )
MyNewData2 $PropertyDmglog  <- log( MyNewData2 $PropertyDmg + 1 )
MyNewData2 $Injury  				<- MyNewData2 $Injuries 
MyNewData2 $Fatality  			<- MyNewData2 $Fatalities
MyNewData2 $BuildPaidlog 		<- log( MyNewData2 $BuildPaid + 1 )
MyNewData2 $ContentPaidlog  <- log( abs( MyNewData2 $ContentPaid ) + 1 )
MyNewData2 $ClaimBcoverlog  <- log( MyNewData2 $ClaimBcover + 1 )
MyNewData2 $ClaimCcoverlog  <- log( MyNewData2 $ClaimCcover + 1 )
MyNewData2 $FIPS						<- factor(MyNewData2 $FIPS)
MyNewData2 $HAAproved				<- MyNewData2  $HARAproved	+ MyNewData2  $HAOAproved
#"DEVELOPED__OPEN_", "DEVELOPED__LOW_I", "DEVELOPED__MEDIU", "DEVELOPED__HIGH_"
MyNewData2 $NETMIG			<- as.numeric( MyNewData2  $netflow	)
MyNewData2 $DEVELOPED		<- as.numeric( MyNewData2 $DEVELOW + 
											MyNewData2 $DEVEMED + 
											MyNewData2 $DEVEHIGH	)												
MyNewData2[is.na(MyNewData2) | MyNewData2 == "Inf"] <- 0
############################Standardize the variables ###################################################
MyNewData2 $CropDmg.res 	 <- scale(MyNewData2$CropDmglog) 
MyNewData2 $PropertyDmg.res  <- scale( log(MyNewData2$PropertyDmg + 1) )
MyNewData2 $LogPropertyDmg  <- log(MyNewData2$PropertyDmg + 1) 
MyNewData2 $Injury.res  	 <- scale(MyNewData2$Injury) 
MyNewData2 $Fatality.res  	 <- scale(MyNewData2$Fatality) 
MyNewData2 $LogFatality	 		<- log(MyNewData2$Fatality+1)
MyNewData2$NETMIG1000 <- MyNewData2$NETMIG / 1000
MyNewData2$Net_Migration.res <- scale(MyNewData2$NETMIG) 
MyNewData2$PNETMIG.res 		 <- scale(MyNewData2$NETMIG / MyNewData2$PopTot )

MyNewData2$DEVELOPED.res 	 <- scale(MyNewData2$DEVELOPED )
MyNewData2$DEVELOPED1.res 	 <- scale(MyNewData2$DEVEMED +  MyNewData2$DEVEHIGH)
MyNewData2$MedHHInc.res 	 <- scale(MyNewData2$MedHHInc) 
MyNewData2$PopTot.res 		 <- scale(MyNewData2$PopTot ) 
MyNewData2$PTotAge65 	<- MyNewData2$TotAge65 / MyNewData2$PopTot
MyNewData2$PUnempolyed 	<- MyNewData2$Unempolyed / MyNewData2$PopTot
MyNewData2$PBelPoverty 	<- MyNewData2$BelPoverty / MyNewData2$PopTot 
MyNewData2$PMinority 	<- MyNewData2$Minority / MyNewData2$PopTot
MyNewData2$TotAge65.res 	<- scale(MyNewData2$TotAge65 / MyNewData2$PopTot)
MyNewData2$Unempolyed.res 	<- scale(MyNewData2$Unempolyed / MyNewData2$PopTot)
MyNewData2$BelPoverty.res 	<- scale(MyNewData2$BelPoverty / MyNewData2$PopTot )
MyNewData2$Minority.res 	<- scale(MyNewData2$Minority / MyNewData2$PopTot)

MyNewData2$OPEN_WATER.res	<- scale(MyNewData2$OPEN_WATER) 
MyNewData2$DEVELOW.res	 	<- scale(MyNewData2$DEVELOW) 
MyNewData2$DEVEMED.res		<- scale(MyNewData2$DEVEMED) 
MyNewData2$DEVEHIGH.res		<- scale(MyNewData2$DEVEHIGH) 
MyNewData2$LogPolicyCost 	<- log( MyNewData2$policyCost + 1 )
MyNewData2$LogPolicyCount 	<- log(MyNewData2$policyCount + 1) 
MyNewData2$policyCost.res 	<- scale( MyNewData2$policyCost + 1 )
MyNewData2$policyCount.res 	<- scale(MyNewData2$policyCount) 
MyNewData2$crsClass.res 	<- scale(MyNewData2$crsClass ) 
MyNewData2$elevationN.res	 <- scale(MyNewData2$elevationN) 

MyNewData2$LogBuildCoverage <- log(MyNewData2$BuildCoverage + 1 )
MyNewData2$BuildCoverage.res <- scale( log(MyNewData2$BuildCoverage + 1 ) )
MyNewData2$floodZoneN.res 	 <- scale(MyNewData2$floodZoneN ) 
MyNewData2$BuildPaid.res 	 <- scale(MyNewData2$BuildPaid) 
MyNewData2$Claimcount.res 	 <- scale(MyNewData2$Claimcount) 
MyNewData2$ContentPaid.res 	 <- scale(MyNewData2$ContentPaid ) 
MyNewData2$ClaimBcover.res 				<- scale(MyNewData2$ClaimBcover) 
MyNewData2$ClaimCcover.res 				<- scale(MyNewData2$ClaimCcover) 
MyNewData2$ClaimFZone.res 				<- scale(MyNewData2$ClaimFZone) 
MyNewData2$ClaimPosFirmN.res 			<- scale(MyNewData2$ClaimPosFirmN) 
MyNewData2$NPMDProperty.res 			<- scale(MyNewData2$NPMDProperty) 
MyNewData2$NPMDProject.res 				<- scale(MyNewData2$NPMDProject) 
MyNewData2$NPMDamount.res 				<- scale( log(MyNewData2$NPMDamount +1) )
MyNewData2$LogNPMDamount 				<- log(MyNewData2$NPMDamount +1)

MyNewData2$PMDProperty.res 				<- scale(MyNewData2$PMDProperty) 
MyNewData2$PMDProject.res 				<- scale(MyNewData2$PMDProject) 
MyNewData2$PMDamount.res 					<- scale(MyNewData2$PMDamount) 
MyNewData2$CountHAR.res						<- scale(MyNewData2$CountHAR) 
MyNewData2$HARAproved.res 				<- scale(MyNewData2$HARAproved) 
MyNewData2$CountHAO.res 					<- scale(MyNewData2$CountHAO) 
MyNewData2$LogHAOAproved 				<- log(MyNewData2$HAOAproved+1) 
MyNewData2$HAOAproved.res 				<- scale( log(MyNewData2$HAOAproved + 1) )
MyNewData2$HAAproved.res 					<- scale(MyNewData2 $HAAproved) 
MyNewData2$PAAmount.res 					<- scale(MyNewData2$PAAmount) 
MyNewData2$PAtotalObl.res 				<- scale(MyNewData2$PAtotalObl) 
MyNewData2 $SBAN.res 							<- scale(MyNewData2$SBAN) 


MyNewData2$LogSBALoan							<- log(MyNewData2$SBALoan + 1) 
MyNewData2$SBALoan.res						<- scale( log(MyNewData2$SBALoan + 1) ) 
MyNewData2$BuildPaidlog.res 			<- scale(MyNewData2$BuildPaidlog) 
MyNewData2$ContentPaidlog.res 		<- scale(MyNewData2$ContentPaidlog) 
MyNewData2$ClaimBcoverlog.res			<- scale(MyNewData2$ClaimBcoverlog) 
MyNewData2$ClaimCcoverlog.res	 		<- scale(MyNewData2$ClaimCcoverlog) 
MyNewData2$NPMDamountlog.res 			<- scale(MyNewData2$NPMDamountlog) 
MyNewData2$DEVELOPED.res 					<- scale(MyNewData2$DEVELOPED ) 
MyNewData2$SOVI_VALUE.res 				<- scale(MyNewData2$SOVI_VALUE ) 
MyNewData2$RESL_VALUE.res 				<- scale(MyNewData2$RESL_VALUE ) 
########lmer 1#############################################################################################
myvars <- names(MyNewData2) %in% c( "STATE", "COUNTY", "FIPS", 
 "Year", "period" , "CropDmglog",        "LogPropertyDmg" , "Injury"  ,       
 "LogFatality"  ,  "NETMIG1000" , "NETMIG",        
"DEVELOPED" ,     "DEVELOPED1",     "MedHHInc" ,      "PopTot"   ,     
"PTotAge65" ,      "PUnempolyed" ,    "PBelPoverty" ,    "Minority"   ,   
"OPEN_WATER" ,    "DEVELOW"  ,      "DEVEMED" ,       "DEVEHIGH.res"   ,   
"LogPolicyCost" ,    "LogPolicyCount" ,   "crsClass" ,      "elevationN"  ,  
"LogBuildCoverage", "BuildPaid" ,     "Claimcount" ,    "ContentPaid" ,   
"ClaimBcover"  , 
"ClaimCcover" ,   "ClaimFZone" ,    "ClaimPosFirmN",  "NPMDProperty" , 
"NPMDProject",    "NPMDamount" ,    "PMDProperty",    "PMDProject" ,   
"PMDamount" ,     "CountHAR" ,      "HARAproved" ,    "CountHAO" ,     
"LogHAOAproved",     "HAAproved" ,     "PAAmount"  ,     "PAtotalObl"  ,  
"SBAN" ,          "LogSBALoan" ,       "BuildPaidlog" ,  "ContentPaidlog",
"ClaimBcoverlog", "ClaimCcoverlog", "NPMDamountlog"  )
MyNewData22 <- MyNewData2[myvars]
MyNewData22 <- MyNewData22[ MyNewData3["Year"] >= 2000, 	]

summary(data$height)

modelstructure <- '
  # measurement model
		Vulnerability 		=~  PUnempolyed + PBelPoverty + PTotAge65	
		PublicAssistance 	=~ 	LogHAOAproved  + NPMDamountlog + LogSBALoan
		FloodInsurance		=~	LogPolicyCount + LogPolicyCost  + LogBuildCoverage
		DisasterDamage		=~	CropDmglog + LogPropertyDmg + LogFatality

		PopuChange  			=~	NETMIG1000
  # regressions
  	PopuChange 				~ Vulnerability + PublicAssistance +  FloodInsurance + DisasterDamage 
	# Relationships among latent variables
  	PublicAssistance 	~	Vulnerability  + DisasterDamage +  FloodInsurance
		FloodInsurance	 	~	Vulnerability  + DisasterDamage 
		Vulnerability    ~   DisasterDamage
  #variances and covariances
  	Vulnerability	 	~~	DisasterDamage
'

fit1d <- sem(modelstructure, data= MyNewData22, std.lv = TRUE, 
			missing = "ML", se = "robust", estimator = "MLR", 
			optim.method=list("BFGS"), check.gradient = TRUE)

summary(fit1d, standardized=TRUE)
