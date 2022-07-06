#User can change these values
StartYear <- 2015
StartProjectionYear <- 2022
EndProjectionYear <- 2052
#User can change these values
StartYear <- 2015
StartProjectionYear <- 2022
EndProjectionYear <- 2052
FileName <- 'data/funding/Funding Model Inputs 2021.xlsx'
#
#Reading Input File
# user_inputs_numeric <- read_excel(FileName, sheet = 'Numeric Inputs')
# user_inputs_character <- read_excel(FileName, sheet = 'Character Inputs')
# Historical_Data <- read_excel(FileName, sheet = 'Historical Data')
# Scenario_Data <- read_excel(FileName, sheet = 'Inv_Returns')
# BenefitPayments <- read_excel(FileName, sheet = 'Benefit Payments')
# 
# saveRDS(user_inputs_numeric, "data/funding/user_inputs_numeric.rds")
# saveRDS(user_inputs_character, "data/funding/user_inputs_character.rds")
# saveRDS(Historical_Data, "data/funding/Historical_Data.rds")
# saveRDS(Scenario_Data, "data/funding/Scenario_Data.rds")
# saveRDS(BenefitPayments, "data/funding/BenefitPayments.rds")


user_inputs_numeric <- readRDS("data/funding/user_inputs_numeric.rds")
user_inputs_character <- readRDS("data/funding/user_inputs_character.rds")
Historical_Data <- readRDS("data/funding/Historical_Data.rds")
Scenario_Data <- readRDS("data/funding/Scenario_Data.rds")
BenefitPayments <- readRDS("data/funding/BenefitPayments.rds")


#################################################################################################################################################################
#
#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = (1 + rate)*pmt * (1 - (1 + rate) ^ (-nper)) / rate
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  # if(nper == 0){
  #   a <- 0
  # }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period, and 0 for beginning of period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

#Get period function
GetNPER <- function(r,g,pv,t,pmt){
  PV <- pv*(1+r)^t
  R <- (1+r)/(1+g) - 1
  TempValue <- PV*R/(pmt*(1+R))
  NPER <- -log(1-TempValue,(1+R))
  if(is.infinite(NPER)) {NPER <- 100}
  if(is.nan(NPER)) {NPER <- 100}
  return(NPER)
}
#
##################################################################################################################################################################
#

#Replace NAs
Historical_Data[is.na(Historical_Data)] <- 0
#
#Reading Values from Input input file and assigning values
#Assigning numeric inputs
for(i in 1:nrow(user_inputs_numeric)){
  if(!is.na(user_inputs_numeric[i,2])){
    assign(as.character(user_inputs_numeric[i,2]),as.double(user_inputs_numeric[i,3]))
  }
}
#
#Assigning character inputs
for(i in 1:nrow(user_inputs_character)){
  if(!is.na(user_inputs_character[i,2])){
    assign(as.character(user_inputs_character[i,2]),as.character(user_inputs_character[i,3]))
  }
}

#Create an empty Matrix for the Projection Years
EmptyMatrix <- matrix(0,(EndProjectionYear - StartProjectionYear + 1), 1)
for(j in 1:length(colnames(Historical_Data))){
  TempMatrix <- rbind(as.matrix(Historical_Data[,j]), EmptyMatrix)
  assign(as.character(colnames(Historical_Data)[j]), TempMatrix)
}
#Assign values for Projection Years
FYE <- StartYear:EndProjectionYear
#Get Start Index, since historical data has 3 rows, we want to start at 4
StartIndex <- StartProjectionYear - StartYear + 1
HistoricalIndex <- StartProjectionYear - StartYear



#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)

#
##################################################################################################################################################################

RunModel <- function(DR_CurrentHires = dis_r_current,           #Discount rate for current hires. Min = 4%. Max = 9%. 0.25% step
                     DR_NewHires = dis_r_new,                   #Discount rate for new hires. Min = 4%. Max = 9%. 0.25% step
                     ReturnType = AnalysisType,                           #"Deterministic" or "Stochastic" type of simulated returns.
                     DeSimType = ScenType,                                #Deterministic return scenarios. 
                     StoSimType = SimType,                                #"Assumed" or "Conservative" (for stochastic analysis)
                     FundingPolicy = ERPolicy,                            #"Variable Statutory", "Fixed Statutory", or "ADC" funding policy.
                     CostShare_AmoNew = CostSharingAmo,                   #"No" or "Yes". "No" means no Amo cost sharing between the employer and new hires.
                     CostShare_NCNew = CostSharingNC,                     #"No" or "Yes". "No" means no Normal cost sharing between the employer and new hires.
                     CurrentDebt_period = NoYearsADC_CurrentDebt,         #Amortization period (in years) for current unfunded liability. 
                     NewDebtCurrentHire_period = NoYearsADC_NewDebtCurrentHire,       #Amortization period (in years) for new unfunded liability created under current hire plan
                     NewDebtNewHire_period = NoYearsADC_NewDebtNewHire,               #Amortization period (in years) for new unfunded liability created under new hire plan  
                     AmoMethod_current = AmoMethod_CurrentHire,                       #"Level %" or "Level dollar" amortization method for unfunded liability created under current hire plan
                     AmoMethod_new = AmoMethod_NewHire,                               #"Level %" or "Level dollar" amortization method for unfunded liability created under new hire plan
                     OneTimeInfusion = CashInfusion,                                  #One time cash infusion in 2022.
                     NewHireDC_choice = DC_NewHires,                                  #Percentage of new hires electing the DC plan. This should be in % unit. 
                     DC_ContRate = DC_Contrib,                                        #DC contribution rate for new hires 
                     BenMult = BenMult_new){  
  
  
  ##Amo period tables
  currentlayer <- seq(CurrentDebt_period, 1)
  futurelayer_currenthire <- seq(NewDebtCurrentHire_period, 1)
  futurelayer_futurehire <- seq(NewDebtNewHire_period, 1)
  n <- max(length(currentlayer), length(futurelayer_currenthire))
  length(currentlayer) <- n
  length(futurelayer_currenthire) <- n
  
  #Amo period table for current hires plan
  OffsetYears_CurrentHires <- rbind(currentlayer, matrix(futurelayer_currenthire, 
                                                         nrow = RowColCount,
                                                         ncol = length(currentlayer),
                                                         byrow = T))
  
  rownames(OffsetYears_CurrentHires) <- NULL         #Remove row names
  
  for (i in 1:ncol(OffsetYears_CurrentHires)) {      #Put the amo periods on diagonal rows
    OffsetYears_CurrentHires[,i] <- lag(OffsetYears_CurrentHires[,i], n = i - 1)
  }
  
  OffsetYears_CurrentHires[is.na(OffsetYears_CurrentHires)] <- 0    #Turn all NAs in the table to 0s
  
  #Amo period table for future hires plan
  OffsetYears_NewHires <- matrix(futurelayer_futurehire, 
                                 nrow = RowColCount + 1,
                                 ncol = length(futurelayer_futurehire),
                                 byrow = T)
  
  for (i in 1:ncol(OffsetYears_NewHires)) {      #Put the amo periods on diagonal rows
    OffsetYears_NewHires[,i] <- lag(OffsetYears_NewHires[,i], n = i - 1)
  }
  
  OffsetYears_NewHires[is.na(OffsetYears_NewHires)] <- 0    #Turn all NAs in the table to 0s
  
  ##Amo base and payment tables
  #Default value is Lv% for Amo Base 
  #If its Level $, then set to 0
  if(AmoMethod_current == "Level $"){
    AmoBaseInc_CurrentHire <- 0
  }
  
  if(AmoMethod_new == "Level $"){
    AmoBaseInc_NewHire <- 0
  }
  
  #Amo base & payment - current hires initial setup
  OutstandingBase_CurrentHires <- matrix(0,RowColCount + 1, length(currentlayer) + 1)
  Amortization_CurrentHires <- matrix(0,RowColCount + 1, length(currentlayer))
  #Initialize the first UAAL layer and amo payment (current hires)
  OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
  Amortization_CurrentHires[1,1] <- PMT(pv = OutstandingBase_CurrentHires[1,1], 
                                        r = CurrentHires_DR[HistoricalIndex], 
                                        g = AmoBaseInc_CurrentHire, 
                                        t = 0.5,
                                        nper = OffsetYears_CurrentHires[1,1])
  
  #Amo base & payment - future hires initial setup
  OutstandingBase_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire) + 1)
  Amortization_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire))
  
  #Scenario Index for referencing later based on investment return data
  Scenario_Data$Assumption[StartIndex:nrow(Scenario_Data)] <- DR_CurrentHires    #Change return values in "Assumption" scenario to match the discount rate input for current hires
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(DeSimType))
  
  #intialize this value at 0 for Total ER Contributions
  Total_ER[StartIndex-1] <- 0
  for(i in StartIndex:length(FYE)){
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    #Payroll
    TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth) 
    if(i == StartIndex){
      PayrollLegacy_Pct[i] <- 0.95
      PayrollNewTier[i] <- 0.05
    } else if (FYE[i] <= 2043) {
      PayrollLegacy_Pct[i] <- PayrollLegacy_Pct[i-1]*0.9
      PayrollNewTier[i] <- 1 - PayrollLegacy_Pct[i]
    } else {
      PayrollLegacy_Pct[i] <- PayrollLegacy_Pct[i-1]*0.8
      PayrollNewTier[i] <- 1 - PayrollLegacy_Pct[i]
    }
    NewHirePayrollDB[i] <- PayrollNewTier[i]*TotalPayroll[i]*(1 - NewHireDC_choice)
    NewHirePayrollDC[i] <- PayrollNewTier[i]*TotalPayroll[i]*NewHireDC_choice
    PayrollLegacy[i] <- PayrollLegacy_Pct[i]*TotalPayroll[i]
    #
    #Discount Rate
    CurrentHires_DR[i] <- DR_CurrentHires
    NewHires_DR[i] <- DR_NewHires
    #
    #Benefit Payments, Admin Expenses
    BenPayments_BaseTotal[i] <- BenPayments_BaseTotal[i-1]*(1+BenPayment_Growth)
    BenPayments_BaseNew[i] <- -1*BenefitPayments$NewHireBP_Pct[i]*(NewHirePayrollDB[i] + NewHirePayrollDC[i])
    BenPayments_NewHires[i] <- -1*BenefitPayments$NewHireBP_Pct[i]*NewHirePayrollDB[i]*BenMult/BenMult_current      #Revise this later with NC ratio
    BenPayments_CurrentHires[i] <- BenPayments_BaseTotal[i] - BenPayments_BaseNew[i]
    
    Refunds[i] <- 0
    AdminExp_CurrentHires[i] <- -1*Admin_Exp_Pct*PayrollLegacy[i]
    AdminExp_NewHires[i] <- -1*Admin_Exp_Pct*NewHirePayrollDB[i]
    #
    ##Accrued Liability and Normal Cost calculations (projection + DR adjustment combined in one place)
    #Normal Cost
    DRDifference_CurrentNC <- 100*(CurrentHires_DR[HistoricalIndex] - CurrentHires_DR[i])
    DRDifference_NewNC <- 100*(NewHires_DR[HistoricalIndex] - NewHires_DR[i])
    MOYNCExist[i] <- PayrollLegacy[i]*NC_CurrentHires_Pct_1*(1 + NCSensDR/100)^DRDifference_CurrentNC     #Revise this later with NC model 
    MOYNCNewHires[i] <- NewHirePayrollDB[i]*NC_NewHires_Pct_1*(1 + NCSensDR/100)^DRDifference_NewNC       #Revise this later with NC model
    
    #Accrued Liability
    DRDifference_CurrentAAL <- 100*(CurrentHires_DR[i-1] - CurrentHires_DR[i])
    DRDifference_NewAAL <- 100*(NewHires_DR[i-1] - NewHires_DR[i])
    AccrLiab_CurrentHires[i] <- (AccrLiab_CurrentHires[i-1]*(1 + CurrentHires_DR[i]) + (MOYNCExist[i] + BenPayments_CurrentHires[i])*(1 + CurrentHires_DR[i])^0.5) * ((1+LiabSensDR/100)^DRDifference_CurrentAAL) * ((1+Convexity/100)^(DRDifference_CurrentAAL^2/2))
    AccrLiab_NewHires[i] <- (AccrLiab_NewHires[i-1]*(1 + NewHires_DR[i]) + (MOYNCNewHires[i] + BenPayments_NewHires[i])*(1 + NewHires_DR[i])^0.5) * ((1+LiabSensDR/100)^DRDifference_NewAAL) * ((1+Convexity/100)^(DRDifference_NewAAL^2/2))
    AccrLiab_Total[i] <- AccrLiab_CurrentHires[i] + AccrLiab_NewHires[i]
    #
    
    #NC, Reduced Rate contribution policy
    # TotalNC_Pct[i-1] <- (MOYNCExistNewDR[i] + MOYNCNewHiresNewDR[i]) / TotalPayroll[i]
    NC_Legacy_Pct[i-1] <- MOYNCExist[i] / PayrollLegacy[i]
    
    if(NewHirePayrollDB[i] > 0){
      NC_NewHires_Pct[i-1] <- MOYNCNewHires[i] / NewHirePayrollDB[i]
    } else {
      NC_NewHires_Pct[i-1] <- 0
    }
    
    Suppl_Contrib[i] <- Suppl_Contrib[i-1]*1.01
    EffStat_ER_Red[i] <- (RedRatFundPeriod_ER*TotalPayroll[i] + Suppl_Contrib[i]) / TotalPayroll[i]
    FixedStat_AmoPayment_Red[i] <- EffStat_ER_Red[i]*TotalPayroll[i] - 
      (NC_Legacy_Pct[i-1] + Admin_Exp_Pct - RedRatFundPeriod_EE)*PayrollLegacy[i] -
      (NC_NewHires_Pct[i-1] + Admin_Exp_Pct - RedRatFundPeriod_EE)*NewHirePayrollDB[i] -
      DC_ContRate*NewHirePayrollDC[i]
    
    FundPeriod_Red[i] <- GetNPER(CurrentHires_DR[i],
                                 AmoBaseInc_CurrentHire,
                                 UAL_AVA[i-1],
                                 0.5,
                                 FixedStat_AmoPayment_Red[i])
    #
    ##ER, EE, Amo Rates
    #Employee normal cost (current hires) 
    if(FundPeriod_Red[i] <= RedRatFundPeriod){
      EE_NC_Legacy_Pct[i-1] <- RedRatFundPeriod_EE
    } else {
      EE_NC_Legacy_Pct[i-1] <- EEContrib_CurrentHires
    }
    
    #Employee normal cost (new hires + cost sharing condition)
    if(CostShare_NCNew == 'Yes'){
      EE_NC_NewHires_Pct[i-1] <- NC_NewHires_Pct[i-1]/2
    } else if (FundPeriod_Red[i] <= RedRatFundPeriod){
      EE_NC_NewHires_Pct[i-1] <- RedRatFundPeriod_EE 
    } else {
      EE_NC_NewHires_Pct[i-1] <- EEContrib_NewHires
    }
    
    #Employer normal cost
    ER_NC_Legacy_Pct[i-1] <- NC_Legacy_Pct[i-1] - EE_NC_Legacy_Pct[i-1]
    ER_NC_NewHires_Pct[i-1] <- NC_NewHires_Pct[i-1] - EE_NC_NewHires_Pct[i-1]
    
    #Amo factor to calculate the variable statutory contribution
    AmoFactor[i] <- as.matrix(PresentValue((1+CurrentHires_DR[i])/(1+AmoBaseInc_CurrentHire)-1,CurrentDebt_period,1) / ((1+CurrentHires_DR[i])^0.5))
    if(FYE[i] < 2025){
      Stat_ER[i] <- Stat_ER[i-1] + 0.001
    } else {
      Stat_ER[i] <- Stat_ER[i-1]
    }
    
    #Fixed Statutory Employer Contribution
    EffStat_ER[i] <- (Stat_ER[i]*TotalPayroll[i] + Suppl_Contrib[i]) / TotalPayroll[i]
    
    FixedStat_AmoPayment[i] <- EffStat_ER[i]*TotalPayroll[i] - 
      (ER_NC_Legacy_Pct[i-1] + Admin_Exp_Pct)*PayrollLegacy[i] -
      (ER_NC_NewHires_Pct[i-1] + Admin_Exp_Pct)*NewHirePayrollDB[i] -
      DC_ContRate*NewHirePayrollDC[i]
    
    #Variable Statutory Employer Contribution
    if((round(FundPeriod[i-1],2) > CurrentDebt_period) && (UAL_AVA[i-1] > 0) && (FYE[i] > 2022)){
      ADC_Cond[i] <- "Yes"
    } else {
      ADC_Cond[i] <- "No"
    }
    
    if(i == StartIndex){
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment[i]
    } else if(UAL_AVA[i-1] <= 0){
      VarStat_AmoPayment[i] <- 0
    } else if(FundPeriod_Red[i] <= RedRatFundPeriod){
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment_Red[i]
    } else if(ADC_Cond[i] == "Yes"){
      VarStat_AmoPayment[i] <- UAL_AVA[i-1] / AmoFactor[i-1]
      #This condition is to say if there was any instance of yes for ADC Cond before current year
    } else if(!is_empty(which(ADC_Cond[StartIndex:i-1] == "Yes"))){
      VarStat_AmoPayment[i] <- VarStat_AmoPayment[i-1]*(1 + AmoBaseInc_CurrentHire)
    } else {
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment[i]
    }
    
    #Funding period for variable statutory contribution
    FundPeriod[i] <- GetNPER(CurrentHires_DR[i],AmoBaseInc_CurrentHire,UAL_AVA[i-1],0.5,VarStat_AmoPayment[i])
    #
    #Amo Rates
    AmoRate_CurrentHires[i-1] <- sum(Amortization_CurrentHires[ProjectionCount,]) / TotalPayroll[i]
    AmoRate_NewHires[i-1] <- sum(Amortization_NewHires[ProjectionCount,]) / (NewHirePayrollDB[i] + NewHirePayrollDC[i])
    
    if(CostShare_AmoNew == "Yes"){
      EE_AmoRate_NewHires[i-1] <- AmoRate_NewHires[i-1] / 2
    } else {
      EE_AmoRate_NewHires[i-1] <- 0
    }
    
    if(FundingPolicy == "Fixed Statutory"){
      StatAmoRate[i-1] <- FixedStat_AmoPayment[i] / TotalPayroll[i]
    } else {
      StatAmoRate[i-1] <- VarStat_AmoPayment[i] / TotalPayroll[i]
    }
    #
    #Cashflows, NC, Amo. Solv, etc.
    EE_NC_CurrentHires[i] <- EE_NC_Legacy_Pct[i-1]*PayrollLegacy[i]
    EE_NC_NewHires[i] <- EE_NC_NewHires_Pct[i-1]*NewHirePayrollDB[i]
    EE_Amo_NewHires[i] <- EE_AmoRate_NewHires[i-1]*NewHirePayrollDB[i]
    ER_NC_CurrentHires[i] <- ER_NC_Legacy_Pct[i-1]*PayrollLegacy[i] - AdminExp_CurrentHires[i]
    ER_NC_NewHires[i] <- ER_NC_NewHires_Pct[i-1]*NewHirePayrollDB[i] - AdminExp_NewHires[i]
    
    if(FundingPolicy == "ADC"){
      ER_Amo_CurrentHires[i] <- max(AmoRate_CurrentHires[i-1]*TotalPayroll[i],-ER_NC_CurrentHires[i])
      ER_Amo_NewHires[i] <- max(AmoRate_NewHires[i-1]*(NewHirePayrollDB[i] + NewHirePayrollDC[i]) - EE_Amo_NewHires[i],-ER_NC_NewHires[i])
    } else {
      ER_Amo_CurrentHires[i] <- max(StatAmoRate[i-1]*PayrollLegacy[i],-ER_NC_CurrentHires[i])
      ER_Amo_NewHires[i] <- max(StatAmoRate[i-1]*(NewHirePayrollDB[i] + NewHirePayrollDC[i]),-ER_NC_NewHires[i])
    }
    
    if(FYE[i] == 2022){
      ERCashInfusion <- OneTimeInfusion
    } else {
      ERCashInfusion <- 0
    }
    
    Additional_ER[i] <- ERCashInfusion
    
    #R#eturn data
    #Assign values for simulation
    if(StoSimType == 'Assumed'){
      SimReturn <- SimReturnAssumed
    } else if(StoSimType == 'Conservative'){
      SimReturn <- SimReturnConservative
    }
    
    #Return data based on deterministic or stochastic
    if((ReturnType == 'Stochastic') && (i >= StartIndex)){
      ROA_MVA[i] <- rnorm(1,SimReturn,SimVolatility)
    } else if(ReturnType == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    
    #Solvency Contribution
    DC_Forfeit[i] <- DC_ContRate*(1 - Ratio_DCVesting)*NewHirePayrollDC[i]
    CashFlows_Total <- BenPayments_CurrentHires[i] + BenPayments_NewHires[i] + Refunds[i] + AdminExp_CurrentHires[i] +
      AdminExp_NewHires[i] + EE_NC_CurrentHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] +
      ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Additional_ER[i] + DC_Forfeit[i]
    
    Solv_Contrib[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + CashFlows_Total*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    Solv_Contrib_CurrentHires[i] <- Solv_Contrib[i] * (AccrLiab_CurrentHires[i] / AccrLiab_Total[i])
    Solv_Contrib_NewHires[i] <- Solv_Contrib[i] * (AccrLiab_NewHires[i] / AccrLiab_Total[i])
    #
    #Net CF, Expected MVA, Gain Loss, Defered Losses for current hires
    NetCF_CurrentHires[i] <- BenPayments_CurrentHires[i] + AdminExp_CurrentHires[i] + EE_NC_CurrentHires[i] +
      ER_NC_CurrentHires[i] + ER_Amo_CurrentHires[i] + Additional_ER[i] + Solv_Contrib_CurrentHires[i] + DC_Forfeit[i]
    
    ExpInvInc_CurrentHires[i] <- (MVA_CurrentHires[i-1]*CurrentHires_DR[i]) + (NetCF_CurrentHires[i]*CurrentHires_DR[i]*0.5)
    ExpectedMVA_CurrentHires[i] <- MVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i]
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + (NetCF_CurrentHires[i])*(1+ROA_MVA[i])^0.5 
    GainLoss_CurrentHires[i] <- MVA_CurrentHires[i] - ExpectedMVA_CurrentHires[i] 
    Year1GL_CurrentHires[i] <- GainLoss_CurrentHires[i]*0.25
    Year2GL_CurrentHires[i] <- Year1GL_CurrentHires[i-1]
    Year3GL_CurrentHires[i] <- Year2GL_CurrentHires[i-1]
    Year4GL_CurrentHires[i] <- Year3GL_CurrentHires[i-1]
    TotalDefered_CurrentHires[i] <- Year1GL_CurrentHires[i] + Year2GL_CurrentHires[i] + Year3GL_CurrentHires[i] + Year4GL_CurrentHires[i]
    
    #Net CF, Expected MVA, Gain Loss, Defered Losses for new hires
    NetCF_NewHires[i] <- BenPayments_NewHires[i] + AdminExp_NewHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] +
      ER_NC_NewHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_NewHires[i]
    
    ExpInvInc_NewHires[i] <- (MVA_NewHires[i-1]*NewHires_DR[i]) + (NetCF_NewHires[i]*NewHires_DR[i]*0.5)
    ExpectedMVA_NewHires[i] <- MVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i]
    MVA_NewHires[i] <- MVA_NewHires[i-1]*(1+ROA_MVA[i]) + (NetCF_NewHires[i])*(1+ROA_MVA[i])^0.5 
    GainLoss_NewHires[i] <- MVA_NewHires[i] - ExpectedMVA_NewHires[i] 
    Year1GL_NewHires[i] <- GainLoss_NewHires[i]*0.25
    Year2GL_NewHires[i] <- Year1GL_NewHires[i-1]
    Year3GL_NewHires[i] <- Year2GL_NewHires[i-1]
    Year4GL_NewHires[i] <- Year3GL_NewHires[i-1]
    TotalDefered_NewHires[i] <- Year1GL_NewHires[i] + Year2GL_NewHires[i] + Year3GL_NewHires[i] + Year4GL_NewHires[i]
    #
    #AVA, MVA, UA, FR
    AVA_CurrentHires[i] <- AVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i] + TotalDefered_CurrentHires[i]
    #AVA_CurrentHires[i] <- MVA_CurrentHires[i] - TotalDefered_CurrentHires[i]
    AVA_CurrentHires[i] <- max(AVA_CurrentHires[i],AVA_lowerbound*MVA_CurrentHires[i])
    AVA_CurrentHires[i] <- min(AVA_CurrentHires[i],AVA_upperbound*MVA_CurrentHires[i])
    UAL_AVA_CurrentHires[i] <- AccrLiab_CurrentHires[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiab_CurrentHires[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- AVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i] + TotalDefered_NewHires[i]
    #AVA_NewHires[i] <- MVA_NewHires[i] - TotalDefered_NewHires[i]
    AVA_NewHires[i] <- max(AVA_NewHires[i],AVA_lowerbound*MVA_NewHires[i])
    AVA_NewHires[i] <- min(AVA_NewHires[i],AVA_upperbound*MVA_NewHires[i])
    UAL_AVA_NewHires[i] <- AccrLiab_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiab_NewHires[i] - MVA_NewHires[i]
    
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiab_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiab_Total[i]
    UAL_AVA[i] <- AccrLiab_Total[i] - AVA[i]
    UAL_MVA[i] <- AccrLiab_Total[i] - MVA[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    
    
    #Employer Contribution
    Total_ERContrib_DB[i] <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + 
      ER_Amo_NewHires[i] + Additional_ER[i] + Solv_Contrib[i]
    
    ERContrib_DC[i] <- DC_ContRate * Ratio_DCVesting * NewHirePayrollDC[i]
    
    Total_ERContrib[i] <- max(Total_ERContrib_DB[i] + ERContrib_DC[i],0)
    
    ER_InflAdj[i] <- Total_ERContrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_ERContrib[i] / TotalPayroll[i]
    
    #All-in Employer Cost
    #The total (cumulative) employer contribution in the first year of projection must be set to equal the ER_InflAdj
    if(i == StartIndex){
      Total_ER[i] <- ER_InflAdj[i] 
    } else {
      Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i] 
    }
    
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    
    
    ##Amortization
    #Current Hires
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)] <- OutstandingBase_CurrentHires[ProjectionCount,1:(ncol(OutstandingBase_CurrentHires)-1)]*(1 + CurrentHires_DR[i]) - (Amortization_CurrentHires[ProjectionCount,1:ncol(Amortization_CurrentHires)]*(1 + CurrentHires_DR[i])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      
      #Amo Layers
      Amortization_CurrentHires[ProjectionCount+1,1:ncol(Amortization_CurrentHires)] <- PMT(pv = OutstandingBase_CurrentHires[ProjectionCount+1,1:(ncol(OutstandingBase_CurrentHires)-1)], 
                                                                                            r = CurrentHires_DR[i], 
                                                                                            g = AmoBaseInc_CurrentHire, 
                                                                                            t = 0.5,
                                                                                            nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,1:ncol(OffsetYears_CurrentHires)],1))
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)] <- OutstandingBase_NewHires[ProjectionCount,1:(ncol(OutstandingBase_NewHires)-1)]*(1 + NewHires_DR[i]) - (Amortization_NewHires[ProjectionCount,1:ncol(Amortization_NewHires)]*(1 + NewHires_DR[i])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      
      #Amo Layers
      Amortization_NewHires[ProjectionCount+1,1:ncol(Amortization_NewHires)] <- PMT(pv = OutstandingBase_NewHires[ProjectionCount+1,1:(ncol(OutstandingBase_NewHires)-1)], 
                                                                                    r = NewHires_DR[i], 
                                                                                    g = AmoBaseInc_NewHire, 
                                                                                    t = 0.5,
                                                                                    nper = pmax(OffsetYears_NewHires[ProjectionCount+1,1:ncol(OffsetYears_NewHires)],1))
    }
    #
  }  
  
  #Join all the outputs together
  #Initialize Output as the first column FYE
  Output <- FYE
  for(i in 2:length(Historical_Data)){
    Output <- cbind(Output,get(colnames(Historical_Data)[i]))
  }
  return(as.data.frame(Output))
}


# test_df <- RunModel(
#   DR_CurrentHires = dis_r_current,
#   DR_NewHires = dis_r_new,
#   # ReturnType = input$AnalysisType_1,
#   DeSimType = "Assumption",
#   # StoSimType = input$SimType_1,
#   FundingPolicy = "Statutory Rate",
#   CostShare_AmoNew = CostSharingAmo,
#   CostShare_NCNew = CostSharingNC,
#   CurrentDebt_period = NoYearsADC_CurrentDebt,
#   NewDebtCurrentHire_period = NoYearsADC_NewDebtCurrentHire,
#   NewDebtNewHire_period = NoYearsADC_NewDebtNewHire,
#   AmoMethod_current = "Level %",
#   AmoMethod_new = "Level %",
#   OneTimeInfusion = CashInfusion,
#   NewHireDC_choice = DC_NewHires,
#   DC_ContRate = DC_Contrib)


##################################################################################################################################################################


# FileName <- 'backend/MT PERS BModel - NDPERS Version.xlsx'
YearStart <- 2022
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2001:(120-20+YearStart)
HireType <- c("New Hire","Legacy")

#Assigning individual  Variables
model_inputs <- readRDS("data/benefits/model_inputs.rds")

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

Type <- "New Hire"
#Import key data tables
# SurvivalRates <- readRDS("backend/survival_rates.rds")
# ScaleBB <- readRDS("backend/scale_bb.rds")
# SalaryGrowth <- readRDS("backend/salary_growth.rds")
SalaryEntry <- readRDS("data/benefits/salary_entry.rds")
# TerminationRate <- readRDS("backend/termination_rate.rds")
# RetirementRates <- readRDS("backend/retirement_rates.rds")


#-----Retirement Type----

RetirementType <- function(Age, YOS, HireType){
  Type = ifelse(HireType == 'New Hire',
                ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                         (Age >= NormalRetAgeII), 'Regular',
                       ifelse((Age >= EarlyRetAge & YOS >= EarlyRetYOS),'Early','None')),
                
                ifelse((Age >= NormalRetAgeI_Legacy & YOS >= NormalYOSI_Legacy) |
                         (Age >= NormalRetAgeII_Legacy) |
                         (YOS >= NormalYOSII_Legacy), 'Regular',
                       ifelse((Age >= EarlyRetAge_Legacy & YOS >= EarlyRetYOS_Legacy) | 
                                (Age < EarlyRetAgeII_Legacy & YOS >= EarlyRetYOSII_Legacy),'Early','None')))
  return(Type)
}


#-----Retirement Eligibilty----
IsRetirementEligible <- function(Age, YOS, HireType){
  Check = ifelse(RetirementType(Age, YOS, HireType) == "Regular" |
                   RetirementType(Age, YOS, HireType) == "Early", TRUE, FALSE)
  return(Check)
}


#----FUNCTION Calculate Cumulative Future Values----
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2 :length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}


#----Mortality calculations-----
# MortalityTable <- expand_grid(19:120, Years) %>%    #Manually set starting age at 19 here to accommodate the 1-year set back for male mortality
#   rename(Age = `19:120`) %>% 
#   left_join(SurvivalRates, by = "Age") %>% 
#   left_join(ScaleBB, by = "Age") %>% 
#   # mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
#   #        FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
#   # entry_age = Age - (Years - YearStart),    #we don't need entry_age as mort rates for Montana PERS don't change over time
#   # YOS = Age - entry_age                     #we don't need YOS as the combined rates will be used (no need to differentiate between active vs. retiree mortality rates)
#   # ) %>% 
#   group_by(Age) %>%
#   #MPcumprod is the cumulative product of (1 - BB rates), starting from 2001. 
#   #to cumsum (1 - BB rates) over 2001+ & then multiply by mort base rates (RP-2000)
#   mutate(MPcumprod_male = cumprod(1 - Male_BB),
#          MPcumprod_female = cumprod(1 - Female_BB),
#          mort_male = Male_Comb_Health_Mort * MPcumprod_male,
#          mort_female = Female_Comb_Health_Mort * MPcumprod_female) %>% 
#   filter(Years == 2020) %>%  #use 2020 rates only, as specified in the 2021 val report
#   ungroup() %>% 
#   mutate(mort_male = lag(mort_male),   #male mortality is set back 1 year
#          mort_male = replace(mort_male, Age == 120, 1),   #restore end value (at age 120) back to 1 to cancel the lag effect
#          mort = (mort_male + mort_female)/2)
# 
# #filter out the necessary variables
# MortalityTable <- MortalityTable %>% 
#   filter(Age >= 20) %>% 
#   select(Age, mort_male, mort_female, mort)

# MortalityTable <- readRDS(MortalityTable, "mortality_table.rds")
MortalityTable <- readRDS("data/benefits/mortality_table.rds")

# saveRDS(MortalityTable, "mortality_table.rds")

#----Separation Rates----
# SeparationRates <- expand_grid(Age, YOS, HireType) %>% 
#   mutate(entry_age = Age - YOS) %>% 
#   filter(entry_age %in% SalaryEntry$entry_age) %>% 
#   arrange(HireType, entry_age, Age) %>% 
#   left_join(TerminationRate, by = "YOS") %>%
#   left_join(RetirementRates, by = "Age") 
# 
# #If you're retirement eligible, use the retirement rates, or else use the regular termination rates
# SeparationRates <- SeparationRates %>% 
#   mutate(retirement_check = IsRetirementEligible(Age,YOS,HireType),
#          SepRate = ifelse(retirement_check == T, 
#                           ifelse(YOS >= 30 | (Age >= 60 & YOS >= 25), Regular_RetRate, Less30_RetRate),
#                           ifelse(Age >= 50 & YOS >= 5, 0, Term)))  %>%
#   group_by(HireType, entry_age) %>% 
#   mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
#          SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
#   ungroup()
# 
# #Filter out unecessary values
# SeparationRates <- SeparationRates %>% select(Age, YOS, HireType, RemainingProb, SepProb)

# saveRDS(SeparationRates, "backend/separation_rates.rds")
SeparationRates <- readRDS("data/benefits/seperation_rates.rds")

#----Salary Data----
#Create a long-form table of Age and YOS and merge with salary data
# SalaryData <- expand_grid(Age, YOS, HireType) %>% 
#   mutate(entry_age = Age - YOS) %>%    #Add entry age
#   filter(entry_age %in% SalaryEntry$entry_age) %>% 
#   arrange(HireType, entry_age, Age) %>% 
#   left_join(SalaryEntry, by = "entry_age") %>% 
#   left_join(SalaryGrowth, by = c("YOS"))

# saveRDS(SalaryData, "backend/salary_data.rds")
SalaryData <- readRDS("data/benefits/salary_data.rds")

#----Normal Cost & Benefit Accrual Function----
benefit_cal <- function(
  output,              
  EE_tier = "New Hire",            #Either "New Hire" or "Legacy" (not used in the Shiny dashboard)
  DB_ARR,
  DB_EE, 
  DC_EE,
  DC_ER,
  DB_mult10,      #Benefit multiplier for < 10 YOS (new hires only) 
  DB_mult10_30,   #Benefit multiplier for 10-30 YOS (new hires only)
  DB_mult30,      #Benefit multiplier for 30 or more YOS (new hires only)
  DB_COLA,        #COLA for new hires only
  ea,            #Only use the numbers from SalaryEntry$entry_age
  DCreturn) {
  
  
  SalaryData <- SalaryData %>% 
    group_by(HireType, entry_age) %>% 
    mutate(Salary = Starting_Salary*cumprod(1+lag(Total_Pay_Increase,default = 0)),
           FAS_years = ifelse(HireType == "New Hire", FinAvgSalaryYears, FinAvgSalaryYears_Legacy),
           #Salary = pmin(Salary_gross, salary_cap),
           # IRSSalaryCap = pmin(Salary,IRSCompLimit),
           FinalAvgSalary = rollmean(lag(Salary), k = FAS_years, fill = NA, align = "right"),
           EEContrib = DB_EE*Salary,
           DBEEBalance = cumFV(Interest, EEContrib),
           CumulativeWage = cumFV(DB_ARR, Salary)) %>% 
    ungroup()
  #
  ################################################################################################################################################################
  #
  #Annuity Factor and Reduced Factor
  AnnFactorData <- expand_grid(Age, HireType) %>%      #don't need entry_age as Montana PERS mort rates are fixed (no generational improvement)
    left_join(MortalityTable %>% select(Age, mort), by = "Age") %>% 
    arrange(HireType, Age) %>% 
    group_by(HireType) %>%
    mutate(COLA = ifelse(HireType == "Legacy", COLA_legacy, DB_COLA),
           surv = cumprod(1 - lag(mort, default = 0)),
           surv_DR = surv/(1+DB_ARR)^(Age - Age[1]),
           surv_DR_COLA = surv_DR * (1+COLA)^(Age - Age[1]),
           AF = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA)
  # AFNormalRetAge = ifelse(HireType == 'New Hire', AF[Age==NormalRetAgeI],AF[Age==NormalRetAgeI_Legacy]),
  # SurvProbNormalRetAge = ifelse(HireType == 'New Hire', surv_DR[Age==NormalRetAgeI],surv_DR[Age==NormalRetAgeI_Legacy])) %>% 
  
  
  # AnnFactorData <- AnnuityF(data = MortalityTable,
  #                           ColaType = "Compound")
  
  #Reduced Factor
  ReducedFactor <- expand_grid(Age, YOS, HireType) %>% 
    arrange(HireType, YOS) %>% 
    mutate(RetType = RetirementType(Age, YOS, HireType),
           norm_retire = ifelse(RetType == "Regular", 1, 0)) %>% 
    left_join(AnnFactorData %>% select(Age, HireType, surv_DR, AF), by = c("Age", "HireType")) %>% 
    group_by(HireType, YOS) %>% 
    mutate(AgeNormRet = max(Age) - sum(norm_retire) + 1,    #This is the earliest age of normal retirement for a given YOS
           RF = ifelse(RetType == "Early", AF[Age == AgeNormRet] * surv_DR[Age == AgeNormRet] / surv_DR / AF,   #Benefit for early retirement  = actuarially reduced amount of the normal benefit the employee would receive at the earliest age of normal retirement. 
                       ifelse(RetType == "None", 0, 1))) %>% 
    rename(RetirementAge = Age) %>% 
    ungroup()
  
  #Check wide format of the Reduced Factor table
  # ReducedFactor_wide <- ReducedFactor %>% 
  #   filter(YOS >= 5,
  #          RetirementAge >= 45,
  #          HireType == "Legacy") %>% 
  #   select(YOS, RetirementAge, RF) %>% 
  #   pivot_wider(names_from = RetirementAge,
  #               values_from = RF)
  
  #
  ################################################################################################################################################################
  #Benefits and Present Value
  
  BenefitsTable <- expand_grid(Age, YOS, RetirementAge, HireType) %>% 
    mutate(entry_age = Age - YOS) %>% 
    filter(entry_age %in% SalaryEntry$entry_age) %>% 
    arrange(HireType, entry_age, Age, RetirementAge) %>% 
    left_join(SalaryData, by = c("Age", "YOS", "HireType","entry_age")) %>% 
    left_join(ReducedFactor %>% select(RetirementAge, YOS, HireType, RF), by = c("RetirementAge", "YOS", "HireType")) %>%
    left_join(AnnFactorData %>% select(Age, HireType, surv_DR, AF), by = c("RetirementAge" = "Age", "HireType")) %>%
    
    #Rename surv_DR and AF to make clear that these variables are at retirement
    rename(surv_DR_ret = surv_DR, AF_Ret = AF) %>% 
    
    #Rejoin the table to get the surv_DR for the termination age
    left_join(AnnFactorData %>% select(Age, HireType, surv_DR), by = c("Age", "HireType")) %>% 
    mutate(
      BenMult = ifelse(HireType == "Legacy",
                       case_when(
                         YOS < 25 ~ BenMult2,
                         TRUE ~ BenMult3
                       ),
                       case_when(
                         YOS < 10 ~ DB_mult10,
                         YOS >= 10 & YOS < 30 ~ DB_mult10_30,
                         TRUE ~ DB_mult30
                       )),
      ReducedFactMult = RF * BenMult, 
      AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
      PensionBenefit = ReducedFactMult * FinalAvgSalary * YOS,
      PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))
  
  #)
  
  #For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
  OptimumBenefit <- BenefitsTable %>% 
    group_by(HireType, entry_age, Age) %>% 
    summarise(MaxBenefit = max(PresentValue)) %>%
    mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
    ungroup()
  
  ####### Benefit Accrual & Normal Cost #######
  #### Real Pension Wealth = Pension Wealth adjusted for inflation
  #### Actuarial PV of Pension Wealth = Pension Wealth 
  #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
  #####################################
  SalaryData <- SalaryData %>% 
    left_join(OptimumBenefit, by = c("HireType", "Age", "entry_age")) %>% 
    left_join(SeparationRates, by = c("HireType", "Age", "YOS")) %>%
    mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
           RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
           PVPenWealth = PenWealth/(1 + DB_ARR)^YOS * SepProb,
           PVCumWage = CumulativeWage/(1 + DB_ARR)^YOS * SepProb)
  
  
  #Calculate normal cost rate for each entry age in each hire type
  NormalCost <- SalaryData %>% 
    group_by(HireType, entry_age) %>% 
    summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
    ungroup()
  
  #View(NormalCost)
  
  #Calculate the aggregate normal cost for a tier (legacy or new hire)
  NC_aggregate <- NormalCost %>% 
    left_join(SalaryEntry %>% select(entry_age, Starting_Salary, count_start), by = "entry_age") %>% 
    group_by(HireType) %>% 
    summarise(NC = sum(normal_cost * Starting_Salary * count_start)/sum(Starting_Salary * count_start)) %>% 
    filter(HireType == EE_tier) %>% 
    pull(NC)
  
  # test <- SalaryData %>% 
  #   filter(entry_age == 22)
  # write.csv(test, "test.csv")
  
  ################################
  
  ####### DC Account Balance 
  SalaryData2 <- SalaryData %>% 
    filter(entry_age == ea, HireType == EE_tier) %>% 
    select(Age, YOS, entry_age, HireType, Starting_Salary, Total_Pay_Increase, Salary, RemainingProb) %>% 
    mutate(DC_EEContrib = Salary * DC_EE,
           DC_ERContrib = Salary * DC_ER,
           DC_Contrib = DC_EEContrib + DC_ERContrib,
           DC_balance = cumFV(DCreturn, DC_Contrib),
           RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
    left_join(SalaryData %>% select(Age, YOS, entry_age, HireType, RealPenWealth), by = c("Age", "YOS", "entry_age", "HireType")) %>% 
    mutate(RealHybridWealth = RealDC_balance + RealPenWealth) %>% 
    filter(Age <= 70)
  
  if (output == "NC") {
    return(NC_aggregate)
  } else if (output == "attrition") {
    return(SalaryData2 %>% 
             select(Age, RemainingProb))
  } else if (output == "DB") {
    return(SalaryData2 %>% 
             select(Age, RealPenWealth, RemainingProb))
  } else if (output == "DC") {
    return(SalaryData2 %>% 
             select(Age, RealDC_balance, RemainingProb))
  } else {
    return(SalaryData2 %>% 
             select(Age, RealHybridWealth, RemainingProb))
  }
}


# benefit_cal(output = "DB",
#             EE_tier = Type,            #Either "New Hire" or "Legacy" (not used in the Shiny dashboard)
#             DB_ARR = ARR,
#             DB_EE = DB_EE_cont,
#             DC_EE = DC_EE_cont,
#             DC_ER = DC_ER_cont,
#             DB_mult10 = BenMult1,      #Benefit multiplier for < 10 YOS (new hires only)
#             DB_mult10_30 = BenMult2,   #Benefit multiplier for 10-30 YOS (new hires only)
#             DB_mult30 = BenMult3,      #Benefit multiplier for 30 or more YOS (new hires only)
#             DB_COLA = COLA_new,        #COLA for new hires only
#             ea = HiringAge,            #Only use the numbers from SalaryEntry$entry_age
#             DCreturn = DC_return) |> as.data.frame()
