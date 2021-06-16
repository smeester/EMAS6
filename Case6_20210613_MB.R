#clear code and load libraries
cat("\014")
rm(list=ls())
gc()

# install.packages("data.table")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("bit64")
# install.packages("ggplot2")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("xgboost")
# install.packages("XGBoostLSS")
# install.packages("fastDummies")
# install.packages("AUC")
# install.packages("caret")
# install.packages("e1071")
# install.packages("randomForest")
# install.packages("DiagrammeR")
# install.packages("openxlsx")
# install.packages("vctrs")
# install.packages("zoo") 
# install.packages("MASS") 
# install.packages("betareg") 
# install.packages("lmtest")
# install.packages("mice")
# install.packages("rcompanion")
# install.packages("memisc")
# install.packages("partykit")
# install.packages("stargazer") 


library(data.table)
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)
library(bit64)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xgboost)
library(fastDummies)
library(AUC)
library(caret)
library(e1071)
library(randomForest)
library(DiagrammeR)
library(openxlsx)
library(vctrs)
library(zoo)
library(MASS)
library(betareg)
library(lmtest)
library(mice)
library(rcompanion)
library(memisc)
library(partykit)
library(stargazer)

'%nin%' = Negate('%in%')
memory.limit(size=50800)

#read data
data <- fread("C:/Users/smeester/Documents/AGAI/EMAS10/Case 6/Data aanlevering/emas_dataset.csv",
              select=c("POOL_ID","LOAN_ID","ACT_PERIOD","CHANNEL","SELLER","SERVICER","MASTER_SERVICER",
                       "ORIG_RATE","CURR_RATE","ORIG_UPB","ISSUANCE_UPB","CURRENT_UPB","ORIG_TERM","ORIG_DATE",
                       "LOAN_AGE","MATR_DT","OLTV","DTI","CSCORE_B","CSCORE_C","FIRST_FLAG","PURPOSE","OCC_STAT",
                       "STATE","MSA","ZIP",
                       "MI_PCT","PRODUCT","DLQ_STATUS","PMT_HISTORY","MOD_FLAG",
                       "FORECLOSURE_DATE","DISPOSITION_DATE",
                       "FORECLOSURE_COSTS","PROPERTY_PRESERVATION_AND_REPAIR_COSTS","ASSET_RECOVERY_COSTS",
                       "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS","ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY",
                       "NET_SALES_PROCEEDS","CREDIT_ENHANCEMENT_PROCEEDS","REPURCHASES_MAKE_WHOLE_PROCEEDS",
                       "OTHER_FORECLOSURE_PROCEEDS","NON_INTEREST_BEARING_UPB","ISSUE_SCOREB","ISSUE_SCOREC",
                       "CURR_SCOREB","CURR_SCOREC","MI_TYPE","DELINQUENT_ACCRUED_INTEREST",
                       "PROPERTY_INSPECTION_WAIVER_INDICATOR","HIGH_BALANCE_LOAN_INDICATOR",
                       "ARM_5_YR_INDICATOR","ARM_PRODUCT_TYPE","ARM_INDEX","ARM_CAP_STRUCTURE",
                       "MARGIN","BALLOON_INDICATOR","PLAN_NUMBER","FORBEARANCE_INDICATOR",
                       "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR","DEAL_NAME","RE_PROCS_FLAG"))


hpi <- read.xlsx("C:/Users/smeester/Projects/EMAS6/HPI_master_USA.xlsx")
gdp <- read.xlsx("C:/Users/barettam/Documents/EMAS/Case 6/Data/GDP.xlsx")

# foreclosure
sum(!is.na(data$FORECLOSURE_DATE))




#verwijderde kolommen: "FIRST_PAY","REM_MONTHS","ADJ_REM_MONTHS","OCLTV","NUM_BO","FIRST_FLAG","PROP",
#"NO_UNITS","OCC_STAT","PPMT_FLG","IO","FIRST_PAY_IO","MNTHS_TO_AMTZ_IO","MOD_FLAG","MI_CANCEL_FLAG",
#"Zero_Bal_Code","ZB_DTE","LAST_UPB","RPRCH_DTE","CURR_SCHD_PRNCPL","TOT_SCHD_PRNCPL","UNSCHD_PRNCPL_CURR",
#"LAST_PAID_INSTALLMENT_DATE","PRINCIPAL_FORGIVENESS_AMOUNT","ORIGINAL_LIST_START_DATE","ORIGINAL_LIST_PRICE",
#"CURRENT_LIST_START_DATE","CURRENT_LIST_PRICE","SERV_IND","CURRENT_PERIOD_MODIFICATION_LOSS_AMOUNT",
#"CUMULATIVE_MODIFICATION_LOSS_AMOUNT","CURRENT_PERIOD_CREDIT_EVENT_NET_GAIN_OR_LOSS",
#"CUMULATIVE_CREDIT_EVENT_NET_GAIN_OR_LOSS","HOMEREADY_PROGRAM_INDICATOR",
#"FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT","RELOCATION_MORTGAGE_INDICATOR","ZERO_BALANCE_CODE_CHANGE_DATE",
#"LOAN_HOLDBACK_INDICATOR","LOAN_HOLDBACK_EFFECTIVE_DATE","MONTHS_UNTIL_FIRST_PAYMENT_RESET",
#"MONTHS_BETWEEN_SUBSEQUENT_PAYMENT_RESET","INTEREST_RATE_CHANGE_DATE","PAYMENT_CHANGE_DATE",
#"INITIAL_INTEREST_RATE_CAP","PERIODIC_INTEREST_RATE_CAP","LIFETIME_INTEREST_RATE_CAP","ADR_TYPE","ADR_COUNT",
#"ADR_UPB"
#convert date format from "m(m)yyyy" to "yyyy-mm"


data$ACT_PERIOD <- paste(with(data, data$ACT_PERIOD %% 10000),
                         ifelse(as.numeric(substr(data$ACT_PERIOD, 1, floor(log10(data$ACT_PERIOD)) - 3)) < 10,
                                paste(0,substr(data$ACT_PERIOD, 1, floor(log10(data$ACT_PERIOD)) - 3),sep=""),
                                substr(data$ACT_PERIOD, 1, floor(log10(data$ACT_PERIOD)) - 3)), sep="-")

data$ORIG_DATE <- paste(with(data, data$ORIG_DATE %% 10000),
                        ifelse(as.numeric(substr(data$ORIG_DATE, 1, floor(log10(data$ORIG_DATE)) - 3)) < 10,
                               paste(0,substr(data$ORIG_DATE, 1, floor(log10(data$ORIG_DATE)) - 3),sep=""),
                               substr(data$ORIG_DATE, 1, floor(log10(data$ORIG_DATE)) - 3)), sep="-")

# Prepare COSTS
data$FORECLOSURE_COSTS [is.na(data$FORECLOSURE_COSTS)]=0
data$PROPERTY_PRESERVATION_AND_REPAIR_COSTS [is.na(data$PROPERTY_PRESERVATION_AND_REPAIR_COSTS)]=0
data$ASSET_RECOVERY_COSTS [is.na(data$ASSET_RECOVERY_COSTS)]=0
data$MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS [is.na(data$MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS)]=0
data$ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY [is.na(data$ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY)]=0

data$COSTS <- data %>% dplyr::select(FORECLOSURE_COSTS,PROPERTY_PRESERVATION_AND_REPAIR_COSTS,ASSET_RECOVERY_COSTS,
              MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS,ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY) %>% transmute(x=rowSums(.))

data_costs <- data %>% dplyr::select(LOAN_ID,COSTS)
data_costs <-data_costs %>% group_by(LOAN_ID) %>% summarise_all(sum)




#LGD == Loss Given Default  calculations

data_lgd <- data %>% dplyr::select(LOAN_ID, ACT_PERIOD, ORIG_RATE, CURRENT_UPB, CURR_RATE, ORIG_UPB, ORIG_TERM, ORIG_DATE,
                            DLQ_STATUS, CHANNEL, OLTV, DTI, PURPOSE, CSCORE_B, CSCORE_C, LOAN_AGE, FIRST_FLAG, STATE,
                            OCC_STAT,MOD_FLAG)



data_lgd$ANNUITY <- with(data_lgd, (1-(1/(1+ORIG_RATE/1200)^ORIG_TERM))/(ORIG_RATE/1200))
data_lgd$MONTHLY_PAYMENT <- with(data_lgd, ORIG_UPB/ANNUITY)


#filter XX
data_lgd <- data_lgd %>% filter(DLQ_STATUS != "XX")

#default calculation
data_lgd$DLQ_STATUS <- as.numeric(data_lgd$DLQ_STATUS)
data_lgd$DLQ_TOTAL <- data_lgd$MONTHLY_PAYMENT * data_lgd$DLQ_STATUS 
data_lgd$DEFAULT <- ifelse(data_lgd$DLQ_TOTAL > 0.01 * data_lgd$CURRENT_UPB & data_lgd$DLQ_STATUS > 2, 1, 0)

# sum(data_lgd$DEFAULT)



# Prepare variables for model choice
# Literature == Do(2018), Predicting Loss severities
# LOAN CHARACTERISTICS: CLTV (current loan to value), Loan size-Original UPB,Loan age,current interest rate CURR_RATE,OLTV
# BORROWER CHARACTERISSTICS:  borrowers credit quality (CSCORE_B, CSCORE_C), first flag, DTI (debt to income)
# Economic conditions: FCRate (average local loan foreclosure), GDP-rate, hpi

data_lgd$ACT_PERIOD<- paste(data_lgd$ACT_PERIOD, "01", sep="-")
data_lgd$ACT_PERIOD <- as.Date(data_lgd$ACT_PERIOD, format =  "%Y-%m-%d")

data_lgd$ORIG_DATE<- paste(data_lgd$ORIG_DATE, "01", sep="-")
data_lgd$ORIG_DATE <- as.Date(data_lgd$ORIG_DATE, format =  "%Y-%m-%d")

data_lgd$VALUE <- with(data_lgd, ORIG_UPB/(OLTV/100))


#hpi
hpi$PERIOD<- paste(hpi$PERIOD, "01", sep="-")
hpi$PERIOD <- as.Date(hpi$PERIOD, format =  "%Y-%m-%d")

data_lgd <- merge(data_lgd, hpi, by.x='ORIG_DATE', by.y='PERIOD')
data_lgd <- merge(data_lgd, hpi, by.x='ACT_PERIOD', by.y='PERIOD')

#make plot of house price index
ggplot(data=hpi, aes(x=PERIOD, y=INDEX)) +
  geom_line() +
  ggtitle("House price index of US") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date") +
  ylab("HPI")


# GDP
gdp$PERIOD<- paste(gdp$PERIOD, "01", sep="-")
gdp$PERIOD <- as.Date(gdp$PERIOD, format =  "%Y-%m-%d")
data_lgd <- merge(data_lgd, gdp, by.x='ACT_PERIOD', by.y='PERIOD')

ggplot(data=gdp , aes(x=PERIOD, y=GDP)) +
 geom_line() +
 ggtitle("GDP rate US") +
 theme(plot.title = element_text(hjust = 0.5)) +
 xlab("Date") +
 ylab("GDP rate")


# data_lgd$VALUE_INDEX <- with(data_lgd, INDEX.y/INDEX.x * VALUE) 

data_lgd$VALUE_INDEX <- with(data_lgd, INDEX.y/INDEX.x * VALUE) 
data_lgd <- left_join(data_lgd, data_costs, by.x='LOAN_ID', by.y='LOAN_ID')
data_lgd$ZERO <- 0


# CLTv current loan to value as extra explanatory variable 
data_lgd$CLT <-with(data_lgd, CURRENT_UPB/(VALUE_INDEX))


# FCR rate
unique_loans_group <- data %>% distinct(LOAN_ID, .keep_all = TRUE) 
unique_loans_group$ACT_PERIOD<- paste(unique_loans_group$ACT_PERIOD, "01", sep="-")
unique_loans_group$ACT_PERIOD <- as.Date(unique_loans_group$ACT_PERIOD, format =  "%Y-%m-%d")
unique_loans_group$year <-year(unique_loans_group$ACT_PERIOD)
numberLoanRegion <-unique_loans_group  %>% group_by(STATE,year) %>% transmute(Count=n())
data_lgd$year <-year(data_lgd$ACT_PERIOD)
numberLoanRegionDefault <-data_lgd %>% group_by(STATE,year,DEFAULT) %>% transmute(Count=n())
numberLoanRegionDefault <- numberLoanRegionDefault %>% filter(DEFAULT == 1)



FCRate <- merge(numberLoanRegion,numberLoanRegionDefault, all=TRUE)
head(FCRate)

FCRate [is.na(FCRate)]=0
FCRate$FCRate<-FCRate$DEFAULT/FCRate$Count
sum(FCRate$FCRate)

# FCRate <- FCRate %>% filter(DEFAULT == 1)
FCRate <- FCRate %>% dplyr::select(year,STATE,FCRate)
FCRate <- FCRate %>% distinct(year, STATE, .keep_all = TRUE)

data_lgd <- merge(data_lgd,FCRate, by.x=c('year','STATE'), by.y=c('year','STATE'))

# write.xlsx(FCRate ,"C:/Users/barettam/Documents/EMAS/Case 6/Data/Output/FCRate.xlsx")



###########################################################################
# Prepare data for prediction (APART OM ALLE 110001 REGELS TE BEHOUDEN)
data_lgd_predict <- data_lgd %>% distinct(LOAN_ID, .keep_all = TRUE)

data_lgd_predict$quarter <- quarters(as.Date(data_lgd_predict$ACT_PERIOD))
data_lgd_predict$MOD_FLAG <- ifelse(data_lgd_predict$MOD_FLAG== 'Y', 1, 0)

# samenvoegen jaren
klasse=data_lgd_predict$year
klassen=c('2010-2012','2013-2017','2018-2019','2020')
index=(klasse=='2010' | klasse=='2011' | klasse=='2012')*1 + (klasse=='2013' | klasse=='2014' | klasse=='2015'|
      klasse=='2016' | klasse=='2017')*2 + (klasse=='2018'| klasse=='2019')*3 + (klasse=='2020')*4
yearklasse=as.factor(klassen[index])
data_lgd_predict$yearklasse=yearklasse

# samenvoegen FCRate
FCRate <- as.data.frame(data_lgd_predict$FCRate)
FCRate <- as.numeric(FCRate[[1]])

# quantile(FCRate,0.50)

klasseFCRate=data_lgd_predict$FCRate
klassen=c('0-0.5','0.5-0.75','0.75-0.9','0.9-1')
indexF=(klasseFCRate <= quantile(FCRate,0.50))*1 + (klasseFCRate > quantile(FCRate,0.50) | klasseFCRate <= quantile(FCRate,0.75))*2 + 
  (klasseFCRate > quantile(FCRate,0.75) | klasseFCRate <= quantile(FCRate,0.90))*3 + (klasseFCRate > quantile(FCRate,0.90))*4 
FCRateklasse=indexF
data_lgd_predict$FCRateklasse=FCRateklasse



###########################################################################

# Prepare data for model
data_lgd <- data_lgd %>% filter(DEFAULT == 1)
data_lgd <- data_lgd %>% distinct(LOAN_ID, .keep_all = TRUE)


# 1 year LIBOR US, 01-01-2021
# https://www.global-rates.com/en/interest-rates/libor/american-dollar/usd-libor-interest-rate-12-months.aspx
rfr <- 0.24688
discount <- (rfr + 5)/1200 #5 = discount rate, 1200 = monthly

# PV (loss) == monthly payments*annuiteit = rest schuld
data_lgd$LOSS <- with(data_lgd, MONTHLY_PAYMENT * ((1-1/(1+discount)^(ORIG_TERM-LOAN_AGE))/discount))

# value=(1-haircut HC)*value index;  value index=index now/index origin
data_lgd$GAIN <- with(data_lgd, 0.8 * VALUE_INDEX)

data_lgd$NET <- with(data_lgd, LOSS + COSTS - GAIN)

sum(data_lgd$NET)


data_lgd[, "LGD"] <- apply(data_lgd[, c(34,36)], 1, max)
data_lgd$LGD <- with(data_lgd, LGD/CURRENT_UPB)


#make histogram of empirical LGD
hist_lgd <- hist(data_lgd$LGD,
                 breaks=100,
                 main="Histogram of empirical loss rate",
                 xlab="Loss rate",
                 ylab="Frequency",
                 xlim=c(0,2))



####### FIT THE BETA REGRESSION MODEL#############

#Transform Inf to NA and Remove NA
data_lgd_model <-data_lgd %>% dplyr::select( "year","STATE", "ACT_PERIOD","ORIG_DATE", "LOAN_ID", "ORIG_RATE", 
                                       "CURRENT_UPB","CURR_RATE","ORIG_UPB","ORIG_TERM",
                                       "OLTV","DTI", "PURPOSE", "CSCORE_B", "LOAN_AGE",
                                       "FIRST_FLAG", "MOD_FLAG","MONTHLY_PAYMENT",  
                                       "DEFAULT", "VALUE", "INDEX.x", "INDEX.y","GDP","VALUE_INDEX",
                                       "COSTS","ZERO","CLT","FCRate","LOSS","GAIN", 
                                       "NET","LGD") 


data_lgd_model[sapply(data_lgd_model, is.infinite)] <- NA
data_lgd_model <- na.omit(data_lgd_model)


# data_lgd$ACT_PERIOD <- gsub("-","",data_lgd$ACT_PERIOD)
# data_lgd$ACT_PERIOD <- as.yearqtr(data_lgd$ACT_PERIOD, "%Y%m")

data_lgd_model$quarter <- quarters(as.Date(data_lgd_model$ACT_PERIOD))

# samenvoegen jaren
klasse=data_lgd_model$year
klassen=c('2010-2012','2013-2017','2018-2019','2020')
index=(klasse=='2010' | klasse=='2011' | klasse=='2012')*1 + (klasse=='2013' | klasse=='2014' | klasse=='2015'|
  klasse=='2016' | klasse=='2017')*2 + (klasse=='2018'| klasse=='2019')*3 + (klasse=='2020')*4
yearklasse=as.factor(klassen[index])
data_lgd_model$yearklasse=yearklasse

# samenvoegen FCRate
FCRate <- as.data.frame(data_lgd_model$FCRate)
FCRate <- as.numeric(FCRate[[1]])

klasseFCRate=data_lgd_model$FCRate
klassen=c('0-0.5','0.5-0.75','0.75-0.9','0.9-1')
indexF=(klasseFCRate <= quantile(FCRate,0.50))*1 + (klasseFCRate > quantile(FCRate,0.50) | klasseFCRate <= quantile(FCRate,0.75))*2 + 
  (klasseFCRate > quantile(FCRate,0.75) | klasseFCRate <= quantile(FCRate,0.90))*3 + (klasseFCRate > quantile(FCRate,0.90))*4 
FCRateklasse=indexF
data_lgd_model$FCRateklasse=FCRateklasse

# mod_flag as dummy variable
data_lgd_model$MOD_FLAG <- ifelse(data_lgd_model$MOD_FLAG == 'Y', 1, 0)
mean(data_lgd_model$MOD_FLAG)


# Y has to be data frame and numerical
LGD <- as.data.frame(data_lgd_model$LGD)
LGD <- as.numeric(LGD[[1]])

# Check Beta distribution
# Mean = alpha/(alpha+beta) 
mu_y=mean(LGD)

# Max=(alpha-1)/(alpha+beta-2)
max(LGD)

# Variance = alpha*beta/(alpha+beta)^2*(alpha+beta+1)
var_y=var(LGD)

estBetaParams <-function(mu,var)
{ 
  alpha <- (1-mu)/(var-1/mu)*mu^2
  beta <- alpha*(1/mu-1)
  return(params=list(r=alpha, s=beta))
}


estBetaParams(mu_y,var_y)

##################################################################################################################

# MODEL
# transform y == as betareg currently only works strictly for data inside the (0,1) interval
# The class of beta regression models, as introduced by Ferrari and Cribari-Neto (2004), 
# is useful for modeling continuous variables y that assume values in the open standard unit
# interval (0,1). {...} Furthermore, if y also assumes the extremes 0 and 1, a useful
# transformation in practice is (y*(n-1)+0.5)/n, where n is a sample size (Smithson and Verkuilen 2006).

# Prepare model
# LGD <- LGD +0.0001
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

Y <- y.transf.betareg(LGD)



# model 1
model_beta1 <- betareg(formula=Y ~ year + ORIG_RATE + PURPOSE+ GDP +  CLT + FCRateklasse + log(ORIG_UPB) + 
                         CSCORE_B+ MOD_FLAG,link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(model_beta1,type="deviance")
# summary(model_beta1, type = "pearson")


# model 2
model_beta2 <- betareg(formula=Y ~ year + ORIG_RATE + PURPOSE+ GDP +  CLT + FCRateklasse + log(ORIG_UPB) + 
                         MOD_FLAG,link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(model_beta2,type="deviance")
# summary(model_beta2, type = "pearson")


# model 3
model_beta3 <- betareg(formula=Y ~ year + ORIG_RATE + GDP +  CLT + FCRateklasse + log(ORIG_UPB) + 
                         MOD_FLAG,link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(model_beta3,type="deviance")


# model 4
model_beta4<-betareg(formula=Y ~ year + ORIG_RATE + GDP +  CLT + FCRateklasse + 
                       MOD_FLAG,link = "logit",
                     data=as.data.frame(data_lgd_model))

summary(model_beta4,type="deviance")
# summary(model_beta4, type = "pearson")


# Model 5
beta.model5 <- betareg(formula=Y ~ ORIG_RATE + CLT + GDP + FCRateklasse + PURPOSE, link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(beta.model5)
# summary(beta.model5, type = "pearson")


# Model 6
beta.model6<- betareg(formula=Y ~ ORIG_RATE + CLT , link = "logit",
                      data=as.data.frame(data_lgd_model))

summary(beta.model6)
# summary(beta.model6, type = "pearson")



# Model 7
beta.model7 <- betareg(formula=Y ~ yearklasse + ORIG_RATE + CLT +FCRateklasse + PURPOSE + MOD_FLAG, link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(beta.model7)
# summary(beta.model7, type = "pearson")


# Model 8
beta.model8 <- betareg(formula=Y ~ yearklasse + ORIG_RATE + CLT + PURPOSE , link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(beta.model8)
# summary(beta.model8, type = "pearson")
AIC(beta.model8)
confint(beta.model8)
coeftest(beta.model8)
waldtest(beta.model8)


# Model 9
beta.model9 <- betareg(formula=Y ~ yearklasse + ORIG_RATE + CLT + GDP, link = "logit",
                       data=as.data.frame(data_lgd_model))

summary(beta.model9)


# Model 10
beta.model10 <- betareg(formula=Y ~ yearklasse + ORIG_RATE + CLT + FCRateklasse, link = "logit",
                        data=as.data.frame(data_lgd_model))

summary(beta.model10)
AIC(beta.model10)
confint(beta.model10)
coeftest(beta.model10)
waldtest(beta.model10)

# Model 11
beta.model11 <- betareg(formula=Y ~ yearklasse + ORIG_RATE + CLT, link = "logit",
                        data=as.data.frame(data_lgd_model))

summary(beta.model11)

# Model GLM
beta.modelGLM <- glm(formula=Y ~yearklasse + ORIG_RATE + CLT +FCRateklasse + MOD_FLAG,  
                     data=as.data.frame(data_lgd_model))

summary(beta.modelGLM)
# summary(beta.modelGLM, type = "pearson")



########################################################################################################

# Prediction for all unique ID
data_lgd_predict <- data_lgd_predict %>% dplyr::select(ACT_PERIOD,LOAN_ID,ORIG_RATE,CURRENT_UPB,CURR_RATE,ORIG_UPB,OLTV,
                                                       MONTHLY_PAYMENT,DLQ_TOTAL,DEFAULT,VALUE,INDEX.x,INDEX.y,
                                                       GDP,VALUE_INDEX,CLT,PURPOSE, MOD_FLAG,FCRate,FCRateklasse,year,yearklasse)



probabilities <- beta.model11 %>% predict(data_lgd_predict, type = "response")
n.obs <- sum(!is.na(probabilities))
probabilities <-((probabilities * n.obs)-0.5)/ n.obs
data_lgd_predict <- cbind(data_lgd_predict,probabilities)


write.xlsx(data_lgd_predict,"C:/Users/barettam/Documents/EMAS/Case 6/Data/Output/Prediction.xlsx")

########################################################################################################
probabilities_lgd <- beta.model8 %>% predict(data_lgd_model, type = "response")
probabilities_lgd <-((probabilities_lgd * n.obs)-0.5)/ n.obs
data_lgd_predictlgd <- cbind(data_lgd_model,probabilities_lgd)

write.xlsx(data_lgd_predictlgd,"C:/Users/barettam/Documents/EMAS/Case 6/Data/Output/PredictionLd.xlsx")


# Predicted Loss rate per yearclass
ggplot(data=data_lgd_predict , aes(x=yearklasse, y=probabilities)) +
  geom_line() +
  ggtitle("Loss rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date") +
  ylab("Loss rate")

# histogram Predicted Loss rate
hist_predicted_lgd <- hist(data_lgd_predict$probabilities,
                 breaks=10,
                 main="Histogram of predicted loss rate",
                 xlab="Loss rate",
                 ylab="Frequency")



# Text output
stargazer(data_lgd_model,
          type = "text", title="Descriptive statistics", digits=1, 
          out="C:/Users/barettam/Documents/EMAS/Case 6/Data/Output/Descriptive statistics.txt") 


options.results ="asis" 
stargazer(beta.model8, type = "text", title="Descriptive statistics model 8", digits=1, out="beta.model8.txt") 


###################################################################################################################

