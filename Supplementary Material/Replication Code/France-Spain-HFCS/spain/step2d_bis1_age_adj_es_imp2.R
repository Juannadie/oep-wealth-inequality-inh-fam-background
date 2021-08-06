#THIS V2 INDICATES THAT WE ARE USING THE NEW CONFIGURATION - YEARS 35 - 85 AND THEN AGE CONTROL

#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

library(Hmisc)
library(reldist)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(snakecase)

options ("scipen"=100, "digits"=10)


#### WE LOAD THE AGGREGATION FROM THE PREVIOUS FILE,  ###############


dataesh <- readRDS(file = "datasets/spain/v2-HFCS14_Spain_after_step_2c-no-pension-ad-eq_bis1_imp2.rds")




#THEN WE CONVERT THE WEALTH DATA INTO NET TERMS OF AGE AND GENDER

dataesh$agedif <- dataesh$age - 65
dataesh$agedif2 <- (dataesh$agedif)^2
dataesh$agedif3 <- (dataesh$agedif)^3
dataesh$agedif4 <- (dataesh$agedif)^4


dataesh$femaledummy <- 0
dataesh$femaledummy[dataesh$sex == "2"] <- 1

#We convert sex to factor for the graph
dataesh$sexfactor <- as.factor (dataesh$sex)
levels(dataesh$sexfactor) <- c("Male","Female")


dataesh$femaleagedif <- dataesh$agedif * dataesh$femaledummy
dataesh$femaleagedif2 <- (dataesh$femaleagedif)^2
dataesh$femaleagedif3 <- (dataesh$femaleagedif)^3
dataesh$femaleagedif4 <- (dataesh$femaleagedif)^4

dataesh$wealth <- dataesh$eqwealth


modelwealth <- lm(log(wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataesh, weights = weight)
#modelwealth <- lm((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataesh, weights = weight)

summary(modelwealth)

tab_model(modelwealth, digits = 3, digits.p = 3, show.se = T, show.fstat = T, show.aic = T)

#dataesh$wealthpredict <- predict(modelwealth)

dataesh$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid #Alpha (termino independiente) + Residuals

dataesh$wealthstandard <- dataesh$wealth - dataesh$wealthpredict

summary(dataesh$wealthstandard)
summary(dataesh$wealth)

summary(dataesh$wealthpredict)
summary(modelwealth$residuals)
summary(modelwealth$resid)
summary(modelwealth$coefficients[1])


dataesh$wealthpredictexp <- exp(dataesh$wealthpredict)


summary(dataesh$wealth)
summary(dataesh$wealthpredictexp)




saveRDS(dataesh, file = "datasets/spain/v2-HFCS14_Spain_after_step_2d-no-pension-ad-eq_bis1_imp2.rds")








