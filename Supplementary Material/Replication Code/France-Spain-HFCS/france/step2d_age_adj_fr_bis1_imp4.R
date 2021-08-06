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

#datafrh <- readRDS(file = "HFCS14_France_after_step_2c.rds")

datafrh <- readRDS(file = "datasets/v2-HFCS14_France_after_step_2c-no-pension-ad-eq_bis1_imp4.rds")




#THEN WE CONVERT THE WEALTH DATA INTO NET TERMS OF AGE AND GENDER

datafrh$agedif <- datafrh$age - 65
datafrh$agedif2 <- (datafrh$agedif)^2
datafrh$agedif3 <- (datafrh$agedif)^3
datafrh$agedif4 <- (datafrh$agedif)^4


datafrh$femaledummy <- 0
datafrh$femaledummy[datafrh$sex == "2"] <- 1

#We convert sex to factor for the graph
datafrh$sexfactor <- as.factor (datafrh$sex)
levels(datafrh$sexfactor) <- c("Male","Female")


datafrh$femaleagedif <- datafrh$agedif * datafrh$femaledummy
datafrh$femaleagedif2 <- (datafrh$femaleagedif)^2
datafrh$femaleagedif3 <- (datafrh$femaleagedif)^3
datafrh$femaleagedif4 <- (datafrh$femaleagedif)^4

datafrh$wealth <- datafrh$eqwealth


modelwealth <- lm(log(wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = datafrh, weights = weight)
#modelwealth <- lm((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = datafrh, weights = weight)

summary(modelwealth)

tab_model(modelwealth, digits = 3, digits.p = 3, show.se = T, show.fstat = T, show.aic = T)

#datafrh$wealthpredict <- predict(modelwealth)

datafrh$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid #Alpha (termino independiente) + Residuals

datafrh$wealthstandard <- datafrh$wealth - datafrh$wealthpredict

summary(datafrh$wealthstandard)
summary(datafrh$wealth)

summary(datafrh$wealthpredict)
summary(modelwealth$residuals)
summary(modelwealth$resid)
summary(modelwealth$coefficients[1])


datafrh$wealthpredictexp <- exp(datafrh$wealthpredict)


summary(datafrh$wealth)
summary(datafrh$wealthpredictexp)


####



saveRDS(datafrh, file = "datasets/v2-HFCS14_France_after_step_2d-no-pension-ad-eq_bis1_imp4.rds")








