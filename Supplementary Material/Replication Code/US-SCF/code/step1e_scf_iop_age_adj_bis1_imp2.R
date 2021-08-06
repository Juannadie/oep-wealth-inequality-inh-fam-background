#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory for the LAPTOP

options ("scipen"=100, "digits"=10)

library(Hmisc)
library(reldist)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(snakecase)
library(survey)
library(quantreg)


#LET US LOAD THE DATA
dataush <- readRDS(file = "datasets/SCF-2016-all-after-1d--bis1-imp2.rds")


#THEN WE CONVERT THE WEALTH DATA INTO NET TERMS OF AGE AND GENDER

dataush$agedif <- dataush$age - 65
dataush$agedif2 <- (dataush$agedif)^2
dataush$agedif3 <- (dataush$agedif)^3
dataush$agedif4 <- (dataush$agedif)^4


dataush$femaledummy <- 0
dataush$femaledummy[dataush$sex == "2"] <- 1

#We convert sex to factor for the graph

dataush$sexfactor <- as.factor (dataush$sex)
levels(dataush$sexfactor) <- c("Male","Female")


#

dataush$femaleagedif <- dataush$agedif * dataush$femaledummy
dataush$femaleagedif2 <- (dataush$femaleagedif)^2
dataush$femaleagedif3 <- (dataush$femaleagedif)^3
dataush$femaleagedif4 <- (dataush$femaleagedif)^4

dataush$wealth <- dataush$eqwealth

NROW(dataush$wealth[dataush$wealth <= 0])


modelwealth <- lm(log(wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataush, weights = weight)

#modelwealth <- lm((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataush, weights = weight)
#modelwealth <- rq((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4,tau = .5, data = dataush, weights = weight)

#The model
#modelwealthpoisson <- svyglm(formula = (wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, design = svydesign(ids = ~1, weights = dataush$weight, data = dataush), rescale = T,family = poisson)

#summodelwealthpoisson<- summary (svyinhpoisson3)

#modelwealthus <- tab_model(modelwealthpoisson, digits = 3, digits.p = 3, show.fstat = T, show.se = T, show.aic = T)

#modelwealthus

#dataush$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid





#modelwealth <- lm((wealth) ~ log(agedif^2) + femaledummy + log(femaleagedif^2), data = dataush, weights = weight)

summary(modelwealth)

modelwealthus <- tab_model(modelwealth, digits = 3, digits.p = 3, show.fstat = T, show.se = T, show.aic = T)

modelwealthus

#dataush$wealthpredict <- predict(modelwealth)

dataush$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid

dataush$wealthstandard <- dataush$wealth - dataush$wealthpredict

summary(dataush$wealthstandard)
summary(dataush$wealth)

summary(dataush$wealthpredict)
summary(modelwealth$residuals)
summary(modelwealth$resid)
summary(modelwealth$coefficients[1])


#dataush$wealthpredictexp <- (dataush$wealthpredict)
dataush$wealthpredictexp <- exp(dataush$wealthpredict)

summary(dataush$wealth)
summary(dataush$wealthpredictexp)

NROW(dataush$wealthpredictexp[dataush$wealthpredictexp <= 0])




saveRDS(dataush, file = "datasets/SCF-2016-all-after-1e--bis1-imp2.rds")

#####





