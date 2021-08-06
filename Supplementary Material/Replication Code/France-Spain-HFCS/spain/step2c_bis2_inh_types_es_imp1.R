#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

library(Hmisc)
library(reldist)
library(tidyverse)

options ("scipen"=100, "digits"=10)

#We can read the data
dataesh <- readRDS(file = "datasets/spain/HFCS14_Spain_after_step_2b-no-pension-ad-eq_imp1.rds") ##


#Let's see the percentage of negative wealth

wealthno <- NROW(dataesh$wealth)
negwealthno <- NROW(dataesh$wealth[dataesh$wealth<0])

#Share of negative wealth

negwealthno/wealthno

#Weighted share of negative wealth

dataesh$wealthdummy <- 1
dataesh$negwealthdummy <- 0
dataesh$negwealthdummy [dataesh$wealth<0] <- 1
weightedwealthno <- sum(as.numeric(dataesh$wealthdummy*dataesh$weight))
weightednegwealthno <- sum(as.numeric(dataesh$negwealthdummy*dataesh$weight))
weigthednegwealthshare <- weightednegwealthno/weightedwealthno

#Let's see the people that have inheritances with negative wealth

NROW(dataesh$wealth[dataesh$wealth <0 & dataesh$inh >0])
NROW(dataesh$inh[dataesh$inh>0]) #A small number

#We filter for the first decile to be out

#dataesh <- dataesh[dataesh$wealth>0,]

q10wealth <- wtd.quantile(dataesh$wealth, q=0.1, weight = dataesh$weight)
dataesh <- dataesh[dataesh$wealth > q10wealth,]

dataesh <- dataesh[dataesh$age >= 35,]
dataesh <- dataesh[dataesh$age <= 80,]

#We filter for existing value of inheritances

dataesh <- dataesh[!is.na(dataesh$inh),]

#We filter with the one with information on parental occupation

dataesh <- dataesh[!is.na(dataesh$occdadhead),]

#What if we use eqinh
#dataesh$eqinh <- dataesh$inh/dataesh$eqscale
dataesh$eqinh <- dataesh$inh/dataesh$adeqscale

#dataesh$inh <- dataesh$eqinh

#We then convert inheritances smaller than 5000 to 5000

inhno <- NROW(dataesh$inh[dataesh$inh>0])
smallinhno <- NROW(dataesh$inh[dataesh$inh>0 & dataesh$inh<5000])
sharesmallinh <- smallinhno/inhno

sharesmallinh #

#Another option is to do it with the equivalised inheritances

eqinhno <- NROW(dataesh$eqinh[dataesh$eqinh>0])
smalleqinhno <- NROW(dataesh$eqinh[dataesh$eqinh>0 & dataesh$eqinh<5000])
sharesmalleqinh <- smalleqinhno/eqinhno

sharesmalleqinh #2.8% (4.1% in the original sample)

## We assume small inheritances are 0, in order to compare better among countries.

dataesh$eqinh[dataesh$eqinh<5000] <- 0

#INHERITANCE TYPES

#FIRST, LET'S SEE IF THE PERSON HAS EXPECTATION OF RECEIVING AN INHERITANCE

#And in the future, (do you/does anyone in your household) expect to receive a substantial gift or inheritance (from someone outside the household)?

summary (dataesh$HH0700)
table(dataesh$HH0700)
table(dataesh$HH0700[dataesh$inh==0]) #Many expect among those not receiving inheritance

#We create a dummy for expect inheritance #We don't have the amount for the EU
dataesh$expectinh <- 0 #THE NA'S IN HH0700 WILL ALSO BE CONSIDERED AS NOT EXPECTING
dataesh$expectinh[dataesh$HH0700 ==1] <- 1

#WE FIRST FILTER THE SAMPLE FOR AGE AND FOR NOT HAVING NA'S IN ANY OTHER CIRCUMSTANCE (EDU/OCC AND GENDER), OR IN THE VALUES OF WEALTH AND INCOME. WE ALSO EXCLUDE NEGATIVE WEALTH FROM THE SAMPLE.


### AND NOW WE CREATE THE 6 INHERITANCE TYPES ####

#NOW WE CLASSIFY THE RECIPIENTS OF INHERITANCES IN EACH QUARTILE

#### WE NOW MAKE THE TYPES WITHOUT TAKING INHERITANCES INTO ACCOUNT##
##Now we go for the amount of the inheritances received in order to obtain the types

#We can also check the inheritance quantiles
quantile (dataesh$inh [dataesh$inh > 0], probs = seq(0, 1, 0.25), na.rm = T)

wtd.quantile (dataesh$inh [dataesh$inh > 0], q = seq(0, 1, 0.25), weight=dataesh$weight[dataesh$inh > 0], na.rm = T)

#We use the weighted quantiles

q0 <- wtd.quantile (dataesh$inh [dataesh$inh > 0], q = 0, weight=dataesh$weight[dataesh$inh > 0], na.rm = T)
q1 <- wtd.quantile (dataesh$inh [dataesh$inh > 0], q = 0.25, weight=dataesh$weight[dataesh$inh > 0], na.rm = T)
q2 <- wtd.quantile (dataesh$inh [dataesh$inh > 0], q = 0.5, weight=dataesh$weight[dataesh$inh > 0], na.rm = T)
q3 <- wtd.quantile (dataesh$inh [dataesh$inh > 0], q = 0.75, weight=dataesh$weight[dataesh$inh > 0], na.rm = T)

#And create the categorical variable accordingly
dataesh$inhcat <- NA

dataesh$inhcat [dataesh$inh < q0 & dataesh$expectinh ==0] <- 1
dataesh$inhcat [dataesh$inh < q0 & dataesh$expectinh ==1] <- 2
dataesh$inhcat [dataesh$inh >= q0 & dataesh$inh < q1] <- 3
dataesh$inhcat [dataesh$inh >= q1 & dataesh$inh < q2] <- 4
dataesh$inhcat [dataesh$inh >= q2 & dataesh$inh < q3] <- 5
dataesh$inhcat [dataesh$inh >= q3] <- 6

table (dataesh$inhcat)


### AND NOW WE CREATE THE 6 INHERITANCE TYPES WITH EQ. INHERITANCE ####

#NOW WE CLASSIFY THE RECIPIENTS OF INHERITANCES IN EACH QUARTILE

#### WE NOW MAKE THE TYPES WITHOUT TAKING INHERITANCES INTO ACCOUNT##
##Now we go for the amount of the inheritances received in order to obtain the types

#We can also check the inheritance quantiles
quantile (dataesh$eqinh [dataesh$eqinh > 0], probs = seq(0, 1, 0.25), na.rm = T)

wtd.quantile (dataesh$eqinh [dataesh$eqinh > 0], q = seq(0, 1, 0.25), weight=dataesh$weight[dataesh$eqinh > 0], na.rm = T)

#We use the weighted quantiles

q0 <- wtd.quantile (dataesh$eqinh [dataesh$eqinh > 0], q = 0, weight=dataesh$weight[dataesh$eqinh > 0], na.rm = T)
q1 <- wtd.quantile (dataesh$eqinh [dataesh$eqinh > 0], q = 0.25, weight=dataesh$weight[dataesh$eqinh > 0], na.rm = T)
q2 <- wtd.quantile (dataesh$eqinh [dataesh$eqinh > 0], q = 0.5, weight=dataesh$weight[dataesh$eqinh > 0], na.rm = T)
q3 <- wtd.quantile (dataesh$eqinh [dataesh$eqinh > 0], q = 0.75, weight=dataesh$weight[dataesh$eqinh > 0], na.rm = T)

#And create the categorical variable accordingly
dataesh$eqinhcat <- NA

dataesh$eqinhcat [dataesh$eqinh < q0] <- 1 #EXPECTING INHERITANCES NOT EXISTING IN SPAIN
dataesh$eqinhcat [dataesh$eqinh >= q0 & dataesh$eqinh < q1] <- 2
dataesh$eqinhcat [dataesh$eqinh >= q1 & dataesh$eqinh < q2] <- 3
dataesh$eqinhcat [dataesh$eqinh >= q2 & dataesh$eqinh < q3] <- 4
dataesh$eqinhcat [dataesh$eqinh >= q3] <- 5

table (dataesh$eqinhcat)


saveRDS(dataesh, file = "datasets/spain/v2-HFCS14_Spain_after_step_2c-no-pension-ad-eq_bis2_imp1.rds")








