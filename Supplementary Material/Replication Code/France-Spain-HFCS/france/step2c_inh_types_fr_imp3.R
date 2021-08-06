#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

library(Hmisc)
library(reldist)
library(tidyverse)

options ("scipen"=100, "digits"=10)

#We can read the data
datafrh <- readRDS(file = "datasets/HFCS14_France_after_step_2b-no-pension-ad-eq_imp3.rds") ##



#Let's see the percentage of negative wealth

wealthno <- NROW(datafrh$wealth)
negwealthno <- NROW(datafrh$wealth[datafrh$wealth<0])

#Share of negative wealth

negwealthno/wealthno

#Weighted share of negative wealth

datafrh$wealthdummy <- 1
datafrh$negwealthdummy <- 0
datafrh$negwealthdummy [datafrh$wealth<0] <- 1
weightedwealthno <- sum(as.numeric(datafrh$wealthdummy*datafrh$weight))
weightednegwealthno <- sum(as.numeric(datafrh$negwealthdummy*datafrh$weight))
weigthednegwealthshare <- weightednegwealthno/weightedwealthno

#Let's see the people that have inheritances with negative wealth

NROW(datafrh$wealth[datafrh$wealth <0 & datafrh$inh >0])
NROW(datafrh$inh[datafrh$inh>0]) #A small number

#We filter for negative wealth, over 40

datafrh <- datafrh[datafrh$wealth>0,]
datafrh <- datafrh[datafrh$age >= 35,]
datafrh <- datafrh[datafrh$age <= 80,]

#We filter for existing value of inheritances

datafrh <- datafrh[!is.na(datafrh$inh),]

#We filter with the one with information on parental occupation

datafrh <- datafrh[!is.na(datafrh$occdadhead),]

#What if we use eqinh
#datafrh$eqinh <- datafrh$inh/datafrh$eqscale
datafrh$eqinh <- datafrh$inh/datafrh$adeqscale


#datafrh$inh <- datafrh$eqinh

#We then convert inheritances smaller than 5000 to 5000

inhno <- NROW(datafrh$inh[datafrh$inh>0])
smallinhno <- NROW(datafrh$inh[datafrh$inh>0 & datafrh$inh<10000])
sharesmallinh <- smallinhno/inhno

sharesmallinh #

#Another option is to do it with the equivalised inheritances

eqinhno <- NROW(datafrh$eqinh[datafrh$eqinh>0])
smalleqinhno <- NROW(datafrh$eqinh[datafrh$eqinh>0 & datafrh$eqinh<10000])
sharesmalleqinh <- smalleqinhno/eqinhno

sharesmalleqinh #2.8% (4.1% in the original sample)

## We assume small inheritances are 0, in order to compare better among countries.

datafrh$eqinh[datafrh$eqinh<5000] <- 0

#INHERITANCE TYPES

#FIRST, LET'S SEE IF THE PERSON HAS EXPECTATION OF RECEIVING AN INHERITANCE

#And in the future, (do you/does anyone in your household) expect to receive a substantial gift or inheritance (from someone outside the household)?

summary (datafrh$HH0700)
table(datafrh$HH0700)
table(datafrh$HH0700[datafrh$inh==0]) #Many expect among those not receiving inheritance

#We create a dummy for expect inheritance #We don't have the amount for the EU
datafrh$expectinh <- 0 #THE NA'S IN HH0700 WILL ALSO BE CONSIDERED AS NOT EXPECTING
datafrh$expectinh[datafrh$HH0700 ==1] <- 1

#WE FIRST FILTER THE SAMPLE FOR AGE AND FOR NOT HAVING NA'S IN ANY OTHER CIRCUMSTANCE (EDU/OCC AND GENDER), OR IN THE VALUES OF WEALTH AND INCOME. WE ALSO EXCLUDE NEGATIVE WEALTH FROM THE SAMPLE.


### AND NOW WE CREATE THE 6 INHERITANCE TYPES ####

#NOW WE CLASSIFY THE RECIPIENTS OF INHERITANCES IN EACH QUARTILE

#### WE NOW MAKE THE TYPES WITHOUT TAKING INHERITANCES INTO ACCOUNT##
##Now we go for the amount of the inheritances received in order to obtain the types

#We can also check the inheritance quantiles
quantile (datafrh$inh [datafrh$inh > 0], probs = seq(0, 1, 0.25), na.rm = T)

wtd.quantile (datafrh$inh [datafrh$inh > 0], q = seq(0, 1, 0.25), weight=datafrh$weight[datafrh$inh > 0], na.rm = T)

#We use the weighted quantiles

q0 <- wtd.quantile(datafrh$inh [datafrh$inh > 0], q = 0, weight=datafrh$weight[datafrh$inh > 0], na.rm = T)
q1 <- wtd.quantile (datafrh$inh [datafrh$inh > 0], q = 0.25, weight=datafrh$weight[datafrh$inh > 0], na.rm = T)
q2 <- wtd.quantile (datafrh$inh [datafrh$inh > 0], q = 0.5, weight=datafrh$weight[datafrh$inh > 0], na.rm = T)
q3 <- wtd.quantile (datafrh$inh [datafrh$inh > 0], q = 0.75, weight=datafrh$weight[datafrh$inh > 0], na.rm = T)

#And create the categorical variable accordingly
datafrh$inhcat <- NA

datafrh$inhcat [datafrh$inh < q0 & datafrh$expectinh ==0] <- 1
datafrh$inhcat [datafrh$inh < q0 & datafrh$expectinh ==1] <- 2
datafrh$inhcat [datafrh$inh >= q0 & datafrh$inh < q1] <- 3
datafrh$inhcat [datafrh$inh >= q1 & datafrh$inh < q2] <- 4
datafrh$inhcat [datafrh$inh >= q2 & datafrh$inh < q3] <- 5
datafrh$inhcat [datafrh$inh >= q3] <- 6

table (datafrh$inhcat)


### AND NOW WE CREATE THE 6 INHERITANCE TYPES WITH EQ. INHERITANCE ####

#NOW WE CLASSIFY THE RECIPIENTS OF INHERITANCES IN EACH QUARTILE

#### WE NOW MAKE THE TYPES WITHOUT TAKING INHERITANCES INTO ACCOUNT##
##Now we go for the amount of the inheritances received in order to obtain the types

#We can also check the inheritance quantiles
quantile (datafrh$eqinh [datafrh$eqinh > 0], probs = seq(0, 1, 0.25), na.rm = T)

wtd.quantile (datafrh$eqinh [datafrh$eqinh > 0], q = seq(0, 1, 0.25), weight=datafrh$weight[datafrh$eqinh > 0], na.rm = T)

#We use the weighted quantiles

q0 <- wtd.quantile (datafrh$eqinh [datafrh$eqinh > 0], q = 0, weight=datafrh$weight[datafrh$eqinh > 0], na.rm = T)
q1 <- wtd.quantile (datafrh$eqinh [datafrh$eqinh > 0], q = 0.25, weight=datafrh$weight[datafrh$eqinh > 0], na.rm = T)
q2 <- wtd.quantile (datafrh$eqinh [datafrh$eqinh > 0], q = 0.5, weight=datafrh$weight[datafrh$eqinh > 0], na.rm = T)
q3 <- wtd.quantile (datafrh$eqinh [datafrh$eqinh > 0], q = 0.75, weight=datafrh$weight[datafrh$eqinh > 0], na.rm = T)

#And create the categorical variable accordingly
datafrh$eqinhcat <- NA

datafrh$eqinhcat [datafrh$eqinh < q0 & datafrh$expectinh ==0] <- 1
datafrh$eqinhcat [datafrh$eqinh < q0 & datafrh$expectinh ==1] <- 2
datafrh$eqinhcat [datafrh$eqinh >= q0 & datafrh$eqinh < q1] <- 3
datafrh$eqinhcat [datafrh$eqinh >= q1 & datafrh$eqinh < q2] <- 4
datafrh$eqinhcat [datafrh$eqinh >= q2 & datafrh$eqinh < q3] <- 5
datafrh$eqinhcat [datafrh$eqinh >= q3] <- 6

table (datafrh$eqinhcat)

saveRDS(datafrh, file = "datasets/v2-HFCS14_France_after_step_2c-no-pension-ad-eq_imp3.rds")








