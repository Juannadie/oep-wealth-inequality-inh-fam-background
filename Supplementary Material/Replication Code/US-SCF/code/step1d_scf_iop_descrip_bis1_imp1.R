#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory for the LAPTOP

options ("scipen"=100, "digits"=10)

library(Hmisc)
library(reldist)
library(tidyverse)

#LET US LOAD THE DATA
dataush <- readRDS(file = "datasets/SCF-2016-all-after-1c.rds")

#Let's see the percentage of negative wealth

wealthno <- NROW(dataush$wealth)
negwealthno <- NROW(dataush$wealth[dataush$wealth<0])

#Share of negative wealth

negwealthno/wealthno

#Weighted share of negative wealth

dataush$wealthdummy <- 1
dataush$negwealthdummy <- 0
dataush$negwealthdummy [dataush$wealth<0] <- 1
weightedwealthno <- sum(as.numeric(dataush$wealthdummy*dataush$weight))
weightednegwealthno <- sum(as.numeric(dataush$negwealthdummy*dataush$weight))
weigthednegwealthshare <- weightednegwealthno/weightedwealthno

#Let's see the people that have inheritances with negative wealth

NROW(dataush$wealth[dataush$wealth <0 & dataush$inh >0])
NROW(dataush$inh[dataush$inh>0]) #A small number


#What if we use eqinh
dataush$eqinh <- dataush$inh/dataush$adeqscale


inhno <- NROW(dataush$inh[dataush$inh>0])
smallinhno <- NROW(dataush$inh[dataush$inh>0 & dataush$inh<5000])
sharesmallinh <- smallinhno/inhno

sharesmallinh #

#Another option is to do it with the equivalised inheritances

eqinhno <- NROW(dataush$eqinh[dataush$eqinh>0])
smalleqinhno <- NROW(dataush$eqinh[dataush$eqinh>0 & dataush$eqinh<5000])
sharesmalleqinh <- smalleqinhno/eqinhno

sharesmalleqinh #


## USE THE AVERAGE OF THE IMPUTATIONS ####

#Reducing the dataset to the number of households

#We could obtain the (weighted) average wealth per household in order to have one observation per household.

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avgwealth = (weighted.mean(x=wealth, w=weight))) #We obtain the weighted mean of all 5 imputations for each household

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avgeqwealth = (weighted.mean(x=eqwealth, w=weight))) #We obtain the weighted mean of all 5 imputations for each household

#We also obtain the same for inheritances

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avginh = (weighted.mean(x=inh, w=weight))) ##We obtain the weighted mean of all 5 imputations for each household

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avgeqinh = (weighted.mean(x=eqinh, w=weight))) ##We obtain the weighted mean of all 5 imputations for each household

#And for income

#We could obtain the (weighted) average wealth per household in order to have one observation per household.

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avgincome = (weighted.mean(x=income, w=weight))) #We obtain the weighted mean of all 5 imputations for each household

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avgeqincome = (weighted.mean(x=eqincome, w=weight))) #We obtain the weighted mean of all 5 imputations for each household

#And we obtain the average weight of all 5 imputations

dataush <- dataush %>%
  group_by(yy1) %>%
  mutate(avgweight = mean(weight)) ##We obtain the weighted mean of all 5 imputations for each household

#Let us check if the different imputations issue influences the inequality indices - No, practically the same as using just one variable... we'd better use the whole multiple imputed sample and run a robustness check with the other variables.


#dataush$wealth <- dataush$avgwealth
#dataush$eqwealth <- dataush$avgeqwealth
#dataush$income <- dataush$avgincome
#dataush$eqincome <- dataush$avgeqincome
#dataush$inh <- dataush$avginh
#dataush$eqinh <- dataush$avgeqinh

## IF WE DO NOT DO WHAT IS ABOVE, WE ARE USING ONLY THE FIRST IMPUTATION INSTEAD OF THE AVERAGE FOR ALL THE ANALYSIS##

#We can also filter for the first imputation (see y1 and yy1) - In this case it would not matter which imputation since we are using the average value of all imputations for variables imputed

dataush1imp <- dataush[dataush$y1%%5 == 1,]
dataush <- dataush1imp


#We then convert inheritances smaller than 10000 to 0

NROW(dataush$eqinh[dataush$eqinh<5000])

dataush$eqinh[dataush$eqinh<5000] <- 0


#We filter for negative wealth, over 40

dataush <- dataush[dataush$wealth>0,]
dataush <- dataush[dataush$age >= 50,] #we use now the threshold at 50 years 
dataush <- dataush[dataush$age <= 80,]




#INHERITANCE TYPES

#FIRST, LET'S SEE IF THE PERSON HAS EXPECTATION OF RECEIVING AN INHERITANCE

#X5819 Do you (or your {husband/wife/partner/spouse}) expect to receive a substantial inheritance or transfer of assets in the future? 1.    *YES 5.    *NO

# X5821 About how much do you expect? $ AMOUNT  0.     Inap. (no expected future inheritance: X5819^=1)

summary (dataush$X5819)
table(dataush$X5819)
table(dataush$X5819[dataush$inh==0]) #Many expect among those not receiving inheritance

summary (dataush$X5821 [dataush$X5819 ==1])
NROW (dataush$X5821 [dataush$X5819 ==1])
NROW (dataush$X5821 [dataush$X5819 ==1 & dataush$X5821 >= 10000]) #Almost all of them are over 10000 USD

#We create a dummy for expect inheritance (if over 10000 USD)
dataush$expectinh <- 0
dataush$expectinh[dataush$X5819 ==1 & dataush$X5821 >= 10000] <- 1

### AND NOW WE CREATE THE 6 INHERITANCE TYPES ####

######## WE GET THE INHERITANCE GROUPS ONLY AFTER ADJUSTING AND ELIMINATING NEGATIVE ADJUSTED WEALTH #####




#### NOW WE USE EQUIVALENT INHERITANCES ####

#X5819 Do you (or your {husband/wife/partner/spouse}) expect to receive a substantial inheritance or transfer of assets in the future? 1.    *YES 5.    *NO

# X5821 About how much do you expect? $ AMOUNT  0.     Inap. (no expected future inheritance: X5819^=1)

summary (dataush$X5819)
table(dataush$X5819)
table(dataush$X5819[dataush$eqinh==0]) #Many expect among those not receiving inheritance

summary (dataush$X5821 [dataush$X5819 ==1])
NROW (dataush$X5821 [dataush$X5819 ==1])
NROW (dataush$X5821 [dataush$X5819 ==1 & dataush$X5821 >= 10000]) #Almost all of them are over 10000 USD

#We create a dummy for expect inheritance (if over 10000 USD)
dataush$expecteqinh <- 0
dataush$expecteqinh[dataush$X5819 ==1 & dataush$X5821 >= 10000] <- 1

### AND NOW WE CREATE THE 6 INHERITANCE TYPES ####

#NOW WE CLASSIFY THE RECIPIENTS OF INHERITANCES IN EACH QUARTILE

#### WE NOW MAKE THE TYPES WITHOUT TAKING INHERITANCES INTO ACCOUNT##
##Now we go for the amount of the inheritances received in order to obtain the types

#We can also check the inheritance quantiles
quantile (dataush$eqinh [dataush$eqinh > 0], probs = seq(0, 1, 0.25))

q0 <- quantile (dataush$eqinh [dataush$eqinh > 0], probs = 0)
q1 <- quantile (dataush$eqinh [dataush$eqinh > 0], probs = 0.25)
q2 <- quantile (dataush$eqinh [dataush$eqinh > 0], probs = 0.5)
q3 <- quantile (dataush$eqinh [dataush$eqinh > 0], probs = 0.75)

#We can also check the inheritance quantiles
quantile (dataush$eqinh [dataush$eqinh > 0], probs = seq(0, 1, 0.25))

q0 <- reldist::wtd.quantile (dataush$eqinh [dataush$eqinh > 0], q = 0, weight=(dataush$weight[dataush$eqinh > 0]), na.rm = T)
q1 <- reldist::wtd.quantile (dataush$eqinh [dataush$eqinh > 0], q = 0.25, weight=dataush$weight[dataush$eqinh > 0], na.rm = T)
q2 <- reldist::wtd.quantile (dataush$eqinh [dataush$eqinh > 0], q = 0.5, weight=dataush$weight[dataush$eqinh > 0], na.rm = T)
q3 <- reldist::wtd.quantile (dataush$eqinh [dataush$eqinh > 0], q = 0.75, weight=dataush$weight[dataush$eqinh > 0], na.rm = T)


#And create the categorical variable accordingly
dataush$eqinhcat <- NA

dataush$eqinhcat [dataush$eqinh < q0 & dataush$expecteqinh ==0] <- 1
dataush$eqinhcat [dataush$eqinh < q0 & dataush$expecteqinh ==1] <- 2
dataush$eqinhcat [dataush$eqinh >= q0 & dataush$eqinh < q1] <- 3
dataush$eqinhcat [dataush$eqinh >= q1 & dataush$eqinh < q2] <- 4
dataush$eqinhcat [dataush$eqinh >= q2 & dataush$eqinh < q3] <- 5
dataush$eqinhcat [dataush$eqinh >= q3] <- 6

table (dataush$eqinhcat)

####


####



saveRDS(dataush, file = "datasets/SCF-2016-all-after-1d-bis1-imp1.rds")

#####



