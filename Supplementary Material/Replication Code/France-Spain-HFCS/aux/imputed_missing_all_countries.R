#we calculate the missing values for Spain

#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

library(foreign)
options ("scipen"=100, "digits"=4)

#setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory # LAPTOP DIRECTORY

library(Hmisc)
library(reldist)
library(tidyverse)
library(np)
library(IC2)
library(dplyr)

options(scipen=100, digits=12)

#LET US LOAD THE DATA

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/datasets/spain/") #Set Working Directory # LAPTOP DIRECTORY

dataesh <- readRDS("v2-HFCS14_Spain_after_step_2d-no-pension-ad-eq_imp1.rds") #creates a list with all the personal files in the eu-silc directory directory

summary(dataesh$HH0401)
summary(dataesh$fHH0401)
table(dataesh$fHH0401)

#### Let's count the imputed observations for each inheritance received ###

table(dataesh$fHH0401)
class(dataesh$fHH0401)

summary(dataesh$HH0401)
obsinh1 <- NROW(dataesh$HH0401[!is.na(dataesh$HH0401) & (dataesh$HH0401 > 0)])
table(dataesh$fHH0401[is.na(dataesh$HH0401)])
missinginh1 <- NROW(dataesh$HH0401[dataesh$fHH0401 > 1])
imputinh1 <- NROW(dataesh$HH0401[dataesh$fHH0401 >= 4050 & dataesh$fHH0401 != 4053]) #Brackets are not considered imputation

shareimp1 <- (imputinh1/obsinh1)*100

summary(dataesh$HH0402)
obsinh2 <- NROW(dataesh$HH0402[!is.na(dataesh$HH0402) & (dataesh$HH0402 > 0)])
table(dataesh$fHH0402[is.na(dataesh$HH0402)])
missinginh2 <- NROW(dataesh$HH0402[dataesh$fHH0402 > 1])
imputinh2 <- NROW(dataesh$HH0402[dataesh$fHH0402  >= 4050 & dataesh$fHH0401 != 4053])

shareimp2 <- (imputinh2/obsinh2)*100


summary(dataesh$HH0403)
obsinh3 <- NROW(dataesh$HH0403[!is.na(dataesh$HH0403) & (dataesh$HH0403 > 0)])
table(dataesh$fHH0403[is.na(dataesh$HH0403)])
missinginh3 <- NROW(dataesh$HH0403[dataesh$fHH0403 > 1])
imputinh3 <- NROW(dataesh$HH0403[dataesh$fHH0403  >= 4050 & dataesh$fHH0401 != 4053])

shareimp3 <- (imputinh3/obsinh3)*100

inhimp1 <- c(obsinh1, imputinh1, shareimp1)
inhimp2 <- c(obsinh2, imputinh2, shareimp2)
inhimp3 <- c(obsinh3, imputinh3, shareimp3)

impinhspain <- rbind(inhimp1, inhimp2, inhimp3)

avginhspain <- ((imputinh1 + imputinh2 + imputinh3)/(obsinh1 + obsinh2 + obsinh3))

#we calculate the missing values for France

#LET US LOAD THE DATA

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/datasets") #Set Working Directory # LAPTOP DIRECTORY

datafrh <- readRDS("v2-HFCS14_France_after_step_2d-no-pension-ad-eq_imp1.rds") #creates a list with all the personal files in the eu-silc directory directory

summary(datafrh$HH0401)
summary(datafrh$fHH0401)
table(datafrh$fHH0401)

#### Let's count the imputed observations for each inheritance received ###

table(datafrh$fHH0401)
class(datafrh$fHH0401)

summary(datafrh$HH0401)
obsinh1 <- NROW(datafrh$HH0401[!is.na(datafrh$HH0401) & (datafrh$HH0401 > 0)])
table(datafrh$fHH0401[is.na(datafrh$HH0401)])
missinginh1 <- NROW(datafrh$HH0401[datafrh$fHH0401 > 1])
imputinh1 <- NROW(datafrh$HH0401[datafrh$fHH0401 >= 4050 & datafrh$fHH0401 != 4053]) #Brackets are not considered imputation

shareimp1 <- (imputinh1/obsinh1)*100

summary(datafrh$HH0402)
obsinh2 <- NROW(datafrh$HH0402[!is.na(datafrh$HH0402) & (datafrh$HH0402 > 0)])
table(datafrh$fHH0402[is.na(datafrh$HH0402)])
missinginh2 <- NROW(datafrh$HH0402[datafrh$fHH0402 > 1])
imputinh2 <- NROW(datafrh$HH0402[datafrh$fHH0402  >= 4050 & datafrh$fHH0401 != 4053])

shareimp2 <- (imputinh2/obsinh2)*100


summary(datafrh$HH0403)
obsinh3 <- NROW(datafrh$HH0403[!is.na(datafrh$HH0403) & (datafrh$HH0403 > 0)])
table(datafrh$fHH0403[is.na(datafrh$HH0403)])
missinginh3 <- NROW(datafrh$HH0403[datafrh$fHH0403 > 1])
imputinh3 <- NROW(datafrh$HH0403[datafrh$fHH0403  >= 4050 & datafrh$fHH0401 != 4053])

shareimp3 <- (imputinh3/obsinh3)*100

inhimp1 <- c(obsinh1, imputinh1, shareimp1)
inhimp2 <- c(obsinh2, imputinh2, shareimp2)
inhimp3 <- c(obsinh3, imputinh3, shareimp3)

impinhfrance <- rbind(inhimp1, inhimp2, inhimp3)

avginhfrance <- ((imputinh1 + imputinh2 + imputinh3)/(obsinh1 + obsinh2 + obsinh3))


## NOW FOR THE US DATA #####

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory # LAPTOP DIRECTORY


dataush <- readRDS(file = "datasets/SCF-2016-all-after-1c.rds")

#WE SELECT THE SAMPLE WE USE

#we equivalise wealth and inheritance to have those variables too

dataush$eqinh <- dataush$inh/dataush$adeqscale
dataush$eqinh[dataush$eqinh<5000] <- 0
dataush$inh[dataush$eqinh<5000] <- 0 #also eliminate inheritances

#Share of recipients
dataush$inhdummy <- 0
dataush$inhdummy[dataush$inh > 0] <- 1
#dataush$inhdummy[dataush$inh = 0] <- 0

#original data excluding missing
#We filter with the one with information on parental occupation
#dataush <- dataush[!is.na(dataush$occdadhead),]
dataush1 <- dataush

#data sampled 35-80
dataush2 <- dataush1[dataush1$age >= 35,]
dataush2 <- dataush2[dataush2$age <= 80,]

#and excluding negative wealth
dataush3 <- dataush2[dataush2$wealth>0,]

#and using equivalent wealth and equivalent inheritance

dataush4 <- dataush3

dataush4$wealth <- dataush4$wealth/dataush4$adeqscale
dataush4$inh <- dataush4$inh/dataush4$adeqscale

#INHERITANCES AMOUNT

#One first thing that we could see is how many imputations are there in the inheritance amount. And now we can see the actual inheritance amount

summary(dataush4$X5804)
dataush4$value1inh <- dataush4$X5804 #First inheritance

#We can check how many values have been imputed for that inheritance

library (tidyverse)

dataush4 <- dataush4 %>%
  group_by(YY1) %>%
  mutate(imputeinh1 = (sd(X5804))) #We check if within each case there has been a difference in the 5 imputed values. If there has not (var = 0), then it is a real value.
summary (dataush4$imputeinh1) #There have been some imputations...

a <- NROW (dataush4$imputeinh1[dataush4$imputeinh1>0]) #1 inheritances imputed
numberimpinh1 <- a/5
#shareimpinh1 <- (NROW (dataush4$imputeinh1[dataush4$imputeinh1>0]))/NROW (dataush4$imputeinh1) #5.5 % of the observations imputed
numberobsinh1 <- (NROW (dataush4$X5804[dataush4$X5804>0]))/5
shareimpinh1 <- numberimpinh1/numberobsinh1


#NOW THE SECOND INHERITANCE

summary(dataush4$X5809)
dataush4$value2inh <- dataush4$X5809 #Second inheritance

#We can check how many values have been imputed for that inheritance

dataush4 <- dataush4 %>%
  group_by(YY1) %>%
  mutate(imputeinh2 = (sd(X5809))) #We check if within each case there has been a difference in the 5 imputed values. If there has not (var = 0), then it is a real value.
summary (dataush4$imputeinh2) #There have been some imputations...

a2 <- NROW (dataush4$imputeinh2[dataush4$imputeinh2>0]) #1 inheritances imputed
numberimpinh2 <- a2/5
#shareimpinh2 <- (NROW (dataush4$imputeinh2[dataush4$imputeinh2>0]))/NROW (dataush4$imputeinh2) #1.6 % of the housholds have this second inheritance imputed
numberobsinh2 <-( NROW (dataush4$X5809[dataush4$X5809>0]))/5
shareimpinh2 <- numberimpinh2/numberobsinh2



#NOW THE THIRD INHERITANCE

summary(dataush4$X5814)
dataush4$value3inh <- dataush4$X5814 #Third inheritance

#We can check how many values have been imputed for that inheritance

dataush4 <- dataush4 %>%
  group_by(YY1) %>%
  mutate(imputeinh3 = (sd(X5814))) #We check if within each case (YY1) there has been a difference in the 5 imputed values. If there has not (var = 0), then it is a real value.
summary (dataush4$imputeinh3) #There have been some imputations...

a2 <- NROW (dataush4$imputeinh3[dataush4$imputeinh3>0]) #1 inheritances imputed
numberimpinh3 <- a2/5
#shareimpinh3 <- (NROW (dataush4$imputeinh3[dataush4$imputeinh3>0]))/NROW (dataush4$imputeinh3) #0.6 % of the housholds have this third inheritance imputed
numberobsinh3 <- (NROW (dataush4$X5814[dataush4$X5814>0]))/5
shareimpinh3 <- numberimpinh3/numberobsinh3


#NOW THE FOURTH (INCLUDES ALL REMAINING) INHERITANCE

summary(dataush4$X5818)
dataush4$value3inh <- dataush4$X5818 #Fourth inheritance

#We can check how many values have been imputed for that inheritance

dataush4 <- dataush4 %>%
  group_by(YY1) %>%
  mutate(imputeinh4 = (sd(X5818))) #We check if within each case (YY1) there has been a difference in the 5 imputed values. If there has not (var = 0), then it is a real value.
summary (dataush4$imputeinh4) #There have been some imputations...

a2 <- NROW (dataush4$imputeinh4[dataush4$imputeinh4>0]) #1 inheritances imputed
numberimpinh4 <- a2/5
numberobsinh4 <- (NROW (dataush4$X5818[dataush4$X5818>0]))/5
#shareimpinh4 <- (NROW (dataush4$imputeinh4[dataush4$imputeinh4>0]))/NROW (dataush4$imputeinh4) #0.3 % of the housholds have this third inheritance imputed (not because this is over the total of households, not on the total of observations)
shareimpinh4 <- numberimpinh4/numberobsinh4

obsinhus <- c(numberobsinh1, numberobsinh2, numberobsinh3, numberobsinh4)
obsinput <- c(numberimpinh1, numberimpinh2, numberimpinh3, numberimpinh4)
impinhus <- c(shareimpinh1, shareimpinh2, shareimpinh3, shareimpinh4)

#Now we could see the total share of observations imputed:
totalobsinh <- numberobsinh1+numberobsinh2+numberobsinh3+numberobsinh4
totalinputinh <- numberimpinh1+numberimpinh2+numberimpinh3+numberimpinh4
shareimptotal <- totalinputinh/totalobsinh
shareimptotal

### NOW IMPUTATIONS FOR THE UK

setwd ("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/data_rds")

dataukh <- readRDS(file = "WAS-After-Step1a-brackets-new-data.rds")

#We could at least subset by head being between and



#dataukh <- readRDS(file = "datauk-new-final-no-cpt-new-data8a.rds")


#NROW(dataukh$flaginputlife1basic[dataukh$flaginputlife1basic == 1 & dataukh$IEValW1>0])
#NROW(dataukh$flaginputlife1basic[dataukh$IEValW1>0])



#Number of individual in our sample
NROW (dataukh)
#Number of households in our sample
NROW(unique(dataukh$CaseW3))


##

NROW (dataukh$IEValW1[dataukh$IHEvW1 == "Yes" & dataukh$IEValBW1 %in% c("Not applicable", "Do not know", "Refusal")])

#BEFORE IMPUTATION, LET'S FIRST COMPUTE THE MISSING DATA THAT WE HAVE...

table(dataukh$IEValW1[dataukh$IHEvW1 == "Yes"& dataukh$IEValBW1 == "Not applicable"])
table(dataukh$IEValW1[dataukh$IHEvW1 == "Yes"& dataukh$IEValBW1 == "Not applicable"])

missinhw1 <- NROW(dataukh$IEValW1[dataukh$IHEvW1 == "Yes" & dataukh$IEValBW1 %in% c("Not applicable", "Do not know", "Refusal") & dataukh$IEValW1 <0]) #Only missing without reporting a bracket value.

inhw1 <- NROW(dataukh$IEValW1[dataukh$IHEvW1 == "Yes"]) #We do the ratio against all possible inheritances.

sharemissinhw1 <- missinhw1/inhw1

missinhw1
inhw1
sharemissinhw1

table(dataukh$IEValBW1[dataukh$IHEvW1 == "Yes"])


#IN HOUSEHOLD TERMS

missinhw1h <- NROW(unique(dataukh$CaseW3[dataukh$IHEvW1 == "Yes" & dataukh$IEValBW1 %in% c("Not applicable", "Do not know", "Refusal") & dataukh$IEValW1 %in% c(-6,-8,-9)]))

inhw1h <- NROW(unique(dataukh$CaseW3[dataukh$IHEvW1 == "Yes"]))

sharemissinhw1h <- missinhw1h/inhw1h

missinhw1h
inhw1h
sharemissinhw1h


#WE CHECK THAT FOR THE SECOND INHERITANCE VARIABLE

table(dataukh$IEVal2W1[dataukh$IHEvW1 == "Yes"& dataukh$IEValB2W1 == "Not applicable"])
table(dataukh$IEVal2W1[dataukh$IHEvW1 == "Yes" & dataukh$IEValB2W1 == "Not applicable"])

missinh2w1 <- NROW(dataukh$IEVal2W1[dataukh$IHEvW1 == "Yes" & dataukh$IEValB2W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHEvNoW1 >1 & dataukh$IEVal2W1 %in% c(-6, -7, -8,-9)])

inh2w1 <- NROW(dataukh$IEVal2W1[dataukh$IHEvW1 == "Yes" & dataukh$IHEvNoW1 >1])

sharemissinh2w1 <- missinh2w1/inh2w1


missinh2w1
inh2w1
sharemissinh2w1

table(dataukh$IEValB2W1[dataukh$IHEvW1 == "Yes" &  dataukh$IHEvNoW1 >1])


#At the household level

missinh2w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHEvW1 == "Yes" & dataukh$IEValB2W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHEvNoW1 >1 & dataukh$IEVal2W1 %in% c(-6,-8,-9)]))

inh2w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHEvW1 == "Yes" &  dataukh$IHEvNoW1 >1]))

sharemissinh2w1h <- missinh2w1h/inh2w1h

missinh2w1h
inh2w1h
sharemissinh2w1h


#WE CHECK THAT FOR THE THIRD INHERITANCE VARIABLE

table(dataukh$IEVal3W1[dataukh$IHEvW1 == "Yes"& dataukh$IEValB3W1 == "Not applicable"])
table(dataukh$IEVal3W1[dataukh$IHEvW1 == "Yes" & dataukh$IEValB3W1 == "Not applicable"])

missinh3w1 <- NROW(dataukh$IEVal3W1[dataukh$IHEvW1 == "Yes" & dataukh$IEValB3W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHEvNoW1 >2 & dataukh$IEVal3W1 %in% c(-6, -8,-9)])

inh3w1 <- NROW(dataukh$IEVal3W1[dataukh$IHEvW1 == "Yes" & dataukh$IHEvNoW1 >2])

sharemissinh3w1 <- missinh3w1/inh3w1

missinh3w1
inh3w1
sharemissinh3w1


#Count also at the household level

missinh3w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHEvW1 == "Yes" & dataukh$IEValB3W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHEvNoW1 >2 & dataukh$IEVal3W1 %in% c(-6, -8,-9)]))

inh3w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHEvW1 == "Yes" &  dataukh$IHEvNoW1 >2]))

sharemissinh3w1h <- missinh3w1h/inh3w1h

missinh3w1h
inh3w1h
sharemissinh3w1h

#NOW LET'S CHECK THE RECENT INHERITANCES

table(dataukh$IValW1[dataukh$IHrecntW1 == "Yes"& dataukh$IValBW1 == "Not applicable"])
table(dataukh$IValW1[dataukh$IHrecntW1 == "Yes"& dataukh$IValBW1 == "Not applicable"])

missinhrcntw1 <- NROW(dataukh$IValW1[dataukh$IHrecntW1 == "Yes" & dataukh$IValBW1 %in% c("Not applicable", "Do not know", "Refusal") & dataukh$IValW1 %in%c(-9,-8,-6)])

inhrcntw1 <- NROW(dataukh$IValW1[dataukh$IHrecntW1 == "Yes"])

sharemissinhrcntw1 <- missinhrcntw1/inhrcntw1

inhrcntw1
missinhrcntw1
sharemissinhrcntw1

#Now at the household level

missinhrcntw1h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW1 == "Yes" & dataukh$IValBW1 %in% c("Not applicable", "Do not know", "Refusal") & dataukh$IValW1 %in%c(-9,-8,-6)]))

inhrcntw1h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW1 == "Yes"]))

sharemissinhrcntw1h <- missinhrcntw1h/inhrcntw1h

inhrcntw1h
missinhrcntw1h
sharemissinhrcntw1h


#WE CHECK THAT FOR THE SECOND INHERITANCE VARIABLE

table(dataukh$IVal2W1[dataukh$IHrecntW1 == "Yes"& dataukh$IValB2W1 == "Not applicable"])

missinhrcnt2w1 <- NROW(dataukh$IVal2W1[dataukh$IHrecntW1 == "Yes" & dataukh$IValB2W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHrcnumW1 >1 & dataukh$IVal2W1 %in% c(-6,-8,-9)])

inhrcnt2w1 <- NROW(dataukh$IVal2W1[dataukh$IHrecntW1 == "Yes" &  dataukh$IHrcnumW1 >1])

sharemissinhrcnt2w1 <- missinhrcnt2w1/inhrcnt2w1

missinhrcnt2w1
inhrcnt2w1
sharemissinhrcnt2w1

#Now at the household level

missinhrcnt2w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW1 == "Yes" & dataukh$IValB2W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHrcnumW1 >1 & dataukh$IVal2W1 %in% c(-6,-8,-9)]))

inhrcnt2w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW1 == "Yes" &  dataukh$IHrcnumW1 >1]))

sharemissinhrcnt2w1h <- missinhrcnt2w1h/inhrcnt2w1h

missinhrcnt2w1h
inhrcnt2w1h
sharemissinhrcnt2w1h

#WE CHECK THAT FOR THE THIRD INHERITANCE VARIABLE

table(dataukh$IVal3W1[dataukh$IHrecntW1 == "Yes"& dataukh$IValB3W1 == "Not applicable"])

missinhrcnt3w1 <- NROW(dataukh$IVal3W1[dataukh$IHrecntW1 == "Yes" & dataukh$IValB3W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHrcnumW1 >2 & dataukh$IVal3W1 %in% c(-6,-8,-9)])

inhrcnt3w1 <- NROW(dataukh$IVal3W1[dataukh$IHrecntW1 == "Yes" &  dataukh$IHrcnumW1 >2])

sharemissinhrcnt3w1 <- missinhrcnt3w1/inhrcnt3w1

missinhrcnt3w1
inhrcnt3w1
sharemissinhrcnt3w1

#Now at the household level

missinhrcnt3w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW1 == "Yes" & dataukh$IValB3W1 %in% c("Not applicable", "Do not know", "Refusal") &  dataukh$IHrcnumW1 >2 & dataukh$IVal3W1 %in% c(-6,-8,-9)]))

inhrcnt3w1h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW1 == "Yes" &  dataukh$IHrcnumW1 >2]))

sharemissinhrcnt3w1h <- missinhrcnt3w1h/inhrcnt3w1h

inhrcnt3w1h
missinhrcnt3w1h
sharemissinhrcnt3w1h


(1737 + 85 + 5)/(2301+112+7)

#NOW WE SEE HOW MANY GIFTS HAVE BEEN IMPUTED

table (dataukh$IGifvalW1)

table(dataukh$ILgiftW1) #Whether they received a Gift

missgiftsW1 <- NROW (dataukh$IGifvalW1[dataukh$IGifvalW1 %in% c(-6,-8, -9) & dataukh$IGfvalbW1 %in% c("Not applicable", "Do not know", "Refusal")])
giftsreceived1 <- NROW (dataukh$IGifvalW1[dataukh$IGifvalW1 != -7])
sharemissgifts1 <- missgiftsW1/giftsreceived1

missgiftsW1
giftsreceived1
sharemissgifts1

#AND NOW LET'S SEE THIS FOR THE HOUSEHOLDS

missgiftsW1h <- NROW(unique(dataukh$CaseW3[dataukh$IGifvalW1 %in% c(-6,-8, -9) & dataukh$IGfvalbW1 %in% c("Not applicable", "Do not know", "Refusal")]))
giftsreceived1h <- NROW(unique(dataukh$CaseW3[dataukh$IGifvalW1 != -7]))
sharemissgifts1h <- missgiftsW1h/giftsreceived1h

missgiftsW1h
giftsreceived1h
sharemissgifts1h

#NOW FOR THE SECOND WAVE RECENT INHERITANCES

table(dataukh$IvalW2[dataukh$IHrecntW2 == "yes" & dataukh$IvalbW2 == "Does not apply"])

missinhrcntw2 <- NROW(dataukh$IvalW2[dataukh$IHrecntW2 == "yes" & dataukh$IvalbW2 %in% c("Does not apply", "do not know", "refusal") & dataukh$IvalW2 %in% c(-9,-8,-6, -7)])

inhrcntw2 <- NROW(dataukh$IvalW2[dataukh$IHrecntW2 == "yes"])

sharemissinhrcntw2 <- missinhrcntw2/inhrcntw2

inhrcntw2
missinhrcntw2
sharemissinhrcntw2


#NOW FOR THE HOUSEHOLDS

table(dataukh$IvalW2[dataukh$IHrecntW2 == "yes" & dataukh$IvalbW2 == "Does not apply"])

missinhrcntw2h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW2 == "yes" & dataukh$IvalbW2 %in% c("Does not apply", "do not know", "refusal") & dataukh$IvalW2 %in% c(-9,-8,-6,-7)]))

inhrcntw2h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW2 == "yes"]))

sharemissinhrcntw2h <- missinhrcntw2h/inhrcntw2h

inhrcntw2h
missinhrcntw2h
sharemissinhrcntw2h

#WE CHECK THAT FOR THE SECOND INHERITANCE VARIABLE (of the second wave)


table(dataukh$Ival2W2[dataukh$IHrecntW2 == "yes" & dataukh$Ivalb2W2 == "Does not apply"])

missinhrcnt2w2 <- NROW(dataukh$Ival2W2[dataukh$IHrecntW2 == "yes" & dataukh$Ivalb2W2 %in% c("Does not apply", "do not know", "refusal") & dataukh$Ival2W2 %in% c(-9,-8,-6)])

inhrcnt2w2 <- NROW(dataukh$Ival2W2[dataukh$IHrecntW2 == "yes" &  dataukh$IHrcnumW2 >1])

sharemissinhrcnt2w2 <- missinhrcnt2w2/inhrcnt2w2

inhrcnt2w2
missinhrcnt2w2
sharemissinhrcnt2w2


#NOW FOR THE HOUSEHOLDS


missinhrcnt2w2h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW2 == "yes" & dataukh$Ivalb2W2 %in% c("Does not apply", "do not know", "refusal") & dataukh$Ival2W2 %in% c(-9,-8,-6)]))

inhrcnt2w2h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW2 == "yes" &  dataukh$IHrcnumW2 >1 ]))

sharemissinhrcnt2w2h <- missinhrcnt2w2h/inhrcnt2w2h

inhrcnt2w2h
missinhrcnt2w2h
sharemissinhrcnt2w2h


#WE CHECK THAT FOR THE THIRD INHERITANCE VARIABLE

table(dataukh$Ival3W2[dataukh$IHrecntW2 == "yes" & dataukh$Ivalb3W2 == "Does not apply"])

missinhrcnt3w2<- NROW(dataukh$Ival3W2[dataukh$IHrecntW2 == "yes" & dataukh$Ivalb3W2%in% c("Does not apply", "do not know", "refusal") & dataukh$Ival3W2 %in% c(-9,-8,-6)])

inhrcnt3w2<- NROW(dataukh$Ival3W2[dataukh$IHrecntW2 == "yes" &  dataukh$IHrcnumW2 >2])

sharemissinhrcnt3w2<- missinhrcnt3w2/inhrcnt3w2

missinhrcnt3w2
inhrcnt3w2
sharemissinhrcnt3w2


#NOW FOR THE HOUSEHOLDS


missinhrcnt3w2h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW2 == "yes" & dataukh$Ivalb3W2 %in% c("Does not apply", "do not know", "refusal") & dataukh$Ival3W2 %in% c(-9,-8,-6)]))

inhrcnt3w2h <- NROW(unique(dataukh$CaseW3[dataukh$IHrecntW2 == "yes" &  dataukh$IHrcnumW2 >2 ]))

sharemissinhrcnt3w2h <- missinhrcnt3w2h/inhrcnt3w2h

inhrcnt3w2h
missinhrcnt3w2h
sharemissinhrcnt3w2h


#NOW WE SEE HOW MANY GIFTS HAVE BEEN IMPUTED

table (dataukh$IgifvalW2)

table(dataukh$ILgiftW2) #Whether they received a Gift

missgiftsW2 <- NROW (dataukh$IgifvalW2[dataukh$IgifvalW2 %in% c(-6,-8, -9)  & dataukh$IgfvalbW2 %in% c("Does not apply", "Do not know", "refusal")])
giftsreceived2 <- NROW (dataukh$IgifvalW2[dataukh$IgifvalW2 != -7])
sharemissgifts2 <- missgiftsW2/giftsreceived2

giftsreceived2
missgiftsW2
sharemissgifts2

#AND NOW LET'S SEE THIS FOR THE HOUSEHOLDS

missgiftsW2h <- NROW(unique(dataukh$CaseW3[dataukh$IgifvalW2 %in% c(-6,-8, -9)& dataukh$IgfvalbW2 %in% c("Does not apply", "Do not know", "refusal")]))
giftsreceived2h <- NROW(unique(dataukh$CaseW3[dataukh$IgifvalW2 != -7]))
sharemissgifts2h <- missgiftsW2h/giftsreceived2h
giftsreceived2h
missgiftsW2h
sharemissgifts2h



#NOW FOR THE THIRD WAVE RECENT INHERITANCES

table(dataukh$IVal1W3[dataukh$IHRECNTw3_i== "Yes"& dataukh$IValB1W3 == "Not applicable"])
table(dataukh$IVal1W3[dataukh$IHRECNTw3_i== "Yes"& dataukh$IValB1W3  == "Does not apply"])

missinhrcntw3 <- NROW(dataukh$IVal1W3 [dataukh$IHRECNTw3_i == "Yes" & dataukh$IValB1W3  %in% c("Does not apply", "Do not know", "Don't know", "Refusal","No answer","Error/Partial" ) & dataukh$IVal1W3  %in%c(-9,-8,-6)])

inhrcntw3 <- NROW(dataukh$IVal1W3 [dataukh$IHRECNTw3_i== "Yes" & dataukh$IVal1W3 != "-7" ])

sharemissinhrcntw3 <- missinhrcntw3/inhrcntw3

missinhrcntw3
inhrcntw3
sharemissinhrcntw3

Imputed <- NROW(dataukh$IVal1W3 [dataukh$IHRECNTw3_i == "Yes" & dataukh$IVAL1w3_iflag == "Imputed"])
Imputed

#Now at the household level

missinhrcntw3h <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i== "Yes" & dataukh$IValB1W3  %in% c("Does not apply", "Do not know", "Don't know", "Refusal","No answer","Error/Partial" ) & dataukh$IVal1W3  %in%c(-9,-8,-6)]))

inhrcntw3h <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i== "Yes" & dataukh$IVal1W3 != "-7"]))

sharemissinhrcntw3h <- missinhrcntw3h/inhrcntw3h

inhrcntw3h
missinhrcntw3h
sharemissinhrcntw3h

Imputed <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i == "Yes" & dataukh$IVAL1w3_iflag == "Imputed"]))



#WE CHECK THAT FOR THE SECOND INHERITANCE VARIABLE


missinhrcnt2w3 <- NROW(dataukh$IVAL2w3_i[dataukh$IHRECNTw3_i== "Yes" & dataukh$IValB2W3 %in% c("Does not apply", "Do not know", "Don't know", "Refusal","No answer","Error/Partial") &  dataukh$IHRCNUMw3_i >1 & dataukh$IVAL2w3_i %in% c(-6,-8,-9)])

inhrcnt2w3 <- NROW(dataukh$IVAL2w3_i[dataukh$IHRECNTw3_i== "Yes" &  dataukh$IHRCNUMw3_i >1])

sharemissinhrcnt2w3 <- missinhrcnt2w3/inhrcnt2w3

missinhrcnt2w3
inhrcnt2w3
sharemissinhrcnt2w3

Imputed <- NROW(dataukh$IVAL2w3_i [dataukh$IHRECNTw3_i == "Yes" & dataukh$IVAL2w3_iflag == "Imputed"])

#Now at the household level

missinhrcnt2w3h <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i== "Yes" & dataukh$IValB2W3 %in% c("Does not apply", "Do not know", "Don't know", "Refusal","No answer","Error/Partial" ) &  dataukh$IHRCNUMw3_i >1 & dataukh$IVAL2w3_i %in% c(-6,-8,-9)]))

inhrcnt2w3h <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i== "Yes" &  dataukh$IHRCNUMw3_i > 1]))

sharemissinhrcnt2w3h <- missinhrcnt2w3h/inhrcnt2w3h

inhrcnt2w3h
missinhrcnt2w3h
sharemissinhrcnt2w3h

Imputed <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i == "Yes" & dataukh$IVAL2w3_iflag == "Imputed"]))


#WE CHECK THAT FOR THE THIRD INHERITANCE VARIABLE

missinhrcnt3w3 <- NROW(dataukh$IVAL3w3_i[dataukh$IHRECNTw3_i== "Yes" & dataukh$IValB3W3 %in% c("Does not apply", "Do not know", "Don't know", "Refusal","No answer","Error/Partial" ) &  dataukh$IHRCNUMw3_i >1 & dataukh$IVAL3w3_i %in% c(-6,-8,-9)])

inhrcnt3w3 <- NROW(dataukh$IVAL3w3_i[dataukh$IHRECNTw3_i== "Yes" &  dataukh$IHRCNUMw3_i >2])

sharemissinhrcnt3w3 <- missinhrcnt3w3/inhrcnt3w3

inhrcnt3w3
missinhrcnt3w3
sharemissinhrcnt3w3

Imputed <- NROW(dataukh$IVAL3w3_i [dataukh$IHRECNTw3_i == "Yes" & dataukh$IVAL3w3_iflag == "Imputed"])

#Now at the household level

missinhrcnt3w3h <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i== "Yes" & dataukh$IValB3W3 %in% c("Does not apply", "Do not know", "Don't know", "Refusal","No answer","Error/Partial" ) &  dataukh$IHRCNUMw3_i >1 & dataukh$IVAL3w3_i %in% c(-6,-8,-9)]))

inhrcnt3w3h <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i== "Yes" &  dataukh$IHRCNUMw3_i > 2]))

sharemissinhrcnt3w3h <- missinhrcnt3w3h/inhrcnt3w3h

inhrcnt3w3h
missinhrcnt3w3h
sharemissinhrcnt3w3h

Imputed <- NROW(unique(dataukh$CaseW3[dataukh$IHRECNTw3_i == "Yes" & dataukh$IVAL3w3_iflag == "Imputed"]))



#NOW WE SEE HOW MANY GIFTS HAVE BEEN IMPUTED

table (dataukh$IGIFVALw3_iflag)
table(dataukh$ILGIFTw3_i) #Whether they received a Gift
table(dataukh$ILGIFTw3_iflag) #Whether they received a Gift
table(dataukh$DVGiftAnnualw3)

#missgiftsW3 <- NROW (dataukh$DVGiftAnnualw3[dataukh$IGIFVALw3_iflag == "Imputed"])


missgiftsW3 <- NROW (dataukh$DVGiftAnnualw3[dataukh$DVGiftAnnualw3 < 0])
giftsreceived3 <- NROW (dataukh$DVGiftAnnualw3[dataukh$DVGiftAnnualw3 > 0])

sharemissgifts3 <- missgiftsW3/giftsreceived3

missgiftsW3
giftsreceived3
sharemissgifts3

ImputedONS <- NROW (dataukh$DVGiftAnnualw3[dataukh$ILGIFTw3_iflag == "Imputed" & dataukh$DVGiftAnnualw3 > 0]) #We consider imputed all that had the ILGIFTw3 question Imputed.

#AND NOW LET'S SEE THIS FOR THE HOUSEHOLDS

missgiftsW3h <- NROW(unique(dataukh$CaseW3[ dataukh$DVGiftAnnualw3 < 0]))
giftsreceived3h <- NROW(unique(dataukh$CaseW3[dataukh$DVGiftAnnualw3 > 0]))

sharemissgifts3h <- missgiftsW3h/giftsreceived3h

giftsreceived3h
missgiftsW3h
sharemissgifts3h

ImputedONS <- NROW (unique(dataukh$CaseW3[dataukh$ILGIFTw3_iflag == "Imputed" & dataukh$DVGiftAnnualw3 > 0])) #We consider imputed all that had the ILGIFTw3 question Imputed.
