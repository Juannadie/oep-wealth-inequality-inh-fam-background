### LET US ANALYSE THE VARIABLES IN WAS AND THEIR MISSING VALUES #
### THEN WE WILL PROCEED TO IMPUTATION ####

library(foreign)
options ("scipen"=100, "digits"=4)
library(tidyverse)
library (mice)

#For Download for Drive (LAPTOP DIRECTORY)
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code") 

alldatawas <- readRDS(file = "data_rds/WAS-After-Step1a-brackets-new-data.rds")

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/imp2") #Set Working Directory for the LAPTOP

#WE ARE GOING TO IMPUTE THE MISSING DATA POR IEValW1, THE VALUE OF THE INHERITANCES RECEIVED PREVIOUS TO THE PAST 5 YEARS.

#WE WILL SUBSET THE SAMPLE TO THE FOLLOWING CRITERIA:

#ONLY INDIVIDUALS WHO HAVE HAVE RECEIVED THESE INHERITANCES (IHEvW1 == "Yes")
#ONLY INDIVIDUALS WHO DO NOT LATER REPORT THAT AMOUNT IN BRACKETS [alldatawas$IEValBW1 == "Does not apply"]. THERE IS STILL 612 CASES MISSING

#THEN WE WILL SELECT THE VARIABLES THAT WE ARE GOING TO USE FOR THE IMPUTATION
#ALL THE CASE IDS VARIABLES FOR FUTURE MATCHING (OR AT LEAST ONE), AGE, YEAR THE INHERITANCE WAS RECEIVED,

#WE DON'T HAVE THE YEAR IT WAS RECEIVED (ALSO CODED ERROR, SO WE HAVE TO USE THE UPDATED VALUE)

#WE GO FOR A SIMPLE SPECIFICATION OF THE MODEL

#Let's try the simple model
alldatawasinputsimple1 <- subset (alldatawas, IHEvW1 == "Yes" & IEValBW1 %in% c("Do not know", "Refusal" , "Not applicable"), select = c(CaseW3, personW3, IEValW1, DVAge17W1, IHrcnumW1, IHFutW1, IHEvNoW1, Ten1W1))

#We code -9, -7 and -6 as NA
alldatawasinputsimple1$IEValW1 [alldatawasinputsimple1$IEValW1 <0 ] <- NA
summary(alldatawasinputsimple1$IEValW1)

#These two variables do not work well as factors in the model (not solving), and also do not work well substituting for NAs
alldatawasinputsimple1$IHEvNoW1[alldatawasinputsimple1$IHEvNoW1<0] <- 0
alldatawasinputsimple1$IHrcnumW1[alldatawasinputsimple1$IHrcnumW1<0] <- 0

#Create Pred Manually
pred <- matrix(0, nrow=8, ncol=8)
#rownames(pred) <-
pred[3,4:8]<-1 #ID variables are excluded from the model, of course
pred

#And now we re-do the imputation with the new predictor matrix

imp <- mice(alldatawasinputsimple1, m=5, method = c("","","pmm","", "", "", "",""), predictorMatrix = pred, maxit = 10, seed = 555) #I'm only imputing my third variable, which is the third column in my dataset


summary(imp$imp$IEValW1$`2`)
attributes(imp$imp$IEValW1)

c.broad <- complete(imp, "broad")

c.broad$inputlife1basic <- (c.broad$IEValW1.2)

c.broad <- cbind (c.broad, alldatawasinputsimple1)

c.broad$flaginputlife1basic <- 0
c.broad$flaginputlife1basic[is.na(c.broad$IEValW1)] <- 1
NROW(c.broad$flaginputlife1basic[is.na(c.broad$IEValW1)])

summary (c.broad$inputlife1basic[c.broad$flaginputlife1basic == 1])
summary (c.broad$inputlife1basic[c.broad$flaginputlife1basic == 0])
sd (c.broad$inputlife1basic[c.broad$flaginputlife1basic == 1])
sd (c.broad$inputlife1basic[c.broad$flaginputlife1basic == 0])
summary (c.broad$inputlife1basic)
summary(alldatawasinputsimple1$IEValW1)

plot(imp)


#We subset the results

lifeinput1basic <- subset(c.broad, select = c(CaseW3.2, personW3.2, inputlife1basic, flaginputlife1basic))


#AND WE MERGED THE IMPUTED VARIABLE AND THE FLAG TO THE MAIN DATASET

alldatawascomplet <- merge (alldatawas, lifeinput1basic, by.x = c("CaseW3", "personW3"), by.y = c("CaseW3.2", "personW3.2"), all.x = T)

### LET'S TRY SECOND LIFETIME INHERITANCE ####

#Let's try the simple model
alldatawasinputsimple2 <- subset (alldatawas, IHEvW1 == "Yes" & IHEvNoW1 >1 & IEValB2W1 %in% c("Do not know", "Refusal" , "Not applicable"), select = c(CaseW3, personW3, IEVal2W1, DVAge17W1,  IHFutW1, IHrcnumW1, Ten1W1))

#We code -9, -7 and -6 as NA
alldatawasinputsimple2$IEVal2W1 [alldatawasinputsimple2$IEVal2W1 <0 ] <- NA
summary(alldatawasinputsimple2$IEVal2W1)

alldatawasinputsimple2$IHrcnumW1[alldatawasinputsimple2$IHrcnumW1<0] <- 0

#Create Pred Manually
pred <- matrix(0, nrow=7, ncol=7) #One less dimension because we do not use the number of inheritances
pred[3,4:7]<-1
pred

#And now we re-do the imputation with the new predictor matrix

#imp <- mice(alldatawasinputsimple2, method = c("","","pmm","", "", "",""), predictorMatrix = pred, maxit = 5, seed = 444) #I'm only imputing my third variable, which is the third column in my dataset

imp <- mice(alldatawasinputsimple2, method = c("","","pmm","", "", "",""), predictorMatrix = pred, maxit = 5, seed = 555) #I'm only imputing my third variable, which is the third column in my dataset

summary(imp$imp$IEVal2W1$`2`)
attributes(imp$imp$IEVal2W1)

c.broad <- complete(imp, "broad")

c.broad$inputlife2basic <- (c.broad$IEVal2W1.2)


c.broad <- cbind (c.broad, alldatawasinputsimple2)

c.broad$flaginputlife2basic <- 0
c.broad$flaginputlife2basic[is.na(c.broad$IEVal2W1)] <- 1
NROW(c.broad$flaginputlife2basic[is.na(c.broad$IEVal2W1)])


summary (c.broad$inputlife2basic[c.broad$flaginputlife2basic == 1])
summary (c.broad$inputlife2basic[c.broad$flaginputlife2basic == 0])
sd (c.broad$inputlife2basic[c.broad$flaginputlife2basic == 1])
sd (c.broad$inputlife2basic[c.broad$flaginputlife2basic == 0])
summary (c.broad$inputlife2basic)
summary(alldatawasinputsimple2$IEVal2W1)

plot(imp)

##NICE WE COULD IMPUT THESE AS WELL##

#We subset the results

lifeinput2basic <- subset(c.broad, select = c(CaseW3.2, personW3.2, inputlife2basic, flaginputlife2basic))


#AND WE MERGED THE IMPUTED VARIABLE AND THE FLAG TO THE MAIN DATASET

alldatawascomplet <- merge (alldatawascomplet, lifeinput2basic, by.x = c("CaseW3", "personW3"), by.y = c("CaseW3.2", "personW3.2"), all.x = T)


##### NOW LET US REPEAT THIS WITH THE VARIABLE THAT HAS MISSING RECENT INHERITANCES ###

#WE GO FOR A SIMPLE SPECIFICATION OF THE MODEL

#Let's try the simple model
alldatawasinputsimplerecent1 <- subset (alldatawas, IHrecntW1 == "Yes" & IEValBW1 %in% c("Do not know", "Refusal" , "Not applicable"), select = c(CaseW3, personW3, IValW1, DVAge17W1, IHrcnumW1, IHFutW1, IHEvNoW1,Ten1W1))

#We code -9, -7 and -6 as NA
alldatawasinputsimplerecent1$IValW1 [alldatawasinputsimplerecent1$IValW1 <0 ] <- NA
summary(alldatawasinputsimplerecent1$IValW1)

alldatawasinputsimplerecent1$IHEvNoW1[alldatawasinputsimplerecent1$IHEvNoW1<0] <- 0
alldatawasinputsimplerecent1$IHrcnumW1[alldatawasinputsimplerecent1$IHrcnumW1<0] <- 0
summary(alldatawasinputsimplerecent1$CaseW3)
summary(alldatawasinputsimplerecent1$personW3)
summary(alldatawasinputsimplerecent1$DVAge17W1)
summary(alldatawasinputsimplerecent1$IHrcnumW1)
summary(alldatawasinputsimplerecent1$IHFutW1)
summary(alldatawasinputsimplerecent1$Ten1W1)


#Create Pred Manually
pred <- matrix(0, nrow=8, ncol=8)
#rownames(pred) <-
pred[3,4:8]<-1
pred

#And now we re-do the imputation with the new predictor matrix

imp <- mice(alldatawasinputsimplerecent1, method = c("","","pmm","", "", "", "",""), predictorMatrix = pred, maxit = 5, seed = 555) #I'm only imputing my third variable, which is the third column in my dataset

summary(imp$imp$IValW1$`2`)
attributes(imp$imp$IValW1)

c.broad <- complete(imp, "broad")

c.broad$inputrecent1basic <- (c.broad$IValW1.2)

c.broad <- cbind (c.broad, alldatawasinputsimplerecent1)

c.broad$flaginputrecent1basic <- 0
c.broad$flaginputrecent1basic[is.na(c.broad$IValW1)] <- 1
NROW(c.broad$flaginputrecent1basic[is.na(c.broad$IValW1)])

summary (c.broad$inputrecent1basic[c.broad$flaginputrecent1basic == 1])
summary (c.broad$inputrecent1basic[c.broad$flaginputrecent1basic == 0])
sd (c.broad$inputrecent1basic[c.broad$flaginputrecent1basic == 1])
sd (c.broad$inputrecent1basic[c.broad$flaginputrecent1basic == 0])
summary (c.broad$inputrecent1basic)
summary(alldatawasinputsimplerecent1$IValW1)

plot(imp)

#AND WE ADD IT TO THE MAIN DATA

#We subset the results

recentinput1basic <- subset(c.broad, select = c(CaseW3.2, personW3.2, inputrecent1basic, flaginputrecent1basic))

#AND WE MERGED THE IMPUTED VARIABLE AND THE FLAG TO THE MAIN DATASET

alldatawascomplet <- merge (alldatawascomplet, recentinput1basic, by.x = c("CaseW3", "personW3"), by.y = c("CaseW3.2", "personW3.2"), all.x = T)


#Check on the effect of averaging imputations
summary(imp$imp$IValW1$`1`) #First Imputation
summary(imp$imp$IValW1$`2`) #Second Imputation
summary(imp$imp$IValW1$`3`) #...
summary(imp$imp$IValW1$`4`)
summary(imp$imp$IValW1$`5`)

summary (c.broad$inputrecent1basic[c.broad$flaginputrecent1basic == 1]) #Average of imputed values

summary(alldatawasinputsimplerecent1$IValW1) #Original Observed Variables




##### SECOND RECENT INHERITANCE ##############

#Let's see the second recent inheritance reported
table (alldatawas$IVal2W1[alldatawas$IHrecntW1 == "Yes"])

#A lot of missing, let's try to impute them

##### NOW LET US REPEAT THIS WITH THE VARIABLE THAT HAS MISSING RECENT INHERITANCES ###

#WE GO FOR A SIMPLE SPECIFICATION OF THE MODEL

#Let's try the simple model
alldatawasinputsimplerecent2 <- subset (alldatawas, IHrecntW1 == "Yes" & IHrcnumW1 >1 & IValB2W1  %in% c("Do not know", "Refusal" , "Not applicable"), select = c(CaseW3, personW3, IVal2W1, DVAge17W1,  IHFutW1, IHEvNoW1, Ten1W1))

#We code -9, -7 and -6 as NA
alldatawasinputsimplerecent2$IVal2W1 [alldatawasinputsimplerecent2$IVal2W1 <0 ] <- NA
summary(alldatawasinputsimplerecent2$IVal2W1)

alldatawasinputsimplerecent2$IHEvNoW1[alldatawasinputsimplerecent2$IHEvNoW1<0] <- 0


#Create Pred Manually
pred <- matrix(0, nrow=7, ncol=7) #One less dimension because we do not use the number of inheritances
pred[3,4:7]<-1
pred

#And now we re-do the imputation with the new predictor matrix

imp <- mice(alldatawasinputsimplerecent2, method = c("","","pmm","", "", "", ""), predictorMatrix = pred, maxit = 5, seed = 444) #I'm only imputing my third variable, which is the third column in my dataset

summary(imp$imp$IVal2W1$`2`)
attributes(imp$imp$IVal2W1)

c.broad <- complete(imp, "broad")

c.broad$inputrecent2basic <- (c.broad$IVal2W1.2)

c.broad <- cbind (c.broad, alldatawasinputsimplerecent2)

c.broad$flaginputrecent2basic <- 0
c.broad$flaginputrecent2basic[is.na(c.broad$IVal2W1)] <- 1
NROW(c.broad$flaginputrecent2basic[is.na(c.broad$IVal2W1)])

summary (c.broad$inputrecent2basic[c.broad$flaginputrecent2basic == 1])
summary (c.broad$inputrecent2basic[c.broad$flaginputrecent2basic == 0])
sd (c.broad$inputrecent2basic[c.broad$flaginputrecent2basic == 1])
sd (c.broad$inputrecent2basic[c.broad$flaginputrecent2basic == 0])
summary (c.broad$inputrecent2basic)
summary(alldatawasinputsimplerecent2$IVal2W1)

plot(imp)

##AND WE ADD IT TO THE MAIN DATA

#We subset the results

recentinput2basic <- subset(c.broad, select = c(CaseW3.2, personW3.2, inputrecent2basic, flaginputrecent2basic))


#AND WE MERGED THE IMPUTED VARIABLE AND THE FLAG TO THE MAIN DATASET

alldatawascomplet <- merge (alldatawascomplet, recentinput2basic, by.x = c("CaseW3", "personW3"), by.y = c("CaseW3.2", "personW3.2"), all.x = T)



### Now we proceed to do the same with the GIFTS ######

table(alldatawas$ILgiftW1) #Whether they received a Gift
table(alldatawas$IGifvalW1[alldatawas$ILgiftW1 =="Yes, received goods"])
table(alldatawas$IGifvalW1[alldatawas$ILgiftW1 =="Yes, received cash gift(s)"])


saveRDS(alldatawascomplet, file = "data_rds/WAS-Nov-After-v-3-2-Step2-Imputation-Pre-Update-new-data.rds")


