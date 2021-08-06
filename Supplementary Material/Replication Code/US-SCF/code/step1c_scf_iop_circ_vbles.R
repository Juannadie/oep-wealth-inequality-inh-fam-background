#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory for the LAPTOP

options ("scipen"=100, "digits"=10)

library(Hmisc)
library(reldist)
library(tidyverse)

#LET US LOAD THE DATA
dataush <- readRDS(file = "datasets/SCF-2016-all-after-1b.rds")

summary(dataush$YY1) #That is the case ID, identical for all imputation replicates. Could be perhaps used for clustering?
summary(dataush$yy1) #The case ID obtained from the aggregation of variables

summary(dataush$X42001) #X42001 Revised Kennickell-Woodburn consistent weight: accounts for systemative deviations from CPS estimates of homeownership by racial/ethnic Groups.  This weight should be used for all estimations using the final 2016 SCF data for which weights are appropriate.

dataush$weight <- dataush$X42001
dataush$hholdweight <- dataush$X42001

#The replicate weights allow to simulate a bootstrap accounting for clustering and sample design. There is 1000 replicate weights for each household, matching with the Y1 variable. This can allow for better estimation of the standard errors, using the replicate weights or the multiplicity factors.
#scf16repw<-read.dta13("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-16/p16_rw1.dta") #Replicates weights load for the Mac Mini

#### WEALTH, INCOME AND EQUIVALENCE SCALES #####

### WE CREATE THE EQ SCALE ROOT SQUARE ###

#Number of householders (?)
summary(dataush$X101)
#Household size
summary(dataush$X7001) # People in the primary economic unit - WE USE THIS ONE
#SQ Root Equivalent Scale...
dataush$eqscale <- (dataush$X7001)^0.5
summary (dataush$X7001)

#NOW WE MEASURE WEALTH

#We convert the factors to numbers
dataush$networth <- as.numeric(as.character(dataush$networth))
dataush$retliq <- as.numeric(as.character(dataush$retqliq))
dataush$cashli <- as.numeric(as.character(dataush$cashli))

#We rename the main variables
dataush$wealth <- dataush$networth - dataush$retliq - dataush$cashli #LET'S CONVERT NET WORTH TO NET WORTH WITHOUT PENSION PLANS AND POTENTIAL LIFE INSURANCE

#Number of householders (?)
summary(dataush$X101)
#Household size
summary(dataush$X7001) # People in the primary income unit, we sill use this for the equivalence scale analysis

dataush$eqscale <- (dataush$X7001)^0.5
summary(dataush$eqscale)

dataush$adults <- rowSums((dataush[c("X8022", "X104", "X110", "X116", "X122", "X128", "X134", "X204", "X210", "X216", "X222", "X228")]) > 16)

summary(dataush$adults)

dataush$adeqscaleoriginal <- (dataush$adults)^0.5
summary(dataush$adeqscaleoriginal)

dataush$numadults <- dataush$adults

#We make the eq. scale ad-hoc (people after the second count less)

dataush$adeqscale <- 1+(dataush$numadults-1)^.5

#We obtain equivalent wealth
dataush$eqwealth <- dataush$wealth / dataush$adeqscale

#NOW WE MEASURE INCOME
dataush$income <- dataush$X5729 #Household Gross Annual (regular) income
#We obtain equivalent wealth
dataush$eqincome <- dataush$income / dataush$adeqscale

##### GENDER AND HOUSEHOLD HEAD #####

#Table for that
#table also for "reversed"

table (dataush$X8000)
table(dataush$hhsex, dataush$married)
table(dataush$hhsex, dataush$X8000)

#WE COULD PROBABLY CLASSIFY AS "FEMALE" HEADS THOSE HOUSEHOLDS WHERE X8000 = 1, BECAUSE IT MEANS THE WOMAN WAS RESPONSIBLE OF THE ECONOMIC AFFAIRS (WE DO NOT HAVE DISCLOSURE ABOUT THE INCOME INFORMATION WITHIN THE HOUSEHOLD, SO THAT SHOULD BE EQUIVALENT TO THE "REFERENCE PERSON" IN OTHER HOUSEHOLDS)

#ATENTION!! IF HHSEX = 1 (MALE) BUT THE HOUSEHOLD REFERENCE PERSON (THE ONE WHO ANSWERS THE QUESTIONNAIRE) HAS BEEN REVERSED (X8000 == 1), THEN WE ASSUME THE REFERENCE PERSON FOR ECONOMIC MATTERS IN THE HOUSEHOLD IS A WOMAN, AND CHANGE THE SEX (WE WOULD DO VICEVERSA, BUT THERE ARE NO CASES BECAUSE BY DEFAULT IF THERE IS A MIXED COUPLE THE HH IS MALE). ALSO IF THERE IS A SAME-SEX MARRIAGE WE DO NOT REVERSE THE SEX EVEN IF THE HOUSEHOLD HEAD IS REVERSED (THAT WOULD MEAN IT IS THE YOUNGER PARTNER ANSWERING)

#OUR GENDER VARIABLE REFLECTS THE GENDER OF THE RESPONSIBLE PERSON IN FINANCIAL MATTERS (CONSISTENT WITH HFCS AND WAS; CHECK)

summary(dataush$X8021) #Sex Variable of head

dataush$sex <- dataush$X8021
dataush$sex[dataush$X8000 == 1] <- dataush$X103[dataush$X8000 == 1] #if reversed we use sex of partner (who is the head)

table(dataush$X8021)
table(dataush$X103)
table (dataush$sex)

### AGE OF THE HOUSEHOLD HEAD ####

#IF THE HOUSEHOLD AND THE PARTNER HAVE BEEN REVERSED, WE USE THE SPOUSE'S AGE (X19)

dataush$age <- dataush$X14
dataush$age[dataush$X8000 == 1] <- dataush$X19[dataush$X8000 == 1]

#This is the variable for the reconciled age of the respondent/partner  (the one to be used in the analysis)
summary (dataush$X14)
summary (dataush$X19)
summary(dataush$age)

### THE INHERITANCE VALUES OBTAINED IN THE PREVIOUS SECTION  ###

dataush$inh <- dataush$inhtotal
dataush$inhonly <- dataush$dataush$inhonly
dataush$giftonly <- dataush$giftonly + dataush$inhtrustonly #Trusts included in the calculation

#WE CONVERT TO ZERO (NO INHERITANCE RECEIVED) THOSE INHERITANCES UNDER 5000

#dataush$inh[dataush$houseinhgrossinput<5000] <-0

  ### AGE AND WEIGHT ARE RENAMED HERE TOO ###

summary (dataush$age[dataush$inh>0])

summary (dataush$age[dataush$inh>5000])

summary (dataush$age)
plot(density(dataush$age[dataush$inh>0]))
plot(density(dataush$age[dataush$inh<=0]))

#####   #NOW ABOUT PARENTAL EDUCATION ##########

#FIRST ABOUT THE MOTHER

#X6032(#1)       What is the highest level of school or the highest degree she
#X6132(#2)       completed?

table(dataush$X6032) #Education level of the mother: -1, less than primary; 7, less than secondary; 8, completed secondary; 9, some tertiary (some college, vocational and associate) ; 12, bachellors or higher
table(dataush$X6132) #Education of the household's partner mother (0 = no spouse or partner)

#RECORD THE HIGHEST LEVEL OF EDUCATION COMPLETED, NOT THE TIME IT TOOK TO COMPLETE IT.  DO NOT INCLUDE TRADE SCHOOLS AS COLLEGE.

#    1.    *1st, 2nd, 3rd, or 4th grade
#     2.    *5th or 6th grade
#     3.    *7th and 8th grade
#     4.    *9th grade
#     5.    *10th grade
#     6.    *11th grade
#     7.    *12th grade, no diploma
#     8.    *High school graduate - high school diploma or equivalent
#     9.    *Some college but no degree
#     10.    *Associate degree in college - occupation/vocation program
#     11.    *Associate degree in college - academic program
#     12.    *Bachelor's degree (for example: BA, AB, BS)
#     13.    *Master's degree ( for exmaple: MA, MS, MENG, MED, MSW, MBA)
#     14.    *Professional school degree (for example: MD, DDS, DVM, LLB, JD)
#     15.    *Doctorate degree (for example: PHD, EDD)
#     -1.    *Less than 1st grade
#     0.     Inap. (no spouse/partner;)
#     *********************************************************
#     FOR THE PUBLIC DATA SET, CODES 2, 3, 4, 5, 6, AND 7
#     ARE COMBINED WITH CODE 1; CODE 10 AND CODE 11 ARE
#     COMBINED WITH CODE 9, AND; CODES 13, 14, AND 15 ARE
#     COMBINED WITH CODE 12 (CODE 8 IS SEPARATE)
#     *********************************************************

#NOW THE SAME ABOUT THE FATHER

#X6033(#1) HIGHEST LEVEL FOR RESPONDENT'S FATHER
#X6133(#2) HIGHEST LEVEL FOR RESPONDENT'S HUSBAND/WIFE FATHER

table(dataush$X6033) #Education level of the father: -1, less than primary; 7, less than secondary; 8, completed secondary; 9, some tertiary (some college, vocational and associate) ; 12, bachellors or higher
table(dataush$X6133) #Education of the household's partner father (0 = no spouse or partner)


#We make variables grouping education in numbers...
#1 = Less than secondary school
#2 = Secondary school finished
#3 = Some college - associate
#4 = College degree or higher

dataush$edumomhead <- NA
dataush$edumomhead [dataush$X6032 == "-1"] <- 1
dataush$edumomhead [dataush$X6032 == "1"] <- 1
dataush$edumomhead [dataush$X6032 == "8"] <- 2
dataush$edumomhead [dataush$X6032 == "9"] <- 3
dataush$edumomhead [dataush$X6032 == "12"] <- 4

dataush$edudadhead <- NA
dataush$edudadhead [dataush$X6033 == "-1"] <- 1
dataush$edudadhead [dataush$X6033 == "1"] <- 1
dataush$edudadhead [dataush$X6033 == "8"] <- 2
dataush$edudadhead [dataush$X6033 == "9"] <- 3
dataush$edudadhead [dataush$X6033 == "12"] <- 4

table (dataush$edumomhead)
table (dataush$edudadhead)

#We select the maximum education using pmax

dataush$maxeduparenthead <- pmax(dataush$edudadhead, dataush$edumomhead, na.rm = T)

table (dataush$maxeduparenthead)

## WE ALSO REPEAT THAT FOR THE SPOUSE PARENTS (VARIABLES X6132 AND X6133 FOR MOTHER AND FATHER)

table(dataush$X6033) #Education level of the father: -1, less than primary; 7, less than secondary; 8, completed secondary; 9, some tertiary (some college, vocational and associate) ; 12, bachellors or higher
table(dataush$X6133) #Education of the household's partner father (0 = no spouse or partner)

#We make variables grouping education in numbers...
#1 = Less than secondary school
#2 = Secondary school finished
#3 = Some college - associate
#4 = College degree or higher

dataush$edumompartner <- NA
dataush$edumompartner [dataush$X6132 == "-1"] <- 1
dataush$edumompartner [dataush$X6132 == "1"] <- 1
dataush$edumompartner [dataush$X6132 == "8"] <- 2
dataush$edumompartner [dataush$X6132 == "9"] <- 3
dataush$edumompartner [dataush$X6132 == "12"] <- 4

dataush$edudadpartner <- NA
dataush$edudadpartner [dataush$X6133 == "-1"] <- 1
dataush$edudadpartner [dataush$X6133 == "1"] <- 1
dataush$edudadpartner [dataush$X6133 == "8"] <- 2
dataush$edudadpartner [dataush$X6133 == "9"] <- 3
dataush$edudadpartner [dataush$X6133 == "12"] <- 4

table (dataush$edumompartner)
table (dataush$edudadpartner)

#We select the maximum education using pmax

dataush$maxeduparentpartner <- pmax(dataush$edudadpartner, dataush$edumompartner, na.rm = T)

table (dataush$maxeduparentpartner)

#And now we proceed to give the maxeducation depending on wether the head is reversed or not

dataush$maxeduparent <- dataush$maxeduparenthead
dataush$maxeduparent[dataush$X8000 == 1] <- dataush$maxeduparentpartner[dataush$X8000 == 1]

table (dataush$maxeduparenthead)
table (dataush$maxeduparentpartner)

table (dataush$maxeduparent)

#We can include also the race variable

table (dataush$X6809) #Race of the head (1-white, 2-black, 3-hispanic/latino, -7 other)

#This question was only asked of the designated respondend, it is already reversed.

table(dataush$race) #is the same, but in this case 1-white, 2-black, 3-hispanic, 5-other







####

saveRDS(dataush, file = "datasets/SCF-2016-all-after-1c.rds")

#####




