#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

#We can read the data
datafr <- readRDS(file = "datasets/HFCS14_France_after_step_2a_imp4.rds") ##

### ONLY FOR THE SHARE OF THE SAMPLE WHO ARE HEADS OF THE HOUSEHOLD ###

#If only taking into account the head of the household (to have only one observation per household)
datafrh <- datafr[datafr$RA0100 == 1,]

### LET'S CONVERT THE CATEGORIES FILE FROM THE US TO THE FRANCE DATA ####

options ("scipen"=100, "digits"=10)


library(Hmisc)
library(reldist)
library(tidyverse)

#WEIGHT
datafrh$weight <- datafrh$HW0010.y

#### WEALTH, INCOME AND EQUIVALENCE SCALES #####




### WE CREATE THE EQ SCALE ROOT SQUARE ###

#Number of household members
summary(datafrh$DH0001)

#SQ Root Equivalent Scale...
datafrh$eqscaleoriginal <- (datafrh$DH0001)^0.5
summary (datafrh$DH0001)

datafrh$numadults <- datafrh$DH0006

#We make the eq. scale ad-hoc (people after the second count less)

datafrh$adeqscale <- 1+(datafrh$numadults-1)^.5


#NOW WE MEASURE WEALTH

#We rename the main variables, to get wealth without pensions

datafrh$wealth <- as.numeric(as.character(datafrh$DN3001))
datafrh$wealth [is.na(datafrh$wealth)] <- 0

######## We first convert inheritances and wealth to USD 2010 ######


#ALREADY PUBLIC AND OCCUPATIONAL PENSIONS ARE OUT
summary(datafrh$wealth)

#AND WE TAKE OUT VOLUNTARY PENSION PLANS / LIFE INSURANCE
summary(datafrh$DA2109)
datafrh$voluntpension <- datafrh$DA2109
datafrh$voluntpension[is.na(datafrh$voluntpension)] <- 0
summary(datafrh$voluntpension)

datafrh$wealth <- datafrh$wealth - datafrh$voluntpension #Wealth without any pension


datafrh$wealth <- datafrh$wealth * 1.1225

datafrh$eqwealth <- datafrh$wealth/datafrh$adeqscale

summary(datafrh$wealth)

#We obtain equivalent wealth
#datafrh$eqwealth <- datafrh$wealth / datafrh$eqscale
#datafrh$eqwealth <- datafrh$wealth / datafrh$adeqscale #We use adults equivalence scale


#NOW WE MEASURE INCOME
datafrh$income <- as.numeric(as.character(datafrh$DI2000)) #Household Gross Annual (regular) income
#We obtain equivalent wealth
#datafrh$eqincome <- datafrh$income/datafrh$eqscale
datafrh$eqincome <- datafrh$income/datafrh$adeqscale


##### GENDER AND HOUSEHOLD HEAD #####

datafrh$sex <- datafrh$RA0200
table (datafrh$sex)

### AGE OF THE HOUSEHOLD HEAD ####

datafrh$age <- datafrh$RA0300

### THE INHERITANCE VALUES OBTAINED IN THE PREVIOUS SECTION  ###

datafrh$inh <- datafrh$inhupd3
datafrh$inhonly <- datafrh$inhonlyupd3
datafrh$giftonly <- datafrh$giftonlyupd3

datafrh$inh <- datafrh$inh * 1.1225


### ALSO INHERITANCES CAN BE EQUIVALISED

#datafrh$eqinh <- datafrh$inh/datafrh$eqscale
#datafrh$eqinhonly <- datafrh$inhonly/datafrh$eqscale
#datafrh$eqgiftonly <- datafrh$giftonly/datafrh$eqscale

datafrh$eqinh <- datafrh$inh/datafrh$adeqscale
datafrh$eqinhonly <- datafrh$inhonly/datafrh$adeqscale
datafrh$eqgiftonly <- datafrh$giftonly/datafrh$adeqscale

## Some descriptives ##
summary (datafrh$age[datafrh$inh>0])
summary (datafrh$age[datafrh$inh>5000])

summary (datafrh$age)
plot(density(datafrh$age[datafrh$inh>0]))
plot(density(datafrh$age[datafrh$inh<=0]))

#### NOW ABOUT PARENTAL OCCUPATION ####

##OCCUPATION OF PARENTS IN FRANCE##

table(datafrh$PNA0700, datafrh$SA0100) #Spain, France and Portugal only have the occupation of the father.
table(datafrh$PNA0701, datafrh$SA0100) #The same three have the occupation of the mother

table(datafrh$PNA0600a, datafrh$SA0100) #Only Italy has the education of the father
table(datafrh$PNA0600b, datafrh$SA0100) #No education of the mother


#####   #NOW ABOUT PARENTAL OCCUPATION ##########

## WE BELIEVE THE FRENCH CODING IS FOLLOWING THIS:

# 10. AGRICULTURE (EXCLUDED, DON'T KNOW WHY)  --- IS 60 in HFCS (Skilled Agriculture)
# 20. TRADE, CRAFT                            --  CRAFT and MANUAL WORKERS (70 in HFCS)
# 30. MANAGER                                 --  30, 40 and 50 collapsed to 20 in HFCS (professionals)
# 40. PROFESSIONAL
# 50. MID LEVEL WORKER (CADRE) (EXCLUDED
# 60. PROFESSIONAL INTERMEDIATE               -- 30 in HFCS (technitians and associate professionals)
# 70. EMPLOYEE                                -- 70 CLERICAL AND SUPPORT in HFCS
# 80. MANUAL WORKER                           -- 80 in HFCS (Plant and machine operators)

#We make variables grouping education in numbers...
#1 = MANUAL WORKER (80)
#2 = EMPLOYEE AND INTERMEDIATE PROFESSIONAL (60 AND 70)
#3 = TRADE, CRAFT (20)
#4 = MANAGER AND PROFESSIONAL (30 AND 40)

#WE ONLY CLASSIFY IN THE OCCUPATION OF THE FATHER, FOR IT IS NOT AVAILABLE FOR THE MOTHER IN MANY CASES AND IT IS NOT CLEARLY AN ORDINAL VARIABLE, UNLIKE EDUCATIONAL LEVEL. (PUT EXAMPLES)

summary(datafrh$PNA0700)
summary(datafrh$PNA0701[is.na(datafrh$PNA0700)])

datafrh$occdadhead <- NA
datafrh$occdadhead [datafrh$PNA0700 == "80"] <- 1 #Manual Worker (Plant and machine operators name in HFCS)
datafrh$occdadhead [datafrh$PNA0700 == "60"] <- 1 #Agriculture (Skilled Agricultural name in HFCS)
datafrh$occdadhead [datafrh$PNA0700 == "40"] <- 2 #Employees (Clerical and Support in HFCS)
datafrh$occdadhead [datafrh$PNA0700 == "70"] <- 3 #Trade Craft (Craft and Manual Workers in HFCS)
datafrh$occdadhead [datafrh$PNA0700 == "30"] <- 3 #Professinal Intermediate (Techncians and professionals)
datafrh$occdadhead [datafrh$PNA0700 == "20"] <- 4 #Manager, Professionals and Mid-level workers
table (datafrh$occdadhead)

#We use mother occupation when father occupation is missing
datafrh$occdadhead [datafrh$PNA0701 == "80" & is.na(datafrh$PNA0700)] <- 1
datafrh$occdadhead [datafrh$PNA0701 == "60" & is.na(datafrh$PNA0700)] <- 1
datafrh$occdadhead [datafrh$PNA0701 == "40" & is.na(datafrh$PNA0700)] <- 2
datafrh$occdadhead [datafrh$PNA0701 == "70" & is.na(datafrh$PNA0700)] <- 3
datafrh$occdadhead [datafrh$PNA0701 == "30" & is.na(datafrh$PNA0700)] <- 3
datafrh$occdadhead [datafrh$PNA0701 == "20" & is.na(datafrh$PNA0700)] <- 4

table (datafrh$occdadhead)
summary (datafrh$occdadhead)

saveRDS(datafrh, file = "datasets/HFCS14_France_after_step_2b-no-pension-ad-eq_imp4.rds")

