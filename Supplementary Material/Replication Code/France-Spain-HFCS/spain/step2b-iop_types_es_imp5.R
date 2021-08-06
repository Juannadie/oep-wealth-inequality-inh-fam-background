#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

#We can read the data
dataes <- readRDS(file = "datasets/spain/HFCS14_Spain_after_step_2a_imp5.rds") ##

### ONLY FOR THE SHARE OF THE SAMPLE WHO ARE HEADS OF THE HOUSEHOLD ###

#If only taking into account the head of the household (to have only one observation per household)
dataesh <- dataes[dataes$RA0100 == 1,]

### LET'S CONVERT THE CATEGORIES FILE FROM THE US TO THE Spain DATA ####

options ("scipen"=100, "digits"=10)


library(Hmisc)
library(reldist)
library(tidyverse)

#WEIGHT
dataesh$weight <- dataesh$HW0010.y

#### WEALTH, INCOME AND EQUIVALENCE SCALES #####

######## We first convert inheritances and wealth to USD 2010 ######







### WE CREATE THE EQ SCALE ROOT SQUARE ###

#Number of household members
summary(dataesh$DH0001)

#SQ Root Equivalent Scale...
dataesh$eqscale <- (dataesh$DH0001)^0.5
summary (dataesh$DH0001)

#And we can have the number of adults (>16)
dataesh$adeqscaleoriginal <- (dataesh$DH0006)^0.5
summary (dataesh$DH0006)

dataesh$numadults <- dataesh$DH0006

#We make the eq. scale ad-hoc (people after the second count less)

dataesh$adeqscale <- 1+(dataesh$numadults-1)^.5



#NOW WE MEASURE WEALTH

#We rename the main variables, to get wealth without pensions

dataesh$wealth <- as.numeric(as.character(dataesh$DN3001))

#ALREADY PUBLIC AND OCCUPATIONAL PENSIONS ARE OUT
summary(dataesh$wealth)

#AND WE TAKE OUT VOLUNTARY PENSION PLANS / LIFE INSURANCE
summary(dataesh$DA2109)
dataesh$voluntpension <- dataesh$DA2109
dataesh$voluntpension[is.na(dataesh$voluntpension)] <- 0
summary(dataesh$voluntpension)

dataesh$wealth <- dataesh$wealth - dataesh$voluntpension #Wealth without any pension


summary(dataesh$wealth)

dataesh$wealth <- dataesh$wealth * 1.1518


#We obtain equivalent wealth
#dataesh$eqwealth <- dataesh$wealth / dataesh$eqscale
dataesh$eqwealth <- dataesh$wealth / dataesh$adeqscale #We use adults equivalence scale


#NOW WE MEASURE INCOME
dataesh$income <- as.numeric(as.character(dataesh$DI2000)) #Household Gross Annual (regular) income
#We obtain equivalent wealth
#dataesh$eqincome <- dataesh$income/dataesh$eqscale
dataesh$eqincome <- dataesh$income/dataesh$adeqscale



##### GENDER AND HOUSEHOLD HEAD #####

dataesh$sex <- dataesh$RA0200
table (dataesh$sex)

### AGE OF THE HOUSEHOLD HEAD ####

dataesh$age <- dataesh$RA0300

### THE INHERITANCE VALUES OBTAINED IN THE PREVIOUS SECTION  ###

dataesh$inh <- dataesh$inhupd3
dataesh$inhonly <- dataesh$inhonlyupd3
dataesh$giftonly <- dataesh$giftonlyupd3

dataesh$inh <- dataesh$inh * 1.1518

### ALSO INHERITANCES CAN BE EQUIVALISED

#dataesh$eqinh <- dataesh$inh/dataesh$eqscale
#dataesh$eqinhonly <- dataesh$inhonly/dataesh$eqscale
#dataesh$eqgiftonly <- dataesh$giftonly/dataesh$eqscale

dataesh$eqinh <- dataesh$inh/dataesh$adeqscale
dataesh$eqinhonly <- dataesh$inhonly/dataesh$adeqscale
dataesh$eqgiftonly <- dataesh$giftonly/dataesh$adeqscale

## Some descriptives ##
summary (dataesh$age[dataesh$inh>0])
summary (dataesh$age[dataesh$inh>5000])

summary (dataesh$age)
plot(density(dataesh$age[dataesh$inh>0]))
plot(density(dataesh$age[dataesh$inh<=0]))

#### NOW ABOUT PARENTAL OCCUPATION ####

##OCCUPATION OF PARENTS IN Spain##

table(dataesh$PNA0700, dataesh$SA0100) #Spain, France and Portugal only have the occupation of the father.
table(dataesh$PNA0701, dataesh$SA0100) #The same three have the occupation of the mother

table(dataesh$PNA0600a, dataesh$SA0100) #Only Italy has the education of the father
table(dataesh$PNA0600b, dataesh$SA0100) #No education of the mother


#####   #NOW ABOUT PARENTAL OCCUPATION ##########

## WE BELIEVE THE ISCO-08 IS AS FOLLOWS

#1 Directores y gerentes
#2 Profesionales cient??ficos e intelectuales
#3 T??cnicos y profesionales de nivel medio
#4 Personal de apoyo administrativo
#5 Trabajadores de los servicios y vendedores de comercios y mercados
#6 Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros
#7 Oficiales, operarios y artesanos de artes mec??nicas y de otros oficios
#8 Operadores de instalaciones y m??quinas y ensambladores
#9 Ocupaciones elementales
#10 Ocupaciones militares


#WE ONLY CLASSIFY IN THE OCCUPATION OF THE FATHER, FOR IT IS NOT AVAILABLE FOR THE MOTHER IN MANY CASES AND IT IS NOT CLEARLY AN ORDINAL VARIABLE, UNLIKE EDUCATIONAL LEVEL. (PUT EXAMPLES)

summary(dataesh$PNA0700)
summary(dataesh$PNA0701[is.na(dataesh$PNA0700)])

dataesh$occdadhead <- NA
dataesh$occdadhead [dataesh$PNA0700 == "90"] <- 1 #Elementary Occupations
dataesh$occdadhead [dataesh$PNA0700 == "60"] <- 1 #Agriculture, fishery
dataesh$occdadhead [dataesh$PNA0700 == "80"] <- 1 #Machine operators, assembly
dataesh$occdadhead [dataesh$PNA0700 == "40"] <- 3 #Support administration staff
dataesh$occdadhead [dataesh$PNA0700 == "50"] <- 3 #Sales and employees
dataesh$occdadhead [dataesh$PNA0700 == "30"] <- 2 #MID LEVEL PROFESSIONALS AND TECHNITIANS
dataesh$occdadhead [dataesh$PNA0700 == "70"] <- 2 #CRAFTMANSHIP, OTHER OCCUPATIONS
dataesh$occdadhead [dataesh$PNA0700 == "20"] <- 4 #HIGH PROFESSIONALS
dataesh$occdadhead [dataesh$PNA0700 == "10"] <- 4 #MANAGERS
table (dataesh$occdadhead)
summary (dataesh$occdadhead)

#We use mother occupation when father occupation is missing
dataesh$occdadhead [dataesh$PNA0701 == "90" & is.na(dataesh$PNA0700)] <- 1 #Elementary Occupations
dataesh$occdadhead [dataesh$PNA0701 == "60" & is.na(dataesh$PNA0700)] <- 1 #Agriculture, fishery
dataesh$occdadhead [dataesh$PNA0701 == "80" & is.na(dataesh$PNA0700)] <- 1 #Machine operators, assembly
dataesh$occdadhead [dataesh$PNA0701 == "40" & is.na(dataesh$PNA0700)] <- 3 #Support administration staff
dataesh$occdadhead [dataesh$PNA0701 == "50" & is.na(dataesh$PNA0700)] <- 3 #Sales and employees
dataesh$occdadhead [dataesh$PNA0701 == "30" & is.na(dataesh$PNA0700)] <- 2 #MID LEVEL PROFESSIONALS AND TECHNITIANS
dataesh$occdadhead [dataesh$PNA0701 == "70" & is.na(dataesh$PNA0700)] <- 2 #CRAFTMANSHIP, OTHER OCCUPATIONS
dataesh$occdadhead [dataesh$PNA0701 == "20" & is.na(dataesh$PNA0700)] <- 4 #HIGH PROFESSIONALS
dataesh$occdadhead [dataesh$PNA0701 == "10" & is.na(dataesh$PNA0700)] <- 4 #MANAGERS

table (dataesh$occdadhead)
summary (dataesh$occdadhead)

saveRDS(dataesh, file = "datasets/spain/HFCS14_Spain_after_step_2b-no-pension-ad-eq_imp5.rds")

