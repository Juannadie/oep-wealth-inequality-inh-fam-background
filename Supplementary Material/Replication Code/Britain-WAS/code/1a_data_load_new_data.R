###IN THIS FIRST STEP WE CREATE THE AGGREGATE DATASET AND GET THE MEAN POINT OF THE AMOUNTS PROVIDED IN BRACKETS###

library(foreign)
library(readstata13)
options ("scipen"=100, "digits"=4)



#For Download for Drive (LAPTOP DIRECTORY)
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code") #Set Working Directory #New May 2020 release with Stata13 files

wasw1h<-read.dta13("data_source/UKDA-7215-stata/stata/stata13_se/was_wave_1_hhold_eul_final.dta")
wasw1p<-read.dta13("data_source/UKDA-7215-stata/stata/stata13_se/was_wave_1_person_eul_final.dta")
wasw2h<-read.dta13("data_source/UKDA-7215-stata/stata/stata13_se/was_wave_2_hhold_eul_final.dta")
wasw2p<-read.dta13("data_source/UKDA-7215-stata/stata/stata13_se/was_wave_2_person_eul_final.dta")
wasw3h<-read.dta13("data_source/UKDA-7215-stata/stata/stata13_se/was_wave_3_hhold_eul_final.dta")
wasw3p<-read.dta13("data_source/UKDA-7215-stata/stata/stata13_se/was_wave_3_person_eul_final.dta")


#Let's see how much many of them were in WAVE 1
#We have to be careful with the capitalization of the variable for the linkage
#We need the case (household) and the person (within the household) for the linkage

summary (wasw5p$casew1)
summary (wasw5p$PersonW1)

summary (wasw1p$CaseW1)
summary (wasw1p$PersonW1)
summary(wasw2p$PersonW1)
summary(wasw2p$case)

#Let's merge all personal databases sequentially

wasw1w2p <- merge(wasw1p,wasw2p,by=c("CaseW1","PersonW1"))
wasw1to3p <- merge(wasw1w2p,wasw3p,by=c("CaseW1","PersonW1", "CaseW2", "PersonW2"))


#When we merge the household file we only use the case, and only with the households files for Wave 3
wasw1to2h <- merge (wasw1h, wasw2h, by=c("CaseW1"))
wasw1to3h <- merge (wasw1to2h, wasw3h, by=c("CaseW1", "CaseW2"))
wasw3full <- merge (wasw1to3p,wasw1to3h,by=c("CaseW1", "CaseW2", "CaseW3"))

saveRDS(wasw3full, file = "data_rds/was-all-w-1-3-waves-wfull-new-data.rds")


### WE CAN START FROM HERE #####

library(foreign)
options ("scipen"=100, "digits"=4)


setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code") #Set Working Directory for the LAPTOP")

alldatawas <- readRDS(file = "data_rds/was-all-w-1-3-waves-wfull-new-data.rds")

##############       WE TURN BRACKETS INTO A (MIDDLE)VALUE      ############


#Lifetime inheritances
#NOW FOR PEOPLE WHO REFUSE TO RELEASE THAT INFORMATION, BRACKETS ARE PROVIDED:

summary (alldatawas$IEValBW1) #Some information in Brackets
summary (alldatawas$IEValB2W1) #Some information in Brackets
levels (alldatawas$IEValBW1)
levels (alldatawas$IEValB2W1)
#levels (alldatawas$IValB2W4)
#levels (alldatawas$IValB1W5)

#So let us create the new numeric variable with a fixed amount for all of those providing the bracket. We would go for the mean value of the bracket.

#FOR PAST INHERITANCES OLDER THAN 5 YEARS OLD

#NOTICE THE WRONG CODE £1,000 OR £1,000 = 1,000

alldatawas$bracked1inhW1past <- 0
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "Less than £1,000"] <-  500
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IEValW1[alldatawas$IEValW1 > 250000])
alldatawas$bracked1inhW1past [alldatawas$IEValBW1 == "£250,000 or more"] <-  a
summary (alldatawas$bracked1inhW1past)
summary (alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0])
NROW (alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0])


#AND NOW FOR THE SECOND INHERITANCE BRACKETED
alldatawas$bracked2inhW1past <- 0
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "Less than £1,000"] <-  500
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IEVal2W1[alldatawas$IEVal2W1 > 250000])
alldatawas$bracked2inhW1past [alldatawas$IEValB2W1 == "£250,000 or more"] <-  a
summary (alldatawas$bracked2inhW1past[alldatawas$bracked2inhW1past>0])
NROW (alldatawas$bracked2inhW1past[alldatawas$bracked2inhW1past>0])

#AND NOW FOR THE THIRD INHERITANCE BRACKETED
alldatawas$bracked3inhW1past <- 0
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "Less than £1,000"] <-  500
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IEVal3W1[alldatawas$IEVal3W1 > 250000])
alldatawas$bracked3inhW1past [alldatawas$IEValB3W1 == "£250,000 or more"] <-  a
summary (alldatawas$bracked3inhW1past)
summary (alldatawas$bracked3inhW1past[alldatawas$bracked3inhW1past>0])
NROW (alldatawas$bracked3inhW1past[alldatawas$bracked3inhW1past>0])

#Let's see the bracketed variables
summary (alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0])
summary (alldatawas$bracked2inhW1past[alldatawas$bracked2inhW1past>0])
summary (alldatawas$bracked3inhW1past[alldatawas$bracked3inhW1past>0])

NROW (alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0])
NROW (alldatawas$bracked2inhW1past[alldatawas$bracked2inhW1past>0])
NROW (alldatawas$bracked3inhW1past[alldatawas$bracked3inhW1past>0])

#And the reported directly without brackets
summary (alldatawas$IEValW1[alldatawas$IEValW1>0])
summary (alldatawas$IEVal2W1[alldatawas$IEVal2W1>0])
summary (alldatawas$IEVal3W1[alldatawas$IEVal3W1>0])

NROW (alldatawas$IEValW1[alldatawas$IEValW1>0])
NROW (alldatawas$IEVal2W1[alldatawas$IEVal2W1>0])
NROW (alldatawas$IEVal3W1[alldatawas$IEVal3W1>0])


#WE ALSO NEED TO PUT VALUE TO THE ONES OBTAINED IN THE 5 YEARS PREVIOUS TO THE SURVEY WHICH ARE PROVIDED IN BRACKETS

alldatawas$bracked1inhW1 <- 0
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "Less than £1,000"] <-  500
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IValW1[alldatawas$IValW1 > 250000])
alldatawas$bracked1inhW1 [alldatawas$IValBW1 == "£250,000 or more"] <-  a
summary (alldatawas$bracked1inhW1)

#AND NOW FOR THE SECOND INHERITANCE BRACKETED
alldatawas$bracked2inhW1 <- 0
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "Less than £1,000 "] <-  500
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IVal2W1[alldatawas$IVal2W1 > 250000])
alldatawas$bracked2inhW1 [alldatawas$IValB2W1 == "£250,000 or more"] <-  a
summary (alldatawas$bracked2inhW1)

#AND NOW FOR THE THIRD INHERITANCE BRACKETED
alldatawas$bracked3inhW1 <- 0
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "Less than £1,000 "] <-  500
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IVal3W1[alldatawas$IVal3W1 > 250000])
alldatawas$bracked3inhW1 [alldatawas$IValB3W1 == "£250,000 or more"] <-  a
summary (alldatawas$bracked3inhW1)

summary (alldatawas$IValBW1)
summary (alldatawas$IValB2W1)
summary (alldatawas$IValB3W1)

#Let's see the bracketed variables
summary (alldatawas$bracked1inhW1[alldatawas$bracked1inhW1>0])
summary (alldatawas$bracked2inhW1[alldatawas$bracked2inhW1>0])
summary (alldatawas$bracked3inhW1[alldatawas$bracked3inhW1>0])

NROW (alldatawas$bracked1inhW1[alldatawas$bracked1inhW1>0])
NROW (alldatawas$bracked2inhW1[alldatawas$bracked2inhW1>0])
NROW (alldatawas$bracked3inhW1[alldatawas$bracked3inhW1>0])

#And the reported directly without brackets
summary (alldatawas$IValW1[alldatawas$IValW1>0])
summary (alldatawas$IVal2W1[alldatawas$IVal2W1>0])
summary (alldatawas$IVal3W1[alldatawas$IVal3W1>0])

NROW (alldatawas$IValW1[alldatawas$IValW1>0])
NROW (alldatawas$IVal2W1[alldatawas$IVal2W1>0])
NROW (alldatawas$IVal3W1[alldatawas$IVal3W1>0])


#WE HAVE TO GIVE VALUES ALSO TO THE INHERITANCES OBTAINED IN THE SECOND AND THIRD WAVES WHICH ARE IN BRACKETS

alldatawas$bracked1inhW2 <- 0
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "Less than £1,000"] <-  500
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£10,000 to £19,999"] <-  15000
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£20,000 to £49,999"] <-  35000
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IvalW2[alldatawas$IvalW2 > 250000])
alldatawas$bracked1inhW2 [alldatawas$IvalbW2 == "£250,000 or more"] <-  a
summary (alldatawas$bracked1inhW2)

#AND NOW FOR THE SECOND INHERITANCE BRACKETED
alldatawas$bracked2inhW2 <- 0
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "Less than £1,000"] <-  500
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£10,000 to £19,999"] <-  15000
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£20,000 to £49,999"] <-  35000
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$Ival2W2[alldatawas$Ival2W2 > 250000])
alldatawas$bracked2inhW2 [alldatawas$Ivalb2W2 == "£250,000 or more"] <-  a
summary (alldatawas$bracked2inhW2)

#AND NOW FOR THE THIRD INHERITANCE BRACKETED
alldatawas$bracked3inhW2 <- 0
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "Less than £1,000 "] <-  500
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£10,000 to £19,999"] <-  15000
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£20,000 to £49,999"] <-  35000
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$Ival3W2[alldatawas$Ival3W2 > 250000])
alldatawas$bracked3inhW2 [alldatawas$Ivalb3W2 == "£250,000 or more"] <-  a
summary (alldatawas$bracked3inhW2)

summary (alldatawas$IvalbW2)
summary (alldatawas$Ivalb2W2)
summary (alldatawas$Ivalb3W2)

#Let's see the bracketed variables
summary (alldatawas$bracked1inhW2[alldatawas$bracked1inhW2>0])
summary (alldatawas$bracked2inhW2[alldatawas$bracked2inhW2>0])
summary (alldatawas$bracked3inhW2[alldatawas$bracked3inhW2>0])

NROW (alldatawas$bracked1inhW2[alldatawas$bracked1inhW2>0])
NROW (alldatawas$bracked2inhW2[alldatawas$bracked2inhW2>0])
NROW (alldatawas$bracked3inhW2[alldatawas$bracked3inhW2>0])

#And the reported directly without brackets
summary (alldatawas$IvalW2[alldatawas$IvalW2>0])
summary (alldatawas$Ival2W2[alldatawas$Ival2W2>0])
summary (alldatawas$Ival3W2[alldatawas$Ival3W2>0])

NROW (alldatawas$IvalW2[alldatawas$IvalW2>0])
NROW (alldatawas$Ival2W2[alldatawas$Ival2W2>0])
NROW (alldatawas$Ival3W2[alldatawas$Ival3W2>0])


#AND FINALLY THE SAME FOR THE INHERITANCES RECEIVED IN THE THIRD WAVE...

alldatawas$bracked1inhW3 <- 0
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "Less than £1,000"] <-  500
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£10,000 to £19,999"] <-  15000
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£20,000 to £49,999"] <-  35000
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IVal1W3 [alldatawas$IVal1W3 > 250000])
alldatawas$bracked1inhW3 [alldatawas$IValB1W3 == "£250,000 or more"] <-  a
summary (alldatawas$bracked1inhW3)

#AND NOW FOR THE SECOND INHERITANCE BRACKETED
alldatawas$bracked2inhW3 <- 0
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "Less than £1,000 "] <-  500
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£10,000 to £19,999"] <-  15000
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£20,000 to £49,999"] <-  35000
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IVal1W3[alldatawas$IVal1W3 > 250000]) #We use IVal1, since there are no second inheritances reported
alldatawas$bracked2inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB2W3 == "£250,000 or more"] <-  a
summary (alldatawas$bracked2inhW3)

#AND NOW FOR THE THIRD INHERITANCE BRACKETED
alldatawas$bracked3inhW3 <- 0
alldatawas$bracked3inhW3 [alldatawas$IVAL3w3_i <0 & alldatawas$IValB3W3 == "Less than £1,000 "] <-  500
alldatawas$bracked3inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB3W3 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked3inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB3W3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked3inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB3W3 == "£10,000 to £19,999"] <-  15000
alldatawas$bracked3inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB3W3 == "£20,000 to £49,999"] <-  35000
alldatawas$bracked3inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB3W3 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked3inhW3 [alldatawas$IVAL2w3_i <0 & alldatawas$IValB3W3 == "£100,000 to £249,999"] <-  175000
#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IVAL3w3_i[alldatawas$IVAL3w3_i > 250000])
alldatawas$bracked3inhW3 [alldatawas$IValB3W3 == "£250,000 or more"] <-  a
summary (alldatawas$bracked3inhW3)

summary (alldatawas$IValB1W3)
summary (alldatawas$IValB2W3)
summary (alldatawas$IValB3W3)

#Let's see the bracketed variables
summary (alldatawas$bracked1inhW3[alldatawas$bracked1inhW3>0])
summary (alldatawas$bracked2inhW3[alldatawas$bracked2inhW3>0])
summary (alldatawas$bracked3inhW3[alldatawas$bracked3inhW3>0])

NROW (alldatawas$bracked1inhW3[alldatawas$bracked1inhW3>0])
NROW (alldatawas$bracked2inhW3[alldatawas$bracked2inhW3>0])
NROW (alldatawas$bracked3inhW3[alldatawas$bracked3inhW3>0])

#And the reported directly without brackets
summary (alldatawas$IVal1W3[alldatawas$IVal1W3>0])
summary (alldatawas$IVAL2w3_i[alldatawas$IVAL2w3_i>0])
summary (alldatawas$IVAL3w3_i[alldatawas$IVAL3w3_i>0])

NROW (alldatawas$IVal1W3[alldatawas$IVal1W3>0])
NROW (alldatawas$IVAL2w3_i[alldatawas$IVAL2w3_i>0])
NROW (alldatawas$IVAL3w3_i[alldatawas$IVAL3w3_i>0])

table (alldatawas$IVAL2w3_i[alldatawas$IVAL2w3_i>0], alldatawas$IVAL2w3_iflag[alldatawas$IVAL2w3_i>0])
table (alldatawas$IVAL3w3_i[alldatawas$IVAL3w3_i>0], alldatawas$IVAL3w3_iflag[alldatawas$IVAL3w3_i>0])
#Very few directly inputed in the values



######## NOW WE HAVE TO ADD GIFTS OBTAINED ONLY IN THE LAST TWO YEARS PREVIOUS TO EACH SURVEY ################
######## Gifts are not looped, the total amount each provided in one asnwer #######

#How many report at least receiving a gift over 500 pounds in the last two years:
#summary(alldatawas$ILGiftW5)
#summary(alldatawas$ILGiftW4)
summary(alldatawas$ILGiftW3)
summary(alldatawas$ILgiftW2) #Different coding
summary(alldatawas$ILgiftW1)

#And the value of them
summary(alldatawas$IGifvalW1)
NROW(alldatawas$IGifvalW1[alldatawas$IGifvalW1>0])
summary(alldatawas$IGifvalW1[alldatawas$IGifvalW1>0])
summary(alldatawas$IGfvalbW1)

#We have to assign a value to the Gifts bands

alldatawas$bracked1giftW1 <- 0
alldatawas$bracked1giftW1 [alldatawas$IGfvalbW1 == "£500 to £999"] <-  750
alldatawas$bracked1giftW1 [alldatawas$IGfvalbW1 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1giftW1 [alldatawas$IGfvalbW1 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1giftW1 [alldatawas$IGfvalbW1 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1giftW1 [alldatawas$IGfvalbW1 == "£25,000 to £49,999"] <-  37500

#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IGifvalW1[alldatawas$IGifvalW1 > 50000])
alldatawas$bracked1giftW1 [alldatawas$IGfvalbW1 == "£50,000 or more"] <-  a
summary (alldatawas$bracked1giftW1)
NROW (alldatawas$bracked1giftW1[alldatawas$bracked1giftW1>0])
summary (alldatawas$bracked1giftW1[alldatawas$bracked1giftW1>0])


#WAVE 2

summary(alldatawas$IgifvalW2[alldatawas$IgifvalW2>0])
NROW (alldatawas$IgifvalW2[alldatawas$IgifvalW2>0])

#We have to assign a value to the Gifts bands

alldatawas$bracked1giftW2 <- 0
alldatawas$bracked1giftW2 [alldatawas$IgfvalbW2 == "£500 to £999"] <-  750
alldatawas$bracked1giftW2 [alldatawas$IgfvalbW2 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1giftW2 [alldatawas$IgfvalbW2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1giftW2 [alldatawas$IgfvalbW2 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1giftW2 [alldatawas$IgfvalbW2 == "£25,000 to £49,999"] <-  37500

#For over 250000 we assign the average of the values above 250000 for the ones that reported inheritances
a <- mean(alldatawas$IgifvalW2[alldatawas$IgifvalW2 > 50000])
alldatawas$bracked1giftW2 [alldatawas$IgfvalbW2 == "£50,000 or more"] <-  a
summary (alldatawas$bracked1giftW2)
NROW (alldatawas$bracked1giftW2[alldatawas$bracked1giftW2>0])
summary(alldatawas$bracked1giftW2[alldatawas$bracked1giftW2>0])


#WAVE 3

summary(alldatawas$IGIFVALw3_iflag)
table(alldatawas$IGfValBW3)

#In Wave 3 we will have to use the DVGiftAnnualW3 variable which is in the user database

#We have to assign a value to the Gifts bands

alldatawas$bracked1giftW3 <- 0
alldatawas$bracked1giftW3 [alldatawas$IGfValBW3 == "£500 to £999"] <-  750
alldatawas$bracked1giftW3 [alldatawas$IGfValBW3 == "£1,000 to £4,999"] <-  3500
alldatawas$bracked1giftW3 [alldatawas$IGfValBW3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1giftW3 [alldatawas$IGfValBW3 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1giftW3 [alldatawas$IGfValBW3 == "£25,000 to £49,999"] <-  37500

alldatawas$bracked1giftW3 [alldatawas$IGfValBW3 == "£50,000 or more"] <-  50000 #NO VALUES OF GIFTS BESIDES BRACKETED VALUES IN WAVE 3...
summary (alldatawas$bracked1giftW3)
NROW (alldatawas$bracked1giftW3[alldatawas$bracked1giftW3>0])
summary (alldatawas$bracked1giftW3[alldatawas$bracked1giftW3>0])


#### TRUSTS #####

#### We first have to convert the brackets of the trusts to their mean value #####

######## NOW WE HAVE TO ADD GIFTS OBTAINED ONLY IN THE LAST TWO YEARS PREVIOUS TO EACH SURVEY ################
######## Gifts are not looped, the total amount each provided in one asnwer #######

#How many report at least receiving a gift over 500 pounds in the last two years:
#summary(alldatawas$ILGiftW5)
#summary(alldatawas$ILGiftW4)
table(alldatawas$TBenefW3)
table(alldatawas$TbenefW2) #Different coding
table(alldatawas$TBenefW1)

#Who settle it (only for the two first trusts in wave 2, the first 3 in wave 3, no information in wave 1)
table(alldatawas$Twho1W2)
table(alldatawas$Twho2W2)
table(alldatawas$TWho1W3)
table(alldatawas$TWho2W3)
table(alldatawas$TWho3W3)


#The amount
table (alldatawas$TbvalbW2)
table (alldatawas$Tbvalb2W2)
table (alldatawas$Tbvalb3W2)

table (alldatawas$TBValB1W3)
table (alldatawas$TBValB2W3)
table (alldatawas$TBValB3W3)


#We have to assign a value to the Gifts bands

#1st Trust Wave 2

alldatawas$bracked1trustW2 <- 0
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "Less than £5,000"] <-  2500
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£100,000 to £249,999"] <-  175000
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£250,000 to £499,999"] <-  375000
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£500,000 to £999,999"] <-  750000
alldatawas$bracked1trustW2 [alldatawas$TbvalbW2 == "£1 million or more"] <-  1500000

#2nd Trust Wave 2

alldatawas$bracked2trustW2 <- 0
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "Less than £5,000"] <-  2500
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£100,000 to £249,999"] <-  175000
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£250,000 to £499,999"] <-  375000
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£500,000 to £999,999"] <-  750000
alldatawas$bracked2trustW2 [alldatawas$Tbvalb2W2 == "£1 million or more"] <-  1500000

#3rd Trust Wave 2

alldatawas$bracked3trustW2 <- 0
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "Less than £5,000"] <-  2500
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£100,000 to £249,999"] <-  175000
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£250,000 to £499,999"] <-  375000
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£500,000 to £999,999"] <-  750000
alldatawas$bracked3trustW2 [alldatawas$Tbvalb3W2 == "£1 million or more"] <-  1500000

summary (alldatawas$bracked1trustW2)
summary (alldatawas$bracked2trustW2)
summary (alldatawas$bracked3trustW2)


#WAVE 3

#1st Trust Wave 3

alldatawas$bracked1trustW3 <- 0
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "Less than £5,000"] <-  2500
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£100,000 to £249,999"] <-  175000
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£250,000 to £499,999"] <-  375000
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£500,000 to £999,999"] <-  750000
alldatawas$bracked1trustW3 [alldatawas$TBValB1W3 == "£1 million or more"] <-  1500000

#2nd Trust Wave 3

alldatawas$bracked2trustW3 <- 0
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "Less than £5,000"] <-  2500
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£100,000 to £249,999"] <-  175000
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£250,000 to £499,999"] <-  375000
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£500,000 to £999,999"] <-  750000
alldatawas$bracked2trustW3 [alldatawas$TBValB2W3 == "£1 million or more"] <-  1500000

#3rd Trust Wave 3

alldatawas$bracked3trustW3 <- 0
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "Less than £5,000"] <-  2500
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£5,000 to £9,999"] <-  7500
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£10,000 to £24,999"] <-  17500
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£25,000 to £49,999"] <-  37500
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£50,000 to £99,999"] <-  75000
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£100,000 to £249,999"] <-  175000
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£250,000 to £499,999"] <-  375000
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£500,000 to £999,999"] <-  750000
alldatawas$bracked3trustW3 [alldatawas$TBValB3W3 == "£1 million or more"] <-  1500000

summary (alldatawas$bracked1trustW3)
summary (alldatawas$bracked2trustW3)
summary (alldatawas$bracked3trustW3)


#BRACKETED VALUES IN WAVE 3...
summary (alldatawas$bracked1trustW3)
NROW (alldatawas$bracked1trustW3[alldatawas$bracked1trustW3>0])
summary (alldatawas$bracked1trustW3[alldatawas$bracked1trustW3>0])



###### AND WE SAVE THE DATA #######

saveRDS(alldatawas, file = "data_rds/WAS-After-Step1a-brackets-new-data.rds")
