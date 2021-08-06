#we transform net to gross the intergenerational transfers amounts

#We used an a version

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)


alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step3a-pre-tax-adjusted-new-data.rds")


###### TAX UPDATING FIRST #####  TAX UPDATING FIRST ###################


#THIS IS THE YEAR THE INHERITANCE WAS OBTAINED

summary(alldatawas$IEYrW1[alldatawas$IEYrW1>0])
table(alldatawas$IEYrW1[alldatawas$IEYrW1>0])

table(alldatawas$IEValW1[alldatawas$IEValW1>100000], alldatawas$IEYrW1[alldatawas$IEValW1>100000])

##### NOW WE ARE WORKING WITH THE DATA ALREADY IMPUTED, SO WE NEED TO ASSIGN A YEAR TO THE IMPUTED DATA. FOR THAT PURPUSE, WE ARE GOING TO FOCUS ON THE AGE OF THE INDIVIDUAL ####
#####

#Let us first create a variable with the decade of the age recipient of inheritances###

summary (alldatawas$dvagew3[alldatawas$IEValW1>0])

alldatawas$dvagew3 <- as.numeric(alldatawas$dvagew3)

alldatawas$decade <- trunc(alldatawas$dvagew3/10)

summary(alldatawas$decade[alldatawas$IEValW1>0])

#Now we get the average year of reception of inheritance for each group

alldatawas <- alldatawas %>%
  group_by(decade) %>%
  mutate(inhyeardecade = trunc(mean(IEYrW1[IEYrW1>0])))


NROW(alldatawas$IEYrW1[alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic)])

table(alldatawas$flaginputlife1basic, alldatawas$IEYrW1) #Before imputing the year

alldatawas$IEYrW1[which(alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic) & alldatawas$IEYrW1 < 1900)] <- alldatawas$inhyeardecade[which(alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic) & alldatawas$IEYrW1 < 1900)] #We add the decade year only to the ones that do not have that information (code -6)

table(alldatawas$flaginputlife1basic, alldatawas$IEYrW1) #After imputing the year


#WE COULD DO THAT WITH THE SECOND INHERITANCE VALUE


NROW(alldatawas$IEYr2W1[alldatawas$flaginputlife2basic == 1 & !is.na(alldatawas$flaginputlife2basic == 1)])

table(alldatawas$flaginputlife2basic, alldatawas$IEYr2W1)

alldatawas$IEYr2W1[which(alldatawas$flaginputlife2basic == 1 & !is.na(alldatawas$flaginputlife2basic) & alldatawas$IEYr2W1 < 1900)] <- alldatawas$inhyeardecade[which(alldatawas$flaginputlife2basic == 1 & !is.na(alldatawas$flaginputlife2basic) & alldatawas$IEYr2W1 < 1900)] #We add the decade year only to the ones that do not have that information (code -6)


table(alldatawas$flaginputlife2basic, alldatawas$IEYr2W1)


#NOW TAKE INTO ACCOUNT THAT WE HAVE TWO VARIABLES FOR PAST INHERITANCES, THE IMPUTED AND THE NON-IMPUTED ONE:

#FOR THE FIRST OLDER THAN 5 YEARS...
NROW(alldatawas$inputlife1basic[alldatawas$inputlife1basic>0 & !is.na(alldatawas$inputlife1basic)])
NROW(alldatawas$IEValW1[alldatawas$IEValW1>0 & !is.na(alldatawas$IEValW1)])
#The first one includes the imputations, 701 observations

summary(alldatawas$IEValW1[alldatawas$IEValW1>0 & !is.na(alldatawas$IEValW1)])
summary(alldatawas$inputlife1basic[alldatawas$IEValW1>0 & !is.na(alldatawas$IEValW1)])
#When there is no imputation they are the same

#We have to replace the original one by the inputed values:
alldatawas$IEValW1[alldatawas$inputlife1basic>0 & !is.na(alldatawas$inputlife1basic) & alldatawas$flaginputlife1basic == 1] <- alldatawas$inputlife1basic[alldatawas$inputlife1basic>0 & !is.na(alldatawas$inputlife1basic) & alldatawas$flaginputlife1basic == 1]


#AND NOW FOR THE SECOND LIFETIME INHERITANCE
NROW(alldatawas$inputlife2basic[alldatawas$inputlife2basic>0 & !is.na(alldatawas$inputlife2basic)])
NROW(alldatawas$IEVal2W1[alldatawas$IEVal2W1>0 & !is.na(alldatawas$IEVal2W1)])
#The first one includes the imputations

summary(alldatawas$IEVal2W1[alldatawas$IEVal2W1>0 & !is.na(alldatawas$IEVal2W1)])
summary(alldatawas$inputlife2basic[alldatawas$IEVal2W1>0 & !is.na(alldatawas$IEVal2W1)])
#When there is no imputation they are the same

#We have to replace the original one by the inputed values:
alldatawas$IEVal2W1[alldatawas$inputlife2basic>0 & !is.na(alldatawas$inputlife2basic) & alldatawas$flaginputlife2basic == 1] <- alldatawas$inputlife2basic[alldatawas$inputlife2basic>0 & !is.na(alldatawas$inputlife2basic) & alldatawas$flaginputlife2basic == 1]


#AND FOR THE FIRST RECENT ONE
NROW(alldatawas$inputrecent1basic[alldatawas$inputrecent1basic>0 & !is.na(alldatawas$inputrecent1basic)])
NROW(alldatawas$IValW1[which (alldatawas$IValW1>0 & alldatawas$inputrecent1basic>0)])
#The first one includes the imputations

summary(alldatawas$IValW1[alldatawas$IValW1>0 & !is.na(alldatawas$IValW1) & !is.na(alldatawas$inputrecent1basic)])
summary(alldatawas$inputrecent1basic[alldatawas$IValW1>0 & !is.na(alldatawas$IValW1)]) #The imputed variable has some NAs, we do not replace tose ones

#When there is no imputation they are the same

#We have to replace the original one by the inputed values:
alldatawas$IValW1[which(alldatawas$inputrecent1basic>0 & !is.na(alldatawas$inputrecent1basic) & alldatawas$flaginputrecent1basic == 1)] <- alldatawas$inputrecent1basic[which (alldatawas$inputrecent1basic>0 & !is.na(alldatawas$inputrecent1basic) & alldatawas$flaginputrecent1basic == 1)]

#AND FOR THE SECOND RECENT ONE
NROW(alldatawas$inputrecent2basic[alldatawas$inputrecent2basic>0 & !is.na(alldatawas$inputrecent2basic)])
NROW(alldatawas$IVal2W1[alldatawas$IVal2W1>0 & !is.na(alldatawas$IVal2W1)])
#The first one includes the imputations

summary(alldatawas$IVal2W1[alldatawas$IVal2W1>0 & !is.na(alldatawas$IVal2W1) & !is.na(alldatawas$inputrecent2basic)])
summary(alldatawas$inputrecent2basic[alldatawas$IVal2W1>0 & !is.na(alldatawas$IVal2W1) & !is.na(alldatawas$inputrecent2basic)]) #The imputed variable has some NAs, we do not replace tose ones

#When there is no imputation they are the same

#We have to replace the original one by the inputed values:
alldatawas$IVal2W1[alldatawas$inputrecent2basic>0 & !is.na(alldatawas$inputrecent2basic) & alldatawas$flaginputrecent2basic == 1] <- alldatawas$inputrecent2basic[alldatawas$inputrecent2basic>0 & !is.na(alldatawas$inputrecent2basic) & alldatawas$flaginputrecent2basic == 1]



#AND NOW WE CAN GO TO ACTUALLY UPDATING


pretax = read.csv("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/aux_files/pre-tax-update.csv", sep = ";", header = T, as.is = T)



#LET'S GET TO IT

alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYrW1, alldatawas$IEValW1) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("IEYrW1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IEValW1[alldatawas1$netmax>0])

alldatawas1$grossinhlifeW1 <- alldatawas1$bracket+(alldatawas1$IEValW1-alldatawas1$netmax)/(1-alldatawas1$rate) #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawas1$grossinhlifeW1[!is.na(alldatawas1$grossinhlifeW1)])

NROW(alldatawas1$IEValW1[alldatawas1$IEValW1>0])

table(alldatawas1$grossinhlifeW1[alldatawas1$grossinhlifeW1>100000], alldatawas1$IEValW1[alldatawas1$grossinhlifeW1>100000])

alldatawas1$pretaxinhlifeW1 <- alldatawas1$IEValW1

alldatawas1$pretaxinhlifeW1[!is.na(alldatawas1$grossinhlifeW1)] <- alldatawas1$grossinhlifeW1[!is.na(alldatawas1$grossinhlifeW1)]

summary(alldatawas1$IEValW1[alldatawas1$IEValW1>0])

summary(alldatawas1$pretaxinhlifeW1[alldatawas1$pretaxinhlifeW1>0])

length(alldatawas1$pretaxinhlifeW1[alldatawas1$pretaxinhlifeW1==alldatawas1$IEValW1])
#[1] 27764
length(alldatawas1$pretaxinhlifeW1[alldatawas1$pretaxinhlifeW1!=alldatawas1$IEValW1])
#[1] 139

NROW(alldatawas1$IEValW1[alldatawas1$IEValW1>140000 & alldatawas1$IEYrW1==1991])

length(alldatawas1$pretaxinhlifeW1[alldatawas1$pretaxinhlifeW1!=alldatawas1$IEValW1 & alldatawas1$IEYrW1==1991])

summary(alldatawas1$IEValW1[alldatawas1$IEValW1>0 & alldatawas1$IEYrW1==1991])

summary(alldatawas1$pretaxinhlifeW1[alldatawas1$pretaxinhlifeW1>0 & alldatawas1$IEYrW1==1991])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlifeW1 <- alldatawas1$pretaxinhlifeW1


## AND THEN REPEAT THE PROCESS FOR THE SECOND LIFETIME INHERITANCE

#LET'S GET TO IT

alldatawas$netmax2 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr2W1, alldatawas$IEVal2W1) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax2)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas2 <- merge(alldatawas, pretax, by.x = c("IEYr2W1", "netmax2"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas2$rate)

table(alldatawas2$netmax2[alldatawas2$netmax2>0])

table(alldatawas2$netmax2[alldatawas2$netmax2>0], alldatawas2$IEVal2W1[alldatawas2$netmax2>0])

alldatawas2$grossinhlife2W1 <- alldatawas2$bracket+(alldatawas2$IEVal2W1-alldatawas2$netmax2)/(1-alldatawas2$rate) #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawas2$grossinhlife2W1[!is.na(alldatawas2$grossinhlife2W1)])

NROW(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>0])

table(alldatawas2$grossinhlife2W1[alldatawas2$grossinhlife2W1>100000], alldatawas2$IEVal2W1[alldatawas2$grossinhlife2W1>100000])

alldatawas2$pretaxinhlife2W1 <- alldatawas2$IEVal2W1

alldatawas2$pretaxinhlife2W1[!is.na(alldatawas2$grossinhlife2W1)] <- alldatawas2$grossinhlife2W1[!is.na(alldatawas2$grossinhlife2W1)]

summary(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>0])

summary(alldatawas2$pretaxinhlife2W1[alldatawas2$pretaxinhlife2W1>0])

length(alldatawas2$pretaxinhlife2W1[alldatawas2$pretaxinhlife2W1==alldatawas2$IEVal2W1])
#[1] 27764
length(alldatawas2$pretaxinhlife2W1[alldatawas2$pretaxinhlife2W1!=alldatawas2$IEVal2W1])
#[1] 139

NROW(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>140000 & alldatawas2$IEYr2W1==1991])

length(alldatawas2$pretaxinhlife2W1[alldatawas2$pretaxinhlife2W1!=alldatawas2$IEVal2W1 & alldatawas2$IEYr2W1==1991])

summary(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>0 & alldatawas2$IEYr2W1==1991])

summary(alldatawas2$pretaxinhlife2W1[alldatawas2$pretaxinhlife2W1>0 & alldatawas2$IEYr2W1==1991])

#We finally add the new gross variable to the original dataset

alldatawas2 <- alldatawas2[order(alldatawas2$CaseW3,alldatawas2$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife2W1 <- alldatawas2$pretaxinhlife2W1


## AND THEN REPEAT THE PROCESS FOR THE THIRD LIFETIME INHERITANCE

#LET'S GET TO IT

alldatawas$netmax3 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr3W1, alldatawas$IEVal3W1) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax3)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas3 <- merge(alldatawas, pretax, by.x = c("IEYr3W1", "netmax3"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas3$rate)

table(alldatawas3$netmax3[alldatawas3$netmax3>0])

table(alldatawas3$netmax3[alldatawas3$netmax3>0], alldatawas3$IEVal3W1[alldatawas3$netmax3>0])

alldatawas3$grossinhlife3W1 <- alldatawas3$bracket+(alldatawas3$IEVal3W1-alldatawas3$netmax3)/(1-alldatawas3$rate) #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawas3$grossinhlife3W1[!is.na(alldatawas3$grossinhlife3W1)])

NROW(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>0])

table(alldatawas3$grossinhlife3W1[alldatawas3$grossinhlife3W1>100000], alldatawas3$IEVal3W1[alldatawas3$grossinhlife3W1>100000])

alldatawas3$pretaxinhlife3W1 <- alldatawas3$IEVal3W1

alldatawas3$pretaxinhlife3W1[!is.na(alldatawas3$grossinhlife3W1)] <- alldatawas3$grossinhlife3W1[!is.na(alldatawas3$grossinhlife3W1)]

summary(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>0])

summary(alldatawas3$pretaxinhlife3W1[alldatawas3$pretaxinhlife3W1>0])

length(alldatawas3$pretaxinhlife3W1[alldatawas3$pretaxinhlife3W1==alldatawas3$IEVal3W1])

length(alldatawas3$pretaxinhlife3W1[alldatawas3$pretaxinhlife3W1!=alldatawas3$IEVal3W1])


NROW(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>140000 & alldatawas3$IEYr3W1==1991])

length(alldatawas3$pretaxinhlife3W1[alldatawas3$pretaxinhlife3W1!=alldatawas3$IEVal3W1 & alldatawas3$IEYr3W1==1991])

summary(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>0 & alldatawas3$IEYr3W1==1991])

summary(alldatawas3$pretaxinhlife3W1[alldatawas3$pretaxinhlife3W1>0 & alldatawas3$IEYr3W1==1991])

#We finally add the new gross variable to the original dataset

alldatawas3 <- alldatawas3[order(alldatawas3$CaseW3,alldatawas3$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife3W1 <- alldatawas3$pretaxinhlife3W1


##### NOW FOR THE FIRST BRACKETED VALUE ####

table(alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0], alldatawas$IEYrW1[alldatawas$bracked1inhW1past>0])

alldatawas$netmaxb1 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYrW1, alldatawas$bracked1inhW1past) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmaxb1)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawasb1 <- merge(alldatawas, pretax, by.x = c("IEYrW1", "netmaxb1"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawasb1$rate)

table(alldatawasb1$netmaxb1[alldatawasb1$netmaxb1>0])

table(alldatawasb1$netmaxb1[alldatawasb1$netmaxb1>0], alldatawasb1$bracked1inhW1past[alldatawasb1$netmaxb1>0])

alldatawasb1$grossinhlifebW1 <- alldatawasb1$bracket+(alldatawasb1$bracked1inhW1past-alldatawasb1$netmaxb1)/(1-alldatawasb1$rate) #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawasb1$grossinhlifebW1[!is.na(alldatawasb1$grossinhlifebW1)])

NROW(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>0])

table(alldatawasb1$grossinhlifebW1[alldatawasb1$grossinhlifebW1>=0], alldatawasb1$bracked1inhW1past[alldatawasb1$grossinhlifebW1>=0])

alldatawasb1$pretaxinhlifebW1 <- alldatawasb1$bracked1inhW1past

alldatawasb1$pretaxinhlifebW1[!is.na(alldatawasb1$grossinhlifebW1) & alldatawasb1$grossinhlifebW1>0 ] <- alldatawasb1$grossinhlifebW1[!is.na(alldatawasb1$grossinhlifebW1) & alldatawasb1$grossinhlifebW1>0]

summary(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>0])

summary(alldatawasb1$pretaxinhlifebW1[alldatawasb1$pretaxinhlifebW1>0])

length(alldatawasb1$pretaxinhlifebW1[alldatawasb1$pretaxinhlifebW1==alldatawasb1$bracked1inhW1past])
#[1] 27764
length(alldatawasb1$pretaxinhlifebW1[alldatawasb1$pretaxinhlifebW1!=alldatawasb1$bracked1inhW1past])
#[1] 139

NROW(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>140000 & alldatawasb1$IEYrW1==1991])

length(alldatawasb1$pretaxinhlifebW1[alldatawasb1$pretaxinhlifebW1!=alldatawasb1$bracked1inhW1past & alldatawasb1$IEYrW1==1991])

summary(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>0 & alldatawasb1$IEYrW1==1991])

summary(alldatawasb1$pretaxinhlifebW1[alldatawasb1$pretaxinhlifebW1>0 & alldatawasb1$IEYrW1==1991])

#We finally add the new gross variable to the original dataset

alldatawasb1 <- alldatawasb1[order(alldatawasb1$CaseW3,alldatawasb1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlifebW1 <- alldatawasb1$pretaxinhlifebW1

summary(alldatawasb1$pretaxinhlifebW1)
summary(alldatawas$pretaxinhlifebW1)
summary(alldatawas$bracked1inhW1past)

summary(alldatawas$bracked1inhW1past)
summary(alldatawas$pretaxinhlifebW1)

NROW(alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0])
NROW(alldatawas$pretaxinhlifebW1[alldatawas$pretaxinhlifebW1>0])

NROW(alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past==0])
NROW(alldatawas$pretaxinhlifebW1[alldatawas$pretaxinhlifebW1==0])


##### NOW FOR THE SECOND BRACKETED VALUE ####

alldatawas$netmaxb2 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr2W1, alldatawas$bracked2inhW1past) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmaxb2)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawasb2 <- merge(alldatawas, pretax, by.x = c("IEYr2W1", "netmaxb2"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawasb2$rate)

table(alldatawasb2$netmaxb2[alldatawasb2$netmaxb2>0])

table(alldatawasb2$netmaxb2[alldatawasb2$netmaxb2>0], alldatawasb2$bracked2inhW1past[alldatawasb2$netmaxb2>0])

alldatawasb2$grossinhlife2bW1 <- alldatawasb2$bracket+(alldatawasb2$bracked2inhW1past-alldatawasb2$netmaxb2)/(1-alldatawasb2$rate) #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawasb2$grossinhlife2bW1[!is.na(alldatawasb2$grossinhlife2bW1)])

NROW(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>0])

table(alldatawasb2$grossinhlife2bW1[alldatawasb2$grossinhlife2bW1>100000], alldatawasb2$bracked2inhW1past[alldatawasb2$grossinhlife2bW1>100000])

alldatawasb2$pretaxinhlife2bW1 <- alldatawasb2$bracked2inhW1past

alldatawasb2$pretaxinhlife2bW1[!is.na(alldatawasb2$grossinhlife2bW1)] <- alldatawasb2$grossinhlife2bW1[!is.na(alldatawasb2$grossinhlife2bW1)]

summary(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>0])

summary(alldatawasb2$pretaxinhlife2bW1[alldatawasb2$pretaxinhlife2bW1>0])

length(alldatawasb2$pretaxinhlife2bW1[alldatawasb2$pretaxinhlife2bW1==alldatawasb2$bracked2inhW1past])
#[1] 27764
length(alldatawasb2$pretaxinhlife2bW1[alldatawasb2$pretaxinhlife2bW1!=alldatawasb2$bracked2inhW1past])
#[1] 139

NROW(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>140000 & alldatawasb2$IEYr2W1==1991])

length(alldatawasb2$pretaxinhlife2bW1[alldatawasb2$pretaxinhlife2bW1!=alldatawasb2$bracked2inhW1past & alldatawasb2$IEYr2W1==1991])

summary(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>0 & alldatawasb2$IEYr2W1==1991])

summary(alldatawasb2$pretaxinhlife2bW1[alldatawasb2$pretaxinhlife2bW1>0 & alldatawasb2$IEYr2W1==1991])

#We finally add the new gross variable to the original dataset

alldatawasb2 <- alldatawasb2[order(alldatawasb2$CaseW3,alldatawasb2$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife2bW1 <- alldatawasb2$pretaxinhlife2bW1



##### NOW FOR THE THIRD BRACKETED VALUE ####

alldatawas$netmaxb3 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr3W1, alldatawas$bracked3inhW1past) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmaxb3)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawasb3 <- merge(alldatawas, pretax, by.x = c("IEYr3W1", "netmaxb3"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawasb3$rate)

table(alldatawasb3$netmaxb3[alldatawasb3$netmaxb3>0])

table(alldatawasb3$netmaxb3[alldatawasb3$netmaxb3>0], alldatawasb3$bracked3inhW1past[alldatawasb3$netmaxb3>0])

alldatawasb3$grossinhlife3bW1 <- alldatawasb3$bracket+(alldatawasb3$bracked3inhW1past-alldatawasb3$netmaxb3)/(1-alldatawasb3$rate) #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawasb3$grossinhlife3bW1[!is.na(alldatawasb3$grossinhlife3bW1)])

NROW(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>0])

table(alldatawasb3$grossinhlife3bW1[alldatawasb3$grossinhlife3bW1>100000], alldatawasb3$bracked3inhW1past[alldatawasb3$grossinhlife3bW1>100000])

alldatawasb3$pretaxinhlife3bW1 <- alldatawasb3$bracked3inhW1past

alldatawasb3$pretaxinhlife3bW1[!is.na(alldatawasb3$grossinhlife3bW1)] <- alldatawasb3$grossinhlife3bW1[!is.na(alldatawasb3$grossinhlife3bW1)]

summary(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>0])

summary(alldatawasb3$pretaxinhlife3bW1[alldatawasb3$pretaxinhlife3bW1>0])

length(alldatawasb3$pretaxinhlife3bW1[alldatawasb3$pretaxinhlife3bW1==alldatawasb3$bracked3inhW1past])
#[1] 27764
length(alldatawasb3$pretaxinhlife3bW1[alldatawasb3$pretaxinhlife3bW1!=alldatawasb3$bracked3inhW1past])
#[1] 139

NROW(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>140000 & alldatawasb3$IEYr3W1==1991])

length(alldatawasb3$pretaxinhlife3bW1[alldatawasb3$pretaxinhlife3bW1!=alldatawasb3$bracked3inhW1past & alldatawasb3$IEYr3W1==1991])

summary(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>0 & alldatawasb3$IEYr3W1==1991])

summary(alldatawasb3$pretaxinhlife3bW1[alldatawasb3$pretaxinhlife3bW1>0 & alldatawasb3$IEYr3W1==1991])

#We finally add the new gross variable to the original dataset

alldatawasb3 <- alldatawasb3[order(alldatawasb3$CaseW3,alldatawasb3$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife3bW1 <- alldatawasb3$pretaxinhlife3bW1

#CONTINUE WITH WAVE ONE RECENT INHERITANCES ####

#Wave one asks for inheritances in the last 5 years. That is, between 2004 and 2008 (2006-2008 is Wave 1). The last inheritance reported as "previous to the last five years" is from 2003, so we should use 2006 as the reference point to update this.

#In 2006 the threshold is 285.000 and the rate is 0.4.

#First recent inheritance

summary(alldatawas$IValW1)

summary(alldatawas$IValW1[alldatawas$IValW1 > 285000])
NROW(alldatawas$IValW1[alldatawas$IValW1 > 285000])

#X = (Y-tT)/(1-t)

alldatawas$IValW1gross <- alldatawas$IValW1

alldatawas$IValW1gross[alldatawas$IValW1 > 285000] <- (alldatawas$IValW1[alldatawas$IValW1 > 285000]-285000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IValW1gross[alldatawas$IValW1gross > 285000])
table(alldatawas$IValW1[alldatawas$IValW1 > 285000])

#First bracketed inheritance

summary(alldatawas$bracked1inhW1)

summary(alldatawas$bracked1inhW1[alldatawas$bracked1inhW1 > 285000])
NROW(alldatawas$bracked1inhW1[alldatawas$bracked1inhW1 > 285000])

#X = (Y-tT)/(1-t)

alldatawas$bracked1inhW1gross <- alldatawas$bracked1inhW1

alldatawas$bracked1inhW1gross[alldatawas$bracked1inhW1 > 285000] <- (alldatawas$bracked1inhW1[alldatawas$bracked1inhW1 > 285000]-285000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked1inhW1gross[alldatawas$bracked1inhW1gross > 285000])
table(alldatawas$bracked1inhW1[alldatawas$bracked1inhW1 > 285000])


#Second recent inheritance

summary(alldatawas$IVal2W1)

summary(alldatawas$IVal2W1[alldatawas$IVal2W1 > 285000])
NROW(alldatawas$IVal2W1[alldatawas$IVal2W1 > 285000])

#X = (Y-tT)/(1-t)

alldatawas$IVal2W1gross <- alldatawas$IVal2W1

alldatawas$IVal2W1gross[alldatawas$IVal2W1 > 285000] <- (alldatawas$IVal2W1[alldatawas$IVal2W1 > 285000]-285000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IVal2W1gross[alldatawas$IVal2W1gross > 285000])
table(alldatawas$IVal2W1[alldatawas$IVal2W1 > 285000])

#Second bracketed inheritance

summary(alldatawas$bracked2inhW1)

summary(alldatawas$bracked2inhW1[alldatawas$bracked2inhW1 > 285000])
NROW(alldatawas$bracked2inhW1[alldatawas$bracked2inhW1 > 285000])

#X = (Y-tT)/(1-t)

alldatawas$bracked2inhW1gross <- alldatawas$bracked2inhW1

alldatawas$bracked2inhW1gross[alldatawas$bracked2inhW1 > 285000] <- (alldatawas$bracked2inhW1[alldatawas$bracked2inhW1 > 285000]-285000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked2inhW1gross[alldatawas$bracked2inhW1gross > 285000])
table(alldatawas$bracked2inhW1[alldatawas$bracked2inhW1 > 285000])

##### Third recent inheritance #####

summary(alldatawas$IVal3W1)

summary(alldatawas$IVal3W1[alldatawas$IVal3W1 > 285000])
NROW(alldatawas$IVal3W1[alldatawas$IVal3W1 > 285000])

#X = (Y-tT)/(1-t)

alldatawas$IVal3W1gross <- alldatawas$IVal3W1

alldatawas$IVal3W1gross[alldatawas$IVal3W1 > 285000] <- (alldatawas$IVal3W1[alldatawas$IVal3W1 > 285000]-285000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IVal3W1gross[alldatawas$IVal3W1gross > 285000])
table(alldatawas$IVal3W1[alldatawas$IVal3W1 > 285000])

#Third bracketed inheritance

summary(alldatawas$bracked3inhW1)

summary(alldatawas$bracked3inhW1[alldatawas$bracked3inhW1 > 285000])
NROW(alldatawas$bracked3inhW1[alldatawas$bracked3inhW1 > 285000])

#X = (Y-tT)/(1-t)

alldatawas$bracked3inhW1gross <- alldatawas$bracked3inhW1

alldatawas$bracked3inhW1gross[alldatawas$bracked3inhW1 > 285000] <- (alldatawas$bracked3inhW1[alldatawas$bracked3inhW1 > 285000]-285000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked3inhW1gross[alldatawas$bracked3inhW1gross > 285000])
table(alldatawas$bracked3inhW1[alldatawas$bracked3inhW1 > 285000])





### CONTINUE WITH WAVE 2  ####

#Wave two asks for inheritances in the last 2 years. That is, between 2008 and 2010 (2006-2008 is Wave 1). We should use 2009 as the reference point to update this.

#In 2009 the threshold is 325.000 and the rate is 0.4.

#First recent inheritance

summary(alldatawas$IvalW2)

summary(alldatawas$IvalW2[alldatawas$IvalW2 > 325000])
NROW(alldatawas$IvalW2[alldatawas$IvalW2 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$IValW2gross <- alldatawas$IvalW2

alldatawas$IValW2gross[alldatawas$IvalW2 > 325000] <- (alldatawas$IvalW2[alldatawas$IvalW2 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IValW2gross[alldatawas$IValW2gross > 325000])
table(alldatawas$IvalW2[alldatawas$IvalW2 > 325000])

#First bracketed inheritance

summary(alldatawas$bracked1inhW2)

summary(alldatawas$bracked1inhW2[alldatawas$bracked1inhW2 > 325000])
NROW(alldatawas$bracked1inhW2[alldatawas$bracked1inhW2 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$bracked1inhW2gross <- alldatawas$bracked1inhW2

alldatawas$bracked1inhW2gross[alldatawas$bracked1inhW2 > 325000] <- (alldatawas$bracked1inhW2[alldatawas$bracked1inhW2 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked1inhW2gross[alldatawas$bracked1inhW2gross > 325000])
table(alldatawas$bracked1inhW2[alldatawas$bracked1inhW2 > 325000])


#Second recent inheritance

summary(alldatawas$Ival2W2)

summary(alldatawas$Ival2W2[alldatawas$Ival2W2 > 325000])
NROW(alldatawas$Ival2W2[alldatawas$Ival2W2 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$IVal2W2gross <- alldatawas$Ival2W2

alldatawas$IVal2W2gross[alldatawas$Ival2W2 > 325000] <- (alldatawas$Ival2W2[alldatawas$Ival2W2 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IVal2W2gross[alldatawas$Ival2W2gross > 325000])
table(alldatawas$Ival2W2[alldatawas$Ival2W2 > 325000])

#Second bracketed inheritance

summary(alldatawas$bracked2inhW2)

summary(alldatawas$bracked2inhW2[alldatawas$bracked2inhW2 > 325000])
NROW(alldatawas$bracked2inhW2[alldatawas$bracked2inhW2 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$bracked2inhW2gross <- alldatawas$bracked2inhW2

alldatawas$bracked2inhW2gross[alldatawas$bracked2inhW2 > 325000] <- (alldatawas$bracked2inhW2[alldatawas$bracked2inhW2 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked2inhW2gross[alldatawas$bracked2inhW2gross > 325000])
table(alldatawas$bracked2inhW2[alldatawas$bracked2inhW2 > 325000])

##### Third recent inheritance #####

summary(alldatawas$Ival3W2)

summary(alldatawas$Ival3W2[alldatawas$Ival3W2 > 325000])
NROW(alldatawas$Ival3W2[alldatawas$Ival3W2 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$IVal3W2gross <- alldatawas$Ival3W2

alldatawas$IVal3W2gross[alldatawas$Ival3W2 > 325000] <- (alldatawas$Ival3W2[alldatawas$Ival3W2 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IVal3W2gross[alldatawas$IVal3W2gross > 325000])
table(alldatawas$Ival3W2[alldatawas$Ival3W2 > 325000])

#Third bracketed inheritance

summary(alldatawas$bracked3inhW2)

summary(alldatawas$bracked3inhW2[alldatawas$bracked3inhW2 > 325000])
NROW(alldatawas$bracked3inhW2[alldatawas$bracked3inhW2 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$bracked3inhW2gross <- alldatawas$bracked3inhW2

alldatawas$bracked3inhW2gross[alldatawas$bracked3inhW2 > 325000] <- (alldatawas$bracked3inhW2[alldatawas$bracked3inhW2 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked3inhW2gross[alldatawas$bracked3inhW2gross > 325000])
table(alldatawas$bracked3inhW2[alldatawas$bracked3inhW2 > 325000])


#CONTINUE WITH WAVE THREE RECENT INHERITANCES ####

#Wave one asks for inheritances in the last 2 years. That is, between 2012 and 2010.

#In 2011 the threshold is 325.000 and the rate is 0.4.

#First recent inheritance

summary(alldatawas$IVal1W3)

summary(alldatawas$IVal1W3[alldatawas$IVal1W3 > 325000])
NROW(alldatawas$IVal1W3[alldatawas$IVal1W3 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$IValW3gross <- alldatawas$IVal1W3

alldatawas$IValW3gross[alldatawas$IVal1W3 > 325000] <- (alldatawas$IVal1W3[alldatawas$IVal1W3 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IValW3gross[alldatawas$IValW3gross > 325000])
table(alldatawas$IVal1W3[alldatawas$IVal1W3 > 325000])

#First bracketed inheritance

summary(alldatawas$bracked1inhW3)

summary(alldatawas$bracked1inhW3[alldatawas$bracked1inhW3 > 325000])
NROW(alldatawas$bracked1inhW3[alldatawas$bracked1inhW3 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$bracked1inhW3gross <- alldatawas$bracked1inhW3

alldatawas$bracked1inhW3gross[alldatawas$bracked1inhW3 > 325000] <- (alldatawas$bracked1inhW3[alldatawas$bracked1inhW3 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked1inhW3gross[alldatawas$bracked1inhW3gross > 325000])
table(alldatawas$bracked1inhW3[alldatawas$bracked1inhW3 > 325000])

summary(alldatawas$bracked1inhW3gross)

#Second recent inheritance

summary(alldatawas$IVal2w3_i)

summary(alldatawas$IVal2w3_i[alldatawas$IVal2w3_i > 325000])
NROW(alldatawas$IVal2w3_i[alldatawas$IVal2w3_i > 325000])

#X = (Y-tT)/(1-t)

alldatawas$IVal2W3gross <- alldatawas$IVal2w3_i

alldatawas$IVal2W3gross[alldatawas$IVal2w3_i > 325000] <- (alldatawas$IVal2w3_i[alldatawas$IVal2w3_i > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IVal2W3gross[alldatawas$IVal2W3gross > 325000])
table(alldatawas$IVal2w3_i[alldatawas$IVal2w3_i > 325000])

#Second bracketed inheritance

summary(alldatawas$bracked2inhW3)

summary(alldatawas$bracked2inhW3[alldatawas$bracked2inhW3 > 325000])
NROW(alldatawas$bracked2inhW3[alldatawas$bracked2inhW3 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$bracked2inhW3gross <- alldatawas$bracked2inhW3

alldatawas$bracked2inhW3gross[alldatawas$bracked2inhW3 > 325000] <- (alldatawas$bracked2inhW3[alldatawas$bracked2inhW3 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked2inhW3gross[alldatawas$bracked2inhW3gross > 325000])
table(alldatawas$bracked2inhW3[alldatawas$bracked2inhW3 > 325000])

##### Third recent inheritance #####

summary(alldatawas$IVal3w3_i)

summary(alldatawas$IVal3w3_i[alldatawas$IVal3w3_i > 325000])
NROW(alldatawas$IVal3w3_i[alldatawas$IVal3w3_i > 325000])

#X = (Y-tT)/(1-t)

alldatawas$IVal3W3gross <- alldatawas$IVal3w3_i

alldatawas$IVal3W3gross[alldatawas$IVal3w3_i > 325000] <- (alldatawas$IVal3w3_i[alldatawas$IVal3w3_i > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$IVal3W3gross[alldatawas$IVal3W3gross > 325000])
table(alldatawas$IVal3w3_i[alldatawas$IVal3w3_i > 325000])

#Third bracketed inheritance

summary(alldatawas$bracked3inhW3)

summary(alldatawas$bracked3inhW3[alldatawas$bracked3inhW3 > 325000])
NROW(alldatawas$bracked3inhW3[alldatawas$bracked3inhW3 > 325000])

#X = (Y-tT)/(1-t)

alldatawas$bracked3inhW3gross <- alldatawas$bracked3inhW3

alldatawas$bracked3inhW3gross[alldatawas$bracked3inhW3 > 325000] <- (alldatawas$bracked3inhW3[alldatawas$bracked3inhW3 > 325000]-325000*0.4)/(1-0.4) #We apply the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

table(alldatawas$bracked3inhW3gross[alldatawas$bracked3inhW3gross > 325000])
table(alldatawas$bracked3inhW3[alldatawas$bracked3inhW3 > 325000])


###### AND WE SAVE THE DATA #######

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step3b-bis-pre-tax-new-data.rds")

