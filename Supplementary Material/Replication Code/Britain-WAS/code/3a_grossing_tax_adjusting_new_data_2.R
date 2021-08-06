#we transform net to gross the intergenerational transfers amounts

#Line 204 has a check of the adjustment working

#We used an a version

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)


alldatawas <- readRDS(file = "data_rds/WAS-Nov-After-v-3-2-Step2-Imputation-Pre-Update-new-data.rds")

###### TAX UPDATING FIRST #####  TAX UPDATING FIRST ###################


#THIS IS THE YEAR THE INHERITANCE WAS OBTAINED

summary(alldatawas$IEYrW1[alldatawas$IEYrW1>0])
table(alldatawas$IEYrW1[alldatawas$IEYrW1>0])

table(alldatawas$IEValW1[alldatawas$IEValW1>100000], alldatawas$IEYrW1[alldatawas$IEValW1>100000])

##### NOW WE ARE WORKING WITH THE DATA ALREADY IMPUTED, SO WE NEED TO ASSIGN A YEAR TO THE IMPUTED DATA. FOR THAT PURPUSE, WE ARE GOING TO FOCUS ON THE AGE OF THE INDIVIDUAL ####
#####

#Let us first create a variable with the decade of the age recipient of inheritances###

summary (alldatawas$dvagew3[alldatawas$IEValW1>0])

alldatawas$dvagew3 <- as.numeric(alldatawas$DVAgeW3Band) #in the new data this is the variable. Only details from 19 to 85

alldatawas$decade <- trunc(alldatawas$DVAgeW3Band/10)

summary(alldatawas$decade[alldatawas$IEValW1>0])

#Now we get the average year of reception of inheritance for each group

alldatawas <- alldatawas %>%
  group_by(decade) %>%
  mutate(inhyeardecade = trunc(mean(IEYrW1[IEYrW1>0])))


NROW(alldatawas$IEYrW1[alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic)])

t1 <- table(alldatawas$flaginputlife1basic, alldatawas$IEYrW1) #Before imputing the year

alldatawas$IEYrW1[which(alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic) & alldatawas$IEYrW1 < 1900)] <- alldatawas$inhyeardecade[which(alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic) & alldatawas$IEYrW1 < 1900)] #We add the decade year only to the ones that do not have that information (code -6)

t2 <- table(alldatawas$flaginputlife1basic, alldatawas$IEYrW1) #After imputing the year


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

#We can compare the age distribution of the imputed and the non-imputed inheritances (net non-updated values)

m1 <- alldatawas %>% filter(IEValW1 >0) %>% group_by(DVAge17W1, flaginputlife1basic) %>% summarise (weighted.mean(IEValW1, w1w3wgt))

m2 <- alldatawas %>% filter(IEValW1 >0) %>% group_by(DVAge17W1, flaginputlife1basic) %>% summarise (NROW(IEValW1))
m2$flaginputlife1basic <- as.factor(m2$flaginputlife1basic)


#And we plot against income
plot1 <- ggplot(data = m1, aes(y = `weighted.mean(IEValW1, w1w3wgt)`, x = DVAge17W1, colour = flaginputlife1basic))+
  geom_point() + xlab("Equivalent Income") + ylab("Intergenerational Tranfers") +
  labs(title = "Britain",
       #subtitle = "Marginal Effect of Income on Intergenerational Transfers",
       x = "Equivalent Income", y = "Transfers per adult") +
  #geom_vline(xintercept=c(wtd.quantile (dataukh3$eqinc, q=c(0.2, 0.5, 0.8), weight=dataukh3$hholdweight)),lty="dotted",col="orange", size = 0.9)+
  #ylim(-0, 200000)+
  labs(colour="Education Level")+
  theme(legend.position = "bottom")
#xlim(-0, 300000)+
plot1

#And we plot against income
plot2 <- ggplot(data = m2, aes(y = `NROW(IEValW1)`, x = DVAge17W1, colour = flaginputlife1basic))+
  geom_point() + xlab("Equivalent Income") + ylab("Intergenerational Tranfers") +
  labs(title = "Britain",
       #subtitle = "Marginal Effect of Income on Intergenerational Transfers",
       x = "Equivalent Income", y = "Transfers per adult") +
  #geom_vline(xintercept=c(wtd.quantile (dataukh3$eqinc, q=c(0.2, 0.5, 0.8), weight=dataukh3$hholdweight)),lty="dotted",col="orange", size = 0.9)+
  #ylim(-0, 200000)+
  labs(colour="Education Level")+
  theme(legend.position = "bottom")
#xlim(-0, 300000)+
plot2

#ggsave(poissonplotuk, file="poisson-uk.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)



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


#pretax = read.csv("pre-tax-update.csv", sep = ";", header = T, as.is = T)

pretax = read.csv("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/aux_files/pre-tax-update-adjusted.csv", sep = ";", header = T, as.is = T)


#### Using the adjusted tax rate ###

#we assume that there can be two descendants. Also assume that many of the top transfers received can be of capital and productive nature, then with an effective tax rate lower than the nominal. In fact, we cut in two the rates when the amount is 10x the threshold (that is in the ADJUSTED csv file already)

#We also assume two possible descendants-siblings, thus doubling the amount (tax would be applied to a bigger amount). Also we also double the exemption threshold in the adjusted .csv file and, consistently, we divide by two the gross amount obtained.


#LET'S GET TO IT

#We first double the amount:

alldatawas$IEValW1double <- alldatawas$IEValW1
alldatawas$IEValW1double[alldatawas$IEValW1>0] <- alldatawas$IEValW1[alldatawas$IEValW1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYrW1, alldatawas$IEValW1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("IEYrW1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IEValW1double[alldatawas1$netmax>0])

alldatawas1$grossinhlifeW1adjusted <- (alldatawas1$bracket+(alldatawas1$IEValW1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhlifeW1adjusted[!is.na(alldatawas1$grossinhlifeW1adjusted)])

NROW(alldatawas1$IEValW1[alldatawas1$IEValW1>0])

table(alldatawas1$grossinhlifeW1adjusted[alldatawas1$grossinhlifeW1adjusted>100000], alldatawas1$IEValW1[alldatawas1$grossinhlifeW1adjusted>100000])

a <- cbind(alldatawas1$grossinhlifeW1adjusted[!is.na(alldatawas1$grossinhlifeW1adjusted)], alldatawas1$IEValW1[!is.na(alldatawas1$grossinhlifeW1adjusted)])

a

#we can see the adjusted value is always equal or greater than the original and the adjustment works

alldatawas1$pretaxinhlifeW1adjusted <- alldatawas1$IEValW1

alldatawas1$pretaxinhlifeW1adjusted[!is.na(alldatawas1$grossinhlifeW1adjusted)] <- alldatawas1$grossinhlifeW1adjusted[!is.na(alldatawas1$grossinhlifeW1adjusted)]

summary(alldatawas1$IEValW1[alldatawas1$IEValW1>0])

summary(alldatawas1$pretaxinhlifeW1adjusted[alldatawas1$pretaxinhlifeW1adjusted>0])

length(alldatawas1$pretaxinhlifeW1adjusted[alldatawas1$pretaxinhlifeW1adjusted==alldatawas1$IEValW1])
#[1] 27764
length(alldatawas1$pretaxinhlifeW1adjusted[alldatawas1$pretaxinhlifeW1adjusted!=alldatawas1$IEValW1])
#[1] 303

NROW(alldatawas1$IEValW1[alldatawas1$IEValW1>140000 & alldatawas1$IEYrW1==1991])

length(alldatawas1$pretaxinhlifeW1adjusted[alldatawas1$pretaxinhlifeW1adjusted!=alldatawas1$IEValW1 & alldatawas1$IEYrW1==1991])

summary(alldatawas1$IEValW1[alldatawas1$IEValW1>0 & alldatawas1$IEYrW1==1991])

summary(alldatawas1$pretaxinhlifeW1adjusted[alldatawas1$pretaxinhlifeW1adjusted>0 & alldatawas1$IEYrW1==1991])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlifeW1adjusted <- alldatawas1$pretaxinhlifeW1adjusted


## AND THEN REPEAT THE PROCESS FOR THE SECOND LIFETIME INHERITANCE

alldatawas$IEVal2W1double <- alldatawas$IEVal2W1
alldatawas$IEVal2W1double[alldatawas$IEVal2W1>0] <- alldatawas$IEVal2W1[alldatawas$IEVal2W1>0]*2


#LET'S GET TO IT

alldatawas$netmax2 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr2W1, alldatawas$IEVal2W1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax2)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas2 <- merge(alldatawas, pretax, by.x = c("IEYr2W1", "netmax2"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas2$rate)

table(alldatawas2$netmax2[alldatawas2$netmax2>0])

table(alldatawas2$netmax2[alldatawas2$netmax2>0], alldatawas2$IEVal2W1double[alldatawas2$netmax2>0])

alldatawas2$grossinhlife2W1adjusted <- (alldatawas2$bracket+(alldatawas2$IEVal2W1double-alldatawas2$netmax2)/(1-alldatawas2$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawas2$grossinhlife2W1adjusted[!is.na(alldatawas2$grossinhlife2W1adjusted)])

NROW(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>0])

table(alldatawas2$grossinhlife2W1adjusted[alldatawas2$grossinhlife2W1adjusted>100000], alldatawas2$IEVal2W1[alldatawas2$grossinhlife2W1adjusted>100000])

alldatawas2$pretaxinhlife2W1adjusted <- alldatawas2$IEVal2W1

alldatawas2$pretaxinhlife2W1adjusted[!is.na(alldatawas2$grossinhlife2W1adjusted)] <- alldatawas2$grossinhlife2W1adjusted[!is.na(alldatawas2$grossinhlife2W1adjusted)]

summary(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>0])

summary(alldatawas2$pretaxinhlife2W1adjusted[alldatawas2$pretaxinhlife2W1adjusted>0])

length(alldatawas2$pretaxinhlife2W1adjusted[alldatawas2$pretaxinhlife2W1adjusted==alldatawas2$IEVal2W1])
#[1] 27764
length(alldatawas2$pretaxinhlife2W1adjusted[alldatawas2$pretaxinhlife2W1adjusted!=alldatawas2$IEVal2W1])
#[1] 139

NROW(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>140000 & alldatawas2$IEYr2W1==2000])

length(alldatawas2$pretaxinhlife2W1adjusted[alldatawas2$pretaxinhlife2W1adjusted!=alldatawas2$IEVal2W1 & alldatawas2$IEYr2W1==2000])

summary(alldatawas2$IEVal2W1[alldatawas2$IEVal2W1>0 & alldatawas2$IEYr2W1==2000])

summary(alldatawas2$pretaxinhlife2W1adjusted[alldatawas2$pretaxinhlife2W1adjusted>0 & alldatawas2$IEYr2W1==2000])

#We finally add the new gross variable to the original dataset

alldatawas2 <- alldatawas2[order(alldatawas2$CaseW3,alldatawas2$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife2W1adjusted <- alldatawas2$pretaxinhlife2W1adjusted


## AND THEN REPEAT THE PROCESS FOR THE THIRD LIFETIME INHERITANCE

alldatawas$IEVal3W1double <- alldatawas$IEValW1
alldatawas$IEVal3W1double[alldatawas$IEVal3W1>0] <- alldatawas$IEVal3W1[alldatawas$IEVal3W1>0]*2


#LET'S GET TO IT

alldatawas$netmax3 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr3W1, alldatawas$IEVal3W1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax3)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas3 <- merge(alldatawas, pretax, by.x = c("IEYr3W1", "netmax3"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas3$rate)

table(alldatawas3$netmax3[alldatawas3$netmax3>0])

table(alldatawas3$netmax3[alldatawas3$netmax3>0], alldatawas3$IEVal3W1[alldatawas3$netmax3>0])

alldatawas3$grossinhlife3W1adjusted <- (alldatawas3$bracket+(alldatawas3$IEVal3W1double-alldatawas3$netmax3)/(1-alldatawas3$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawas3$grossinhlife3W1adjusted[!is.na(alldatawas3$grossinhlife3W1adjusted)])

NROW(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>0])

table(alldatawas3$grossinhlife3W1adjusted[alldatawas3$grossinhlife3W1adjusted>100000], alldatawas3$IEVal3W1[alldatawas3$grossinhlife3W1adjusted>100000])

alldatawas3$pretaxinhlife3W1adjusted <- alldatawas3$IEVal3W1

alldatawas3$pretaxinhlife3W1adjusted[!is.na(alldatawas3$grossinhlife3W1adjusted)] <- alldatawas3$grossinhlife3W1adjusted[!is.na(alldatawas3$grossinhlife3W1adjusted)]

summary(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>0])

summary(alldatawas3$pretaxinhlife3W1adjusted[alldatawas3$pretaxinhlife3W1adjusted>0])

length(alldatawas3$pretaxinhlife3W1adjusted[alldatawas3$pretaxinhlife3W1adjusted==alldatawas3$IEVal3W1])

length(alldatawas3$pretaxinhlife3W1adjusted[alldatawas3$pretaxinhlife3W1adjusted!=alldatawas3$IEVal3W1])


NROW(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>140000 & alldatawas3$IEYr3W1==1991])

length(alldatawas3$pretaxinhlife3W1adjusted[alldatawas3$pretaxinhlife3W1adjusted!=alldatawas3$IEVal3W1 & alldatawas3$IEYr3W1==1991])

summary(alldatawas3$IEVal3W1[alldatawas3$IEVal3W1>0 & alldatawas3$IEYr3W1==1991])

summary(alldatawas3$pretaxinhlife3W1adjusted[alldatawas3$pretaxinhlife3W1adjusted>0 & alldatawas3$IEYr3W1==1991])

#We finally add the new gross variable to the original dataset

alldatawas3 <- alldatawas3[order(alldatawas3$CaseW3,alldatawas3$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife3W1adjusted <- alldatawas3$pretaxinhlife3W1adjusted


##### NOW FOR THE FIRST BRACKETED VALUE ####

alldatawas$bracked1inhW1pastdouble <- alldatawas$bracked1inhW1past
alldatawas$bracked1inhW1pastdouble[alldatawas$bracked1inhW1past>0] <- alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0]*2

table(alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1pastdouble>0], alldatawas$IEYrW1[alldatawas$bracked1inhW1pastdouble>0])

alldatawas$netmaxb1 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYrW1, alldatawas$bracked1inhW1pastdouble) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmaxb1)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawasb1 <- merge(alldatawas, pretax, by.x = c("IEYrW1", "netmaxb1"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawasb1$rate)

table(alldatawasb1$netmaxb1[alldatawasb1$netmaxb1>0])

table(alldatawasb1$netmaxb1[alldatawasb1$netmaxb1>0], alldatawasb1$bracked1inhW1pastdouble[alldatawasb1$netmaxb1>0])

alldatawasb1$grossinhlifebW1 <- (alldatawasb1$bracket+(alldatawasb1$bracked1inhW1pastdouble-alldatawasb1$netmaxb1)/(1-alldatawasb1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawasb1$grossinhlifebW1adjusted[!is.na(alldatawasb1$grossinhlifebW1adjusted)])

NROW(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>0])

table(alldatawasb1$grossinhlifebW1adjusted[alldatawasb1$grossinhlifebW1adjusted>=0], alldatawasb1$bracked1inhW1past[alldatawasb1$grossinhlifebW1adjusted>=0])

alldatawasb1$pretaxinhlifebW1adjusted <- alldatawasb1$bracked1inhW1past

alldatawasb1$pretaxinhlifebW1adjusted[!is.na(alldatawasb1$grossinhlifebW1adjusted) & alldatawasb1$grossinhlifebW1adjusted>0 ] <- alldatawasb1$grossinhlifebW1adjusted[!is.na(alldatawasb1$grossinhlifebW1adjusted) & alldatawasb1$grossinhlifebW1adjusted>0]

summary(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>0])

summary(alldatawasb1$pretaxinhlifebW1adjusted[alldatawasb1$pretaxinhlifebW1adjusted>0])

length(alldatawasb1$pretaxinhlifebW1adjusted[alldatawasb1$pretaxinhlifebW1adjusted==alldatawasb1$bracked1inhW1past])
#[1] 27764
length(alldatawasb1$pretaxinhlifebW1adjusted[alldatawasb1$pretaxinhlifebW1adjusted!=alldatawasb1$bracked1inhW1past])
#[1] 139

NROW(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>140000 & alldatawasb1$IEYrW1==1991])

length(alldatawasb1$pretaxinhlifebW1adjusted[alldatawasb1$pretaxinhlifebW1adjusted!=alldatawasb1$bracked1inhW1past & alldatawasb1$IEYrW1==1991])

summary(alldatawasb1$bracked1inhW1past[alldatawasb1$bracked1inhW1past>0 & alldatawasb1$IEYrW1==1991])

summary(alldatawasb1$pretaxinhlifebW1adjusted[alldatawasb1$pretaxinhlifebW1adjusted>0 & alldatawasb1$IEYrW1==1991])

#We finally add the new gross variable to the original dataset

alldatawasb1 <- alldatawasb1[order(alldatawasb1$CaseW3,alldatawasb1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlifebW1adjusted <- alldatawasb1$pretaxinhlifebW1adjusted

summary(alldatawasb1$pretaxinhlifebW1adjusted)
summary(alldatawas$pretaxinhlifebW1adjusted)
summary(alldatawas$bracked1inhW1past)

summary(alldatawas$bracked1inhW1past)
summary(alldatawas$pretaxinhlifebW1adjusted)

NROW(alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past>0])
NROW(alldatawas$pretaxinhlifebW1adjusted[alldatawas$pretaxinhlifebW1adjusted>0])

NROW(alldatawas$bracked1inhW1past[alldatawas$bracked1inhW1past==0])
NROW(alldatawas$pretaxinhlifebW1adjusted[alldatawas$pretaxinhlifebW1adjusted==0])


##### NOW FOR THE SECOND BRACKETED VALUE ####

alldatawas$bracked2inhW1pastdouble <- alldatawas$bracked2inhW1past
alldatawas$bracked2inhW1pastdouble[alldatawas$bracked2inhW1past>0] <- alldatawas$bracked2inhW1past[alldatawas$bracked2inhW1past>0]*2


alldatawas$netmaxb2 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr2W1, alldatawas$bracked2inhW1pastdouble) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmaxb2)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawasb2 <- merge(alldatawas, pretax, by.x = c("IEYr2W1", "netmaxb2"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawasb2$rate)

table(alldatawasb2$netmaxb2[alldatawasb2$netmaxb2>0])

table(alldatawasb2$netmaxb2[alldatawasb2$netmaxb2>0], alldatawasb2$bracked2inhW1pastdouble[alldatawasb2$netmaxb2>0])

alldatawasb2$grossinhlife2bW1adjusted <- (alldatawasb2$bracket+(alldatawasb2$bracked2inhW1pastdouble-alldatawasb2$netmaxb2)/(1-alldatawasb2$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawasb2$grossinhlife2bW1adjusted[!is.na(alldatawasb2$grossinhlife2bW1adjusted)])

NROW(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>0])

table(alldatawasb2$grossinhlife2bW1adjusted[alldatawasb2$grossinhlife2bW1adjusted>100000], alldatawasb2$bracked2inhW1past[alldatawasb2$grossinhlife2bW1adjusted>100000])

alldatawasb2$pretaxinhlife2bW1adjusted <- alldatawasb2$bracked2inhW1past

alldatawasb2$pretaxinhlife2bW1adjusted[!is.na(alldatawasb2$grossinhlife2bW1adjusted)] <- alldatawasb2$grossinhlife2bW1adjusted[!is.na(alldatawasb2$grossinhlife2bW1adjusted)]

summary(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>0])

summary(alldatawasb2$pretaxinhlife2bW1adjusted[alldatawasb2$pretaxinhlife2bW1adjusted>0])

length(alldatawasb2$pretaxinhlife2bW1adjusted[alldatawasb2$pretaxinhlife2bW1adjusted==alldatawasb2$bracked2inhW1past])
#[1] 27764
length(alldatawasb2$pretaxinhlife2bW1adjusted[alldatawasb2$pretaxinhlife2bW1adjusted!=alldatawasb2$bracked2inhW1past])
#[1] 139

NROW(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>140000 & alldatawasb2$IEYr2W1==1991])

length(alldatawasb2$pretaxinhlife2bW1adjusted[alldatawasb2$pretaxinhlife2bW1adjusted!=alldatawasb2$bracked2inhW1past & alldatawasb2$IEYr2W1==1991])

summary(alldatawasb2$bracked2inhW1past[alldatawasb2$bracked2inhW1past>0 & alldatawasb2$IEYr2W1==1991])

summary(alldatawasb2$pretaxinhlife2bW1adjusted[alldatawasb2$pretaxinhlife2bW1adjusted>0 & alldatawasb2$IEYr2W1==1991])

#We finally add the new gross variable to the original dataset

alldatawasb2 <- alldatawasb2[order(alldatawasb2$CaseW3,alldatawasb2$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife2bW1adjusted <- alldatawasb2$pretaxinhlife2bW1adjusted



##### NOW FOR THE THIRD BRACKETED VALUE ####

alldatawas$bracked3inhW1pastdouble <- alldatawas$bracked3inhW1past
alldatawas$bracked3inhW1pastdouble[alldatawas$bracked3inhW1past>0] <- alldatawas$bracked3inhW1past[alldatawas$bracked3inhW1past>0]*2



alldatawas$netmaxb3 <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$IEYr3W1, alldatawas$bracked3inhW1pastdouble) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmaxb3)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawasb3 <- merge(alldatawas, pretax, by.x = c("IEYr3W1", "netmaxb3"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawasb3$rate)

table(alldatawasb3$netmaxb3[alldatawasb3$netmaxb3>0])

table(alldatawasb3$netmaxb3[alldatawasb3$netmaxb3>0], alldatawasb3$bracked3inhW1pastdouble[alldatawasb3$netmaxb3>0])

alldatawasb3$grossinhlife3bW1adjusted <- (alldatawasb3$bracket+(alldatawasb3$bracked3inhW1pastdouble-alldatawasb3$netmaxb3)/(1-alldatawasb3$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount.

NROW(alldatawasb3$grossinhlife3bW1adjusted[!is.na(alldatawasb3$grossinhlife3bW1adjusted)])

NROW(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>0])

table(alldatawasb3$grossinhlife3bW1adjusted[alldatawasb3$grossinhlife3bW1adjusted>100000], alldatawasb3$bracked3inhW1past[alldatawasb3$grossinhlife3bW1adjusted>100000])

alldatawasb3$pretaxinhlife3bW1adjusted <- alldatawasb3$bracked3inhW1past

alldatawasb3$pretaxinhlife3bW1adjusted[!is.na(alldatawasb3$grossinhlife3bW1adjusted)] <- alldatawasb3$grossinhlife3bW1adjusted[!is.na(alldatawasb3$grossinhlife3bW1adjusted)]

summary(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>0])

summary(alldatawasb3$pretaxinhlife3bW1adjusted[alldatawasb3$pretaxinhlife3bW1adjusted>0])

length(alldatawasb3$pretaxinhlife3bW1adjusted[alldatawasb3$pretaxinhlife3bW1adjusted==alldatawasb3$bracked3inhW1past])
#[1] 27764
length(alldatawasb3$pretaxinhlife3bW1adjusted[alldatawasb3$pretaxinhlife3bW1adjusted!=alldatawasb3$bracked3inhW1past])
#[1] 139

NROW(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>140000 & alldatawasb3$IEYr3W1==1991])

length(alldatawasb3$pretaxinhlife3bW1adjusted[alldatawasb3$pretaxinhlife3bW1adjusted!=alldatawasb3$bracked3inhW1past & alldatawasb3$IEYr3W1==1991])

summary(alldatawasb3$bracked3inhW1past[alldatawasb3$bracked3inhW1past>0 & alldatawasb3$IEYr3W1==1991])

summary(alldatawasb3$pretaxinhlife3bW1adjusted[alldatawasb3$pretaxinhlife3bW1adjusted>0 & alldatawasb3$IEYr3W1==1991])

#We finally add the new gross variable to the original dataset

alldatawasb3 <- alldatawasb3[order(alldatawasb3$CaseW3,alldatawasb3$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhlife3bW1adjusted <- alldatawasb3$pretaxinhlife3bW1adjusted



#CONTINUE WITH WAVE ONE RECENT INHERITANCES ####

#We create a variable with the year for each of the recent

alldatawas$yearinhrecent1 <- 0

alldatawas$yearinhrecent1[alldatawas$IValW1 >0] <- 2008
alldatawas$yearinhrecent1[alldatawas$IVal2W1 > 0] <- 2008
alldatawas$yearinhrecent1[alldatawas$IVal3W1 > 0] <- 2008

alldatawas$yearinhrecent1[alldatawas$bracked1inhW1 > 0] <- 2008
alldatawas$yearinhrecent1[alldatawas$bracked2inhW1 > 0] <- 2008
alldatawas$yearinhrecent1[alldatawas$bracked3inhW1 > 0] <- 2008


#Now for the second wave

alldatawas$yearinhrecent2 <- 0


alldatawas$yearinhrecent2[alldatawas$IvalW2 > 0] <- 2010
alldatawas$yearinhrecent2[alldatawas$Ival2W2 > 0] <- 2010
alldatawas$yearinhrecent2[alldatawas$Ival3W2 > 0] <- 2010

alldatawas$yearinhrecent2[alldatawas$bracked1inhW2 > 0] <- 2010
alldatawas$yearinhrecent2[alldatawas$bracked2inhW2 > 0] <- 2010
alldatawas$yearinhrecent2[alldatawas$bracked3inhW2 > 0] <- 2010

#Now for the third wave

alldatawas$yearinhrecent3 <- 0


alldatawas$yearinhrecent3[alldatawas$IVal1W3 > 0] <- 2012
alldatawas$yearinhrecent3[alldatawas$IVal2w3_i > 0] <- 2012
alldatawas$yearinhrecent3[alldatawas$IVal3w3_i > 0] <- 2012

alldatawas$yearinhrecent3[alldatawas$bracked1inhW3 > 0] <- 2012
alldatawas$yearinhrecent3[alldatawas$bracked2inhW3 > 0] <- 2012
alldatawas$yearinhrecent3[alldatawas$bracked3inhW3 > 0] <- 2012

####

#LET'S GET TO IT

#We first double the amount:

alldatawas$IValW1double <- alldatawas$IValW1
alldatawas$IValW1double[alldatawas$IValW1>0] <- alldatawas$IValW1[alldatawas$IValW1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent1, alldatawas$IValW1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IValW1double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent1W1adjusted <- (alldatawas1$bracket+(alldatawas1$IValW1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent1W1adjusted[!is.na(alldatawas1$grossinhrecent1W1adjusted)])

NROW(alldatawas1$IValW1[alldatawas1$IValW1>0])

table(alldatawas1$grossinhrecent1W1adjusted[alldatawas1$grossinhrecent1W1adjusted>100000], alldatawas1$IValW1[alldatawas1$grossinhrecent1W1adjusted>100000])

alldatawas1$pretaxinhrecent1W1adjusted <- alldatawas1$IValW1

alldatawas1$pretaxinhrecent1W1adjusted[!is.na(alldatawas1$grossinhrecent1W1adjusted)] <- alldatawas1$grossinhrecent1W1adjusted[!is.na(alldatawas1$grossinhrecent1W1adjusted)]

summary(alldatawas1$IValW1[alldatawas1$IValW1>0])

summary(alldatawas1$pretaxinhrecent1W1adjusted[alldatawas1$pretaxinhrecent1W1adjusted>0])

length(alldatawas1$pretaxinhrecent1W1adjusted[alldatawas1$pretaxinhrecent1W1adjusted==alldatawas1$IValW1])
#[1] 27764
length(alldatawas1$pretaxinhrecent1W1adjusted[alldatawas1$pretaxinhrecent1W1adjusted!=alldatawas1$IValW1])
#[1] 139

NROW(alldatawas1$IValW1[alldatawas1$IValW1>140000 & alldatawas1$yearinhrecent1==2008])

length(alldatawas1$pretaxinhrecent1W1adjusted[alldatawas1$pretaxinhrecent1W1adjusted!=alldatawas1$IValW1 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$IValW1[alldatawas1$IValW1>0 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$pretaxinhrecent1W1adjusted[alldatawas1$pretaxinhrecent1W1adjusted>0 & alldatawas1$yearinhrecent1==2008])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent1W1adjusted <- alldatawas1$pretaxinhrecent1W1adjusted

## And now the second recent inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$IVal2W1double <- alldatawas$IVal2W1
alldatawas$IVal2W1double[alldatawas$IVal2W1>0] <- alldatawas$IVal2W1[alldatawas$IVal2W1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent1, alldatawas$IVal2W1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IVal2W1double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent2W1adjusted <- (alldatawas1$bracket+(alldatawas1$IVal2W1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent2W1adjusted[!is.na(alldatawas1$grossinhrecent2W1adjusted)])

NROW(alldatawas1$IVal2W1[alldatawas1$IVal2W1>0])

table(alldatawas1$grossinhrecent2W1adjusted[alldatawas1$grossinhrecent2W1adjusted>100000], alldatawas1$IVal2W1[alldatawas1$grossinhrecent2W1adjusted>100000])

alldatawas1$pretaxinhrecent2W1adjusted <- alldatawas1$IVal2W1

alldatawas1$pretaxinhrecent2W1adjusted[!is.na(alldatawas1$grossinhrecent2W1adjusted)] <- alldatawas1$grossinhrecent2W1adjusted[!is.na(alldatawas1$grossinhrecent2W1adjusted)]

summary(alldatawas1$IVal2W1[alldatawas1$IVal2W1>0])

summary(alldatawas1$pretaxinhrecent2W1adjusted[alldatawas1$pretaxinhrecent2W1adjusted>0])

length(alldatawas1$pretaxinhrecent2W1adjusted[alldatawas1$pretaxinhrecent2W1adjusted==alldatawas1$IVal2W1])
#[1] 27764
length(alldatawas1$pretaxinhrecent2W1adjusted[alldatawas1$pretaxinhrecent2W1adjusted!=alldatawas1$IVal2W1])
#[1] 139

NROW(alldatawas1$IVal2W1[alldatawas1$IVal2W1>140000 & alldatawas1$yearinhrecent1==2008])

length(alldatawas1$pretaxinhrecent2W1adjusted[alldatawas1$pretaxinhrecent2W1adjusted!=alldatawas1$IVal2W1 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$IVal2W1[alldatawas1$IVal2W1>0 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$pretaxinhrecent2W1adjusted[alldatawas1$pretaxinhrecent2W1adjusted>0 & alldatawas1$yearinhrecent1==2008])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent2W1adjusted <- alldatawas1$pretaxinhrecent2W1adjusted

## And now the third recent inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$IVal3W1double <- alldatawas$IVal3W1
alldatawas$IVal3W1double[alldatawas$IVal3W1>0] <- alldatawas$IVal3W1[alldatawas$IVal3W1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent1, alldatawas$IVal3W1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IVal3W1double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent3W1adjusted <- (alldatawas1$bracket+(alldatawas1$IVal3W1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent3W1adjusted[!is.na(alldatawas1$grossinhrecent3W1adjusted)])

NROW(alldatawas1$IVal3W1[alldatawas1$IVal3W1>0])

table(alldatawas1$grossinhrecent3W1adjusted[alldatawas1$grossinhrecent3W1adjusted>100000], alldatawas1$IVal3W1[alldatawas1$grossinhrecent3W1adjusted>100000])

alldatawas1$pretaxinhrecent3W1adjusted <- alldatawas1$IVal3W1

alldatawas1$pretaxinhrecent3W1adjusted[!is.na(alldatawas1$grossinhrecent3W1adjusted)] <- alldatawas1$grossinhrecent3W1adjusted[!is.na(alldatawas1$grossinhrecent3W1adjusted)]

summary(alldatawas1$IVal3W1[alldatawas1$IVal3W1>0])

summary(alldatawas1$pretaxinhrecent3W1adjusted[alldatawas1$pretaxinhrecent3W1adjusted>0])

length(alldatawas1$pretaxinhrecent3W1adjusted[alldatawas1$pretaxinhrecent3W1adjusted==alldatawas1$IVal3W1])
#[1] 27764
length(alldatawas1$pretaxinhrecent3W1adjusted[alldatawas1$pretaxinhrecent3W1adjusted!=alldatawas1$IVal3W1])
#[1] 139

NROW(alldatawas1$IVal3W1[alldatawas1$IVal3W1>140000 & alldatawas1$yearinhrecent1==2008])

length(alldatawas1$pretaxinhrecent3W1adjusted[alldatawas1$pretaxinhrecent3W1adjusted!=alldatawas1$IVal3W1 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$IVal3W1[alldatawas1$IVal3W1>0 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$pretaxinhrecent3W1adjusted[alldatawas1$pretaxinhrecent3W1adjusted>0 & alldatawas1$yearinhrecent1==2008])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent3W1adjusted <- alldatawas1$pretaxinhrecent3W1adjusted

## And now the first recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked1inhW1double <- alldatawas$bracked1inhW1
alldatawas$bracked1inhW1double[alldatawas$bracked1inhW1>0] <- alldatawas$bracked1inhW1[alldatawas$bracked1inhW1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent1, alldatawas$bracked1inhW1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked1inhW1double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked1inhW1adjusted <- (alldatawas1$bracket+(alldatawas1$bracked1inhW1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked1inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW1adjusted)])

NROW(alldatawas1$bracked1inhW1[alldatawas1$bracked1inhW1>0])

table(alldatawas1$grossinhrecentbracked1inhW1adjusted[alldatawas1$grossinhrecentbracked1inhW1adjusted>100000], alldatawas1$bracked1inhW1[alldatawas1$grossinhrecentbracked1inhW1adjusted>100000])

alldatawas1$pretaxinhrecentbracked1inhW1adjusted <- alldatawas1$bracked1inhW1

alldatawas1$pretaxinhrecentbracked1inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW1adjusted)] <- alldatawas1$grossinhrecentbracked1inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW1adjusted)]

summary(alldatawas1$bracked1inhW1[alldatawas1$bracked1inhW1>0])

summary(alldatawas1$pretaxinhrecentbracked1inhW1adjusted[alldatawas1$pretaxinhrecentbracked1inhW1adjusted>0])

length(alldatawas1$pretaxinhrecentbracked1inhW1adjusted[alldatawas1$pretaxinhrecentbracked1inhW1adjusted==alldatawas1$bracked1inhW1])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked1inhW1adjusted[alldatawas1$pretaxinhrecentbracked1inhW1adjusted!=alldatawas1$bracked1inhW1])
#[1] 139

NROW(alldatawas1$bracked1inhW1[alldatawas1$bracked1inhW1>140000 & alldatawas1$yearinhrecent1==2008])

length(alldatawas1$pretaxinhrecentbracked1inhW1adjusted[alldatawas1$pretaxinhrecentbracked1inhW1adjusted!=alldatawas1$bracked1inhW1 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$bracked1inhW1[alldatawas1$bracked1inhW1>0 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$pretaxinhrecentbracked1inhW1adjusted[alldatawas1$pretaxinhrecentbracked1inhW1adjusted>0 & alldatawas1$yearinhrecent1==2008])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked1inhW1adjusted <- alldatawas1$pretaxinhrecentbracked1inhW1adjusted


## And now the second recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked2inhW1double <- alldatawas$bracked2inhW1
alldatawas$bracked2inhW1double[alldatawas$bracked2inhW1>0] <- alldatawas$bracked2inhW1[alldatawas$bracked2inhW1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent1, alldatawas$bracked2inhW1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked2inhW1double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked2inhW1adjusted <- (alldatawas1$bracket+(alldatawas1$bracked2inhW1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked2inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW1adjusted)])

NROW(alldatawas1$bracked2inhW1[alldatawas1$bracked2inhW1>0])

table(alldatawas1$grossinhrecentbracked2inhW1adjusted[alldatawas1$grossinhrecentbracked2inhW1adjusted>100000], alldatawas1$bracked2inhW1[alldatawas1$grossinhrecentbracked2inhW1adjusted>100000])

alldatawas1$pretaxinhrecentbracked2inhW1adjusted <- alldatawas1$bracked2inhW1

alldatawas1$pretaxinhrecentbracked2inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW1adjusted)] <- alldatawas1$grossinhrecentbracked2inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW1adjusted)]

summary(alldatawas1$bracked2inhW1[alldatawas1$bracked2inhW1>0])

summary(alldatawas1$pretaxinhrecentbracked2inhW1adjusted[alldatawas1$pretaxinhrecentbracked2inhW1adjusted>0])

length(alldatawas1$pretaxinhrecentbracked2inhW1adjusted[alldatawas1$pretaxinhrecentbracked2inhW1adjusted==alldatawas1$bracked2inhW1])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked2inhW1adjusted[alldatawas1$pretaxinhrecentbracked2inhW1adjusted!=alldatawas1$bracked2inhW1])
#[1] 139

NROW(alldatawas1$bracked2inhW1[alldatawas1$bracked2inhW1>140000 & alldatawas1$yearinhrecent1==2008])

length(alldatawas1$pretaxinhrecentbracked2inhW1adjusted[alldatawas1$pretaxinhrecentbracked2inhW1adjusted!=alldatawas1$bracked2inhW1 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$bracked2inhW1[alldatawas1$bracked2inhW1>0 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$pretaxinhrecentbracked2inhW1adjusted[alldatawas1$pretaxinhrecentbracked2inhW1adjusted>0 & alldatawas1$yearinhrecent1==2008])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked2inhW1adjusted <- alldatawas1$pretaxinhrecentbracked2inhW1adjusted


## And now the third recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked3inhW1double <- alldatawas$bracked3inhW1
alldatawas$bracked3inhW1double[alldatawas$bracked3inhW1>0] <- alldatawas$bracked3inhW1[alldatawas$bracked3inhW1>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent1, alldatawas$bracked3inhW1double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent1", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked3inhW1double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked3inhW1adjusted <- (alldatawas1$bracket+(alldatawas1$bracked3inhW1double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked3inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW1adjusted)])

NROW(alldatawas1$bracked3inhW1[alldatawas1$bracked3inhW1>0])

table(alldatawas1$grossinhrecentbracked3inhW1adjusted[alldatawas1$grossinhrecentbracked3inhW1adjusted>100000], alldatawas1$bracked3inhW1[alldatawas1$grossinhrecentbracked3inhW1adjusted>100000])

alldatawas1$pretaxinhrecentbracked3inhW1adjusted <- alldatawas1$bracked3inhW1

alldatawas1$pretaxinhrecentbracked3inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW1adjusted)] <- alldatawas1$grossinhrecentbracked3inhW1adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW1adjusted)]

summary(alldatawas1$bracked3inhW1[alldatawas1$bracked3inhW1>0])

summary(alldatawas1$pretaxinhrecentbracked3inhW1adjusted[alldatawas1$pretaxinhrecentbracked3inhW1adjusted>0])

length(alldatawas1$pretaxinhrecentbracked3inhW1adjusted[alldatawas1$pretaxinhrecentbracked3inhW1adjusted==alldatawas1$bracked3inhW1])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked3inhW1adjusted[alldatawas1$pretaxinhrecentbracked3inhW1adjusted!=alldatawas1$bracked3inhW1])
#[1] 139

NROW(alldatawas1$bracked3inhW1[alldatawas1$bracked3inhW1>140000 & alldatawas1$yearinhrecent1==2008])

length(alldatawas1$pretaxinhrecentbracked3inhW1adjusted[alldatawas1$pretaxinhrecentbracked3inhW1adjusted!=alldatawas1$bracked3inhW1 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$bracked3inhW1[alldatawas1$bracked3inhW1>0 & alldatawas1$yearinhrecent1==2008])

summary(alldatawas1$pretaxinhrecentbracked3inhW1adjusted[alldatawas1$pretaxinhrecentbracked3inhW1adjusted>0 & alldatawas1$yearinhrecent1==2008])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked3inhW1adjusted <- alldatawas1$pretaxinhrecentbracked3inhW1adjusted






###### NOW WITH WAVE 2 RECENT ONES #####








#LET'S GET TO IT

#We first double the amount:

alldatawas$IvalW2double <- alldatawas$IvalW2
alldatawas$IvalW2double[alldatawas$IvalW2>0] <- alldatawas$IvalW2[alldatawas$IvalW2>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent2, alldatawas$IvalW2double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent2", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IvalW2double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent1W2adjusted <- (alldatawas1$bracket+(alldatawas1$IvalW2double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent1W2adjusted[!is.na(alldatawas1$grossinhrecent1W2adjusted)])

NROW(alldatawas1$IvalW2[alldatawas1$IvalW2>0])

table(alldatawas1$grossinhrecent1W2adjusted[alldatawas1$grossinhrecent1W2adjusted>100000], alldatawas1$IvalW2[alldatawas1$grossinhrecent1W2adjusted>100000])

alldatawas1$pretaxinhrecent1W2adjusted <- alldatawas1$IvalW2

alldatawas1$pretaxinhrecent1W2adjusted[!is.na(alldatawas1$grossinhrecent1W2adjusted)] <- alldatawas1$grossinhrecent1W2adjusted[!is.na(alldatawas1$grossinhrecent1W2adjusted)]

summary(alldatawas1$IvalW2[alldatawas1$IvalW2>0])

summary(alldatawas1$pretaxinhrecent1W2adjusted[alldatawas1$pretaxinhrecent1W2adjusted>0])

length(alldatawas1$pretaxinhrecent1W2adjusted[alldatawas1$pretaxinhrecent1W2adjusted==alldatawas1$IvalW2])
#[1] 27764
length(alldatawas1$pretaxinhrecent1W2adjusted[alldatawas1$pretaxinhrecent1W2adjusted!=alldatawas1$IvalW2])
#[1] 139

NROW(alldatawas1$IvalW2[alldatawas1$IvalW2>140000 & alldatawas1$yearinhrecent2==2010])

length(alldatawas1$pretaxinhrecent1W2adjusted[alldatawas1$pretaxinhrecent1W2adjusted!=alldatawas1$IvalW2 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$IvalW2[alldatawas1$IvalW2>0 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$pretaxinhrecent1W2adjusted[alldatawas1$pretaxinhrecent1W2adjusted>0 & alldatawas1$yearinhrecent2==2010])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent1W2adjusted <- alldatawas1$pretaxinhrecent1W2adjusted

## And now the second recent inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$Ival2W2double <- alldatawas$Ival2W2
alldatawas$Ival2W2double[alldatawas$Ival2W2>0] <- alldatawas$Ival2W2[alldatawas$Ival2W2>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent2, alldatawas$Ival2W2double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent2", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$Ival2W2double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent2W2adjusted <- (alldatawas1$bracket+(alldatawas1$Ival2W2double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent2W2adjusted[!is.na(alldatawas1$grossinhrecent2W2adjusted)])

NROW(alldatawas1$Ival2W2[alldatawas1$Ival2W2>0])

table(alldatawas1$grossinhrecent2W2adjusted[alldatawas1$grossinhrecent2W2adjusted>100000], alldatawas1$Ival2W2[alldatawas1$grossinhrecent2W2adjusted>100000])

alldatawas1$pretaxinhrecent2W2adjusted <- alldatawas1$Ival2W2

alldatawas1$pretaxinhrecent2W2adjusted[!is.na(alldatawas1$grossinhrecent2W2adjusted)] <- alldatawas1$grossinhrecent2W2adjusted[!is.na(alldatawas1$grossinhrecent2W2adjusted)]

summary(alldatawas1$Ival2W2[alldatawas1$Ival2W2>0])

summary(alldatawas1$pretaxinhrecent2W2adjusted[alldatawas1$pretaxinhrecent2W2adjusted>0])

length(alldatawas1$pretaxinhrecent2W2adjusted[alldatawas1$pretaxinhrecent2W2adjusted==alldatawas1$Ival2W2])
#[1] 27764
length(alldatawas1$pretaxinhrecent2W2adjusted[alldatawas1$pretaxinhrecent2W2adjusted!=alldatawas1$Ival2W2])
#[1] 139

NROW(alldatawas1$Ival2W2[alldatawas1$Ival2W2>140000 & alldatawas1$yearinhrecent2==2010])

length(alldatawas1$pretaxinhrecent2W2adjusted[alldatawas1$pretaxinhrecent2W2adjusted!=alldatawas1$Ival2W2 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$Ival2W2[alldatawas1$Ival2W2>0 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$pretaxinhrecent2W2adjusted[alldatawas1$pretaxinhrecent2W2adjusted>0 & alldatawas1$yearinhrecent2==2010])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent2W2adjusted <- alldatawas1$pretaxinhrecent2W2adjusted

## And now the third recent inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$Ival3W2double <- alldatawas$Ival3W2
alldatawas$Ival3W2double[alldatawas$Ival3W2>0] <- alldatawas$Ival3W2[alldatawas$Ival3W2>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent2, alldatawas$Ival3W2double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent2", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$Ival3W2double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent3W2adjusted <- (alldatawas1$bracket+(alldatawas1$Ival3W2double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent3W2adjusted[!is.na(alldatawas1$grossinhrecent3W2adjusted)])

NROW(alldatawas1$Ival3W2[alldatawas1$Ival3W2>0])

table(alldatawas1$grossinhrecent3W2adjusted[alldatawas1$grossinhrecent3W2adjusted>100000], alldatawas1$Ival3W2[alldatawas1$grossinhrecent3W2adjusted>100000])

alldatawas1$pretaxinhrecent3W2adjusted <- alldatawas1$Ival3W2

alldatawas1$pretaxinhrecent3W2adjusted[!is.na(alldatawas1$grossinhrecent3W2adjusted)] <- alldatawas1$grossinhrecent3W2adjusted[!is.na(alldatawas1$grossinhrecent3W2adjusted)]

summary(alldatawas1$Ival3W2[alldatawas1$Ival3W2>0])

summary(alldatawas1$pretaxinhrecent3W2adjusted[alldatawas1$pretaxinhrecent3W2adjusted>0])

length(alldatawas1$pretaxinhrecent3W2adjusted[alldatawas1$pretaxinhrecent3W2adjusted==alldatawas1$Ival3W2])
#[1] 27764
length(alldatawas1$pretaxinhrecent3W2adjusted[alldatawas1$pretaxinhrecent3W2adjusted!=alldatawas1$Ival3W2])
#[1] 139

NROW(alldatawas1$Ival3W2[alldatawas1$Ival3W2>140000 & alldatawas1$yearinhrecent2==2010])

length(alldatawas1$pretaxinhrecent3W2adjusted[alldatawas1$pretaxinhrecent3W2adjusted!=alldatawas1$Ival3W2 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$Ival3W2[alldatawas1$Ival3W2>0 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$pretaxinhrecent3W2adjusted[alldatawas1$pretaxinhrecent3W2adjusted>0 & alldatawas1$yearinhrecent2==2010])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent3W2adjusted <- alldatawas1$pretaxinhrecent3W2adjusted

## And now the first recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked1inhW2double <- alldatawas$bracked1inhW2
alldatawas$bracked1inhW2double[alldatawas$bracked1inhW2>0] <- alldatawas$bracked1inhW2[alldatawas$bracked1inhW2>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent2, alldatawas$bracked1inhW2double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent2", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked1inhW2double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked1inhW2adjusted <- (alldatawas1$bracket+(alldatawas1$bracked1inhW2double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked1inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW2adjusted)])

NROW(alldatawas1$bracked1inhW2[alldatawas1$bracked1inhW2>0])

table(alldatawas1$grossinhrecentbracked1inhW2adjusted[alldatawas1$grossinhrecentbracked1inhW2adjusted>100000], alldatawas1$bracked1inhW2[alldatawas1$grossinhrecentbracked1inhW2adjusted>100000])

alldatawas1$pretaxinhrecentbracked1inhW2adjusted <- alldatawas1$bracked1inhW2

alldatawas1$pretaxinhrecentbracked1inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW2adjusted)] <- alldatawas1$grossinhrecentbracked1inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW2adjusted)]

summary(alldatawas1$bracked1inhW2[alldatawas1$bracked1inhW2>0])

summary(alldatawas1$pretaxinhrecentbracked1inhW2adjusted[alldatawas1$pretaxinhrecentbracked1inhW2adjusted>0])

length(alldatawas1$pretaxinhrecentbracked1inhW2adjusted[alldatawas1$pretaxinhrecentbracked1inhW2adjusted==alldatawas1$bracked1inhW2])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked1inhW2adjusted[alldatawas1$pretaxinhrecentbracked1inhW2adjusted!=alldatawas1$bracked1inhW2])
#[1] 139

NROW(alldatawas1$bracked1inhW2[alldatawas1$bracked1inhW2>140000 & alldatawas1$yearinhrecent2==2010])

length(alldatawas1$pretaxinhrecentbracked1inhW2adjusted[alldatawas1$pretaxinhrecentbracked1inhW2adjusted!=alldatawas1$bracked1inhW2 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$bracked1inhW2[alldatawas1$bracked1inhW2>0 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$pretaxinhrecentbracked1inhW2adjusted[alldatawas1$pretaxinhrecentbracked1inhW2adjusted>0 & alldatawas1$yearinhrecent2==2010])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked1inhW2adjusted <- alldatawas1$pretaxinhrecentbracked1inhW2adjusted


## And now the second recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked2inhW2double <- alldatawas$bracked2inhW2
alldatawas$bracked2inhW2double[alldatawas$bracked2inhW2>0] <- alldatawas$bracked2inhW2[alldatawas$bracked2inhW2>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent2, alldatawas$bracked2inhW2double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent2", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked2inhW2double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked2inhW2adjusted <- (alldatawas1$bracket+(alldatawas1$bracked2inhW2double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked2inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW2adjusted)])

NROW(alldatawas1$bracked2inhW2[alldatawas1$bracked2inhW2>0])

table(alldatawas1$grossinhrecentbracked2inhW2adjusted[alldatawas1$grossinhrecentbracked2inhW2adjusted>100000], alldatawas1$bracked2inhW2[alldatawas1$grossinhrecentbracked2inhW2adjusted>100000])

alldatawas1$pretaxinhrecentbracked2inhW2adjusted <- alldatawas1$bracked2inhW2

alldatawas1$pretaxinhrecentbracked2inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW2adjusted)] <- alldatawas1$grossinhrecentbracked2inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW2adjusted)]

summary(alldatawas1$bracked2inhW2[alldatawas1$bracked2inhW2>0])

summary(alldatawas1$pretaxinhrecentbracked2inhW2adjusted[alldatawas1$pretaxinhrecentbracked2inhW2adjusted>0])

length(alldatawas1$pretaxinhrecentbracked2inhW2adjusted[alldatawas1$pretaxinhrecentbracked2inhW2adjusted==alldatawas1$bracked2inhW2])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked2inhW2adjusted[alldatawas1$pretaxinhrecentbracked2inhW2adjusted!=alldatawas1$bracked2inhW2])
#[1] 139

NROW(alldatawas1$bracked2inhW2[alldatawas1$bracked2inhW2>140000 & alldatawas1$yearinhrecent2==2010])

length(alldatawas1$pretaxinhrecentbracked2inhW2adjusted[alldatawas1$pretaxinhrecentbracked2inhW2adjusted!=alldatawas1$bracked2inhW2 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$bracked2inhW2[alldatawas1$bracked2inhW2>0 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$pretaxinhrecentbracked2inhW2adjusted[alldatawas1$pretaxinhrecentbracked2inhW2adjusted>0 & alldatawas1$yearinhrecent2==2010])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked2inhW2adjusted <- alldatawas1$pretaxinhrecentbracked2inhW2adjusted


## And now the third recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked3inhW2double <- alldatawas$bracked3inhW2
alldatawas$bracked3inhW2double[alldatawas$bracked3inhW2>0] <- alldatawas$bracked3inhW2[alldatawas$bracked3inhW2>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent2, alldatawas$bracked3inhW2double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent2", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked3inhW2double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked3inhW2adjusted <- (alldatawas1$bracket+(alldatawas1$bracked3inhW2double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked3inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW2adjusted)])

NROW(alldatawas1$bracked3inhW2[alldatawas1$bracked3inhW2>0])

table(alldatawas1$grossinhrecentbracked3inhW2adjusted[alldatawas1$grossinhrecentbracked3inhW2adjusted>100000], alldatawas1$bracked3inhW2[alldatawas1$grossinhrecentbracked3inhW2adjusted>100000])

alldatawas1$pretaxinhrecentbracked3inhW2adjusted <- alldatawas1$bracked3inhW2

alldatawas1$pretaxinhrecentbracked3inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW2adjusted)] <- alldatawas1$grossinhrecentbracked3inhW2adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW2adjusted)]

summary(alldatawas1$bracked3inhW2[alldatawas1$bracked3inhW2>0])

summary(alldatawas1$pretaxinhrecentbracked3inhW2adjusted[alldatawas1$pretaxinhrecentbracked3inhW2adjusted>0])

length(alldatawas1$pretaxinhrecentbracked3inhW2adjusted[alldatawas1$pretaxinhrecentbracked3inhW2adjusted==alldatawas1$bracked3inhW2])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked3inhW2adjusted[alldatawas1$pretaxinhrecentbracked3inhW2adjusted!=alldatawas1$bracked3inhW2])
#[1] 139

NROW(alldatawas1$bracked3inhW2[alldatawas1$bracked3inhW2>140000 & alldatawas1$yearinhrecent2==2010])

length(alldatawas1$pretaxinhrecentbracked3inhW2adjusted[alldatawas1$pretaxinhrecentbracked3inhW2adjusted!=alldatawas1$bracked3inhW2 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$bracked3inhW2[alldatawas1$bracked3inhW2>0 & alldatawas1$yearinhrecent2==2010])

summary(alldatawas1$pretaxinhrecentbracked3inhW2adjusted[alldatawas1$pretaxinhrecentbracked3inhW2adjusted>0 & alldatawas1$yearinhrecent2==2010])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked3inhW2adjusted <- alldatawas1$pretaxinhrecentbracked3inhW2adjusted





########### AND NOW LET'S GO FOR WAVE 3 ################









#LET'S GET TO IT

#We first double the amount:

alldatawas$IVal1W3double <- alldatawas$IVal1W3
alldatawas$IVal1W3double[alldatawas$IVal1W3>0] <- alldatawas$IVal1W3[alldatawas$IVal1W3>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent3, alldatawas$IVal1W3double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent3", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IVal1W3double[alldatawas1$netmax>0])

alldatawas1$grossinhrecent1W3adjusted <- (alldatawas1$bracket+(alldatawas1$IVal1W3double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent1W3adjusted[!is.na(alldatawas1$grossinhrecent1W3adjusted)])

NROW(alldatawas1$IVal1W3[alldatawas1$IVal1W3>0])

table(alldatawas1$grossinhrecent1W3adjusted[alldatawas1$grossinhrecent1W3adjusted>100000], alldatawas1$IVal1W3[alldatawas1$grossinhrecent1W3adjusted>100000])

alldatawas1$pretaxinhrecent1W3adjusted <- alldatawas1$IVal1W3

alldatawas1$pretaxinhrecent1W3adjusted[!is.na(alldatawas1$grossinhrecent1W3adjusted)] <- alldatawas1$grossinhrecent1W3adjusted[!is.na(alldatawas1$grossinhrecent1W3adjusted)]

summary(alldatawas1$IVal1W3[alldatawas1$IVal1W3>0])

summary(alldatawas1$pretaxinhrecent1W3adjusted[alldatawas1$pretaxinhrecent1W3adjusted>0])

length(alldatawas1$pretaxinhrecent1W3adjusted[alldatawas1$pretaxinhrecent1W3adjusted==alldatawas1$IVal1W3])
#[1] 27764
length(alldatawas1$pretaxinhrecent1W3adjusted[alldatawas1$pretaxinhrecent1W3adjusted!=alldatawas1$IVal1W3])
#[1] 139

NROW(alldatawas1$IVal1W3[alldatawas1$IVal1W3>140000 & alldatawas1$yearinhrecent3==2012])

length(alldatawas1$pretaxinhrecent1W3adjusted[alldatawas1$pretaxinhrecent1W3adjusted!=alldatawas1$IVal1W3 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$IVal1W3[alldatawas1$IVal1W3>0 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$pretaxinhrecent1W3adjusted[alldatawas1$pretaxinhrecent1W3adjusted>0 & alldatawas1$yearinhrecent3==2012])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent1W3adjusted <- alldatawas1$pretaxinhrecent1W3adjusted

## And now the second recent inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$IVal2w3_idouble <- alldatawas$IVal2w3_i
alldatawas$IVal2w3_idouble[alldatawas$IVal2w3_i>0] <- alldatawas$IVal2w3_i[alldatawas$IVal2w3_i>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent3, alldatawas$IVal2w3_idouble) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent3", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IVal2w3_idouble[alldatawas1$netmax>0])

alldatawas1$grossinhrecent2W3adjusted <- (alldatawas1$bracket+(alldatawas1$IVal2w3_idouble-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent2W3adjusted[!is.na(alldatawas1$grossinhrecent2W3adjusted)])

NROW(alldatawas1$IVal2w3_i[alldatawas1$IVal2w3_i>0])

table(alldatawas1$grossinhrecent2W3adjusted[alldatawas1$grossinhrecent2W3adjusted>100000], alldatawas1$IVal2w3_i[alldatawas1$grossinhrecent2W3adjusted>100000])

alldatawas1$pretaxinhrecent2W3adjusted <- alldatawas1$IVal2w3_i

alldatawas1$pretaxinhrecent2W3adjusted[!is.na(alldatawas1$grossinhrecent2W3adjusted)] <- alldatawas1$grossinhrecent2W3adjusted[!is.na(alldatawas1$grossinhrecent2W3adjusted)]

summary(alldatawas1$IVal2w3_i[alldatawas1$IVal2w3_i>0])

summary(alldatawas1$pretaxinhrecent2W3adjusted[alldatawas1$pretaxinhrecent2W3adjusted>0])

length(alldatawas1$pretaxinhrecent2W3adjusted[alldatawas1$pretaxinhrecent2W3adjusted==alldatawas1$IVal2w3_i])
#[1] 27764
length(alldatawas1$pretaxinhrecent2W3adjusted[alldatawas1$pretaxinhrecent2W3adjusted!=alldatawas1$IVal2w3_i])
#[1] 139

NROW(alldatawas1$IVal2w3_i[alldatawas1$IVal2w3_i>140000 & alldatawas1$yearinhrecent3==2012])

length(alldatawas1$pretaxinhrecent2W3adjusted[alldatawas1$pretaxinhrecent2W3adjusted!=alldatawas1$IVal2w3_i & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$IVal2w3_i[alldatawas1$IVal2w3_i>0 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$pretaxinhrecent2W3adjusted[alldatawas1$pretaxinhrecent2W3adjusted>0 & alldatawas1$yearinhrecent3==2012])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent2W3adjusted <- alldatawas1$pretaxinhrecent2W3adjusted

## And now the third recent inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$IVal3w3_idouble <- alldatawas$IVal3w3_i
alldatawas$IVal3w3_idouble[alldatawas$IVal3w3_i>0] <- alldatawas$IVal3w3_i[alldatawas$IVal3w3_i>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent3, alldatawas$IVal3w3_idouble) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent3", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$IVal3w3_idouble[alldatawas1$netmax>0])

alldatawas1$grossinhrecent3W3adjusted <- (alldatawas1$bracket+(alldatawas1$IVal3w3_idouble-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecent3W3adjusted[!is.na(alldatawas1$grossinhrecent3W3adjusted)])

NROW(alldatawas1$IVal3w3_i[alldatawas1$IVal3w3_i>0])

table(alldatawas1$grossinhrecent3W3adjusted[alldatawas1$grossinhrecent3W3adjusted>100000], alldatawas1$IVal3w3_i[alldatawas1$grossinhrecent3W3adjusted>100000])

alldatawas1$pretaxinhrecent3W3adjusted <- alldatawas1$IVal3w3_i

alldatawas1$pretaxinhrecent3W3adjusted[!is.na(alldatawas1$grossinhrecent3W3adjusted)] <- alldatawas1$grossinhrecent3W3adjusted[!is.na(alldatawas1$grossinhrecent3W3adjusted)]

summary(alldatawas1$IVal3w3_i[alldatawas1$IVal3w3_i>0])

summary(alldatawas1$pretaxinhrecent3W3adjusted[alldatawas1$pretaxinhrecent3W3adjusted>0])

length(alldatawas1$pretaxinhrecent3W3adjusted[alldatawas1$pretaxinhrecent3W3adjusted==alldatawas1$IVal3w3_i])
#[1] 27764
length(alldatawas1$pretaxinhrecent3W3adjusted[alldatawas1$pretaxinhrecent3W3adjusted!=alldatawas1$IVal3w3_i])
#[1] 139

NROW(alldatawas1$IVal3w3_i[alldatawas1$IVal3w3_i>140000 & alldatawas1$yearinhrecent3==2012])

length(alldatawas1$pretaxinhrecent3W3adjusted[alldatawas1$pretaxinhrecent3W3adjusted!=alldatawas1$IVal3w3_i & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$IVal3w3_i[alldatawas1$IVal3w3_i>0 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$pretaxinhrecent3W3adjusted[alldatawas1$pretaxinhrecent3W3adjusted>0 & alldatawas1$yearinhrecent3==2012])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecent3W3adjusted <- alldatawas1$pretaxinhrecent3W3adjusted

## And now the first recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked1inhW3double <- alldatawas$bracked1inhW3
alldatawas$bracked1inhW3double[alldatawas$bracked1inhW3>0] <- alldatawas$bracked1inhW3[alldatawas$bracked1inhW3>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent3, alldatawas$bracked1inhW3double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent3", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked1inhW3double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked1inhW3adjusted <- (alldatawas1$bracket+(alldatawas1$bracked1inhW3double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked1inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW3adjusted)])

NROW(alldatawas1$bracked1inhW3[alldatawas1$bracked1inhW3>0])

table(alldatawas1$grossinhrecentbracked1inhW3adjusted[alldatawas1$grossinhrecentbracked1inhW3adjusted>100000], alldatawas1$bracked1inhW3[alldatawas1$grossinhrecentbracked1inhW3adjusted>100000])

alldatawas1$pretaxinhrecentbracked1inhW3adjusted <- alldatawas1$bracked1inhW3

alldatawas1$pretaxinhrecentbracked1inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW3adjusted)] <- alldatawas1$grossinhrecentbracked1inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked1inhW3adjusted)]

summary(alldatawas1$bracked1inhW3[alldatawas1$bracked1inhW3>0])

summary(alldatawas1$pretaxinhrecentbracked1inhW3adjusted[alldatawas1$pretaxinhrecentbracked1inhW3adjusted>0])

length(alldatawas1$pretaxinhrecentbracked1inhW3adjusted[alldatawas1$pretaxinhrecentbracked1inhW3adjusted==alldatawas1$bracked1inhW3])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked1inhW3adjusted[alldatawas1$pretaxinhrecentbracked1inhW3adjusted!=alldatawas1$bracked1inhW3])
#[1] 139

NROW(alldatawas1$bracked1inhW3[alldatawas1$bracked1inhW3>140000 & alldatawas1$yearinhrecent3==2012])

length(alldatawas1$pretaxinhrecentbracked1inhW3adjusted[alldatawas1$pretaxinhrecentbracked1inhW3adjusted!=alldatawas1$bracked1inhW3 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$bracked1inhW3[alldatawas1$bracked1inhW3>0 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$pretaxinhrecentbracked1inhW3adjusted[alldatawas1$pretaxinhrecentbracked1inhW3adjusted>0 & alldatawas1$yearinhrecent3==2012])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked1inhW3adjusted <- alldatawas1$pretaxinhrecentbracked1inhW3adjusted


## And now the second recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked2inhW3double <- alldatawas$bracked2inhW3
alldatawas$bracked2inhW3double[alldatawas$bracked2inhW3>0] <- alldatawas$bracked2inhW3[alldatawas$bracked2inhW3>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent3, alldatawas$bracked2inhW3double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent3", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked2inhW3double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked2inhW3adjusted <- (alldatawas1$bracket+(alldatawas1$bracked2inhW3double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked2inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW3adjusted)])

NROW(alldatawas1$bracked2inhW3[alldatawas1$bracked2inhW3>0])

table(alldatawas1$grossinhrecentbracked2inhW3adjusted[alldatawas1$grossinhrecentbracked2inhW3adjusted>100000], alldatawas1$bracked2inhW3[alldatawas1$grossinhrecentbracked2inhW3adjusted>100000])

alldatawas1$pretaxinhrecentbracked2inhW3adjusted <- alldatawas1$bracked2inhW3

alldatawas1$pretaxinhrecentbracked2inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW3adjusted)] <- alldatawas1$grossinhrecentbracked2inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked2inhW3adjusted)]

summary(alldatawas1$bracked2inhW3[alldatawas1$bracked2inhW3>0])

summary(alldatawas1$pretaxinhrecentbracked2inhW3adjusted[alldatawas1$pretaxinhrecentbracked2inhW3adjusted>0])

length(alldatawas1$pretaxinhrecentbracked2inhW3adjusted[alldatawas1$pretaxinhrecentbracked2inhW3adjusted==alldatawas1$bracked2inhW3])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked2inhW3adjusted[alldatawas1$pretaxinhrecentbracked2inhW3adjusted!=alldatawas1$bracked2inhW3])
#[1] 139

NROW(alldatawas1$bracked2inhW3[alldatawas1$bracked2inhW3>140000 & alldatawas1$yearinhrecent3==2012])

length(alldatawas1$pretaxinhrecentbracked2inhW3adjusted[alldatawas1$pretaxinhrecentbracked2inhW3adjusted!=alldatawas1$bracked2inhW3 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$bracked2inhW3[alldatawas1$bracked2inhW3>0 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$pretaxinhrecentbracked2inhW3adjusted[alldatawas1$pretaxinhrecentbracked2inhW3adjusted>0 & alldatawas1$yearinhrecent3==2012])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked2inhW3adjusted <- alldatawas1$pretaxinhrecentbracked2inhW3adjusted


## And now the third recent bracked inheritance on first wave

#LET'S GET TO IT

#We first double the amount:

alldatawas$bracked3inhW3double <- alldatawas$bracked3inhW3
alldatawas$bracked3inhW3double[alldatawas$bracked3inhW3>0] <- alldatawas$bracked3inhW3[alldatawas$bracked3inhW3>0]*2


alldatawas$netmax <- mapply(function(y, e) with(pretax, max(net[year == y & net < e])), alldatawas$yearinhrecent3, alldatawas$bracked3inhW3double) #Get the max net amount they get within each years possible brackets, to now the threshold from with to apply the marginal rate.

table(alldatawas$netmax)

#And we merge the rate and the gross amount (all from the pretax file) with the main dataset...

alldatawas1 <- merge(alldatawas, pretax, by.x = c("yearinhrecent3", "netmax"), by.y = c("year", "net"), all.x = TRUE)

table(alldatawas1$rate)

table(alldatawas1$netmax[alldatawas1$netmax>0])

table(alldatawas1$netmax[alldatawas1$netmax>0], alldatawas1$bracked3inhW3double[alldatawas1$netmax>0])

alldatawas1$grossinhrecentbracked3inhW3adjusted <- (alldatawas1$bracket+(alldatawas1$bracked3inhW3double-alldatawas1$netmax)/(1-alldatawas1$rate))/2 #We have the gross amount corresponding to the last net amount in the brackets ($bracket) and to that add the result of applying the formula on the net amount declared over the amount corresponding to the last bracket. That way we obtain the total equivalent gross amount

NROW(alldatawas1$grossinhrecentbracked3inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW3adjusted)])

NROW(alldatawas1$bracked3inhW3[alldatawas1$bracked3inhW3>0])

table(alldatawas1$grossinhrecentbracked3inhW3adjusted[alldatawas1$grossinhrecentbracked3inhW3adjusted>100000], alldatawas1$bracked3inhW3[alldatawas1$grossinhrecentbracked3inhW3adjusted>100000])

alldatawas1$pretaxinhrecentbracked3inhW3adjusted <- alldatawas1$bracked3inhW3

alldatawas1$pretaxinhrecentbracked3inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW3adjusted)] <- alldatawas1$grossinhrecentbracked3inhW3adjusted[!is.na(alldatawas1$grossinhrecentbracked3inhW3adjusted)]

summary(alldatawas1$bracked3inhW3[alldatawas1$bracked3inhW3>0])

summary(alldatawas1$pretaxinhrecentbracked3inhW3adjusted[alldatawas1$pretaxinhrecentbracked3inhW3adjusted>0])

length(alldatawas1$pretaxinhrecentbracked3inhW3adjusted[alldatawas1$pretaxinhrecentbracked3inhW3adjusted==alldatawas1$bracked3inhW3])
#[1] 27764
length(alldatawas1$pretaxinhrecentbracked3inhW3adjusted[alldatawas1$pretaxinhrecentbracked3inhW3adjusted!=alldatawas1$bracked3inhW3])
#[1] 139

NROW(alldatawas1$bracked3inhW3[alldatawas1$bracked3inhW3>140000 & alldatawas1$yearinhrecent3==2012])

length(alldatawas1$pretaxinhrecentbracked3inhW3adjusted[alldatawas1$pretaxinhrecentbracked3inhW3adjusted!=alldatawas1$bracked3inhW3 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$bracked3inhW3[alldatawas1$bracked3inhW3>0 & alldatawas1$yearinhrecent3==2012])

summary(alldatawas1$pretaxinhrecentbracked3inhW3adjusted[alldatawas1$pretaxinhrecentbracked3inhW3adjusted>0 & alldatawas1$yearinhrecent3==2012])

#We finally add the new gross variable to the original dataset

alldatawas1 <- alldatawas1[order(alldatawas1$CaseW3,alldatawas1$personW3),]
alldatawas <- alldatawas[order(alldatawas$CaseW3,alldatawas$personW3),]

alldatawas$pretaxinhrecentbracked3inhW3adjusted <- alldatawas1$pretaxinhrecentbracked3inhW3adjusted


###### AND WE SAVE THE DATA #######

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step3a-pre-tax-adjusted-new-data.rds")

