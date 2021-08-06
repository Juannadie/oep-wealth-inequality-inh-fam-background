#### FIRST WE LOAD THE USUAL FILES TO GET THE VARIABLES ####

options ("scipen"=100, "digits"=6)

library(Hmisc)
library(reldist)
library(tidyverse)
library(IC2)
library(quantreg)
library(dineq)
library(uqr)
library(matrixStats)
library(reshape2)
library(gridExtra)
library(survey)

setwd ("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code")

dataukh <- readRDS(file = "imp5/data_rds/datauk-new-final-no-cpt-new-data7c.rds")


dataukh$wealth <- dataukh$wealth * 1.585 #(we put everything in 2016 USD)

dataukh$inh <- dataukh$inh * 1.585 #We convert the inheritances to USD dollars aswell


### AGE AND WEIGHT ARE RENAMED HERE TOO ###

dataukh$weight <- dataukh$hholdweight #

dataukh$age <- dataukh$DVAgeW3Band

dataukh$sex <- dataukh$sexw3

#dataukh$sex2 <- NA
#dataukh$sex2[dataukh$sex == "Male"] <- 1
#dataukh$sex2[dataukh$sex == "Female"] <- 2

#dataukh$sex <- dataukh$sex2

#We get the average age of the whole sample after filtering this

avgagesample <- weighted.mean (dataukh$age, w=dataukh$weight)

summary (dataukh$age)
plot(density(dataukh$age[dataukh$inh>0]))
plot(density(dataukh$age[dataukh$inh<=0]))

#We filter for the ones older than 40

dataukh <- dataukh[dataukh$age>=50,]
dataukh <- dataukh[dataukh$age <= 80,]

avgagesample <- weighted.mean (dataukh$age, w=dataukh$weight)


##Let's see the percentage of negative wealth

wealthno <- NROW(dataukh$wealth)
negwealthno <- NROW(dataukh$wealth[dataukh$wealth<0])

#Share of negative wealth

negwealthno/wealthno #Much smaller amount than in the U.S.

#Weighted share of negative wealth

dataukh$wealthdummy <- 1
dataukh$negwealthdummy <- 0
dataukh$negwealthdummy [dataukh$wealth<0] <- 1
weightedwealthno <- sum(as.numeric(dataukh$wealthdummy*dataukh$weight))
weightednegwealthno <- sum(as.numeric(dataukh$negwealthdummy*dataukh$weight))
weigthednegwealthshare <- weightednegwealthno/weightedwealthno
weigthednegwealthshare

#Let's see the people that have inheritances with negative wealth

NROW(dataukh$wealth[dataukh$wealth <0 & dataukh$inh >0])
NROW(dataukh$inh[dataukh$inh>0]) #A small number

#We filter excluding those with negative wealth

dataukh <- dataukh[dataukh$wealth >0,]


#Household size
summary(dataukh$DVHsizeW3) #WE USE THIS ONE
#SQ Root Equivalent Scale...
dataukh$eqscale <- (dataukh$DVHsizeW3)^0.5
summary (dataukh$eqscale)

#We can get the number of adults
dataukh$numadults <- dataukh$NumAdultW3.x
dataukh$adeqscale <- 1+(dataukh$numadults-1)^.5
#dataukh$adeqscale <- (dataukh$numadults)^.5
summary (dataukh$adeqscale)

dataukh$eqinh <- dataukh$inh/dataukh$adeqscale #Eqscale is plan no. of adults
dataukh$eqwealth <- dataukh$wealth/dataukh$adeqscale


#We then convert equivalent inheritances smaller than 5000 USD to 0 (non-recipients)
dataukh$eqinh[dataukh$eqinh<5000] <-0

#LET US DEFINE THE CIRCUMSTANCES AND THE INHERITANCES AND WEALTH VARIABLES

######## PARENTAL EDUCATION ############# WE GET FOUR TYPES FOR THAT #####

#LET US ALSO SEE HOW MANY OF THE RESPONDENTS HAVE INFORMATION ABOUT PARENTAL BACKGROUND

table (dataukh$FBMoEdW2) #The information comes on Wave 2
table (dataukh$FBMoEdW3) #Not much about mothers education on Wave 3, because only asks the ones who did not answer in the previous wave.

table (dataukh$FBFaEdW2) #The information comes on Wave 2
table (dataukh$FBFaEdW3) #Not much about mothers education on Wave 3, because only asks the ones who did not answer in the previous wave.

#We make variables grouping education in numbers...

dataukh$edudadW2 <- NA
dataukh$edudadW2 [dataukh$FBFaEdW2 == "Don t know"] <- 1
dataukh$edudadW2 [dataukh$FBFaEdW2 == "Did not go to school"] <- 1
dataukh$edudadW2 [dataukh$FBFaEdW2 == "Left school before 15"] <- 2
dataukh$edudadW2 [dataukh$FBFaEdW2 == "Left school 15 or 16"] <- 2
dataukh$edudadW2 [dataukh$FBFaEdW2 == "Left school 17 or 18"] <- 3
dataukh$edudadW2 [dataukh$FBFaEdW2 == "Further quals or certs after leaving"] <- 3
dataukh$edudadW2 [dataukh$FBFaEdW2 == "University or higher degree"] <- 4

#We obtain the ranking variable as the ranking (normalized to 1) within each type
dataukh <- dataukh %>%
  group_by(FBFaEdW2) %>%
  mutate(my_rank_edu = (rank(wealth, ties.method = 'random')/length(wealth)))

sc4typesuk <- ggplot()+
  geom_point(data=dataukh, aes(x=my_rank_edu, y=wealth, col=FBFaEdW2, fill=FBFaEdW2), pch=21, size=1)+
  #geom_point(data=dataplot, aes(x=rank, y=smoothnetwealth, col="Fit"), pch=16, size=1)+
  scale_y_continuous(limits = c(0, 4000000))+
  xlab("Rank within Type")+
  ylab("Equivalent wealth (millions of GBP)
       Obs. < 4 million GBP")+
  ggtitle("Wealth and parental education
          United Kingdom")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title=element_blank()
  )



#We make variables grouping education in numbers...

dataukh$edudadW3 <- NA
dataukh$edudadW3 [dataukh$FBFaEdW3 == "Did not go to school at all"] <- 1
dataukh$edudadW3 [dataukh$FBFaEdW3 == "Left school before the age of 15"] <- 2
dataukh$edudadW3 [dataukh$FBFaEdW3 == "Left school at 15 or 16"] <- 2
dataukh$edudadW3 [dataukh$FBFaEdW3 == "Left school at 17 or 18"] <- 3
dataukh$edudadW3 [dataukh$FBFaEdW3 == "Gained further qualifications after leaving"] <- 3
dataukh$edudadW3 [dataukh$FBFaEdW3 == "Gained a university degree or higher degree"] <- 4

#We obtain the ranking variable as the ranking (normalized to 1) within each type
dataukh <- dataukh %>%
  group_by(FBFaEdW3) %>%
  mutate(my_rank_edu = (rank(wealth, ties.method = 'random')/length(wealth)))

sc4typesuk2 <- ggplot()+
  geom_point(data=dataukh, aes(x=my_rank_edu, y=wealth, col=FBFaEdW3, fill=FBFaEdW3), pch=21, size=1)+
  #geom_point(data=dataplot, aes(x=rank, y=smoothnetwealth, col="Fit"), pch=16, size=1)+
  scale_y_continuous(limits = c(0, 4000000))+
  xlab("Rank within Type")+
  ylab("Equivalent wealth (millions of GBP)
       Obs. < 4 million GBP")+
  ggtitle("Wealth and parental education
          United Kingdom")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title=element_blank()
  )



#We make variables grouping education in numbers...

dataukh$edumomW2 <- NA
dataukh$edumomW2 [dataukh$FBMoEdW2 == "Don t know"] <- 1
dataukh$edumomW2 [dataukh$FBMoEdW2 == "Did not go to school"] <- 1
dataukh$edumomW2 [dataukh$FBMoEdW2 == "Left school before 15"] <- 2
dataukh$edumomW2 [dataukh$FBMoEdW2 == "Left school 15 or 16"] <- 2
dataukh$edumomW2 [dataukh$FBMoEdW2 == "Left school 17 or 18"] <- 3
dataukh$edumomW2 [dataukh$FBMoEdW2 == "Further quals or certs after leaving"] <- 3
dataukh$edumomW2 [dataukh$FBMoEdW2 == "University or higher degree"] <- 4

#We make variables grouping education in numbers...

dataukh$edumomW3 <- NA
dataukh$edumomW3 [dataukh$FBMoEdW3 == "Did not go to school at all"] <- 1
dataukh$edumomW3 [dataukh$FBMoEdW3 == "Left school before the age of 15"] <- 2
dataukh$edumomW3 [dataukh$FBMoEdW3 == "Left school at 15 or 16"] <- 2
dataukh$edumomW3 [dataukh$FBMoEdW3 == "Left school at 17 or 18"] <- 3
dataukh$edumomW3 [dataukh$FBMoEdW3 == "Gained further qualifications after leaving"] <- 3
dataukh$edumomW3 [dataukh$FBMoEdW3 == "Gained a university degree or higher degree"] <- 4

#Now we create a variable with the maximum education for the person, taking into account father and mother, and also both waves:

dataukh$maxeduparent <- pmax(dataukh$edudadW2, dataukh$edudadW3, dataukh$edumomW2, dataukh$edumomW3, na.rm = T)
dataukh$edudad <-  pmax(dataukh$edudadW2, dataukh$edudadW3, na.rm=T)
dataukh$edumom <-  pmax(dataukh$edumomW2, dataukh$edumomW3, na.rm=T)

summary (dataukh$maxeduparent)
table(dataukh$maxeduparent)
summary (dataukh$edudad)
summary (dataukh$edumom)

####

dataukh %>%
  group_by(FBMoEdW2) %>%
  summarise(mean = median(wealth))


#We filter for the ones that have the information about parental education

dataukh <- dataukh[!is.na(dataukh$maxeduparent),]


#CREATE INHERITANCE CATEGORIES, TAKING INTO ACCOUNT EXPECTATION OF INHERITANCES

#We then convert equivalent inheritances smaller than 5000 USD to 0 (non-recipients)

eqinhno <- NROW(dataukh$eqinh[dataukh$eqinh>0])
smalleqinhno <- NROW(dataukh$eqinh[dataukh$eqinh>0 & dataukh$eqinh<10000])# which is 10000 USD in POUNDS of 2012, 10000/1.463 = 3418... That would be inheritances under 10000 USD
sharesmalleqinh <- smalleqinhno/eqinhno

sharesmalleqinh #25% aprox


#WEALTH, WE GET 4 QUARTILES FOR THAT, PLUS NON RECIPIENTS NOT EXPECTING AND RECIPIENTS EXPECTING ONE IN THE FUTURE

summary(dataukh$IHFutW1)
summary(dataukh$IHFutW1[dataukh$inh<=0])

dataukh$expectinh <- 0
dataukh$expectinh [dataukh$IHFutW1 == "Definitely will" & dataukh$inh<=0] <- 1
dataukh$expectinh [dataukh$IHFutW1 == "Very likely" & dataukh$inh<=0] <- 1
dataukh$expectinh [dataukh$IHFutW1 == "Fairly likely" & dataukh$inh<=0] <- 1

summary (dataukh$expectinh)
table (dataukh$expectinh)

#WE EXCLUDE THE EXPECTED INHERITANCES UNDER 10000 USD

table(dataukh$IFutVIBW1[dataukh$expectinh ==1])

#dataukh$expectinh [dataukh$IFutVIBW1 == "Refused"] <- 0
dataukh$expectinh [dataukh$IFutVIBW1 == "Less than \xa31,000"] <- 0
dataukh$expectinh [dataukh$IFutVIBW1 == "\xa31,000 to \xa34,999"] <- 0
dataukh$expectinh [dataukh$IFutVIBW1 == "\2435,000 to \2439,999"] <- 0 #We exclude smaller than 10000 only
#dataukh$expectinh [dataukh$IFutVIBW1 == "Don t know (Spontaneous only)"] <- 0

table(dataukh$IFutVIBW1[dataukh$expectinh ==1])




### NOW WE MAKE THE DISTINCTION FOR THE EQUIVALISED INHERITANCES ###

### MAKING THE ONES UNDER 10000 USD EQUAL TO ZERO ### ### AND WEIGHTING THE QUANTILES ###


#We can also check the inheritance quantiles
quantile (dataukh$eqinh [dataukh$eqinh > 0], probs = seq(0, 1, 0.25))

q0 <- reldist::wtd.quantile (dataukh$eqinh [dataukh$eqinh > 0], q = 0, weight=dataukh$weight[dataukh$eqinh > 0], na.rm = T)
q1 <- reldist::wtd.quantile (dataukh$eqinh [dataukh$eqinh > 0], q = 0.25, weight=dataukh$weight[dataukh$eqinh > 0], na.rm = T)
q2 <- reldist::wtd.quantile (dataukh$eqinh [dataukh$eqinh > 0], q = 0.5, weight=dataukh$weight[dataukh$eqinh > 0], na.rm = T)
q3 <- reldist::wtd.quantile (dataukh$eqinh [dataukh$eqinh > 0], q = 0.75, weight=dataukh$weight[dataukh$eqinh > 0], na.rm = T)

#And create the categorical variable accordingly
dataukh$eqinhcat <- NA

dataukh$eqinhcat [dataukh$eqinh < q0 & dataukh$expectinh ==0] <- 1
dataukh$eqinhcat [dataukh$eqinh < q0 & dataukh$expectinh ==1] <- 2
dataukh$eqinhcat [dataukh$eqinh >= q0 & dataukh$eqinh < q1] <- 3
dataukh$eqinhcat [dataukh$eqinh >= q1 & dataukh$eqinh < q2] <- 4
dataukh$eqinhcat [dataukh$eqinh >= q2 & dataukh$eqinh < q3] <- 5
dataukh$eqinhcat [dataukh$eqinh >= q3] <- 6

table (dataukh$eqinhcat)

####

saveRDS(dataukh, file = "data_rds/datauk-new-final-no-cpt-new-data8a-bis1.rds")





