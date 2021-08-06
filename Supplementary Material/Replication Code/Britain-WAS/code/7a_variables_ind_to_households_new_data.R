#final variables creation

library(foreign)
library(tidyverse)
library(stats)
library(Hmisc)
library(reldist)
library(tidyverse)
library(IC2)
library(quantreg)
library(dineq)
options ("scipen"=100, "digits"=4)



datauk <- readRDS(file = "data_rds/WAS-After-v3-2-Step6a-Households-new-data.rds")


library(Hmisc)
library(reldist)
library(tidyverse)
library(IC2)
library(quantreg)
library(dineq)

#Some descriptives

summary(datauk$totalinhbaseline[datauk$totalinhbaseline>0]) #TAXES NET
summary(datauk$totalinhbasicinput[datauk$totalinhbasicinput>0]) #NET, IMPUTATION
summary(datauk$totalinhbaselinegross[datauk$totalinhbaselinegross>0])#GROSS
summary(datauk$totalinhgrossinput[datauk$totalinhgrossinput>0])#GROSS, IMPUTATION

summary(datauk$totalinhbaselinegrossadjusted[datauk$totalinhbaselinegrossadjusted>0])#GROSS
summary(datauk$totalinhgrossadjustedinput[datauk$totalinhgrossadjustedinput>0])#GROSS, IMPUTATION

NROW(datauk$totalinhbaseline[datauk$totalinhbaseline>0]) #TAXES NET
NROW(datauk$totalinhbasicinput[datauk$totalinhbasicinput>0]) #NET, IMPUTATION
NROW(datauk$totalinhbaselinegross[datauk$totalinhbaselinegross>0])#GROSS
NROW(datauk$totalinhgrossinput[datauk$totalinhgrossinput>0])#GROSS, IMPUTATION

NROW(datauk$totalinhbaselinegrossadjusted[datauk$totalinhbaselinegrossadjusted>0])#GROSS ADJUSTED
NROW(datauk$totalinhgrossadjustedinput[datauk$totalinhgrossadjustedinput>0])#GROSS ADJUSTED, IMPUTATION

summary(datauk$houseinhbaseline[datauk$houseinhbaseline>0]) #TAXES NET
summary(datauk$houseinhbasicinput[datauk$houseinhbasicinput>0]) #NET, IMPUTATION
summary(datauk$houseinhbaselinegross[datauk$houseinhbaselinegross>0])#GROSS
summary(datauk$houseinhgrossinput[datauk$houseinhgrossinput>0])#GROSS, IMPUTATION
summary(datauk$houseinhbaselinegrossadjusted[datauk$houseinhbaselinegrossadjusted>0])#GROSS ADJUSTED
summary(datauk$houseinhgrossadjustedinput[datauk$houseinhgrossadjustedinput>0])#GROSS ADJUSTED, IMPUTATION


NROW(datauk$houseinhbaseline[datauk$houseinhbaseline>0]) #TAXES NET
NROW(datauk$houseinhbasicinput[datauk$houseinhbasicinput>0]) #NET, IMPUTATION
NROW(datauk$houseinhbaselinegross[datauk$houseinhbaselinegross>0])#GROSS
NROW(datauk$houseinhgrossinput[datauk$houseinhgrossinput>0])#GROSS, IMPUTATION
NROW(datauk$houseinhbaselinegrossadjusted[datauk$houseinhbaselinegrossadjusted>0])#GROSS
NROW(datauk$houseinhgrossadjustedinput[datauk$houseinhgrossadjustedinput>0])#GROSS, IMPUTATION


##### WE CAN GET HERE THE NET AND GROSS VALUE OF THE OUTLIER, WHEN USING ONLY HALF OUTLIER #####


inhoutlieroriginal <- (datauk$IEValW1 [datauk$CaseW3 == 16678])

inhoutlierbaselineupd  <- (datauk$houseinhbaseline [datauk$CaseW3 == 16678])

inhoutliergrossupd  <- (datauk$houseinhbaselinegross [datauk$CaseW3 == 16678])

inhoutliergrossadjustedupd  <- (datauk$houseinhbaselinegrossadjusted [datauk$CaseW3 == 16678])

tableoutlierhalf <- as.data.frame(c(inhoutlieroriginal, inhoutlierbaselineupd, inhoutliergrossupd, inhoutliergrossadjustedupd))




##### Let's see what does the business have in business debt #######

summary(datauk$Bdebts1W3)

datauk$busdebt3w1 <- 0
datauk$busdebt3w1[datauk$Bdebts1W3>0] <- datauk$Bdebts1W3[datauk$Bdebts1W3>0]
summary(datauk$busdebt3w1)

datauk$busdebt3w2 <- 0
datauk$busdebt3w2[datauk$Bdebts2W3>0] <- datauk$Bdebts2W3[datauk$Bdebts2W3>0]
summary(datauk$busdebt3w2)

datauk$busdebt3w3 <- 0
datauk$busdebt3w3[datauk$Bdebts3W3>0] <- datauk$Bdebts3W3[datauk$Bdebts3W3>0]
summary(datauk$busdebt3w3)

datauk$busdebt <-datauk$busdebt3w1 + datauk$busdebt3w2 + datauk$busdebt3w3

#WE ARE GOING TO ADD THE BUSINESS WEALTH FOR ALL INDIVIDUALS IN THE HOUSEHOLD AND THEN AGGREGATE THEM BY HOUSEHOLD ###

datauk$buswealth3w1 <- 0
datauk$buswealth3w1[datauk$Bval1W3>0] <- datauk$Bval1W3[datauk$Bval1W3>0]
datauk$buswealth3w2 <- 0
datauk$buswealth3w2[datauk$Bval2W3>0] <- datauk$Bval2W3[datauk$Bval2W3>0]
datauk$buswealth3w3 <- 0
datauk$buswealth3w3[datauk$Bval3W3>0] <- datauk$Bval3W3[datauk$Bval3W3>0]

#Now the brackets
datauk$buswealth3wb1 <- 0
datauk$buswealth3wb1 [datauk$BvalB1W3 == 'Less than £100'] <- 50
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£100 to £9,999'] <- 5050
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£10,000 to £49,999'] <- 30000
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£50,000 to £99,999'] <- 75000
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£100,000 to £249,999'] <- 175000
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£250,000 to £499,999'] <- 375000
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£500,000 to £999,999'] <- 750000
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£1,000,000 to £1,999,999'] <- 1500000
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£2,000,000 to £4,999,999'] <- 3500000
av <- mean(datauk$buswealth3w1[datauk$buswealth3w1>5000000])
datauk$buswealth3wb1 [datauk$BvalB1W3 == '£5 million or more'] <- av #We apply the average of the actual data over 5 million

#Now the brackets
datauk$buswealth3wb2 <- 0
datauk$buswealth3wb2 [datauk$BvalB2W3 == 'Less than £100'] <- 50
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£100 to £9,999'] <- 5050
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£10,000 to £49,999'] <- 30000
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£50,000 to £99,999'] <- 75000
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£100,000 to £249,999'] <- 175000
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£250,000 to £499,999'] <- 375000
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£500,000 to £999,999'] <- 750000
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£1,000,000 to £1,999,999'] <- 1500000
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£2,000,000 to £4,999,999'] <- 3500000
av2 <- mean(datauk$buswealth3w2[datauk$buswealth3w2>5000000])
datauk$buswealth3wb2 [datauk$BvalB2W3 == '£5 million or more'] <- 5000000 #We apply the average of the actual data over 5 million or 5 million is there is not such average

#Now the brackets
datauk$buswealth3wb3 <- 0
datauk$buswealth3wb3 [datauk$BvalB3W3 == 'Less than £100'] <- 50
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£100 to £9,999'] <- 5050
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£10,000 to £49,999'] <- 30000
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£50,000 to £99,999'] <- 75000
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£100,000 to £249,999'] <- 175000
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£250,000 to £499,999'] <- 375000
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£500,000 to £999,999'] <- 750000
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£1,000,000 to £1,999,999'] <- 1500000
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£2,000,000 to £4,999,999'] <- 3500000
av2 <- mean(datauk$buswealth3w2[datauk$buswealth3w2>5000000])
datauk$buswealth3wb3 [datauk$BvalB3W3 == '£5 million or more'] <- 5000000 #We apply the average of the actual data over 5 million or 5 million is there is not such average

#We plug the bracket value where the actual precise value is missing
datauk$buswealth3w1[datauk$buswealth3w1 == 0 & datauk$buswealth3wb1 != 0] <- datauk$buswealth3wb1 [datauk$buswealth3w1 == 0 & datauk$buswealth3wb1 != 0]
datauk$buswealth3w2[datauk$buswealth3w2 == 0 & datauk$buswealth3wb2 != 0] <- datauk$buswealth3wb2 [datauk$buswealth3w2 == 0 & datauk$buswealth3wb2 != 0]
datauk$buswealth3w3[datauk$buswealth3w3 == 0 & datauk$buswealth3wb3 != 0] <- datauk$buswealth3wb3 [datauk$buswealth3w3 == 0 & datauk$buswealth3wb3 != 0]

#### Remember we are talking about net business wealth here

#1st business

datauk$buswealth1 <- datauk$buswealth3w1 #note this means wave 3, first business
NROW(datauk$buswealth1[datauk$buswealth1>0])
summary(datauk$buswealth1[datauk$buswealth1>0])

#2nd business

datauk$buswealth2 <- datauk$buswealth3w2 #note this means wave 3, second business
NROW(datauk$buswealth2[datauk$buswealth2>0])
summary(datauk$buswealth2[datauk$buswealth2>0])

#3rdbusiness

datauk$buswealth3 <- datauk$buswealth3w3 #note this means wave 3, third business
NROW(datauk$buswealth3[datauk$buswealth3>0])
summary(datauk$buswealth3[datauk$buswealth3>0])

#Now we aggregate the value of the three own business

datauk$buswealthnet <- datauk$buswealth1 + datauk$buswealth2 + datauk$buswealth3
NROW(datauk$buswealthnet[datauk$buswealthnet>0])
summary(datauk$buswealthnet[datauk$buswealthnet>0])


####### And we can get gross business wealth by adding the debt to each business ############

datauk$busgrosswealth1 <- datauk$buswealth1 + datauk$busdebt3w1
datauk$busgrosswealth2 <- datauk$buswealth2 + datauk$busdebt3w2
datauk$busgrosswealth3 <- datauk$buswealth3 + datauk$busdebt3w3

datauk$busgrosswealth <- datauk$busgrosswealth1 + datauk$busgrosswealth2 + datauk$busgrosswealth3


#Let's see if the individual aggregates hold for gross, debt and net business wealth

summary(datauk$busgrosswealth)
summary(datauk$busdebt)
summary(datauk$buswealthnet)

datauk$busnetwealth <- datauk$busgrosswealth - datauk$busdebt

summary(datauk$busnetwealth)

### And we aggregate that by households
#Business debt

datauk <- datauk %>%
  group_by(CaseW3) %>%
  mutate(fambusdebt = sum(busdebt))

NROW(datauk$fambusdebt[datauk$fambusdebt>0])
summary(datauk$fambusdebt[datauk$fambusdebt>0])

#Gross wealth

datauk <- datauk %>%
  group_by(CaseW3) %>%
  mutate(fambusgrosswealth = sum(busgrosswealth))

NROW(datauk$fambusgrosswealth[datauk$fambusgrosswealth>0])
summary(datauk$fambusgrosswealth[datauk$fambusgrosswealth>0])

### And we aggregate that by households

datauk <- datauk %>%
  group_by(CaseW3) %>%
  mutate(fambuswealthnet = sum(buswealthnet))

NROW(datauk$fambuswealthnet[datauk$fambuswealthnet>0])
summary(datauk$fambuswealthnet[datauk$fambuswealthnet>0])

#### And we check it is correct

datauk$fambusnetwealth <- datauk$fambusgrosswealth - datauk$fambusdebt
summary(datauk$fambusnetwealth)
summary(datauk$fambuswealthnet)


#THERE IS MORE INDIVIDUALS WITH HOUSE INHERITANCE BECAUSE WE ASSIGN THE INHERITANCE TO ALL HOUSE MEMBERS, NOT ONLY TO THE ONES RECEIVING THEM

#WEIGHTS

#hholds weights version 1 #THIS IS THE HOUSEHOLD WEIGHT WE SHOULD USE, AS INDICATED BY THE ONS
datauk <- datauk %>%
  group_by(CaseW3) %>%
  mutate(hholdweight = mean(w1w3wgt))


#Summary only for households
NROW (datauk$houseinhbaseline[datauk$ISHRPW3 == "Is the HRP"])


#We can get max income, later for filtering the head

datauk$totalindincome <- datauk$DVGIEMPw3  + datauk$DVGISEw3  + datauk$DVTotAllBenAnnualw3  + datauk$DVGIPpenw3  + datauk$DvGIINVw3  + datauk$DVGiftAnnualw3

summary(datauk$totalindincome)


#datauk <- datauk[datauk$CaseW3 != 16678,]


#LET'S DO THE ANALYSIS FOR THE NET VALUE BASIC INPUT###

#WE CAN ALSO RESTRICT THE SAMPLE TO HOUSEHOLDS, FILTERING FOR THE HOUSEHOLD HEAD ###

dataukheads <- subset(datauk, datauk$ISHRPW3 == "Is the HRP") #HRP is the max income, and then the max age

dataukleft <- subset(datauk, !datauk$CaseW3 %in% dataukheads$CaseW3)

headdataukleft<- dataukleft %>% group_by(CaseW3) %>% filter(totalindincome==max(totalindincome)) %>% group_by(CaseW3)  %>% filter(DVAgeW3Band==max(DVAgeW3Band)) %>% group_by(CaseW3) %>% filter(personW3==min(personW3)) #In the houses without HRP we choose the older one and in second order the person with the highest number


#And then we bind it

dataukh <- rbind(dataukheads, headdataukleft)


#### net values no imputation

dataukh$inhnet <- dataukh$houseinhbaseline
dataukh$inhonlynet <- dataukh$houseinhonly
dataukh$giftonlynet <- dataukh$housegiftonly

#net values with imputation

dataukh$inhnetimp <- dataukh$houseinhbasicinput
dataukh$inhonlynetimp <- dataukh$houseinhonlybasicinput
dataukh$giftonlynetimp <- dataukh$housegiftonlybasicinput


summary(dataukh$inhnet)
summary(dataukh$inhnetimp)

summary(dataukh$inhnet[dataukh$inhnet>0])
summary(dataukh$inhnetimp[dataukh$inhnetimp>0])

NROW(dataukh$inhnet[dataukh$inhnet>0])
NROW(dataukh$inhnetimp[dataukh$inhnetimp>0])


#gross adjusted values no imputation

dataukh$inhgross <- dataukh$houseinhbaselinegrossadjusted
dataukh$inhonlygross <- dataukh$houseinhonlygrossadjusted
dataukh$giftonlygross <- dataukh$housegiftonly

#gross adjusted values with imputation

dataukh$inhgrossimp <- dataukh$houseinhgrossadjustedinput #Already aggregated by households the inheritances (Step 4)
dataukh$inhonlygrossimp<- dataukh$houseinhonlygrossadjustedinput
dataukh$giftonlygrossimp <- dataukh$housegiftonlygrossadjustedinput

summary(dataukh$inhgross)
summary(dataukh$inhgrossimp)

summary(dataukh$inhgross[dataukh$inhgross>0])
summary(dataukh$inhgrossimp[dataukh$inhgrossimp>0])

NROW (dataukh$inhgross[dataukh$inhgross>0])
NROW (dataukh$inhgrossimp[dataukh$inhgrossimp>0])


# summary of variables

summary(dataukh$inhnet)
summary(dataukh$inhnetimp)
summary(dataukh$inhgross)
summary(dataukh$inhgrossimp)

# count of variables

NROW(dataukh$inhnet[dataukh$inhnet>0])
NROW(dataukh$inhnetimp[dataukh$inhnetimp>0])
NROW(dataukh$inhgross[dataukh$inhgross>0])
NROW(dataukh$inhgrossimp[dataukh$inhgrossimp>0])

dataukh$inh <- dataukh$inhgrossimp #we keep gross imputed version
######

saveRDS(dataukh, file ="data_rds/dataukh-v3-2-from-step7a-new-data.rds")
