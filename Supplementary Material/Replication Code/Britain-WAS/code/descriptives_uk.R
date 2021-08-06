#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

library(Hmisc)
library(reldist)
library(tidyverse)
library(IC2)


options ("scipen"=100, "digits"=10)

#some functions
### We set up the functions for the Top and Bottom shares

top10share <- function (x,w) {
  q9 <- (wtd.quantile(x, q=0.9, weight=w, na.rm = T ))
  sum10share <- sum(x[x>q9]*w[x>q9])
  totalsum <- sum(x*w)
  share <- sum10share/totalsum
  return(share)
}

top20share <- function (x,w) {
  q8 <- (wtd.quantile(x, q=0.8, weight=w, na.rm = T ))
  sum20share <- sum(x[x>q8]*w[x>q8])
  totalsum <- sum(x*w)
  share <- sum20share/totalsum
  return(share)
}

bottom50share <- function (x,w) {
  q5 <- (wtd.quantile(x, q=0.5, weight=w, na.rm = T ))
  sum50share <- sum(x[x<q5]*w[x<q5])
  totalsum <- sum(x*w)
  share <- sum50share/totalsum
  return(share)
}

#let's load baseline for each was imputation


dataukh1imp <- readRDS(file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/imp1/data_rds/datauk-new-final-no-cpt-new-data7c.rds")
dataukh2imp <- readRDS(file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/imp2/data_rds/datauk-new-final-no-cpt-new-data7c.rds")
dataukh3imp <- readRDS(file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/imp3/data_rds/datauk-new-final-no-cpt-new-data7c.rds")
dataukh4imp <- readRDS(file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/imp4/data_rds/datauk-new-final-no-cpt-new-data7c.rds")
dataukh5imp <- readRDS(file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/imp5/data_rds/datauk-new-final-no-cpt-new-data7c.rds")


imps <- list (dataukh1imp, dataukh2imp, dataukh3imp, dataukh4imp, dataukh5imp)


setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/data_rds") #Set Working Directory # LAPTOP DIRECTORY

temp5 = list.files(pattern="WAS-After-Step-v2-IO-8b-imp*") #creates a list with all the personal files in the eu-silc directory directory
imps5 <- lapply(temp5, readRDS) #reads the list (note, alphabetical order)

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code") #Set Working Directory # LAPTOP DIRECTORY


#START LOOP

for (i in 1:length(imps)) {
  dataukh <-  imps[[i]]


  dataukh$wealth <- dataukh$wealth * 1.585 #(we put everything in 2016 USD
  dataukh$inh <- dataukh$inh * 1.585 #We convert the inheritances to USD dollars aswell


  ### AGE AND WEIGHT ARE RENAMED HERE TOO ###
  dataukh$weight <- dataukh$hholdweight #
  dataukh$age <- dataukh$DVAgeW3Band
  dataukh$sex <- dataukh$sexw3

  #We can get the number of adults
  dataukh$numadults <- dataukh$NumAdultW3.x
  dataukh$adeqscale <- 1+(dataukh$numadults-1)^.5
  #dataukh$adeqscale <- (dataukh$numadults)^.5
  summary (dataukh$adeqscale)

  dataukh$eqinh <- dataukh$inh/dataukh$adeqscale #Eqscale is plan no. of adults
  dataukh$eqwealth <- dataukh$wealth/dataukh$adeqscale


  dataukh$eqinh[dataukh$eqinh<5000] <- 0
  dataukh$inh[dataukh$eqinh<5000] <- 0 #also eliminate inheritances

  #Share of recipients
  dataukh$inhdummy <- 0
  dataukh$inhdummy[dataukh$inh > 0] <- 1
  #dataukh$inhdummy[dataukh$inh = 0] <- 0


  #We filter for the ones that have the information about parental education
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


  #We make variables grouping education in numbers...

  dataukh$edudadW3 <- NA
  dataukh$edudadW3 [dataukh$FBFaEdW3 == "Did not go to school at all"] <- 1
  dataukh$edudadW3 [dataukh$FBFaEdW3 == "Left school before the age of 15"] <- 2
  dataukh$edudadW3 [dataukh$FBFaEdW3 == "Left school at 15 or 16"] <- 2
  dataukh$edudadW3 [dataukh$FBFaEdW3 == "Left school at 17 or 18"] <- 3
  dataukh$edudadW3 [dataukh$FBFaEdW3 == "Gained further qualifications after leaving"] <- 3
  dataukh$edudadW3 [dataukh$FBFaEdW3 == "Gained a university degree or higher degree"] <- 4


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



  #We filter for the ones that have the information about parental education

  dataukh <- dataukh[!is.na(dataukh$maxeduparent),]



  #original data excluding missing
  #We filter with the one with information on parental occupation
  #dataukh <- dataukh[!is.na(dataukh$occdadhead),]
  dataukh1 <- dataukh

  #data sampled 35-80
  dataukh2 <- dataukh1[dataukh1$age >= 35,]
  dataukh2 <- dataukh2[dataukh2$age <= 80,]

  #and excluding negative wealth
  dataukh3 <- dataukh2[dataukh2$wealth>0,]

  #and using equivalent wealth and equivalent inheritance

  dataukh4 <- dataukh3

  dataukh4$wealth <- dataukh4$wealth/dataukh4$adeqscale
  dataukh4$inh <- dataukh4$inh/dataukh4$adeqscale

  #and our final sample

  dataukh5 <-  imps5[[i]]
  #wealth is already equivalised here
  dataukh5$inh <- dataukh5$eqinh


  #Share of recipients
  dataukh5$inhdummy <- 0
  dataukh5$inhdummy[dataukh5$inh > 0] <- 1
  #dataukh5$inhdummy[dataukh5$inh == 0] <- 0

  dataukh5$wealth <- dataukh5$wealthpredictexp #We use the value of wealth without using taking into account age


  #let's calculate the descriptives

  #obs

  obs1 <- as.numeric(length (dataukh1$wealth))
  obs2 <- as.numeric(length (dataukh2$wealth))
  obs3 <- as.numeric(length (dataukh3$wealth))
  obs4 <- as.numeric(length (dataukh4$wealth))
  obs5 <- as.numeric(length (dataukh5$wealth))

  obsall <- c(obs1, obs2, obs3, obs4, obs5)


  #share receiving

  sharerecipients1 <- vector()
  sharerecipients2 <- vector()
  sharerecipients3 <- vector()
  sharerecipients4 <- vector()
  sharerecipients5 <- vector()


  sharerecipients1 <- sum(as.numeric((dataukh1$inhdummy[dataukh1$inhdummy == 1])*(dataukh1$weight[dataukh1$inhdummy == 1])))/sum(as.numeric(dataukh1$weight), na.rm = T)
  sharerecipients2 <- sum(as.numeric((dataukh2$inhdummy[dataukh2$inhdummy == 1])*(dataukh2$weight[dataukh2$inhdummy == 1])))/sum(as.numeric(dataukh2$weight), na.rm = T)
  sharerecipients3 <- sum(as.numeric((dataukh3$inhdummy[dataukh3$inhdummy == 1])*(dataukh3$weight[dataukh3$inhdummy == 1])))/sum(as.numeric(dataukh3$weight), na.rm = T)
  sharerecipients4 <- sum(as.numeric((dataukh4$inhdummy[dataukh4$inhdummy == 1])*(dataukh4$weight[dataukh4$inhdummy == 1])))/sum(as.numeric(dataukh4$weight), na.rm = T)
  sharerecipients5 <- sum(as.numeric((dataukh5$inhdummy[dataukh5$inhdummy == 1])*(dataukh5$weight[dataukh5$inhdummy == 1])))/sum(as.numeric(dataukh5$weight), na.rm = T)

  shrep1 <- mean(sharerecipients1)
  shrep2 <- mean(sharerecipients2)
  shrep3 <- mean(sharerecipients3)
  shrep4 <- mean(sharerecipients4)
  shrep5 <- mean(sharerecipients5)

  shrepall <- c(shrep1, shrep2, shrep3, shrep4, shrep5)


  #Age of the sample

  agemean1 <- vector()
  agemean2 <- vector()
  agemean3 <- vector()
  agemean4 <- vector()
  agemean5 <- vector()

  agemean1 <- weighted.mean (dataukh1$age, w=dataukh1$weight)
  agemean2 <- weighted.mean (dataukh2$age, w=dataukh2$weight)
  agemean3 <- weighted.mean (dataukh3$age, w=dataukh3$weight)
  agemean4 <- weighted.mean (dataukh4$age, w=dataukh4$weight)
  agemean5 <- weighted.mean (dataukh5$age, w=dataukh5$weight)

  agem1 <- mean(agemean1)
  agem2 <- mean(agemean2)
  agem3 <- mean(agemean3)
  agem4 <- mean(agemean4)
  agem5 <- mean(agemean5)

  agemall <- c(agem1, agem2, agem3, agem4, agem5)


  #ager of recipients

  agermean1 <- vector()
  agermean2 <- vector()
  agermean3 <- vector()
  agermean4 <- vector()
  agermean5 <- vector()

  #Age of the one receiving inheritances
  agermean1 <- weighted.mean (dataukh1$age[dataukh1$inh>0], w=dataukh1$weight[dataukh1$inh>0])
  agermean2 <- weighted.mean (dataukh2$age[dataukh2$inh>0], w=dataukh2$weight[dataukh2$inh>0])
  agermean3 <- weighted.mean (dataukh3$age[dataukh3$inh>0], w=dataukh3$weight[dataukh3$inh>0])
  agermean4 <- weighted.mean (dataukh4$age[dataukh4$inh>0], w=dataukh4$weight[dataukh4$inh>0])
  agermean5 <- weighted.mean (dataukh5$age[dataukh5$inh>0], w=dataukh5$weight[dataukh5$inh>0])

  agerm1 <- mean(agermean1)
  agerm2 <- mean(agermean2)
  agerm3 <- mean(agermean3)
  agerm4 <- mean(agermean4)
  agerm5 <- mean(agermean5)

  agermall <- c(agerm1, agerm2, agerm3, agerm4, agerm5)




  #avgwealth

  avgwealthmean1 <- vector()
  avgwealthmean2 <- vector()
  avgwealthmean3 <- vector()
  avgwealthmean4 <- vector()
  avgwealthmean5 <- vector()

  #wealth
  avgwealthmean1 <- weighted.mean (dataukh1$wealth, w=dataukh1$weight)
  avgwealthmean2 <- weighted.mean (dataukh2$wealth, w=dataukh2$weight)
  avgwealthmean3 <- weighted.mean (dataukh3$wealth, w=dataukh3$weight)
  avgwealthmean4 <- weighted.mean (dataukh4$wealth, w=dataukh4$weight)
  avgwealthmean5 <- weighted.mean (dataukh5$wealth, w=dataukh5$weight)

  avgwealthm1 <- mean(avgwealthmean1)
  avgwealthm2 <- mean(avgwealthmean2)
  avgwealthm3 <- mean(avgwealthmean3)
  avgwealthm4 <- mean(avgwealthmean4)
  avgwealthm5 <- mean(avgwealthmean5)

  avgwealthmall <- c(avgwealthm1, avgwealthm2, avgwealthm3, avgwealthm4, avgwealthm5)



  #avginh of recipients

  avginhmean1 <- vector()
  avginhmean2 <- vector()
  avginhmean3 <- vector()
  avginhmean4 <- vector()
  avginhmean5 <- vector()

  #inh of the one receiving inheritances
  avginhmean1 <- weighted.mean (dataukh1$inh[dataukh1$inh>0], w=dataukh1$weight[dataukh1$inh>0], na.rm = T)
  avginhmean2 <- weighted.mean (dataukh2$inh[dataukh2$inh>0], w=dataukh2$weight[dataukh2$inh>0], na.rm = T)
  avginhmean3 <- weighted.mean (dataukh3$inh[dataukh3$inh>0], w=dataukh3$weight[dataukh3$inh>0], na.rm = T)
  avginhmean4 <- weighted.mean (dataukh4$inh[dataukh4$inh>0], w=dataukh4$weight[dataukh4$inh>0], na.rm = T)
  avginhmean5 <- weighted.mean (dataukh5$inh[dataukh5$inh>0], w=dataukh5$weight[dataukh5$inh>0], na.rm = T)

  avginhm1 <- mean(avginhmean1)
  avginhm2 <- mean(avginhmean2)
  avginhm3 <- mean(avginhmean3)
  avginhm4 <- mean(avginhmean4)
  avginhm5 <- mean(avginhmean5)

  avginhmall <- c(avginhm1, avginhm2, avginhm3, avginhm4, avginhm5)

  #now the MLD index

  theilwealth1 <- vector()
  theilwealth2 <- vector()
  theilwealth3 <- vector()
  theilwealth4 <- vector()
  theilwealth5 <- vector()

  #Theil wealth
  theillist <- calcGEI (dataukh1$wealth, w=dataukh1$weight, alpha = 0)
  theilwealth1 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataukh2$wealth, w=dataukh2$weight, alpha = 0)
  theilwealth2 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataukh3$wealth, w=dataukh3$weight, alpha = 0)
  theilwealth3 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataukh4$wealth, w=dataukh4$weight, alpha = 0)
  theilwealth4 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataukh5$wealth, w=dataukh5$weight, alpha = 0)
  theilwealth5 <- as.numeric(theillist$ineq$index)

  theilwm1 <- mean(theilwealth1)
  theilwm2 <- mean(theilwealth2)
  theilwm3 <- mean(theilwealth3)
  theilwm4 <- mean(theilwealth4)
  theilwm5 <- mean(theilwealth5)

  theilwmall <- c(theilwm1, theilwm2, theilwm3, theilwm4, theilwm5)

  #bottom 50% share

  bottom50sh1 <- vector()
  bottom50sh2 <- vector()
  bottom50sh3 <- vector()
  bottom50sh4 <- vector()
  bottom50sh5 <- vector()

  #Bottom 50% share
  bottom50sh1 <- bottom50share (dataukh1$wealth, dataukh1$weight)
  bottom50sh2 <- bottom50share (dataukh2$wealth, dataukh2$weight)
  bottom50sh3 <- bottom50share (dataukh3$wealth, dataukh3$weight)
  bottom50sh4 <- bottom50share (dataukh4$wealth, dataukh4$weight)
  bottom50sh5 <- bottom50share (dataukh5$wealth, dataukh5$weight)

  b50m1 <- mean(bottom50sh1)
  b50m2 <- mean(bottom50sh2)
  b50m3 <- mean(bottom50sh3)
  b50m4 <- mean(bottom50sh4)
  b50m5 <- mean(bottom50sh5)

  b50mall <- c(b50m1, b50m2, b50m3, b50m4, b50m5)

  #top 20% share

  top20sh1 <- vector()
  top20sh2 <- vector()
  top20sh3 <- vector()
  top20sh4 <- vector()
  top20sh5 <- vector()

  #top 20% share
  top20sh1 <- top20share (dataukh1$wealth, dataukh1$weight)
  top20sh2 <- top20share (dataukh2$wealth, dataukh2$weight)
  top20sh3 <- top20share (dataukh3$wealth, dataukh3$weight)
  top20sh4 <- top20share (dataukh4$wealth, dataukh4$weight)
  top20sh5 <- top20share (dataukh5$wealth, dataukh5$weight)

  t20m1 <- mean(top20sh1)
  t20m2 <- mean(top20sh2)
  t20m3 <- mean(top20sh3)
  t20m4 <- mean(top20sh4)
  t20m5 <- mean(top20sh5)

  t20mall <- c(t20m1, t20m2, t20m3, t20m4, t20m5)

  #top 10% share

  top10sh1 <- vector()
  top10sh2 <- vector()
  top10sh3 <- vector()
  top10sh4 <- vector()
  top10sh5 <- vector()

  #top 10% share
  top10sh1 <- top10share (dataukh1$wealth, dataukh1$weight)
  top10sh2 <- top10share (dataukh2$wealth, dataukh2$weight)
  top10sh3 <- top10share (dataukh3$wealth, dataukh3$weight)
  top10sh4 <- top10share (dataukh4$wealth, dataukh4$weight)
  top10sh5 <- top10share (dataukh5$wealth, dataukh5$weight)

  t10m1 <- mean(top10sh1)
  t10m2 <- mean(top10sh2)
  t10m3 <- mean(top10sh3)
  t10m4 <- mean(top10sh4)
  t10m5 <- mean(top10sh5)

  t10mall <- c(t10m1, t10m2, t10m3, t10m4, t10m5)

  descrip <- rbind(obsall, shrepall, agemall, agermall, avgwealthmall, avginhmall, theilwmall, b50mall, t20mall, t10mall)

  write.csv (descrip, file = "results/descriptives_uk.csv")



#### descriptives categories / with final subsample and equivalised non-adjusted values

#We can also check the inheritance quantiles

q0 <- vector ()
q1 <- vector ()
q2 <- vector ()
q3 <- vector ()


q0 <- reldist::wtd.quantile (dataukh4$eqinh [dataukh4$eqinh > 0], q = 0, weight=dataukh4$weight[dataukh4$eqinh > 0], na.rm = T)
q1 <- reldist::wtd.quantile (dataukh4$eqinh [dataukh4$eqinh > 0], q = 0.25, weight=dataukh4$weight[dataukh4$eqinh > 0], na.rm = T)
q2 <- reldist::wtd.quantile (dataukh4$eqinh [dataukh4$eqinh > 0], q = 0.5, weight=dataukh4$weight[dataukh4$eqinh > 0], na.rm = T)
q3 <- reldist::wtd.quantile (dataukh4$eqinh [dataukh4$eqinh > 0], q = 0.75, weight=dataukh4$weight[dataukh4$eqinh > 0], na.rm = T)

q0m <- mean (q0)
q1m <- mean (q1)
q2m <- mean (q2)
q3m <- mean (q3)

#expecting inh

summary(dataukh4$IHFutW1)
summary(dataukh4$IHFutW1[dataukh4$inh<=0])

dataukh4$expectinh <- 0
dataukh4$expectinh [dataukh4$IHFutW1 == "Definitely will" & dataukh4$inh<=0] <- 1
dataukh4$expectinh [dataukh4$IHFutW1 == "Very likely" & dataukh4$inh<=0] <- 1
dataukh4$expectinh [dataukh4$IHFutW1 == "Fairly likely" & dataukh4$inh<=0] <- 1

summary (dataukh4$expectinh)
table (dataukh4$expectinh)

#WE EXCLUDE THE EXPECTED INHERITANCES UNDER 10000 USD

table(dataukh4$IFutVIBW1[dataukh4$expectinh ==1])

#dataukh4$expectinh [dataukh4$IFutVIBW1 == "Refused"] <- 0
dataukh4$expectinh [dataukh4$IFutVIBW1 == "Less than \xa31,000"] <- 0
dataukh4$expectinh [dataukh4$IFutVIBW1 == "\xa31,000 to \xa34,999"] <- 0
dataukh4$expectinh [dataukh4$IFutVIBW1 == "\2435,000 to \2439,999"] <- 0 #We exclude smaller than 10000 only
#dataukh4$expectinh [dataukh4$IFutVIBW1 == "Don t know (Spontaneous only)"] <- 0

table(dataukh4$IFutVIBW1[dataukh4$expectinh ==1])

shareexpecting <- vector()

shareexpecting <- sum(dataukh4$weight[dataukh4$expectinh ==1 & dataukh4$inh==0], na.rm = T)/sum(as.numeric(dataukh4$weight[dataukh4$inh==0]), na.rm = T)

shareexpectingm <- mean(shareexpecting)

#education

table (dataukh4$maxeduparent)

shedulow <- vector ()
shedumidlow <- vector ()
shedumidhigh <- vector ()
sheduhigh <- vector ()

shedulow <- sum(as.numeric((dataukh4$weight[dataukh4$maxeduparent == 1 ])), na.rm = T)/sum(as.numeric(dataukh4$weight), na.rm = T)

shedumidlow<- sum(as.numeric((dataukh4$weight[dataukh4$maxeduparent == 2 ])), na.rm = T)/sum(as.numeric(dataukh4$weight), na.rm = T)

shedumidhigh<- sum(as.numeric((dataukh4$weight[dataukh4$maxeduparent == 3 ])), na.rm = T)/sum(as.numeric(dataukh4$weight), na.rm = T)

sheduhigh<- sum(as.numeric((dataukh4$weight[dataukh4$maxeduparent == 4 ])), na.rm = T)/sum(as.numeric(dataukh4$weight), na.rm = T)

shedulowm <- mean (shedulow)
shedumidlowm <- mean (shedumidlow)
shedumidhighm <- mean (shedumidhigh)
sheduhighm <- mean (sheduhigh)

#women

table(dataukh4$sex)

shwomen <- vector ()

shwomen <- sum(as.numeric((dataukh4$weight[dataukh4$sex == "Female" ])), na.rm = T)/sum(as.numeric(dataukh4$weight), na.rm = T)

shwomenm <- mean(shwomen)

descriptives_types_uk <- c(q1m, q2m, q3m, shareexpectingm, shedulowm, shedumidlowm, shedumidhighm, sheduhighm, shwomenm)

write.csv (descriptives_types_uk, file = "results/descriptives_types_uk.csv")


#shares with negative wealth now (among the 35-80 sample)

shareng <- vector ()
sharengw <- vector ()
sharerecipientsposw <- vector ()
sharerecipientsnegw <- vector ()
sharerecipientsposwbottom50 <-vector ()
sharelrecipientsposw <-vector ()
sharelrecipientsnegw <- vector ()
sharelrecipientsposwbottom50 <- vector ()

shareneg <- NROW(dataukh2$weight[dataukh2$wealth <= 0])/NROW(dataukh2$weight)
sharenegw <- sum(as.numeric(dataukh2$weight[dataukh2$wealth <= 0]))/sum(as.numeric(dataukh2$weight), na.rm = T)

sharerecipientsposw <- sum(as.numeric((dataukh3$inhdummy[dataukh3$inhdummy == 1])*(dataukh3$weight[dataukh3$inhdummy == 1])))/sum(as.numeric(dataukh3$weight), na.rm = T)

sharerecipientsnegw <- sum(as.numeric((dataukh2$inhdummy[dataukh2$inhdummy == 1 & dataukh2$wealth <=0])*(dataukh2$weight[dataukh2$inhdummy == 1 & dataukh2$wealth <=0])))/sum(as.numeric(dataukh2$weight[dataukh2$wealth <=0]), na.rm = T)

sharerecipientsposwbottom50 <- sum(as.numeric((dataukh3$inhdummy[dataukh3$inhdummy == 1 & dataukh3$wealth < (reldist::wtd.quantile (dataukh3$wealth, q = 0.5, weight=dataukh3$weight, na.rm = T))])*(dataukh3$weight[dataukh3$inhdummy == 1 & dataukh3$wealth < (reldist::wtd.quantile (dataukh3$wealth, q = 0.5, weight=dataukh3$weight, na.rm = T))])))/sum(as.numeric(dataukh3$weight[dataukh3$wealth < (reldist::wtd.quantile (dataukh3$wealth, q = 0.5, weight=dataukh3$weight, na.rm = T))]), na.rm = T)

#dummy for large inheritances
dataukh2$linhdummy <- 0
dataukh2$linhdummy[dataukh2$inh > q3m] <- 1

dataukh3$linhdummy <- 0
dataukh3$linhdummy[dataukh3$inh > q3m] <- 1

sharelrecipientsposw <- sum(as.numeric((dataukh3$linhdummy[dataukh3$linhdummy == 1])*(dataukh3$weight[dataukh3$linhdummy == 1])))/sum(as.numeric(dataukh3$weight), na.rm = T)

sharelrecipientsnegw <- sum(as.numeric((dataukh2$linhdummy[dataukh2$linhdummy == 1 & dataukh2$wealth <=0])*(dataukh2$weight[dataukh2$linhdummy == 1 & dataukh2$wealth <=0])))/sum(as.numeric(dataukh2$weight[dataukh2$wealth <=0]), na.rm = T)

sharelrecipientsposwbottom50 <- sum(as.numeric((dataukh3$linhdummy[dataukh3$linhdummy == 1 & dataukh3$wealth < (reldist::wtd.quantile (dataukh3$wealth, q = 0.5, weight=dataukh3$weight, na.rm = T))])*(dataukh3$weight[dataukh3$linhdummy == 1 & dataukh3$wealth < (reldist::wtd.quantile (dataukh3$wealth, q = 0.5, weight=dataukh3$weight, na.rm = T))])))/sum(as.numeric(dataukh3$weight[dataukh3$wealth < (reldist::wtd.quantile (dataukh3$wealth, q = 0.5, weight=dataukh3$weight, na.rm = T))]), na.rm = T)

sharengmean <- mean (shareneg)
sharengwmean <- mean (sharenegw)
sharerecipientsposwmean <- mean(sharerecipientsposw)
sharerecipientsnegwmean <- mean(sharerecipientsnegw)
sharerecipientsposwbottom50mean <-mean(sharerecipientsposwbottom50)
sharelrecipientsposwmean <- mean(sharelrecipientsposw)
sharelrecipientsnegwmean <- mean(sharelrecipientsnegw)
sharelrecipientsposwbottom50mean <-mean(sharelrecipientsposwbottom50)

neg_check_uk <- c(sharengmean, sharengwmean, sharerecipientsposwmean, sharerecipientsposwbottom50mean, sharerecipientsnegwmean, sharelrecipientsposwmean, sharelrecipientsposwbottom50mean, sharelrecipientsnegwmean)

write.csv (neg_check_uk, file = "results/negcheck_uk.csv")


}

