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

#let's do a loop with all imputations and store each value in a vector

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/datasets") #Set Working Directory # LAPTOP DIRECTORY

temp = list.files(pattern="HFCS14_France_after_step_2b-no-pension-ad-eq_*") #creates a list with all the personal files in the eu-silc directory directory
imps <- lapply(temp, readRDS) #reads the list (note, alphabetical order)

temp5 = list.files(pattern="v2-HFCS14_France_after_step_2d-no-pension-ad-eq_imp*") #creates a list with all the personal files in the eu-silc directory directory
imps5 <- lapply(temp5, readRDS) #reads the list (note, alphabetical order)

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory # LAPTOP DIRECTORY


#START LOOP

for (i in 1:length(imps)) {
  datafrh <-  imps[[i]]


#We can read the data
#datafrh <- readRDS(file = "datasets/HFCS14_France_after_step_2b-no-pension-ad-eq_imp1.rds") ##

#we equivalise wealth and inheritance to have those variables too

datafrh$eqinh <- datafrh$inh/datafrh$adeqscale
datafrh$eqinh[datafrh$eqinh<5000] <- 0
datafrh$inh[datafrh$eqinh<5000] <- 0 #also eliminate inheritances

#Share of recipients
datafrh$inhdummy <- 0
datafrh$inhdummy[datafrh$inh > 0] <- 1
#datafrh$inhdummy[datafrh$inh = 0] <- 0


#original data excluding missing
#We filter with the one with information on parental occupation
datafrh <- datafrh[!is.na(datafrh$occdadhead),]
datafrh1 <- datafrh

datafrh <- datafrh[!is.na(datafrh$inh),]

#data sampled 35-80
datafrh2 <- datafrh1[datafrh1$age >= 35,]
datafrh2 <- datafrh2[datafrh2$age <= 80,]

#and excluding negative wealth
datafrh3 <- datafrh2[datafrh2$wealth>0,]

#and using equivalent wealth and equivalent inheritance

datafrh4 <- datafrh3

datafrh4$wealth <- datafrh4$wealth/datafrh4$adeqscale
datafrh4$inh <- datafrh4$inh/datafrh4$adeqscale

#and our final sample

datafrh5 <-  imps5[[i]]
#wealth is already equivalised here
datafrh5$inh <- datafrh5$eqinh


#Share of recipients
datafrh5$inhdummy <- 0
datafrh5$inhdummy[datafrh5$inh > 0] <- 1
datafrh5$inhdummy[datafrh5$inh == 0] <- 0

datafrh5$wealth <- datafrh5$wealthpredictexp #We use the value of wealth without using taking into account age


#let's calculate the descriptives

#obs

obs1 <- as.numeric(length (datafrh1$wealth))
obs2 <- as.numeric(length (datafrh2$wealth))
obs3 <- as.numeric(length (datafrh3$wealth))
obs4 <- as.numeric(length (datafrh4$wealth))
obs5 <- as.numeric(length (datafrh5$wealth))

obsall <- c(obs1, obs2, obs3, obs4, obs5)


#share receiving

sharerecipients1 <- vector()
sharerecipients2 <- vector()
sharerecipients3 <- vector()
sharerecipients4 <- vector()
sharerecipients5 <- vector()


sharerecipients1 <- sum(as.numeric((datafrh1$inhdummy[datafrh1$inhdummy == 1])*(datafrh1$weight[datafrh1$inhdummy == 1])))/sum(as.numeric(datafrh1$weight), na.rm = T)
sharerecipients2 <- sum(as.numeric((datafrh2$inhdummy[datafrh2$inhdummy == 1])*(datafrh2$weight[datafrh2$inhdummy == 1])))/sum(as.numeric(datafrh2$weight), na.rm = T)
sharerecipients3 <- sum(as.numeric((datafrh3$inhdummy[datafrh3$inhdummy == 1])*(datafrh3$weight[datafrh3$inhdummy == 1])))/sum(as.numeric(datafrh3$weight), na.rm = T)
sharerecipients4 <- sum(as.numeric((datafrh4$inhdummy[datafrh4$inhdummy == 1])*(datafrh4$weight[datafrh4$inhdummy == 1])))/sum(as.numeric(datafrh4$weight), na.rm = T)
sharerecipients5 <- sum(as.numeric((datafrh5$inhdummy[datafrh5$inhdummy == 1])*(datafrh5$weight[datafrh5$inhdummy == 1])))/sum(as.numeric(datafrh5$weight), na.rm = T)

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

agemean1 <- weighted.mean (datafrh1$age, w=datafrh1$weight)
agemean2 <- weighted.mean (datafrh2$age, w=datafrh2$weight)
agemean3 <- weighted.mean (datafrh3$age, w=datafrh3$weight)
agemean4 <- weighted.mean (datafrh4$age, w=datafrh4$weight)
agemean5 <- weighted.mean (datafrh5$age, w=datafrh5$weight)

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
agermean1 <- weighted.mean (datafrh1$age[datafrh1$inh>0], w=datafrh1$weight[datafrh1$inh>0])
agermean2 <- weighted.mean (datafrh2$age[datafrh2$inh>0], w=datafrh2$weight[datafrh2$inh>0])
agermean3 <- weighted.mean (datafrh3$age[datafrh3$inh>0], w=datafrh3$weight[datafrh3$inh>0])
agermean4 <- weighted.mean (datafrh4$age[datafrh4$inh>0], w=datafrh4$weight[datafrh4$inh>0])
agermean5 <- weighted.mean (datafrh5$age[datafrh5$inh>0], w=datafrh5$weight[datafrh5$inh>0])

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
avgwealthmean1 <- weighted.mean (datafrh1$wealth, w=datafrh1$weight)
avgwealthmean2 <- weighted.mean (datafrh2$wealth, w=datafrh2$weight)
avgwealthmean3 <- weighted.mean (datafrh3$wealth, w=datafrh3$weight)
avgwealthmean4 <- weighted.mean (datafrh4$wealth, w=datafrh4$weight)
avgwealthmean5 <- weighted.mean (datafrh5$wealth, w=datafrh5$weight)

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
avginhmean1 <- weighted.mean (datafrh1$inh[datafrh1$inh>0], w=datafrh1$weight[datafrh1$inh>0], na.rm = T)
avginhmean2 <- weighted.mean (datafrh2$inh[datafrh2$inh>0], w=datafrh2$weight[datafrh2$inh>0], na.rm = T)
avginhmean3 <- weighted.mean (datafrh3$inh[datafrh3$inh>0], w=datafrh3$weight[datafrh3$inh>0], na.rm = T)
avginhmean4 <- weighted.mean (datafrh4$inh[datafrh4$inh>0], w=datafrh4$weight[datafrh4$inh>0], na.rm = T)
avginhmean5 <- weighted.mean (datafrh5$inh[datafrh5$inh>0], w=datafrh5$weight[datafrh5$inh>0], na.rm = T)

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
theillist <- calcGEI (datafrh1$wealth, w=datafrh1$weight, alpha = 0)
theilwealth1 <- as.numeric(theillist$ineq$index)
theillist <- calcGEI (datafrh2$wealth, w=datafrh2$weight, alpha = 0)
theilwealth2 <- as.numeric(theillist$ineq$index)
theillist <- calcGEI (datafrh3$wealth, w=datafrh3$weight, alpha = 0)
theilwealth3 <- as.numeric(theillist$ineq$index)
theillist <- calcGEI (datafrh4$wealth, w=datafrh4$weight, alpha = 0)
theilwealth4 <- as.numeric(theillist$ineq$index)
theillist <- calcGEI (datafrh5$wealth, w=datafrh5$weight, alpha = 0)
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
bottom50sh1 <- bottom50share (datafrh1$wealth, datafrh1$weight)
bottom50sh2 <- bottom50share (datafrh2$wealth, datafrh2$weight)
bottom50sh3 <- bottom50share (datafrh3$wealth, datafrh3$weight)
bottom50sh4 <- bottom50share (datafrh4$wealth, datafrh4$weight)
bottom50sh5 <- bottom50share (datafrh5$wealth, datafrh5$weight)

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
top20sh1 <- top20share (datafrh1$wealth, datafrh1$weight)
top20sh2 <- top20share (datafrh2$wealth, datafrh2$weight)
top20sh3 <- top20share (datafrh3$wealth, datafrh3$weight)
top20sh4 <- top20share (datafrh4$wealth, datafrh4$weight)
top20sh5 <- top20share (datafrh5$wealth, datafrh5$weight)

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
top10sh1 <- top10share (datafrh1$wealth, datafrh1$weight)
top10sh2 <- top10share (datafrh2$wealth, datafrh2$weight)
top10sh3 <- top10share (datafrh3$wealth, datafrh3$weight)
top10sh4 <- top10share (datafrh4$wealth, datafrh4$weight)
top10sh5 <- top10share (datafrh5$wealth, datafrh5$weight)

t10m1 <- mean(top10sh1)
t10m2 <- mean(top10sh2)
t10m3 <- mean(top10sh3)
t10m4 <- mean(top10sh4)
t10m5 <- mean(top10sh5)

t10mall <- c(t10m1, t10m2, t10m3, t10m4, t10m5)

descripfr <- rbind(obsall, shrepall, agemall, agermall, avgwealthmall, avginhmall, theilwmall, b50mall, t20mall, t10mall)

write.csv (descripfr, file = "results/france/descriptives_france.csv")


#descriptive types
#We can also check the inheritance quantiles

q0 <- vector ()
q1 <- vector ()
q2 <- vector ()
q3 <- vector ()


q0 <- reldist::wtd.quantile (datafrh4$eqinh [datafrh4$eqinh > 0], q = 0, weight=datafrh4$weight[datafrh4$eqinh > 0], na.rm = T)
q1 <- reldist::wtd.quantile (datafrh4$eqinh [datafrh4$eqinh > 0], q = 0.25, weight=datafrh4$weight[datafrh4$eqinh > 0], na.rm = T)
q2 <- reldist::wtd.quantile (datafrh4$eqinh [datafrh4$eqinh > 0], q = 0.5, weight=datafrh4$weight[datafrh4$eqinh > 0], na.rm = T)
q3 <- reldist::wtd.quantile (datafrh4$eqinh [datafrh4$eqinh > 0], q = 0.75, weight=datafrh4$weight[datafrh4$eqinh > 0], na.rm = T)

q0m <- mean (q0)
q1m <- mean (q1)
q2m <- mean (q2)
q3m <- mean (q3)

#expecting inh

#We create a dummy for expect inheritance #We don't have the amount for the EU
datafrh4$expectinh <- 0 #THE NA'S IN HH0700 WILL ALSO BE CONSIDERED AS NOT EXPECTING
datafrh4$expectinh[datafrh4$HH0700 ==1] <- 1

shareexpecting <- vector()

shareexpecting <- sum(as.numeric(datafrh4$weight[datafrh4$inhdummy == 0 & datafrh4$expectinh ==1]), na.rm = T)/sum(as.numeric(datafrh4$weight[datafrh4$inhdummy == 0]), na.rm = T)

shareexpectingm <- mean(shareexpecting)


#occupation

table (datafrh4$occdadhead)

shocclow <- vector ()
shoccmidlow <- vector ()
shoccmidhigh <- vector ()
shocchigh <- vector ()

shocclow <- sum(as.numeric((datafrh4$weight[datafrh4$occdadhead == 1 ])), na.rm = T)/sum(as.numeric(datafrh4$weight), na.rm = T)

shoccmidlow<- sum(as.numeric((datafrh4$weight[datafrh4$occdadhead == 2 ])), na.rm = T)/sum(as.numeric(datafrh4$weight), na.rm = T)

shoccmidhigh<- sum(as.numeric((datafrh4$weight[datafrh4$occdadhead == 3 ])), na.rm = T)/sum(as.numeric(datafrh4$weight), na.rm = T)

shocchigh<- sum(as.numeric((datafrh4$weight[datafrh4$occdadhead == 4 ])), na.rm = T)/sum(as.numeric(datafrh4$weight), na.rm = T)

shocclowm <- mean (shocclow)
shoccmidlowm <- mean (shoccmidlow)
shoccmidhighm <- mean (shoccmidhigh)
shocchighm <- mean (shocchigh)

#women

table(datafrh4$sex)

shwomen <- vector ()

shwomen <- sum(as.numeric((datafrh4$weight[datafrh4$sex == 2])), na.rm = T)/sum(as.numeric(datafrh4$weight), na.rm = T)

shwomenm <- mean(shwomen)

descriptives_types_fr <- c(q1m, q2m, q3m, shareexpectingm, shocclowm, shoccmidlowm, shoccmidhighm, shocchighm, shwomenm)

write.csv (descriptives_types_fr, file = "results/france/descriptives_types_france.csv")


#shares with negative wealth now (among the 35-80 sample)


shareng <- vector ()
sharengw <- vector ()
sharerecipientsposw <- vector ()
sharerecipientsnegw <- vector ()
sharerecipientsposwbottom50 <-vector ()
sharelrecipientsposw <-vector ()
sharelrecipientsnegw <- vector ()
sharelrecipientsposwbottom50 <- vector ()

shareneg <- NROW(datafrh2$weight[datafrh2$wealth <= 0])/NROW(datafrh2$weight)
sharenegw <- sum(as.numeric(datafrh2$weight[datafrh2$wealth <= 0]))/sum(as.numeric(datafrh2$weight), na.rm = T)

sharerecipientsposw <- sum(as.numeric((datafrh3$inhdummy[datafrh3$inhdummy == 1])*(datafrh3$weight[datafrh3$inhdummy == 1])))/sum(as.numeric(datafrh3$weight), na.rm = T)

sharerecipientsnegw <- sum(as.numeric((datafrh2$inhdummy[datafrh2$inhdummy == 1 & datafrh2$wealth <=0])*(datafrh2$weight[datafrh2$inhdummy == 1 & datafrh2$wealth <=0])))/sum(as.numeric(datafrh2$weight[datafrh2$wealth <=0]), na.rm = T)

sharerecipientsposwbottom50 <- sum(as.numeric((datafrh3$inhdummy[datafrh3$inhdummy == 1 & datafrh3$wealth < (reldist::wtd.quantile (datafrh3$wealth, q = 0.5, weight=datafrh3$weight, na.rm = T))])*(datafrh3$weight[datafrh3$inhdummy == 1 & datafrh3$wealth < (reldist::wtd.quantile (datafrh3$wealth, q = 0.5, weight=datafrh3$weight, na.rm = T))])))/sum(as.numeric(datafrh3$weight[datafrh3$wealth < (reldist::wtd.quantile (datafrh3$wealth, q = 0.5, weight=datafrh3$weight, na.rm = T))]), na.rm = T)

#dummy for large inheritances
datafrh2$linhdummy <- 0
datafrh2$linhdummy[datafrh2$inh > q3m] <- 1

datafrh3$linhdummy <- 0
datafrh3$linhdummy[datafrh3$inh > q3m] <- 1

sharelrecipientsposw <- sum(as.numeric((datafrh3$linhdummy[datafrh3$linhdummy == 1])*(datafrh3$weight[datafrh3$linhdummy == 1])))/sum(as.numeric(datafrh3$weight), na.rm = T)

sharelrecipientsnegw <- sum(as.numeric((datafrh2$linhdummy[datafrh2$linhdummy == 1 & datafrh2$wealth <=0])*(datafrh2$weight[datafrh2$linhdummy == 1 & datafrh2$wealth <=0])))/sum(as.numeric(datafrh2$weight[datafrh2$wealth <=0]), na.rm = T)

sharelrecipientsposwbottom50 <- sum(as.numeric((datafrh3$linhdummy[datafrh3$linhdummy == 1 & datafrh3$wealth < (reldist::wtd.quantile (datafrh3$wealth, q = 0.5, weight=datafrh3$weight, na.rm = T))])*(datafrh3$weight[datafrh3$linhdummy == 1 & datafrh3$wealth < (reldist::wtd.quantile (datafrh3$wealth, q = 0.5, weight=datafrh3$weight, na.rm = T))])))/sum(as.numeric(datafrh3$weight[datafrh3$wealth < (reldist::wtd.quantile (datafrh3$wealth, q = 0.5, weight=datafrh3$weight, na.rm = T))]), na.rm = T)

sharengmean <- mean (shareneg)
sharengwmean <- mean (sharenegw)
sharerecipientsposwmean <- mean(sharerecipientsposw)
sharerecipientsnegwmean <- mean(sharerecipientsnegw)
sharerecipientsposwbottom50mean <-mean(sharerecipientsposwbottom50)
sharelrecipientsposwmean <- mean(sharelrecipientsposw)
sharelrecipientsnegwmean <- mean(sharelrecipientsnegw)
sharelrecipientsposwbottom50mean <-mean(sharelrecipientsposwbottom50)

neg_check_fr <- c(sharengmean, sharengwmean, sharerecipientsposwmean, sharerecipientsposwbottom50mean, sharerecipientsnegwmean, sharelrecipientsposwmean, sharelrecipientsposwbottom50mean, sharelrecipientsnegwmean)

write.csv (neg_check_fr, file = "results/france/negcheck_fr.csv")

}














