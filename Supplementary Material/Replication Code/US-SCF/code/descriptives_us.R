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

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory # LAPTOP DIRECTORY


dataush <- readRDS(file = "datasets/SCF-2016-all-after-1c.rds")


dataush1imp <- dataush[dataush$y1%%5 == 1,]
dataush2imp <- dataush[dataush$y1%%5 == 2,]
dataush3imp <- dataush[dataush$y1%%5 == 3,]
dataush4imp <- dataush[dataush$y1%%5 == 4,]
dataush5imp <- dataush[dataush$y1%%5 == 5,]

imps <- list (dataush1imp, dataush2imp, dataush3imp, dataush4imp, dataush5imp)


setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code/datasets") #Set Working Directory # LAPTOP DIRECTORY

temp5 = list.files(pattern="SCF-2016-all-after-1e-imp*") #creates a list with all the personal files in the eu-silc directory directory
imps5 <- lapply(temp5, readRDS) #reads the list (note, alphabetical order)

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory # LAPTOP DIRECTORY


#START LOOP

for (i in 1:length(imps)) {
  dataush <-  imps[[i]]


  #We can read the data
  #dataush <- readRDS(file = "datasets/HFCS14_France_after_step_2b-no-pension-ad-eq_imp1.rds") ##

  #we equivalise wealth and inheritance to have those variables too

  dataush$eqinh <- dataush$inh/dataush$adeqscale
  dataush$eqinh[dataush$eqinh<5000] <- 0
  dataush$inh[dataush$eqinh<5000] <- 0 #also eliminate inheritances

  #Share of recipients
  dataush$inhdummy <- 0
  dataush$inhdummy[dataush$inh > 0] <- 1
  #dataush$inhdummy[dataush$inh = 0] <- 0

  #original data excluding missing
  #We filter with the one with information on parental occupation
  #dataush <- dataush[!is.na(dataush$occdadhead),]
  dataush1 <- dataush

  #data sampled 35-80
  dataush2 <- dataush1[dataush1$age >= 35,]
  dataush2 <- dataush2[dataush2$age <= 80,]

  #and excluding negative wealth
  dataush3 <- dataush2[dataush2$wealth>0,]

  #and using equivalent wealth and equivalent inheritance

  dataush4 <- dataush3

  dataush4$wealth <- dataush4$wealth/dataush4$adeqscale
  dataush4$inh <- dataush4$inh/dataush4$adeqscale

  #and our final sample

  dataush5 <-  imps5[[i]]
  #wealth is already equivalised here
  dataush5$inh <- dataush5$eqinh


  #Share of recipients
  dataush5$inhdummy <- 0
  dataush5$inhdummy[dataush5$inh > 0] <- 1
  #dataush5$inhdummy[dataush5$inh == 0] <- 0

  dataush5$wealth <- dataush5$wealthpredictexp #We use the value of wealth without using taking into account age


  #let's calculate the descriptives

  #obs

  obs1 <- as.numeric(length (dataush1$wealth))
  obs2 <- as.numeric(length (dataush2$wealth))
  obs3 <- as.numeric(length (dataush3$wealth))
  obs4 <- as.numeric(length (dataush4$wealth))
  obs5 <- as.numeric(length (dataush5$wealth))

  obsall <- c(obs1, obs2, obs3, obs4, obs5)


  #share receiving

  sharerecipients1 <- vector()
  sharerecipients2 <- vector()
  sharerecipients3 <- vector()
  sharerecipients4 <- vector()
  sharerecipients5 <- vector()


  sharerecipients1 <- sum(as.numeric((dataush1$inhdummy[dataush1$inhdummy == 1])*(dataush1$weight[dataush1$inhdummy == 1])))/sum(as.numeric(dataush1$weight), na.rm = T)
  sharerecipients2 <- sum(as.numeric((dataush2$inhdummy[dataush2$inhdummy == 1])*(dataush2$weight[dataush2$inhdummy == 1])))/sum(as.numeric(dataush2$weight), na.rm = T)
  sharerecipients3 <- sum(as.numeric((dataush3$inhdummy[dataush3$inhdummy == 1])*(dataush3$weight[dataush3$inhdummy == 1])))/sum(as.numeric(dataush3$weight), na.rm = T)
  sharerecipients4 <- sum(as.numeric((dataush4$inhdummy[dataush4$inhdummy == 1])*(dataush4$weight[dataush4$inhdummy == 1])))/sum(as.numeric(dataush4$weight), na.rm = T)
  sharerecipients5 <- sum(as.numeric((dataush5$inhdummy[dataush5$inhdummy == 1])*(dataush5$weight[dataush5$inhdummy == 1])))/sum(as.numeric(dataush5$weight), na.rm = T)

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

  agemean1 <- weighted.mean (dataush1$age, w=dataush1$weight)
  agemean2 <- weighted.mean (dataush2$age, w=dataush2$weight)
  agemean3 <- weighted.mean (dataush3$age, w=dataush3$weight)
  agemean4 <- weighted.mean (dataush4$age, w=dataush4$weight)
  agemean5 <- weighted.mean (dataush5$age, w=dataush5$weight)

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
  agermean1 <- weighted.mean (dataush1$age[dataush1$inh>0], w=dataush1$weight[dataush1$inh>0])
  agermean2 <- weighted.mean (dataush2$age[dataush2$inh>0], w=dataush2$weight[dataush2$inh>0])
  agermean3 <- weighted.mean (dataush3$age[dataush3$inh>0], w=dataush3$weight[dataush3$inh>0])
  agermean4 <- weighted.mean (dataush4$age[dataush4$inh>0], w=dataush4$weight[dataush4$inh>0])
  agermean5 <- weighted.mean (dataush5$age[dataush5$inh>0], w=dataush5$weight[dataush5$inh>0])

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
  avgwealthmean1 <- weighted.mean (dataush1$wealth, w=dataush1$weight)
  avgwealthmean2 <- weighted.mean (dataush2$wealth, w=dataush2$weight)
  avgwealthmean3 <- weighted.mean (dataush3$wealth, w=dataush3$weight)
  avgwealthmean4 <- weighted.mean (dataush4$wealth, w=dataush4$weight)
  avgwealthmean5 <- weighted.mean (dataush5$wealth, w=dataush5$weight)

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
  avginhmean1 <- weighted.mean (dataush1$inh[dataush1$inh>0], w=dataush1$weight[dataush1$inh>0], na.rm = T)
  avginhmean2 <- weighted.mean (dataush2$inh[dataush2$inh>0], w=dataush2$weight[dataush2$inh>0], na.rm = T)
  avginhmean3 <- weighted.mean (dataush3$inh[dataush3$inh>0], w=dataush3$weight[dataush3$inh>0], na.rm = T)
  avginhmean4 <- weighted.mean (dataush4$inh[dataush4$inh>0], w=dataush4$weight[dataush4$inh>0], na.rm = T)
  avginhmean5 <- weighted.mean (dataush5$inh[dataush5$inh>0], w=dataush5$weight[dataush5$inh>0], na.rm = T)

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
  theillist <- calcGEI (dataush1$wealth, w=dataush1$weight, alpha = 0)
  theilwealth1 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataush2$wealth, w=dataush2$weight, alpha = 0)
  theilwealth2 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataush3$wealth, w=dataush3$weight, alpha = 0)
  theilwealth3 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataush4$wealth, w=dataush4$weight, alpha = 0)
  theilwealth4 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataush5$wealth, w=dataush5$weight, alpha = 0)
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
  bottom50sh1 <- bottom50share (dataush1$wealth, dataush1$weight)
  bottom50sh2 <- bottom50share (dataush2$wealth, dataush2$weight)
  bottom50sh3 <- bottom50share (dataush3$wealth, dataush3$weight)
  bottom50sh4 <- bottom50share (dataush4$wealth, dataush4$weight)
  bottom50sh5 <- bottom50share (dataush5$wealth, dataush5$weight)

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
  top20sh1 <- top20share (dataush1$wealth, dataush1$weight)
  top20sh2 <- top20share (dataush2$wealth, dataush2$weight)
  top20sh3 <- top20share (dataush3$wealth, dataush3$weight)
  top20sh4 <- top20share (dataush4$wealth, dataush4$weight)
  top20sh5 <- top20share (dataush5$wealth, dataush5$weight)

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
  top10sh1 <- top10share (dataush1$wealth, dataush1$weight)
  top10sh2 <- top10share (dataush2$wealth, dataush2$weight)
  top10sh3 <- top10share (dataush3$wealth, dataush3$weight)
  top10sh4 <- top10share (dataush4$wealth, dataush4$weight)
  top10sh5 <- top10share (dataush5$wealth, dataush5$weight)

  t10m1 <- mean(top10sh1)
  t10m2 <- mean(top10sh2)
  t10m3 <- mean(top10sh3)
  t10m4 <- mean(top10sh4)
  t10m5 <- mean(top10sh5)

  t10mall <- c(t10m1, t10m2, t10m3, t10m4, t10m5)

  descrip <- rbind(obsall, shrepall, agemall, agermall, avgwealthmall, avginhmall, theilwmall, b50mall, t20mall, t10mall)

  write.csv (descrip, file = "results/us/descriptives_us.csv")



  #### descriptives categories / with final subsample and equivalised non-adjusted values

  #We can also check the inheritance quantiles

  q0 <- vector ()
  q1 <- vector ()
  q2 <- vector ()
  q3 <- vector ()


  q0 <- reldist::wtd.quantile (dataush4$eqinh [dataush4$eqinh > 0], q = 0, weight=dataush4$weight[dataush4$eqinh > 0], na.rm = T)
  q1 <- reldist::wtd.quantile (dataush4$eqinh [dataush4$eqinh > 0], q = 0.25, weight=dataush4$weight[dataush4$eqinh > 0], na.rm = T)
  q2 <- reldist::wtd.quantile (dataush4$eqinh [dataush4$eqinh > 0], q = 0.5, weight=dataush4$weight[dataush4$eqinh > 0], na.rm = T)
  q3 <- reldist::wtd.quantile (dataush4$eqinh [dataush4$eqinh > 0], q = 0.75, weight=dataush4$weight[dataush4$eqinh > 0], na.rm = T)

  q0m <- mean (q0)
  q1m <- mean (q1)
  q2m <- mean (q2)
  q3m <- mean (q3)

  #expecting inh

  summary (dataush4$X5819)
  table(dataush4$X5819)
  table(dataush4$X5819[dataush4$eqinh==0]) #Many expect among those not receiving inheritance

  summary (dataush4$X5821 [dataush4$X5819 ==1])
  NROW (dataush4$X5821 [dataush4$X5819 ==1])
  NROW (dataush4$X5821 [dataush4$X5819 ==1 & dataush4$X5821 >= 10000]) #Almost all of them are over 10000 USD

  #We create a dummy for expect inheritance (if over 10000 USD)
  dataush4$expecteqinh <- 0
  dataush4$expecteqinh[dataush4$X5819 ==1 & dataush4$X5821 >= 10000] <- 1

  dataush4$expectinh <- dataush4$expecteqinh

  shareexpecting <- vector()

  shareexpecting <- sum(as.numeric(dataush4$weight[dataush4$inhdummy == 0 & dataush4$expectinh ==1]), na.rm = T)/sum(as.numeric(dataush4$weight[dataush4$inhdummy == 0]), na.rm = T)

  shareexpectingm <- mean(shareexpecting)

  #education

  table (dataush4$maxeduparent)

  shedulow <- vector ()
  shedumidlow <- vector ()
  shedumidhigh <- vector ()
  sheduhigh <- vector ()

  shedulow <- sum(as.numeric((dataush4$weight[dataush4$maxeduparent == 1 ])), na.rm = T)/sum(as.numeric(dataush4$weight), na.rm = T)

  shedumidlow<- sum(as.numeric((dataush4$weight[dataush4$maxeduparent == 2 ])), na.rm = T)/sum(as.numeric(dataush4$weight), na.rm = T)

  shedumidhigh<- sum(as.numeric((dataush4$weight[dataush4$maxeduparent == 3 ])), na.rm = T)/sum(as.numeric(dataush4$weight), na.rm = T)

  sheduhigh<- sum(as.numeric((dataush4$weight[dataush4$maxeduparent == 4 ])), na.rm = T)/sum(as.numeric(dataush4$weight), na.rm = T)

  shedulowm <- mean (shedulow)
  shedumidlowm <- mean (shedumidlow)
  shedumidhighm <- mean (shedumidhigh)
  sheduhighm <- mean (sheduhigh)

  #women

  table(dataush4$sex)

  shwomen <- vector ()

  shwomen <- sum(as.numeric((dataush4$weight[dataush4$sex == 2])), na.rm = T)/sum(as.numeric(dataush4$weight), na.rm = T)

  shwomenm <- mean(shwomen)

  descriptives_types_us <- c(q1m, q2m, q3m, shareexpectingm, shedulowm, shedumidlowm, shedumidhighm, sheduhighm, shwomenm)

  write.csv (descriptives_types_us, file = "results/us/descriptives_types_us.csv")

  #shares with negative wealth now (among the 35-80 sample)


  shareng <- vector ()
  sharengw <- vector ()
  sharerecipientsposw <- vector ()
  sharerecipientsnegw <- vector ()
  sharerecipientsposwbottom50 <-vector ()
  sharelrecipientsposw <-vector ()
  sharelrecipientsnegw <- vector ()
  sharelrecipientsposwbottom50 <- vector ()

  shareneg <- NROW(dataush2$weight[dataush2$wealth <= 0])/NROW(dataush2$weight)
  sharenegw <- sum(as.numeric(dataush2$weight[dataush2$wealth <= 0]))/sum(as.numeric(dataush2$weight), na.rm = T)

  sharerecipientsposw <- sum(as.numeric((dataush3$inhdummy[dataush3$inhdummy == 1])*(dataush3$weight[dataush3$inhdummy == 1])))/sum(as.numeric(dataush3$weight), na.rm = T)

  sharerecipientsnegw <- sum(as.numeric((dataush2$inhdummy[dataush2$inhdummy == 1 & dataush2$wealth <=0])*(dataush2$weight[dataush2$inhdummy == 1 & dataush2$wealth <=0])))/sum(as.numeric(dataush2$weight[dataush2$wealth <=0]), na.rm = T)

  sharerecipientsposwbottom50 <- sum(as.numeric((dataush3$inhdummy[dataush3$inhdummy == 1 & dataush3$wealth < (reldist::wtd.quantile (dataush3$wealth, q = 0.5, weight=dataush3$weight, na.rm = T))])*(dataush3$weight[dataush3$inhdummy == 1 & dataush3$wealth < (reldist::wtd.quantile (dataush3$wealth, q = 0.5, weight=dataush3$weight, na.rm = T))])))/sum(as.numeric(dataush3$weight[dataush3$wealth < (reldist::wtd.quantile (dataush3$wealth, q = 0.5, weight=dataush3$weight, na.rm = T))]), na.rm = T)

  #dummy for large inheritances
  dataush2$linhdummy <- 0
  dataush2$linhdummy[dataush2$inh > q3m] <- 1

  dataush3$linhdummy <- 0
  dataush3$linhdummy[dataush3$inh > q3m] <- 1

  sharelrecipientsposw <- sum(as.numeric((dataush3$linhdummy[dataush3$linhdummy == 1])*(dataush3$weight[dataush3$linhdummy == 1])))/sum(as.numeric(dataush3$weight), na.rm = T)

  sharelrecipientsnegw <- sum(as.numeric((dataush2$linhdummy[dataush2$linhdummy == 1 & dataush2$wealth <=0])*(dataush2$weight[dataush2$linhdummy == 1 & dataush2$wealth <=0])))/sum(as.numeric(dataush2$weight[dataush2$wealth <=0]), na.rm = T)

  sharelrecipientsposwbottom50 <- sum(as.numeric((dataush3$linhdummy[dataush3$linhdummy == 1 & dataush3$wealth < (reldist::wtd.quantile (dataush3$wealth, q = 0.5, weight=dataush3$weight, na.rm = T))])*(dataush3$weight[dataush3$linhdummy == 1 & dataush3$wealth < (reldist::wtd.quantile (dataush3$wealth, q = 0.5, weight=dataush3$weight, na.rm = T))])))/sum(as.numeric(dataush3$weight[dataush3$wealth < (reldist::wtd.quantile (dataush3$wealth, q = 0.5, weight=dataush3$weight, na.rm = T))]), na.rm = T)

  sharengmean <- mean (shareneg)
  sharengwmean <- mean (sharenegw)
  sharerecipientsposwmean <- mean(sharerecipientsposw)
  sharerecipientsnegwmean <- mean(sharerecipientsnegw)
  sharerecipientsposwbottom50mean <-mean(sharerecipientsposwbottom50)
  sharelrecipientsposwmean <- mean(sharelrecipientsposw)
  sharelrecipientsnegwmean <- mean(sharelrecipientsnegw)
  sharelrecipientsposwbottom50mean <-mean(sharelrecipientsposwbottom50)

  neg_check_us <- c(sharengmean, sharengwmean, sharerecipientsposwmean, sharerecipientsposwbottom50mean, sharerecipientsnegwmean, sharelrecipientsposwmean, sharelrecipientsposwbottom50mean, sharelrecipientsnegwmean)

  write.csv (neg_check_us, file = "results/us/negcheck_us.csv")

}















