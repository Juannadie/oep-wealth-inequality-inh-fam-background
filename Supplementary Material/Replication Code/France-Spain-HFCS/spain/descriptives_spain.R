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

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/datasets/spain") #Set Working Directory # LAPTOP DIRECTORY

temp = list.files(pattern="HFCS14_Spain_after_step_2b-no-pension-ad-eq_*") #creates a list with all the personal files in the eu-silc directory directory
imps <- lapply(temp, readRDS) #reads the list (note, alphabetical order)

temp5 = list.files(pattern="v2-HFCS14_Spain_after_step_2d-no-pension-ad-eq_imp*") #creates a list with all the personal files in the eu-silc directory directory
imps5 <- lapply(temp5, readRDS) #reads the list (note, alphabetical order)

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory # LAPTOP DIRECTORY


#START LOOP

for (i in 1:length(imps)) {
  dataesh <-  imps[[i]]


  #We can read the data
  #dataesh <- readRDS(file = "datasets/HFCS14_France_after_step_2b-no-pension-ad-eq_imp1.rds") ##

  #we equivalise wealth and inheritance to have those variables too

  dataesh$eqinh <- dataesh$inh/dataesh$adeqscale
  dataesh$eqinh[dataesh$eqinh<5000] <- 0
  dataesh$inh[dataesh$eqinh<5000] <- 0 #also eliminate inheritances

  #Share of recipients
  dataesh$inhdummy <- 0
  dataesh$inhdummy[dataesh$inh > 0] <- 1
  #dataesh$inhdummy[dataesh$inh = 0] <- 0

  #original data excluding missing
  #We filter with the one with information on parental occupation
  dataesh <- dataesh[!is.na(dataesh$occdadhead),]
  dataesh1 <- dataesh

  #data sampled 35-80
  dataesh2 <- dataesh1[dataesh1$age >= 35,]
  dataesh2 <- dataesh2[dataesh2$age <= 80,]

  #and excluding negative wealth
  dataesh3 <- dataesh2[dataesh2$wealth>0,]

  #and using equivalent wealth and equivalent inheritance

  dataesh4 <- dataesh3

  dataesh4$wealth <- dataesh4$wealth/dataesh4$adeqscale
  dataesh4$inh <- dataesh4$inh/dataesh4$adeqscale

  #and our final sample

  dataesh5 <-  imps5[[i]]
  #wealth is already equivalised here
  dataesh5$inh <- dataesh5$eqinh


  #Share of recipients
  dataesh5$inhdummy <- 0
  dataesh5$inhdummy[dataesh5$inh > 0] <- 1
  dataesh5$inhdummy[dataesh5$inh == 0] <- 0

  dataesh5$wealth <- dataesh5$wealthpredictexp #We use the value of wealth without using taking into account age


  #let's calculate the descriptives

  #obs

  obs1 <- as.numeric(length (dataesh1$wealth))
  obs2 <- as.numeric(length (dataesh2$wealth))
  obs3 <- as.numeric(length (dataesh3$wealth))
  obs4 <- as.numeric(length (dataesh4$wealth))
  obs5 <- as.numeric(length (dataesh5$wealth))

  obsall <- c(obs1, obs2, obs3, obs4, obs5)


  #share receiving

  sharerecipients1 <- vector()
  sharerecipients2 <- vector()
  sharerecipients3 <- vector()
  sharerecipients4 <- vector()
  sharerecipients5 <- vector()


  sharerecipients1 <- sum(as.numeric((dataesh1$inhdummy[dataesh1$inhdummy == 1])*(dataesh1$weight[dataesh1$inhdummy == 1])))/sum(as.numeric(dataesh1$weight), na.rm = T)
  sharerecipients2 <- sum(as.numeric((dataesh2$inhdummy[dataesh2$inhdummy == 1])*(dataesh2$weight[dataesh2$inhdummy == 1])))/sum(as.numeric(dataesh2$weight), na.rm = T)
  sharerecipients3 <- sum(as.numeric((dataesh3$inhdummy[dataesh3$inhdummy == 1])*(dataesh3$weight[dataesh3$inhdummy == 1])))/sum(as.numeric(dataesh3$weight), na.rm = T)
  sharerecipients4 <- sum(as.numeric((dataesh4$inhdummy[dataesh4$inhdummy == 1])*(dataesh4$weight[dataesh4$inhdummy == 1])))/sum(as.numeric(dataesh4$weight), na.rm = T)
  sharerecipients5 <- sum(as.numeric((dataesh5$inhdummy[dataesh5$inhdummy == 1])*(dataesh5$weight[dataesh5$inhdummy == 1])))/sum(as.numeric(dataesh5$weight), na.rm = T)

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

  agemean1 <- weighted.mean (dataesh1$age, w=dataesh1$weight)
  agemean2 <- weighted.mean (dataesh2$age, w=dataesh2$weight)
  agemean3 <- weighted.mean (dataesh3$age, w=dataesh3$weight)
  agemean4 <- weighted.mean (dataesh4$age, w=dataesh4$weight)
  agemean5 <- weighted.mean (dataesh5$age, w=dataesh5$weight)

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
  agermean1 <- weighted.mean (dataesh1$age[dataesh1$inh>0], w=dataesh1$weight[dataesh1$inh>0])
  agermean2 <- weighted.mean (dataesh2$age[dataesh2$inh>0], w=dataesh2$weight[dataesh2$inh>0])
  agermean3 <- weighted.mean (dataesh3$age[dataesh3$inh>0], w=dataesh3$weight[dataesh3$inh>0])
  agermean4 <- weighted.mean (dataesh4$age[dataesh4$inh>0], w=dataesh4$weight[dataesh4$inh>0])
  agermean5 <- weighted.mean (dataesh5$age[dataesh5$inh>0], w=dataesh5$weight[dataesh5$inh>0])

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
  avgwealthmean1 <- weighted.mean (dataesh1$wealth, w=dataesh1$weight)
  avgwealthmean2 <- weighted.mean (dataesh2$wealth, w=dataesh2$weight)
  avgwealthmean3 <- weighted.mean (dataesh3$wealth, w=dataesh3$weight)
  avgwealthmean4 <- weighted.mean (dataesh4$wealth, w=dataesh4$weight)
  avgwealthmean5 <- weighted.mean (dataesh5$wealth, w=dataesh5$weight)

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
  avginhmean1 <- weighted.mean (dataesh1$inh[dataesh1$inh>0], w=dataesh1$weight[dataesh1$inh>0], na.rm = T)
  avginhmean2 <- weighted.mean (dataesh2$inh[dataesh2$inh>0], w=dataesh2$weight[dataesh2$inh>0], na.rm = T)
  avginhmean3 <- weighted.mean (dataesh3$inh[dataesh3$inh>0], w=dataesh3$weight[dataesh3$inh>0], na.rm = T)
  avginhmean4 <- weighted.mean (dataesh4$inh[dataesh4$inh>0], w=dataesh4$weight[dataesh4$inh>0], na.rm = T)
  avginhmean5 <- weighted.mean (dataesh5$inh[dataesh5$inh>0], w=dataesh5$weight[dataesh5$inh>0], na.rm = T)

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
  theillist <- calcGEI (dataesh1$wealth, w=dataesh1$weight, alpha = 0)
  theilwealth1 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataesh2$wealth, w=dataesh2$weight, alpha = 0)
  theilwealth2 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataesh3$wealth, w=dataesh3$weight, alpha = 0)
  theilwealth3 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataesh4$wealth, w=dataesh4$weight, alpha = 0)
  theilwealth4 <- as.numeric(theillist$ineq$index)
  theillist <- calcGEI (dataesh5$wealth, w=dataesh5$weight, alpha = 0)
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
  bottom50sh1 <- bottom50share (dataesh1$wealth, dataesh1$weight)
  bottom50sh2 <- bottom50share (dataesh2$wealth, dataesh2$weight)
  bottom50sh3 <- bottom50share (dataesh3$wealth, dataesh3$weight)
  bottom50sh4 <- bottom50share (dataesh4$wealth, dataesh4$weight)
  bottom50sh5 <- bottom50share (dataesh5$wealth, dataesh5$weight)

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
  top20sh1 <- top20share (dataesh1$wealth, dataesh1$weight)
  top20sh2 <- top20share (dataesh2$wealth, dataesh2$weight)
  top20sh3 <- top20share (dataesh3$wealth, dataesh3$weight)
  top20sh4 <- top20share (dataesh4$wealth, dataesh4$weight)
  top20sh5 <- top20share (dataesh5$wealth, dataesh5$weight)

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
  top10sh1 <- top10share (dataesh1$wealth, dataesh1$weight)
  top10sh2 <- top10share (dataesh2$wealth, dataesh2$weight)
  top10sh3 <- top10share (dataesh3$wealth, dataesh3$weight)
  top10sh4 <- top10share (dataesh4$wealth, dataesh4$weight)
  top10sh5 <- top10share (dataesh5$wealth, dataesh5$weight)

  t10m1 <- mean(top10sh1)
  t10m2 <- mean(top10sh2)
  t10m3 <- mean(top10sh3)
  t10m4 <- mean(top10sh4)
  t10m5 <- mean(top10sh5)

  t10mall <- c(t10m1, t10m2, t10m3, t10m4, t10m5)

  descrip <- rbind(obsall, shrepall, agemall, agermall, avgwealthmall, avginhmall, theilwmall, b50mall, t20mall, t10mall)

  write.csv (descrip, file = "results/spain/descriptives_spain.csv")


  #### descriptives categories / with final subsample and equivalised non-adjusted values

  #We can also check the inheritance quantiles

  q0 <- vector ()
  q1 <- vector ()
  q2 <- vector ()
  q3 <- vector ()


  q0 <- reldist::wtd.quantile (dataesh4$eqinh [dataesh4$eqinh > 0], q = 0, weight=dataesh4$weight[dataesh4$eqinh > 0], na.rm = T)
  q1 <- reldist::wtd.quantile (dataesh4$eqinh [dataesh4$eqinh > 0], q = 0.25, weight=dataesh4$weight[dataesh4$eqinh > 0], na.rm = T)
  q2 <- reldist::wtd.quantile (dataesh4$eqinh [dataesh4$eqinh > 0], q = 0.5, weight=dataesh4$weight[dataesh4$eqinh > 0], na.rm = T)
  q3 <- reldist::wtd.quantile (dataesh4$eqinh [dataesh4$eqinh > 0], q = 0.75, weight=dataesh4$weight[dataesh4$eqinh > 0], na.rm = T)

  q0m <- mean (q0)
  q1m <- mean (q1)
  q2m <- mean (q2)
  q3m <- mean (q3)

  #expecting inh
  #NA
  shareexpectingm <- NA

  #occupation

  table (dataesh4$occdadhead)

  shocclow <- vector ()
  shoccmidlow <- vector ()
  shoccmidhigh <- vector ()
  shocchigh <- vector ()

  shocclow <- sum(as.numeric((dataesh4$weight[dataesh4$occdadhead == 1 ])), na.rm = T)/sum(as.numeric(dataesh4$weight), na.rm = T)

  shoccmidlow<- sum(as.numeric((dataesh4$weight[dataesh4$occdadhead == 2 ])), na.rm = T)/sum(as.numeric(dataesh4$weight), na.rm = T)

  shoccmidhigh<- sum(as.numeric((dataesh4$weight[dataesh4$occdadhead == 3 ])), na.rm = T)/sum(as.numeric(dataesh4$weight), na.rm = T)

  shocchigh<- sum(as.numeric((dataesh4$weight[dataesh4$occdadhead == 4 ])), na.rm = T)/sum(as.numeric(dataesh4$weight), na.rm = T)

  shocclowm <- mean (shocclow)
  shoccmidlowm <- mean (shoccmidlow)
  shoccmidhighm <- mean (shoccmidhigh)
  shocchighm <- mean (shocchigh)

  #women

  table(dataesh4$sex)

  shwomen <- vector ()

  shwomen <- sum(as.numeric((dataesh4$weight[dataesh4$sex == 2])), na.rm = T)/sum(as.numeric(dataesh4$weight), na.rm = T)

  shwomenm <- mean(shwomen)

  descriptives_types_es <- c(q1m, q2m, q3m, shareexpectingm, shocclowm, shoccmidlowm, shoccmidhighm, shocchighm, shwomenm)

  write.csv (descriptives_types_es, file = "results/spain/descriptives_types_spain.csv")

  #shares with negative wealth now (among the 35-80 sample)


  shareng <- vector ()
  sharengw <- vector ()
  sharerecipientsposw <- vector ()
  sharerecipientsnegw <- vector ()
  sharerecipientsposwbottom50 <-vector ()
  sharelrecipientsposw <-vector ()
  sharelrecipientsnegw <- vector ()
  sharelrecipientsposwbottom50 <- vector ()

  shareneg <- NROW(dataesh2$weight[dataesh2$wealth <= 0])/NROW(dataesh2$weight)
  sharenegw <- sum(as.numeric(dataesh2$weight[dataesh2$wealth <= 0]))/sum(as.numeric(dataesh2$weight), na.rm = T)

  sharerecipientsposw <- sum(as.numeric((dataesh3$inhdummy[dataesh3$inhdummy == 1])*(dataesh3$weight[dataesh3$inhdummy == 1])))/sum(as.numeric(dataesh3$weight), na.rm = T)

  sharerecipientsnegw <- sum(as.numeric((dataesh2$inhdummy[dataesh2$inhdummy == 1 & dataesh2$wealth <=0])*(dataesh2$weight[dataesh2$inhdummy == 1 & dataesh2$wealth <=0])))/sum(as.numeric(dataesh2$weight[dataesh2$wealth <=0]), na.rm = T)

  sharerecipientsposwbottom50 <- sum(as.numeric((dataesh3$inhdummy[dataesh3$inhdummy == 1 & dataesh3$wealth < (reldist::wtd.quantile (dataesh3$wealth, q = 0.5, weight=dataesh3$weight, na.rm = T))])*(dataesh3$weight[dataesh3$inhdummy == 1 & dataesh3$wealth < (reldist::wtd.quantile (dataesh3$wealth, q = 0.5, weight=dataesh3$weight, na.rm = T))])))/sum(as.numeric(dataesh3$weight[dataesh3$wealth < (reldist::wtd.quantile (dataesh3$wealth, q = 0.5, weight=dataesh3$weight, na.rm = T))]), na.rm = T)

  #dummy for large inheritances
  dataesh2$linhdummy <- 0
  dataesh2$linhdummy[dataesh2$inh > q3m] <- 1

  dataesh3$linhdummy <- 0
  dataesh3$linhdummy[dataesh3$inh > q3m] <- 1

  sharelrecipientsposw <- sum(as.numeric((dataesh3$linhdummy[dataesh3$linhdummy == 1])*(dataesh3$weight[dataesh3$linhdummy == 1])))/sum(as.numeric(dataesh3$weight), na.rm = T)

  sharelrecipientsnegw <- sum(as.numeric((dataesh2$linhdummy[dataesh2$linhdummy == 1 & dataesh2$wealth <=0])*(dataesh2$weight[dataesh2$linhdummy == 1 & dataesh2$wealth <=0])))/sum(as.numeric(dataesh2$weight[dataesh2$wealth <=0]), na.rm = T)

  sharelrecipientsposwbottom50 <- sum(as.numeric((dataesh3$linhdummy[dataesh3$linhdummy == 1 & dataesh3$wealth < (reldist::wtd.quantile (dataesh3$wealth, q = 0.5, weight=dataesh3$weight, na.rm = T))])*(dataesh3$weight[dataesh3$linhdummy == 1 & dataesh3$wealth < (reldist::wtd.quantile (dataesh3$wealth, q = 0.5, weight=dataesh3$weight, na.rm = T))])))/sum(as.numeric(dataesh3$weight[dataesh3$wealth < (reldist::wtd.quantile (dataesh3$wealth, q = 0.5, weight=dataesh3$weight, na.rm = T))]), na.rm = T)

  sharengmean <- mean (shareneg)
  sharengwmean <- mean (sharenegw)
  sharerecipientsposwmean <- mean(sharerecipientsposw)
  sharerecipientsnegwmean <- mean(sharerecipientsnegw)
  sharerecipientsposwbottom50mean <-mean(sharerecipientsposwbottom50)
  sharelrecipientsposwmean <- mean(sharelrecipientsposw)
  sharelrecipientsnegwmean <- mean(sharelrecipientsnegw)
  sharelrecipientsposwbottom50mean <-mean(sharelrecipientsposwbottom50)

  neg_check_es <- c(sharengmean, sharengwmean, sharerecipientsposwmean, sharerecipientsposwbottom50mean, sharerecipientsnegwmean, sharelrecipientsposwmean, sharelrecipientsposwbottom50mean, sharelrecipientsnegwmean)

  write.csv (neg_check_es, file = "results/spain/negcheck_es.csv")

}














