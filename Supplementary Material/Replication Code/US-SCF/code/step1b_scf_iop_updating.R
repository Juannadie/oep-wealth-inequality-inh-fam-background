#STEP 1B - UPDATING AND INH - GIFT DISENTANGLING

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory for the LAPTOP

options ("scipen"=100, "digits"=6)


alldatascf <- readRDS(file = "datasets/SCF-2016-all.rds") #LOAD THE DATA FROM 1A


#INHERITANCES AMOUNT

#One first thing that we could see is how many imputations are there in the inheritance amount. And now we can see the actual inheritance amount

summary(alldatascf$X5804)
alldatascf$value1inh <- alldatascf$X5804 #First inheritance

summary(alldatascf$X5809)
alldatascf$value2inh <- alldatascf$X5809 #Second inheritance

summary(alldatascf$X5814)
alldatascf$value3inh <- alldatascf$X5814 #Third inheritance

summary(alldatascf$X5818)
alldatascf$value3inh <- alldatascf$X5818 #Fourth inheritance


#NEXT STEP IS TO UPDATE THE INHERITANCES TO PRESENT VALUE

#####################

#WE DO IT FOR THE 2016 WAVE, THE ONE WE HAVE INFORMATION ABOUT, INFO OBTAINED FROM

#https://beta.bls.gov/dataViewer/view/timeseries/CUUR0000SA0
#https://beta.bls.gov/dataQuery/find?fq=survey:[cu]&s=popularity:D

cpi = read.csv("aux/cpi-US-2016.csv", sep = ";", header = T, as.is = T)

cpi$year <- as.numeric(cpi$year)

#THIS IS THE YEAR THE INHERITANCE WAS OBTAINED

summary(alldatascf$X5805[alldatascf$value1inh>0])
table(alldatascf$X5805[alldatascf$value1inh>0])
alldatascf$yearinh1 <- alldatascf$X5805

alldatascf$value1inhupd <- alldatascf$value1inh

#This is the right way to update the amounts...

for (i in 1940:2016) {
  alldatascf$value1inhupd <- ifelse (alldatascf$yearinh1==i & alldatascf$value1inh >0, alldatascf$value1inh*(cpi$multiplier[cpi$year==i]), (alldatascf$value1inhupd))
}

summary (alldatascf$value1inh)
summary (alldatascf$value1inhupd)

#We check that the formula to update the values has worked out correctly...
mean55 <- mean(alldatascf$value1inh[alldatascf$yearinh1==1955])
meanupd55 <- mean(alldatascf$value1inhupd[alldatascf$yearinh1==1955])
multiplier55<- meanupd55/mean55 #Just as expected
multiplier55

mean2000 <- mean(alldatascf$value1inh[alldatascf$yearinh1==2000])
meanupd2000 <- mean(alldatascf$value1inhupd[alldatascf$yearinh1==2000])
multiplier2000 <- meanupd2000/mean2000 #Just as expected


#NOW WE PROCEED WITH THE SECOND INHERITANCE

summary(alldatascf$X5810[alldatascf$value2inh>0])
table(alldatascf$X5810[alldatascf$value2inh>0])
alldatascf$yearinh2 <- alldatascf$X5810

alldatascf$value2inhupd <- alldatascf$value2inh

#This is the right way to update the amounts...

for (i in 1940:2016) {
  alldatascf$value2inhupd <- ifelse (alldatascf$yearinh2==i & alldatascf$value2inh >0,alldatascf$value2inh*(cpi$multiplier[cpi$year==i]), (alldatascf$value2inhupd))
}

summary (alldatascf$value2inh)
summary (alldatascf$value2inhupd)

#We check that the formula to update the values has worked out correctly...
mean55 <- mean(alldatascf$value2inh[alldatascf$yearinh2==1955])
meanupd55 <- mean(alldatascf$value2inhupd[alldatascf$yearinh2==1955])
multiplier55 <- meanupd55/mean55 #Just as expected
multiplier55

#We check that the formula to update the values has worked out correctly...
table (alldatascf$value2inh[alldatascf$yearinh2==1955])
table (alldatascf$value2inhupd[alldatascf$yearinh2==1955])

mean2000 <- mean(alldatascf$value2inh[alldatascf$yearinh2==2000])
meanupd2000 <- mean(alldatascf$value2inhupd[alldatascf$yearinh2==2000])
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000

#NOW WE PROCEED WITH THE THIRD INHERITANCE

summary(alldatascf$X5815[alldatascf$value3inh>0])
table(alldatascf$X5815[alldatascf$value3inh>0])
alldatascf$yearinh3 <- alldatascf$X5815

alldatascf$value3inhupd <- alldatascf$value3inh

#This is the right way to update the amounts...

for (i in 1940:2016) {
  alldatascf$value3inhupd <- ifelse (alldatascf$yearinh3==i & alldatascf$value3inh >0,alldatascf$value3inh*(cpi$multiplier[cpi$year==i]), (alldatascf$value3inhupd))
}

summary (alldatascf$value3inh)
summary (alldatascf$value3inhupd)

#Now we have to add all the inheritances. The remaining inheritances reported do not have information about the year, we do not update the value (conservative measure).

yearinhmedian <- median(alldatascf$yearinh1[alldatascf$yearinh1>0]) #Median is 2005
alldatascf$value4inhupd <-  alldatascf$X5818*(cpi$multiplier[cpi$year==yearinhmedian])

summary (alldatascf$X5818)
summary (alldatascf$value4inhupd)

#And we can obtain the updated value of inheritances aggregated

alldatascf$updinh <- alldatascf$value1inhupd + alldatascf$value2inhupd + alldatascf$value3inhupd + alldatascf$value4inhupd

alldatascf$inh <- alldatascf$updinh

totalweightedinh <- sum(alldatascf$inh*alldatascf$X42001)


#But we can only isolate the share of inheritances that were only a transfer or gift

summary (alldatascf$value1inhupd[alldatascf$value1inhupd>0])
summary (alldatascf$value1inhupd[alldatascf$X5803 == 3]) #That is a gift or a trust

NROW(alldatascf$value1inhupd[alldatascf$value1inhupd>0])
NROW (alldatascf$value1inhupd[alldatascf$X5803 == 3])

summary (alldatascf$value2inhupd[alldatascf$value2inhupd>0])
summary (alldatascf$value2inhupd[alldatascf$X5808 == 3]) #That is a gift or a trust

NROW(alldatascf$value2inhupd[alldatascf$value2inhupd>0])
NROW (alldatascf$value2inhupd[alldatascf$X5808 == 3])

summary (alldatascf$value3inhupd[alldatascf$value3inhupd>0])
summary (alldatascf$value3inhupd[alldatascf$X5813 == 3]) #That is a gift or a trust

NROW(alldatascf$value3inhupd[alldatascf$value3inhupd>0])
NROW (alldatascf$value3inhupd[alldatascf$X5813 == 3])

alldatascf$valuegifts1 <- 0
alldatascf$valuegifts1[alldatascf$X5803 == 3] <- alldatascf$value1inhupd[alldatascf$X5803 == 3]
alldatascf$valuegifts2 <- 0
alldatascf$valuegifts2[alldatascf$X5808 == 3] <- alldatascf$value2inhupd[alldatascf$X5808 == 3]
alldatascf$valuegifts3 <- 0
alldatascf$valuegifts3[alldatascf$X5813 == 3] <- alldatascf$value3inhupd[alldatascf$X5813 == 3]

alldatascf$valuegifts <- alldatascf$valuegifts1 + alldatascf$valuegifts2 + alldatascf$valuegifts3

summary(alldatascf$valuegifts)
summary(alldatascf$inh)

alldatascf$giftonly <- alldatascf$valuegifts


#But we can only isolate the share of inheritances that were only inheritances

summary (alldatascf$value1inhupd[alldatascf$value1inhupd>0])
summary (alldatascf$value1inhupd[alldatascf$X5803 == 1]) #That is inheritance or inherited trust

NROW(alldatascf$value1inhupd[alldatascf$value1inhupd>0])
NROW (alldatascf$value1inhupd[alldatascf$X5803 == 1])

summary (alldatascf$value2inhupd[alldatascf$value2inhupd>0])
summary (alldatascf$value2inhupd[alldatascf$X5808 == 1]) #That is a inheritance or a inherited trust

NROW(alldatascf$value2inhupd[alldatascf$value2inhupd>0])
NROW (alldatascf$value2inhupd[alldatascf$X5808 == 1])

summary (alldatascf$value3inhupd[alldatascf$value3inhupd>0])
summary (alldatascf$value3inhupd[alldatascf$X5813 == 1]) #That is a inheritance or inherited trust

NROW(alldatascf$value3inhupd[alldatascf$value3inhupd>0])
NROW (alldatascf$value3inhupd[alldatascf$X5813 == 1])

alldatascf$valueinhonly1 <- 0
alldatascf$valueinhonly1[alldatascf$X5803 == 1] <- alldatascf$value1inhupd[alldatascf$X5803 == 1]
alldatascf$valueinhonly2 <- 0
alldatascf$valueinhonly2[alldatascf$X5808 == 1] <- alldatascf$value2inhupd[alldatascf$X5808 == 1]
alldatascf$valueinhonly3 <- 0
alldatascf$valueinhonly3[alldatascf$X5813 == 1] <- alldatascf$value3inhupd[alldatascf$X5813 == 1]

alldatascf$valueinhonly <- alldatascf$valueinhonly1 + alldatascf$valueinhonly2 + alldatascf$valueinhonly3 + alldatascf$value4inhupd

summary(alldatascf$valueinhonly)

alldatascf$inhonly <- alldatascf$valueinhonly

summary (alldatascf$inh)

### NOW WE CAN ADD INHERITANCES EXCLUDING TRUSTS ####

alldatascf$inhnotrust <- alldatascf$inhonly + alldatascf$giftonly
summary(alldatascf$inhnotrust)


#But we can only isolate the share of inheritances that were only given trusts

summary (alldatascf$value1inhupd[alldatascf$value1inhupd>0])
summary (alldatascf$value1inhupd[alldatascf$X5803 == 2]) #That is inheritance or inherited trust

NROW(alldatascf$value1inhupd[alldatascf$value1inhupd>0])
NROW (alldatascf$value1inhupd[alldatascf$X5803 == 2])

summary (alldatascf$value2inhupd[alldatascf$value2inhupd>0])
summary (alldatascf$value2inhupd[alldatascf$X5808 == 2]) #That is a inheritance or a inherited trust

NROW(alldatascf$value2inhupd[alldatascf$value2inhupd>0])
NROW (alldatascf$value2inhupd[alldatascf$X5808 == 2])

summary (alldatascf$value3inhupd[alldatascf$value3inhupd>0])
summary (alldatascf$value3inhupd[alldatascf$X5813 == 2]) #That is a inheritance or inherited trust

NROW(alldatascf$value3inhupd[alldatascf$value3inhupd>0])
NROW (alldatascf$value3inhupd[alldatascf$X5813 == 2])

alldatascf$valueinhtrustonly1 <- 0
alldatascf$valueinhtrustonly1[alldatascf$X5803 == 2] <- alldatascf$value1inhupd[alldatascf$X5803 == 2]
alldatascf$valueinhtrustonly2 <- 0
alldatascf$valueinhtrustonly2[alldatascf$X5808 == 2] <- alldatascf$value2inhupd[alldatascf$X5808 == 2]
alldatascf$valueinhtrustonly3 <- 0
alldatascf$valueinhtrustonly3[alldatascf$X5813 == 2] <- alldatascf$value3inhupd[alldatascf$X5813 == 2]

alldatascf$valueinhtrustonly <- alldatascf$valueinhtrustonly1 + alldatascf$valueinhtrustonly2 + alldatascf$valueinhtrustonly3

summary(alldatascf$valueinhtrustonly)

alldatascf$inhtrustonly <- alldatascf$valueinhtrustonly

summary (alldatascf$inh)

### NOW WE CAN ADD INHERITANCES INCLUDING TRUSTS ####

alldatascf$inhtotal <- alldatascf$inhonly + alldatascf$giftonly + alldatascf$inhtrustonly
summary(alldatascf$inhtotal)

summary(alldatascf$inhtotal)
summary(alldatascf$inh)


####


saveRDS(alldatascf, file = "datasets/SCF-2016-all-after-1b.rds")

#####


