#we update the net values (no grossing, to have a version of that option as well)
#We used an a version

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)



alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step3b-bis-pre-tax-new-data.rds")

####################### UPDATING OF INHERITANCES TO TODAY'S (2012, THIRD WAVE) MONEY #########################################


#HOWEVER, WE SHOULD UPDATE THE VALUE OF INHERITANCES RECEIVED BEFORE 5 YEARS AGO TO 2012 LEVELS - UPDATING UP TO WAVE 3


#WE DO IT FOR THE 2012 WAVE, THE ONE WE HAVE INFORMATION ABOUT
cpi = read.csv("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/aux_files/cpi-UK-2012.csv", sep = ";", header = T, as.is = T)
#NOTE BEFORE 1949 WE UPDATE BY THE SAME MULTIPLIER, THE DATA DO NOT GO FURTHER BACK

cpi <- cpi[rowSums(is.na(cpi)) != ncol(cpi),]


#THIS IS THE YEAR THE INHERITANCE WAS OBTAINED

summary(alldatawas$IEYrW1[alldatawas$IEYrW1>0])
table(alldatawas$IEYrW1[alldatawas$IEYrW1>0])

table(alldatawas$IEValW1[alldatawas$IEValW1>100000], alldatawas$IEYrW1[alldatawas$IEValW1>100000])


alldatawas$IEValW1upd <- alldatawas$IEValW1

for (i in 1940:2012) {
  alldatawas$IEValW1upd <- ifelse (alldatawas$IEYrW1==i & alldatawas$IEValW1 >0, alldatawas$IEValW1*(cpi$multiplier[cpi$year==i]), (alldatawas$IEValW1upd))
}

cpi$multiplier[cpi$year==1957]

#We check that the formula to update the values has worked out correctly...
alldatawas$IEValW1[alldatawas$IEYrW1==1957] #There's only one observation for 1957
alldatawas$IEValW1upd[alldatawas$IEYrW1==1957] #It is correctly multiplied by 20.53

#We compare the values before and after updating
summary(alldatawas$IEValW1 [alldatawas$IEValW1>0]) #What are the statistics of that inheritance
summary(alldatawas$IEValW1upd [alldatawas$IEValW1upd>0]) #What are the statistics of that inheritance

#WE DO THE UPDATING ALSO FOR THE BRACKET ASSIGNED VALUE FOR THIS INHERITANCE

table(alldatawas$bracked1inhW1past, alldatawas$IEYrW1)

alldatawas$bracked1inhW1pastupd <- alldatawas$bracked1inhW1past
for (i in 1940:2012) {
  alldatawas$bracked1inhW1pastupd <- ifelse (alldatawas$IEYrW1==i & alldatawas$bracked1inhW1past >0, alldatawas$bracked1inhW1past*(cpi$multiplier[cpi$year==i]), (alldatawas$bracked1inhW1pastupd))
}

#We check that the formula to update the values has worked out correctly...
vv <- alldatawas$bracked1inhW1past[alldatawas$IEYrW1==1956]
vvv <- alldatawas$bracked1inhW1pastupd[alldatawas$IEYrW1==1956] #It is correctly multiplied by 21,54
vvv/vv

#We compare the values before and after updating
summary(alldatawas$bracked1inhW1past [alldatawas$bracked1inhW1past>0]) #What are the statistics of that inheritance
summary(alldatawas$bracked1inhW1pastupd [alldatawas$bracked1inhW1pastupd>0]) #What are the statistics of that inheritance
table (alldatawas$bracked1inhW1pastupd, alldatawas$IEYrW1)


#WE DO UPDATING FOR THE SECOND REPORTED INHERITANCE

alldatawas$IEVal2W1upd <- alldatawas$IEVal2W1
for (i in 1940:2012) {
  alldatawas$IEVal2W1upd <- ifelse (alldatawas$IEYr2W1==i & alldatawas$IEVal2W1 >0,alldatawas$IEVal2W1*(cpi$multiplier[cpi$year==i]), (alldatawas$IEVal2W1upd))
}

#We check that the formula to update the values has worked out correctly...
zz <- alldatawas$IEVal2W1[alldatawas$IEYr2W1==1972]
zzz <- alldatawas$IEVal2W1upd[alldatawas$IEYr2W1==1972] #It is correctly multiplied by 11.61
zzz/zz

table (alldatawas$IEVal2W1, alldatawas$IEYr2W1)

#WE DO THE UPDATING ALSO FOR THE BRACKET ASSIGNED VALUE FOR THIS SECOND INHERITANCE

alldatawas$bracked2inhW1pastupd <- alldatawas$bracked2inhW1past
for (i in 1940:2012) {
  alldatawas$bracked2inhW1pastupd <- ifelse (alldatawas$IEYr2W1==i & alldatawas$bracked2inhW1past >0, alldatawas$bracked2inhW1past*(cpi$multiplier[cpi$year==i]), (alldatawas$bracked2inhW1pastupd))
}

#We check that the formula to update the values has worked out correctly...
vv <- alldatawas$bracked2inhW1past[alldatawas$IEYr2W1==1988]
vvv <- alldatawas$bracked2inhW1pastupd[alldatawas$IEYr2W1==1988] #It is correctly multiplied by 2.31
vvv/vv

#We compare the values before and after updating
summary(alldatawas$bracked2inhW1past [alldatawas$bracked2inhW1past>0]) #What are the statistics of that inheritance
summary(alldatawas$bracked2inhW1pastupd [alldatawas$bracked2inhW1pastupd>0]) #What are the statistics of that inheritance
table (alldatawas$bracked2inhW1past, alldatawas$IEYr2W1)


#WE DO UPDATING FOR THE THIRD REPORTED INHERITANCE

alldatawas$IEVal3W1upd <- alldatawas$IEVal3W1
for (i in 1940:2012) {
  alldatawas$IEVal3W1upd <- ifelse (alldatawas$IEYr3W1==i & alldatawas$IEVal3W1 >0,alldatawas$IEVal3W1*(cpi$multiplier[cpi$year==i]), (alldatawas$IEVal3W1upd))
}

#We compare the values before and after updating
summary(alldatawas$IEVal3W1 [alldatawas$IEVal3W1>0]) #What are the statistics of that inheritance
summary(alldatawas$IEVal3W1upd [alldatawas$IEVal3W1upd>0]) #What are the statistics of that inheritance

#WE DO THE UPDATING ALSO FOR THE BRACKET ASSIGNED VALUE FOR THIS THIRD INHERITANCE

alldatawas$bracked3inhW1pastupd <- alldatawas$bracked3inhW1past
for (i in 1940:2012) {
  alldatawas$bracked3inhW1pastupd <- ifelse (alldatawas$IEYr3W1==i & alldatawas$bracked3inhW1past >0, alldatawas$bracked3inhW1past*(cpi$multiplier[cpi$year==i]), (alldatawas$bracked3inhW1pastupd))
}

#We check that the formula to update the values has worked out correctly...
vv <- alldatawas$bracked3inhW1past[alldatawas$IEYr3W1==1988]
vvv <- alldatawas$bracked3inhW1pastupd[alldatawas$IEYr3W1==1988] #It is correctly multiplied by 24,97
vvv/vv

#We compare the values before and after updating
summary(alldatawas$bracked3inhW1past [alldatawas$bracked3inhW1past>0]) #What are the statistics of that inheritance
summary(alldatawas$bracked3inhW1pastupd [alldatawas$bracked3inhW1pastupd>0]) #What are the statistics of that inheritance
table (alldatawas$bracked3inhW1past, alldatawas$IEYr3W1)

#### SAVE THE DATA ####

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step4a-Update-Net-new-data.rds")
