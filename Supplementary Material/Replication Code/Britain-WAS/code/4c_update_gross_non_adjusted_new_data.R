#we update the gross non-adjusted values

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)


alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step4b-gross-updated-adjusted-new-data.rds")



####################### UPDATING OF INHERITANCES TO TODAY'S (2012, THIRD WAVE) MONEY #########################################


#HOWEVER, WE SHOULD UPDATE THE VALUE OF INHERITANCES RECEIVED BEFORE 5 YEARS AGO TO 2012 LEVELS - UPDATING UP TO WAVE 3

#LET'S DOWNLOAD THE CONSUMER PRICE INDEX FOR THAT
#cpi = read.csv("cpi-UK-hist.csv", sep = ";", header = T, as.is = T)
#WE DO IT FOR THE 2012 WAVE, THE ONE WE HAVE INFORMATION ABOUT
cpi = read.csv("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/aux_files/cpi-UK-2012.csv", sep = ";", header = T, as.is = T)
#NOTE BEFORE 1949 WE UPDATE BY THE SAME MULTIPLIER, THE DATA DO NOT GO FURTHER BACK

cpi <- cpi[rowSums(is.na(cpi)) != ncol(cpi),]


summary(alldatawas$IEValW1)
summary(alldatawas$pretaxinhlifeW1)

#THIS IS THE YEAR THE INHERITANCE WAS OBTAINED

summary(alldatawas$IEYrW1[alldatawas$IEYrW1>0])
table(alldatawas$IEYrW1[alldatawas$IEYrW1>0])

table(alldatawas$pretaxinhlifeW1[alldatawas$pretaxinhlifeW1>100000], alldatawas$IEYrW1[alldatawas$pretaxinhlifeW1>100000])


alldatawas$pretaxinhlifeW1upd <- alldatawas$pretaxinhlifeW1

for (i in 1940:2012) {
  alldatawas$pretaxinhlifeW1upd <- ifelse (alldatawas$IEYrW1==i & alldatawas$pretaxinhlifeW1 >0, alldatawas$pretaxinhlifeW1*(cpi$multiplier[cpi$year==i]), (alldatawas$pretaxinhlifeW1upd))
}

cpi$multiplier[cpi$year==1957]

#We check that the formula to update the values has worked out correctly...
alldatawas$pretaxinhlifeW1[alldatawas$IEYrW1==1957] #There's only one observation for 1957
alldatawas$pretaxinhlifeW1upd[alldatawas$IEYrW1==1957] #It is correctly multiplied by 20.53

#We compare the values before and after updating
summary(alldatawas$pretaxinhlifeW1 [alldatawas$pretaxinhlifeW1>0]) #What are the statistics of that inheritance
summary(alldatawas$pretaxinhlifeW1upd [alldatawas$pretaxinhlifeW1upd>0]) #What are the statistics of that inheritance

#WE DO THE UPDATING ALSO FOR THE BRACKET ASSIGNED VALUE FOR THIS INHERITANCE

table(alldatawas$pretaxinhlifebW1, alldatawas$IEYrW1)

alldatawas$pretaxinhlifebW1upd <- alldatawas$pretaxinhlifebW1
for (i in 1940:2012) {
  alldatawas$pretaxinhlifebW1upd <- ifelse (alldatawas$IEYrW1==i & alldatawas$pretaxinhlifebW1 >0, alldatawas$pretaxinhlifebW1*(cpi$multiplier[cpi$year==i]), (alldatawas$pretaxinhlifebW1upd))
}

#We check that the formula to update the values has worked out correctly...
vv <- alldatawas$pretaxinhlifebW1[alldatawas$IEYrW1==1956]
vvv <- alldatawas$pretaxinhlifebW1upd[alldatawas$IEYrW1==1956] #It is correctly multiplied by 21,54
vvv/vv

#We compare the values before and after updating
summary(alldatawas$pretaxinhlifebW1 [alldatawas$pretaxinhlifebW1>0]) #What are the statistics of that inheritance
summary(alldatawas$pretaxinhlifebW1upd [alldatawas$pretaxinhlifebW1upd>0]) #What are the statistics of that inheritance
table (alldatawas$pretaxinhlifebW1upd, alldatawas$IEYrW1)


#WE DO UPDATING FOR THE SECOND REPORTED INHERITANCE

alldatawas$pretaxinhlife2W1upd <- alldatawas$pretaxinhlife2W1
for (i in 1940:2012) {
  alldatawas$pretaxinhlife2W1upd <- ifelse (alldatawas$IEYr2W1==i & alldatawas$pretaxinhlife2W1 >0,alldatawas$pretaxinhlife2W1*(cpi$multiplier[cpi$year==i]), (alldatawas$pretaxinhlife2W1upd))
}

#We check that the formula to update the values has worked out correctly...
zz <- alldatawas$pretaxinhlife2W1[alldatawas$IEYr2W1==1972]
zzz <- alldatawas$pretaxinhlife2W1upd[alldatawas$IEYr2W1==1972] #It is correctly multiplied by 11.61
zzz/zz

#We compare the values before and after updating
summary(alldatawas$pretaxinhlife2W1 [alldatawas$pretaxinhlife2W1>0]) #What are the statistics of that inheritance
summary(alldatawas$pretaxinhlife2W1upd [alldatawas$pretaxinhlife2W1upd>0]) #What are the statistics of that inheritance

table (alldatawas$pretaxinhlife2W1, alldatawas$IEYr2W1)

#WE DO THE UPDATING ALSO FOR THE BRACKET ASSIGNED VALUE FOR THIS SECOND INHERITANCE

alldatawas$pretaxinhlife2bW1upd <- alldatawas$pretaxinhlife2bW1
for (i in 1940:2012) {
  alldatawas$pretaxinhlife2bW1upd <- ifelse (alldatawas$IEYr2W1==i & alldatawas$pretaxinhlife2bW1 >0, alldatawas$pretaxinhlife2bW1*(cpi$multiplier[cpi$year==i]), (alldatawas$pretaxinhlife2bW1upd))
}

#We check that the formula to update the values has worked out correctly...
vv <- alldatawas$pretaxinhlife2bW1[alldatawas$IEYr2W1==1988]
vvv <- alldatawas$pretaxinhlife2bW1upd[alldatawas$IEYr2W1==1988] #It is correctly multiplied by 2.31
vvv/vv

#We compare the values before and after updating
summary(alldatawas$pretaxinhlife2bW1 [alldatawas$pretaxinhlife2bW1>0]) #What are the statistics of that inheritance
summary(alldatawas$pretaxinhlife2bW1upd [alldatawas$pretaxinhlife2bW1upd>0]) #What are the statistics of that inheritance
table (alldatawas$pretaxinhlife2bW1, alldatawas$IEYr2W1)


#WE DO UPDATING FOR THE THIRD REPORTED INHERITANCE

alldatawas$pretaxinhlife3W1upd <- alldatawas$pretaxinhlife3W1
for (i in 1940:2012) {
  alldatawas$pretaxinhlife3W1upd <- ifelse (alldatawas$IEYr3W1==i & alldatawas$pretaxinhlife3W1 >0,alldatawas$pretaxinhlife3W1*(cpi$multiplier[cpi$year==i]), (alldatawas$pretaxinhlife3W1upd))
}

#We compare the values before and after updating
summary(alldatawas$pretaxinhlife3W1 [alldatawas$pretaxinhlife3W1>0]) #What are the statistics of that inheritance
summary(alldatawas$pretaxinhlife3W1upd [alldatawas$pretaxinhlife3W1upd>0]) #What are the statistics of that inheritance

#WE DO THE UPDATING ALSO FOR THE BRACKET ASSIGNED VALUE FOR THIS THIRD INHERITANCE

alldatawas$pretaxinhlife3bW1upd <- alldatawas$pretaxinhlife3bW1
for (i in 1940:2012) {
  alldatawas$pretaxinhlife3bW1upd <- ifelse (alldatawas$IEYr3W1==i & alldatawas$pretaxinhlife3bW1 >0, alldatawas$pretaxinhlife3bW1*(cpi$multiplier[cpi$year==i]), (alldatawas$pretaxinhlife3bW1upd))
}

#We check that the formula to update the values has worked out correctly...
vv <- alldatawas$pretaxinhlife3bW1[alldatawas$IEYr3W1==1988]
vvv <- alldatawas$pretaxinhlife3bW1upd[alldatawas$IEYr3W1==1988] #It is correctly multiplied by 24,97
vvv/vv

#We compare the values before and after updating
summary(alldatawas$pretaxinhlife3bW1 [alldatawas$pretaxinhlife3bW1>0]) #What are the statistics of that inheritance
summary(alldatawas$pretaxinhlife3bW1upd [alldatawas$pretaxinhlife3bW1upd>0]) #What are the statistics of that inheritance
table (alldatawas$pretaxinhlife3bW1, alldatawas$IEYr3W1)



###### AND WE SAVE THE DATA #######

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step4c-gross-updated-new-data.rds")
