#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

#We can read the data
#data1full <- readRDS(file = "datasets/HFCS14_input1_full.rds") ##
data2full <- readRDS(file = "datasets/HFCS14_input2_full.rds") ##
#data3full <- readRDS(file = "HFCS14_input3_full.rds") ##
#data4full <- readRDS(file = "HFCS14_input4_full.rds") ##
#data5full <- readRDS(file = "HFCS14_input5_full.rds") ##

#We could also read this if we wanted to track the imputations, comparing them in the dataset that has both variables.
#data1.2full <- readRDS(file = "HFCS14_inputs1_2_full.rds") ## TO AVOID DOING THE JOINING AGAIN


#AND NOW WE FILTER FOR FRANCE

datafr <- data2full[data2full$SA0100 == 'FR',] #

### NOW WE HAVE TO UPDATE THE INHERITANCES VALUE BEFORE WE GO ON WITH THE ANALYSIS ###

cpi = read.csv("cpi/cpi-FR-2014.csv", sep = ";", header = T, as.is = T)

cpi$year <- as.numeric(cpi$year)

#Before updating we add their value

datafr$value1inh <- datafr$HH0401 #First inheritance
datafr$value2inh <- datafr$HH0402 #Second inheritance
datafr$value3inh <- datafr$HH0403 #Third inheritance

#Let's see the years the inheritances were received
table(datafr$HH0201) #For the first inheritance
table(datafr$HH0202) #For the second inheritance
table(datafr$HH0203) #For the third inheritance

datafr$yearinh1 <- datafr$HH0201
datafr$yearinh2 <- datafr$HH0202
datafr$yearinh3 <- datafr$HH0203

#We do not have indices to update before 1935

datafr$yearinh1 [datafr$yearinh1 < 1935] <- 1935

datafr$value1inhupd <- datafr$value1inh

#This is the right way to update the amounts...

for (i in 1935:2014) {
  datafr$value1inhupd <- ifelse (datafr$yearinh1==i & datafr$value1inh >0,datafr$value1inh*(cpi$multiplier[cpi$year==i]), (datafr$value1inhupd))
}

summary (datafr$value1inh)
summary (datafr$value1inhupd)

#We check that the formula to update the values has worked out correctly...
mean56 <- mean(datafr$value1inh[datafr$yearinh1==1956], na.rm = T)
meanupd56 <- mean(datafr$value1inhupd[datafr$yearinh1==1956], na.rm = T)
multiplier56 <- meanupd56/mean56
multiplier56 #Just as expected

mean2000 <- mean(datafr$value1inh[datafr$yearinh1==2000], na.rm = T)
meanupd2000 <- mean(datafr$value1inhupd[datafr$yearinh1==2000], na.rm = T)
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000

#NOW WE PROCEED WITH THE SECOND INHERITANCE

#We do not have indices to update before 1956
datafr$yearinh2 [datafr$yearinh2 < 1935] <- 1935

datafr$value2inhupd <- datafr$value2inh

#This is the right way to update the amounts...

for (i in 1935:2014) {
  datafr$value2inhupd <- ifelse (datafr$yearinh2==i & datafr$value2inh >0,datafr$value2inh*(cpi$multiplier[cpi$year==i]), (datafr$value2inhupd))
}

summary (datafr$value2inh)
summary (datafr$value2inhupd)

#We check that the formula to update the values has worked out correctly...
mean56 <- mean(datafr$value2inh[datafr$yearinh2==1956], na.rm = T)
meanupd56 <- mean(datafr$value2inhupd[datafr$yearinh2==1956], na.rm = T)
multiplier56 <- meanupd56/mean56 #Just as expected
multiplier56

mean2000 <- mean(datafr$value2inh[datafr$yearinh2==2000], na.rm = T)
meanupd2000 <- mean(datafr$value2inhupd[datafr$yearinh2==2000], na.rm = T)
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000

#NOW WE PROCEED WITH THE THIRD INHERITANCE

#We do not have indices to update before 1956
datafr$yearinh3 [datafr$yearinh3 < 1935] <- 1935

datafr$value3inhupd <- datafr$value3inh

#This is the right way to update the amounts...

for (i in 1935:2014) {datafr$value3inhupd <- ifelse (datafr$yearinh3==i & datafr$value3inh >0,datafr$value3inh*(cpi$multiplier[cpi$year==i]), (datafr$value3inhupd))}

summary (datafr$value3inh)
summary (datafr$value3inhupd)

#We check that the formula to update the values has worked out correctly...
mean56 <- mean(datafr$value3inh[datafr$yearinh3==1957], na.rm = T)
meanupd56 <- mean(datafr$value3inhupd[datafr$yearinh3==1957], na.rm = T)
multiplier56 <- meanupd56/mean56 #Just as expected
multiplier56

mean2000 <- mean(datafr$value3inh[datafr$yearinh3==2000], na.rm = T)
meanupd2000 <- mean(datafr$value3inhupd[datafr$yearinh3==2000], na.rm = T)
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000


#AGGREGATION OF INHERITANCES

#And we can obtain the value of inheritances aggregated
#datafr$inh.1 <- datafr$value1inh + datafr$value2inh + datafr$value3inh
#Better this way to not let NAs spoil the sum
datafr$inh <- rowSums(datafr[,c("value1inh","value2inh","value3inh")], na.rm=TRUE)

#And we can obtain the updated value of inheritances aggregated
datafr$inhupd <- rowSums(datafr[,c("value1inhupd","value2inhupd","value3inhupd")], na.rm=TRUE)

summary(datafr$inh[datafr$inh>0])
summary(datafr$inhupd[datafr$inhupd>0])

#We can now differenciate between gifts and inheritances with variable HH050x

NROW (datafr$HH0401[!is.na(datafr$HH0401) & datafr$HH0401>0])

summary (datafr$HH0501[datafr$value1inhupd>0])

summary (datafr$HH0501)
table (datafr$HH0502)
table (datafr$HH0503)

#NOW WE ADD ONLY THE VALUES WHICH ARE INHERITANCES - ALSO ASSUMING NA AS INHERITANCES

datafr$value1inhonlyupd <- 0
datafr$value1inhonlyupd[datafr$HH0501 == 2 | (is.na(datafr$HH0501))] <- datafr$value1inhupd[datafr$HH0501 == 2 | (is.na(datafr$HH0501))]
summary(datafr$value1inhonlyupd)
NROW(datafr$value1inhonlyupd[datafr$value1inhonlyupd>0])

datafr$value2inhonlyupd <- 0
datafr$value2inhonlyupd[datafr$HH0502 == 2 | (is.na(datafr$HH0502))] <- datafr$value2inhupd [datafr$HH0502 == 2 | (is.na(datafr$HH0502))]
summary(datafr$value2inhonlyupd)

datafr$value3inhonlyupd <- 0
datafr$value3inhonlyupd[datafr$HH0503 == 2 | (is.na(datafr$HH0503))] <- datafr$value3inhupd [datafr$HH0503 == 2 | (is.na(datafr$HH0503))]
summary(datafr$value3inhonlyupd)

#And we can obtain the updated value of inheritances aggregated
datafr$inhonlyupd <- rowSums(datafr[,c("value1inhonlyupd","value2inhonlyupd","value3inhonlyupd")], na.rm=TRUE)


#AND NOW THE SAME WITH GIFTS

datafr$value1giftonlyupd <- 0
datafr$value1giftonlyupd[datafr$HH0501 == 1 & (!is.na(datafr$HH0501))] <- datafr$value1inhupd[datafr$HH0501 == 1 & (!is.na(datafr$HH0501))]
summary(datafr$value1giftonlyupd)

datafr$value2giftonlyupd <- 0
datafr$value2giftonlyupd[datafr$HH0502 == 1 & (!is.na(datafr$HH0502))] <- datafr$value2inhupd [datafr$HH0502 == 1 & (!is.na(datafr$HH0502))]
summary(datafr$value2giftonlyupd)

datafr$value3giftonlyupd <- 0
datafr$value3giftonlyupd[datafr$HH0503 == 1 & (!is.na(datafr$HH0503))] <- datafr$value3inhupd [datafr$HH0503 == 1 & (!is.na(datafr$HH0503))]
summary(datafr$value3giftonlyupd)

#And we can obtain the updated value of inheritances aggregated
datafr$giftonlyupd <- rowSums(datafr[,c("value1giftonlyupd","value2giftonlyupd","value3giftonlyupd")], na.rm=TRUE)


summary(datafr$inh[datafr$inh>0])
summary(datafr$inhupd[datafr$inhupd>0])
summary(datafr$inhonlyupd[datafr$inhonlyupd>0])
summary(datafr$giftonlyupd[datafr$giftonlyupd>0])

NROW(datafr$inh[datafr$inh>0])
NROW(datafr$inhupd[datafr$inhupd>0])
NROW(datafr$inhonlyupd[datafr$inhonlyupd>0])
NROW(datafr$giftonlyupd[datafr$giftonlyupd>0])

### HERE IS THE PREFERRED MEASURE #####

#### NOW THE SAME WITH ALL INHERITANCES ADDING THE VALUE OF THE INHERITED HOUSE IF IT HAS NOT BEEN COUNTED AS AN INHERITANCE IN THE HH0401 QUESTION ####

#WE CREATE A NEW VARIABLE FOR MAIN RESIDENCE VALUE IF THAT VALUE IS NOT MENTIONED IN THE OTHER INHERITANCE VARIABLE ##

datafr$housevalue <- 0
datafr$housevalue[!is.na(datafr$HB0900)] <- datafr$HB0900[!is.na(datafr$HB0900)]


datafr$housevalue [datafr$value1inh == datafr$HB0800] <- 0
datafr$housevalue [datafr$value2inh == datafr$HB0800] <- 0
datafr$housevalue [datafr$value3inh == datafr$HB0800] <- 0

#It's the same to use HH0401 or value1inh. In the next three lines we compare with the updated reported value

datafr$housevalue [datafr$HH0401 == datafr$HB0900] <- 0
datafr$housevalue [datafr$HH0402 == datafr$HB0900] <- 0
datafr$housevalue [datafr$HH0403 == datafr$HB0900] <- 0

summary (datafr$housevalue)

#WE THEN ADD THE VALUE OF THIS INHERITED RESIDENCE TO THE UPDATED VALUE, CONTROLLING THAT THE HOUSE WAS INHERITED AND WHETHER THE HOUSEHOLD OWNS ALL OR ONLY PART OF IT

datafr$valueresidinh3 <- 0

datafr$valueresidinh3 [datafr$HB0300 == 1 & datafr$HB0600 %in% c(3,4)] <- datafr$housevalue [datafr$HB0300 == 1 & datafr$HB0600 %in% c(3,4)] #The current market value of the residence now if no inheritance reported and residence owned in full

table(datafr$housevalue [datafr$HB0300 == 1 & datafr$HB0600 %in% c(3,4)])

datafr$valueresidinh3 [datafr$inhupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(3,4)] <- (datafr$housevalue[datafr$inhupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(3,4)] * datafr$HB0500[datafr$inhupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(3,4)]) / 100 #The current market value of the residence now if no inheritance reported and residence owned in full

table(datafr$housevalue[datafr$HB0300 == 2 & datafr$HB0600 %in% c(3,4)],datafr$HB0500[datafr$HB0300 == 2 & datafr$HB0600 %in% c(3,4)])

summary(datafr$valueresidinh3)
summary(datafr$valueresidinh3[datafr$valueresidinh3>0])
NROW(datafr$valueresidinh3[datafr$valueresidinh3>0])

#NOW ONLY IF THE HOUSE WAS INHERITED

datafr$valueresidinhonly3 <- 0

datafr$valueresidinhonly3 [datafr$HB0300 == 1 & datafr$HB0600 %in% c(3)] <- datafr$housevalue [datafr$HB0300 == 1 & datafr$HB0600 %in% c(3)] #The current market value of the residence now if no inheritance reported and residence owned in full

table(datafr$housevalue [datafr$HB0300 == 1 & datafr$HB0600 %in% c(3)])

datafr$valueresidinhonly3 [datafr$inhonlyupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(3)] <- (datafr$housevalue[datafr$inhonlyupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(3)] * datafr$HB0500[datafr$inhonlyupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(3)]) / 100 #The current market value of the residence now if no inheritance reported and residence owned in full

table(datafr$housevalue[datafr$HB0300 == 2 & datafr$HB0600 %in% c(3)],datafr$HB0500[datafr$HB0300 == 2 & datafr$HB0600 %in% c(3)])

summary(datafr$valueresidinhonly3)
summary(datafr$valueresidinhonly3[datafr$valueresidinhonly3>0])
NROW(datafr$valueresidinhonly3[datafr$valueresidinhonly3>0])

#NOW ONLY IF THE HOUSE WAS GIVEN

datafr$valueresidgiftonly3 <- 0

datafr$valueresidgiftonly3 [datafr$HB0300 == 1 & datafr$HB0600 %in% c(4)] <- datafr$housevalue [datafr$HB0300 == 1 & datafr$HB0600 %in% c(4)] #The current market value of the residence now if no inheritance reported and residence owned in full

table(datafr$housevalue [datafr$HB0300 == 1 & datafr$HB0600 %in% c(4)])

datafr$valueresidgiftonly3 [datafr$giftonlyupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(4)] <- (datafr$housevalue[datafr$giftonlyupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(4)] * datafr$HB0500[datafr$giftonlyupd == 0 & datafr$HB0300 == 2 & datafr$HB0600 %in% c(4)]) / 100 #The current market value of the residence now if no inheritance reported and residence owned in full

table(datafr$housevalue[datafr$HB0300 == 2 & datafr$HB0600 %in% c(4)],datafr$HB0500[datafr$HB0300 == 2 & datafr$HB0600 %in% c(4)])

summary(datafr$valueresidgiftonly3)
summary(datafr$valueresidgiftonly3[datafr$valueresidgiftonly3>0])
NROW(datafr$valueresidgiftonly3[datafr$valueresidgiftonly3>0])


#USING ONLY THE LWS DATASET, WE CANNOT UPDATE THE VALUES, SO WE CAN ONLY ADD THE DIFFERENT AMOUNTS OF INHERITANCES AT THEIR REGULAR VALUE

#And we can obtain the updated value of inheritances aggregated

datafr$inhupd3 <- datafr$inhupd + datafr$valueresidinh3
summary (datafr$inhupd3)

datafr$inhonlyupd3 <- datafr$inhonlyupd + datafr$valueresidinhonly3
summary (datafr$inhonlyupd3)

datafr$giftonlyupd3 <- datafr$giftonlyupd + datafr$valueresidgiftonly3

summary (datafr$inhupd3)
summary (datafr$inhonlyupd3)
summary (datafr$giftonlyupd3)


#We save the data
saveRDS(datafr, file = "datasets/HFCS14_France_after_step_2a_imp2.rds")
