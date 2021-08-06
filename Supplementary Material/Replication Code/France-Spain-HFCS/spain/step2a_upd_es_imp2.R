#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

#We can read the data
#data1full <- readRDS(file = "datasets/HFCS14_input1_full.rds") ##
data2full <- readRDS(file = "datasets/HFCS14_input2_full.rds") ##
#data3full <- readRDS(file = "datasets/HFCS14_input3_full.rds") ##
#data4full <- readRDS(file = "datasets/HFCS14_input4_full.rds") ##
#data5full <- readRDS(file = "datasets/HFCS14_input5_full.rds") ##

#We could also read this if we wanted to track the imputations, comparing them in the dataset that has both variables.
#data1.2full <- readRDS(file = "HFCS14_inputs1_2_full.rds") ## TO AVOID DOING THE JOINING AGAIN


#AND NOW WE FILTER FOR SPAIN

dataes <- data2full[data2full$SA0100 == 'ES',] #

### NOW WE HAVE TO UPDATE THE INHERITANCES VALUE BEFORE WE GO ON WITH THE ANALYSIS ###

cpi = read.csv("cpi/cpi-ES-2012.csv", sep = ";", header = T, as.is = T)

cpi$year <- as.numeric(cpi$year)

#Before updating we add their value

dataes$value1inh <- dataes$HH0401 #First inheritance
dataes$value2inh <- dataes$HH0402 #Second inheritance
dataes$value3inh <- dataes$HH0403 #Third inheritance

#Let's see the years the inheritances were received
table(dataes$HH0201) #For the first inheritance
table(dataes$HH0202) #For the second inheritance
table(dataes$HH0203) #For the third inheritance

dataes$yearinh1 <- dataes$HH0201
dataes$yearinh2 <- dataes$HH0202
dataes$yearinh3 <- dataes$HH0203

#We do not have indices to update before 1935

dataes$yearinh1 [dataes$yearinh1 < 1935] <- 1935

dataes$value1inhupd <- dataes$value1inh

#This is the right way to update the amounts...

for (i in 1935:2014) {
  dataes$value1inhupd <- ifelse (dataes$yearinh1==i & dataes$value1inh >0,dataes$value1inh*(cpi$multiplier[cpi$year==i]), (dataes$value1inhupd))
}

summary (dataes$value1inh)
summary (dataes$value1inhupd)

#We check that the formula to update the values has worked out correctly...
mean56 <- mean(dataes$value1inh[dataes$yearinh1==1956], na.rm = T)
meanupd56 <- mean(dataes$value1inhupd[dataes$yearinh1==1956], na.rm = T)
multiplier56 <- meanupd56/mean56
multiplier56 #Just as expected

mean2000 <- mean(dataes$value1inh[dataes$yearinh1==2000], na.rm = T)
meanupd2000 <- mean(dataes$value1inhupd[dataes$yearinh1==2000], na.rm = T)
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000

#NOW WE PROCEED WITH THE SECOND INHERITANCE

#We do not have indices to update before 1956
dataes$yearinh2 [dataes$yearinh2 < 1935] <- 1935

dataes$value2inhupd <- dataes$value2inh

#This is the right way to update the amounts...

for (i in 1935:2014) {
  dataes$value2inhupd <- ifelse (dataes$yearinh2==i & dataes$value2inh >0,dataes$value2inh*(cpi$multiplier[cpi$year==i]), (dataes$value2inhupd))
}

summary (dataes$value2inh)
summary (dataes$value2inhupd)

#We check that the formula to update the values has worked out correctly...
mean56 <- mean(dataes$value2inh[dataes$yearinh2==1956], na.rm = T)
meanupd56 <- mean(dataes$value2inhupd[dataes$yearinh2==1956], na.rm = T)
multiplier56 <- meanupd56/mean56 #Just as expected
multiplier56

mean2000 <- mean(dataes$value2inh[dataes$yearinh2==2000], na.rm = T)
meanupd2000 <- mean(dataes$value2inhupd[dataes$yearinh2==2000], na.rm = T)
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000

#NOW WE PROCEED WITH THE THIRD INHERITANCE

#We do not have indices to update before 1956
dataes$yearinh3 [dataes$yearinh3 < 1935] <- 1935

dataes$value3inhupd <- dataes$value3inh

#This is the right way to update the amounts...

for (i in 1935:2014) {dataes$value3inhupd <- ifelse (dataes$yearinh3==i & dataes$value3inh >0,dataes$value3inh*(cpi$multiplier[cpi$year==i]), (dataes$value3inhupd))}

summary (dataes$value3inh)
summary (dataes$value3inhupd)

#We check that the formula to update the values has worked out correctly...
mean56 <- mean(dataes$value3inh[dataes$yearinh3==1957], na.rm = T)
meanupd56 <- mean(dataes$value3inhupd[dataes$yearinh3==1957], na.rm = T)
multiplier56 <- meanupd56/mean56 #Just as expected
multiplier56

mean2000 <- mean(dataes$value3inh[dataes$yearinh3==2000], na.rm = T)
meanupd2000 <- mean(dataes$value3inhupd[dataes$yearinh3==2000], na.rm = T)
multiplier2000 <- meanupd2000/mean2000 #Just as expected
multiplier2000


#AGGREGATION OF INHERITANCES

#And we can obtain the value of inheritances aggregated
#dataes$inh.1 <- dataes$value1inh + dataes$value2inh + dataes$value3inh
#Better this way to not let NAs spoil the sum
dataes$inh <- rowSums(dataes[,c("value1inh","value2inh","value3inh")], na.rm=TRUE)

#And we can obtain the updated value of inheritances aggregated
dataes$inhupd <- rowSums(dataes[,c("value1inhupd","value2inhupd","value3inhupd")], na.rm=TRUE)

summary(dataes$inh[dataes$inh>0])
summary(dataes$inhupd[dataes$inhupd>0])

#We can now differenciate between gifts and inheritances with variable HH050x

NROW (dataes$HH0401[!is.na(dataes$HH0401) & dataes$HH0401>0])

summary (dataes$HH0501[dataes$value1inhupd>0])

summary (dataes$HH0501)
table (dataes$HH0502)
table (dataes$HH0503)

#NOW WE ADD ONLY THE VALUES WHICH ARE INHERITANCES - ALSO ASSUMING NA AS INHERITANCES

dataes$value1inhonlyupd <- 0
dataes$value1inhonlyupd[dataes$HH0501 == 2 | (is.na(dataes$HH0501))] <- dataes$value1inhupd[dataes$HH0501 == 2 | (is.na(dataes$HH0501))]
summary(dataes$value1inhonlyupd)
NROW(dataes$value1inhonlyupd[dataes$value1inhonlyupd>0])

dataes$value2inhonlyupd <- 0
dataes$value2inhonlyupd[dataes$HH0502 == 2 | (is.na(dataes$HH0502))] <- dataes$value2inhupd [dataes$HH0502 == 2 | (is.na(dataes$HH0502))]
summary(dataes$value2inhonlyupd)

dataes$value3inhonlyupd <- 0
dataes$value3inhonlyupd[dataes$HH0503 == 2 | (is.na(dataes$HH0503))] <- dataes$value3inhupd [dataes$HH0503 == 2 | (is.na(dataes$HH0503))]
summary(dataes$value3inhonlyupd)

#And we can obtain the updated value of inheritances aggregated
dataes$inhonlyupd <- rowSums(dataes[,c("value1inhonlyupd","value2inhonlyupd","value3inhonlyupd")], na.rm=TRUE)


#AND NOW THE SAME WITH GIFTS

dataes$value1giftonlyupd <- 0
dataes$value1giftonlyupd[dataes$HH0501 == 1 & (!is.na(dataes$HH0501))] <- dataes$value1inhupd[dataes$HH0501 == 1 & (!is.na(dataes$HH0501))]
summary(dataes$value1giftonlyupd)

dataes$value2giftonlyupd <- 0
dataes$value2giftonlyupd[dataes$HH0502 == 1 & (!is.na(dataes$HH0502))] <- dataes$value2inhupd [dataes$HH0502 == 1 & (!is.na(dataes$HH0502))]
summary(dataes$value2giftonlyupd)

dataes$value3giftonlyupd <- 0
dataes$value3giftonlyupd[dataes$HH0503 == 1 & (!is.na(dataes$HH0503))] <- dataes$value3inhupd [dataes$HH0503 == 1 & (!is.na(dataes$HH0503))]
summary(dataes$value3giftonlyupd)

#And we can obtain the updated value of inheritances aggregated
dataes$giftonlyupd <- rowSums(dataes[,c("value1giftonlyupd","value2giftonlyupd","value3giftonlyupd")], na.rm=TRUE)


summary(dataes$inh[dataes$inh>0])
summary(dataes$inhupd[dataes$inhupd>0])
summary(dataes$inhonlyupd[dataes$inhonlyupd>0])
summary(dataes$giftonlyupd[dataes$giftonlyupd>0])

NROW(dataes$inh[dataes$inh>0])
NROW(dataes$inhupd[dataes$inhupd>0])
NROW(dataes$inhonlyupd[dataes$inhonlyupd>0])
NROW(dataes$giftonlyupd[dataes$giftonlyupd>0])

### HERE IS THE PREFERRED MEASURE #####

#### NOW THE SAME WITH ALL INHERITANCES ADDING THE VALUE OF THE INHERITED HOUSE IF IT HAS NOT BEEN COUNTED AS AN INHERITANCE IN THE HH0401 QUESTION ####

#WE CREATE A NEW VARIABLE FOR MAIN RESIDENCE VALUE IF THAT VALUE IS NOT MENTIONED IN THE OTHER INHERITANCE VARIABLE ##

dataes$housevalue <- 0
dataes$housevalue[!is.na(dataes$HB0900)] <- dataes$HB0900[!is.na(dataes$HB0900)]


dataes$housevalue [dataes$value1inh == dataes$HB0800] <- 0
dataes$housevalue [dataes$value2inh == dataes$HB0800] <- 0
dataes$housevalue [dataes$value3inh == dataes$HB0800] <- 0

#It's the same to use HH0401 or value1inh. In the next three lines we compare with the updated reported value

dataes$housevalue [dataes$HH0401 == dataes$HB0900] <- 0
dataes$housevalue [dataes$HH0402 == dataes$HB0900] <- 0
dataes$housevalue [dataes$HH0403 == dataes$HB0900] <- 0

summary (dataes$housevalue)

#WE THEN ADD THE VALUE OF THIS INHERITED RESIDENCE TO THE UPDATED VALUE, CONTROLLING THAT THE HOUSE WAS INHERITED AND WHETHER THE HOUSEHOLD OWNS ALL OR ONLY PART OF IT

dataes$valueresidinh3 <- 0

dataes$valueresidinh3 [dataes$HB0300 == 1 & dataes$HB0600 %in% c(3,4)] <- dataes$housevalue [dataes$HB0300 == 1 & dataes$HB0600 %in% c(3,4)] #The current market value of the residence now if no inheritance reported and residence owned in full

table(dataes$housevalue [dataes$HB0300 == 1 & dataes$HB0600 %in% c(3,4)])

dataes$valueresidinh3 [dataes$inhupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(3,4)] <- (dataes$housevalue[dataes$inhupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(3,4)] * dataes$HB0500[dataes$inhupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(3,4)]) / 100 #The current market value of the residence now if no inheritance reported and residence owned in full

table(dataes$housevalue[dataes$HB0300 == 2 & dataes$HB0600 %in% c(3,4)],dataes$HB0500[dataes$HB0300 == 2 & dataes$HB0600 %in% c(3,4)])

summary(dataes$valueresidinh3)
summary(dataes$valueresidinh3[dataes$valueresidinh3>0])
NROW(dataes$valueresidinh3[dataes$valueresidinh3>0])

#NOW ONLY IF THE HOUSE WAS INHERITED

dataes$valueresidinhonly3 <- 0

dataes$valueresidinhonly3 [dataes$HB0300 == 1 & dataes$HB0600 %in% c(3)] <- dataes$housevalue [dataes$HB0300 == 1 & dataes$HB0600 %in% c(3)] #The current market value of the residence now if no inheritance reported and residence owned in full

table(dataes$housevalue [dataes$HB0300 == 1 & dataes$HB0600 %in% c(3)])

dataes$valueresidinhonly3 [dataes$inhonlyupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(3)] <- (dataes$housevalue[dataes$inhonlyupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(3)] * dataes$HB0500[dataes$inhonlyupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(3)]) / 100 #The current market value of the residence now if no inheritance reported and residence owned in full

table(dataes$housevalue[dataes$HB0300 == 2 & dataes$HB0600 %in% c(3)],dataes$HB0500[dataes$HB0300 == 2 & dataes$HB0600 %in% c(3)])

summary(dataes$valueresidinhonly3)
summary(dataes$valueresidinhonly3[dataes$valueresidinhonly3>0])
NROW(dataes$valueresidinhonly3[dataes$valueresidinhonly3>0])

#NOW ONLY IF THE HOUSE WAS GIVEN

dataes$valueresidgiftonly3 <- 0

dataes$valueresidgiftonly3 [dataes$HB0300 == 1 & dataes$HB0600 %in% c(4)] <- dataes$housevalue [dataes$HB0300 == 1 & dataes$HB0600 %in% c(4)] #The current market value of the residence now if no inheritance reported and residence owned in full

table(dataes$housevalue [dataes$HB0300 == 1 & dataes$HB0600 %in% c(4)])

dataes$valueresidgiftonly3 [dataes$giftonlyupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(4)] <- (dataes$housevalue[dataes$giftonlyupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(4)] * dataes$HB0500[dataes$giftonlyupd == 0 & dataes$HB0300 == 2 & dataes$HB0600 %in% c(4)]) / 100 #The current market value of the residence now if no inheritance reported and residence owned in full

table(dataes$housevalue[dataes$HB0300 == 2 & dataes$HB0600 %in% c(4)],dataes$HB0500[dataes$HB0300 == 2 & dataes$HB0600 %in% c(4)])

summary(dataes$valueresidgiftonly3)
summary(dataes$valueresidgiftonly3[dataes$valueresidgiftonly3>0])
NROW(dataes$valueresidgiftonly3[dataes$valueresidgiftonly3>0])


#USING ONLY THE LWS DATASET, WE CANNOT UPDATE THE VALUES, SO WE CAN ONLY ADD THE DIFFERENT AMOUNTS OF INHERITANCES AT THEIR REGULAR VALUE

#And we can obtain the updated value of inheritances aggregated

dataes$inhupd3 <- dataes$inhupd + dataes$valueresidinh3
summary (dataes$inhupd3)

dataes$inhonlyupd3 <- dataes$inhonlyupd + dataes$valueresidinhonly3
summary (dataes$inhonlyupd3)

dataes$giftonlyupd3 <- dataes$giftonlyupd + dataes$valueresidgiftonly3

summary (dataes$inhupd3)
summary (dataes$inhonlyupd3)
summary (dataes$giftonlyupd3)



#We save the data
saveRDS(dataes, file = "datasets/spain/HFCS14_Spain_after_step_2a_imp2.rds")
