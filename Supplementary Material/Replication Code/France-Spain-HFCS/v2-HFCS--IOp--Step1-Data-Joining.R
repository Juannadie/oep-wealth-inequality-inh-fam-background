#SETTING THE DIRECTORY

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS/Data 2014/Files") #Set Working Directory ECB Data (Mac Computer)
options ("scipen"=10000, "digits"= 6)

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS/Data 2014/Files") #Set Working Directory ECB Data (Laptop Mac Computer)
options ("scipen"=10000, "digits"= 6)


#WE USE THE THIRD VERSION OF THE DATA (WAVE 2). VERSION 2.1.

#We first work with the first imputation method (imp1). We can in the final analysis make an average of all five imputation tables, or use replicate weights to proceed with the analysis of imputation.

data1a = read.csv("D1.csv", header = T, sep=",") #Derived variables
data1b = read.csv("H1.csv", header = T, sep=",") #Household Data
data1c = read.csv("P1.csv", header = T, sep=",") #Personal Data
data1d = read.csv("HN1.csv", header = T, sep=",") #Household Data Non-Core
data1e = read.csv("PN1.csv", header = T, sep=",") #Personal Data Non-Core

#data1f = read.csv("W.csv", header = T, sep=",") #Replicate Weights #THAT'S RESERVED FOR VARIANCE ESTIMATION... NOT USED FOR THE MOMENT



#In household survey the variable id is small case...
data1b$ID <- data1b$id
#In personal survey survey the variable id is small case...
data1c$ID <- data1c$id
data1c$HID <- data1c$hid
#In household survey non core the variable id is small case...
data1d$ID <- data1d$id
#In personal survey non core the variable id is small case...
data1e$ID <- data1e$id
data1e$HID <- data1e$hid
#In fact only only the Derived variables had capital letters...

#We can merge variables sequentially
data1house <- merge (data1a, data1b, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data1house <- merge (data1house, data1d, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data1house$HID <- data1house$ID #We create the house ID (HID) variable in this variable
data1house$ID <- NULL #ID meant house ID in the household files

#Note also that "survey" is all small letters
data1personal <- merge (data1c, data1e, by = c("SA0010", "RA0010", "SA0100", "IM0100", "survey", "ID", "HID")) #We merge for household ID, country and individual ID (Individual files, c and d, do have both ID and HID)
data1full <- merge (data1house, data1personal, by = c("SA0010", "SA0100", "IM0100", "survey", "HID"), sort=T) #We merge for household ID, which is called HID in the personal file and just ID in the other one


### WE REPEAT THAT FOR THE SECOND IMPUTATION OF THE DATA ###

data2a = read.csv("D2.csv", header = T, sep=",") #Derived variables
data2b = read.csv("H2.csv", header = T, sep=",") #Household Data
data2c = read.csv("P2.csv", header = T, sep=",") #Personal Data
data2d = read.csv("HN2.csv", header = T, sep=",") #Household Data Non-Core
data2e = read.csv("PN2.csv", header = T, sep=",") #Personal Data Non-Core

#In household survey the variable id is small case...
data2b$ID <- data2b$id
#In personal survey survey the variable id is small case...
data2c$ID <- data2c$id
data2c$HID <- data2c$hid
#In household survey non corethe variable id is small case...
data2d$ID <- data2d$id
#In personal survey non core the variable id is small case...
data2e$ID <- data2e$id
data2e$HID <- data2e$hid

#We can merge variables
data2house <- merge (data2a, data2b, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data2house <- merge (data2house, data2d, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data2house$HID <- data2house$ID #We create the house ID variable in this variable
data2house$ID <- NULL

#Note also that "survey" is all small letters in this wave...

data2personal <- merge (data2c, data2e, by = c("SA0010", "RA0010", "SA0100", "IM0100", "survey", "ID", "HID")) #We merge for household ID, country and individual ID
data2full <- merge (data2house, data2personal, by = c("SA0010", "SA0100", "IM0100", "survey", "HID"), sort=T) #We merge for household ID, which is called HID in the personal file and just ID in the other one.


### WE REPEAT THAT FOR THE THIRD IMPUTATION OF THE DATA ###

data3a = read.csv("D3.csv", header = T, sep=",") #Derived variables
data3b = read.csv("H3.csv", header = T, sep=",") #Household Data
data3c = read.csv("P3.csv", header = T, sep=",") #Personal Data
data3d = read.csv("HN3.csv", header = T, sep=",") #Household Data Non-Core
data3e = read.csv("PN3.csv", header = T, sep=",") #Personal Data Non-Core

#In household survey the variable id is small case...
data3b$ID <- data3b$id
#In personal survey survey the variable id is small case...
data3c$ID <- data3c$id
data3c$HID <- data3c$hid
#In household survey non corethe variable id is small case...
data3d$ID <- data3d$id
#In personal survey non core the variable id is small case...
data3e$ID <- data3e$id
data3e$HID <- data3e$hid

#We can merge variables
data3house <- merge (data3a, data3b, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data3house <- merge (data3house, data3d, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data3house$HID <- data3house$ID #We create the house ID variable in this variable
data3house$ID <- NULL

#Note also that "survey" is all small letters in this wave...

data3personal <- merge (data3c, data3e, by = c("SA0010", "RA0010", "SA0100", "IM0100", "survey", "ID", "HID")) #We merge for household ID, country and individual ID
data3full <- merge (data3house, data3personal, by = c("SA0010", "SA0100", "IM0100", "survey", "HID"), sort=T) #We merge for household ID, which is called HID in the personal file and just ID in the other one.


### WE REPEAT THAT FOR THE FOURTH IMPUTATION OF THE DATA ###

data4a = read.csv("D4.csv", header = T, sep=",") #Derived variables
data4b = read.csv("H4.csv", header = T, sep=",") #Household Data
data4c = read.csv("P4.csv", header = T, sep=",") #Personal Data
data4d = read.csv("HN4.csv", header = T, sep=",") #Household Data Non-Core
data4e = read.csv("PN4.csv", header = T, sep=",") #Personal Data Non-Core

#In household survey the variable id is small case...
data4b$ID <- data4b$id
#In personal survey survey the variable id is small case...
data4c$ID <- data4c$id
data4c$HID <- data4c$hid
#In household survey non corethe variable id is small case...
data4d$ID <- data4d$id
#In personal survey non core the variable id is small case...
data4e$ID <- data4e$id
data4e$HID <- data4e$hid

#We can merge variables
data4house <- merge (data4a, data4b, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data4house <- merge (data4house, data4d, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data4house$HID <- data4house$ID #We create the house ID variable in this variable
data4house$ID <- NULL

#Note also that "survey" is all small letters in this wave...

data4personal <- merge (data4c, data4e, by = c("SA0010", "RA0010", "SA0100", "IM0100", "survey", "ID", "HID")) #We merge for household ID, country and individual ID
data4full <- merge (data4house, data4personal, by = c("SA0010", "SA0100", "IM0100", "survey", "HID"), sort=T) #We merge for household ID, which is called HID in the personal file and just ID in the other one.


### WE REPEAT THAT FOR THE FIFTH IMPUTATION OF THE DATA ###

data5a = read.csv("D3.csv", header = T, sep=",") #Derived variables
data5b = read.csv("H3.csv", header = T, sep=",") #Household Data
data5c = read.csv("P3.csv", header = T, sep=",") #Personal Data
data5d = read.csv("HN3.csv", header = T, sep=",") #Household Data Non-Core
data5e = read.csv("PN3.csv", header = T, sep=",") #Personal Data Non-Core

#In household survey the variable id is small case...
data5b$ID <- data5b$id
#In personal survey survey the variable id is small case...
data5c$ID <- data5c$id
data5c$HID <- data5c$hid
#In household survey non corethe variable id is small case...
data5d$ID <- data5d$id
#In personal survey non core the variable id is small case...
data5e$ID <- data5e$id
data5e$HID <- data5e$hid

#We can merge variables
data5house <- merge (data5a, data5b, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data5house <- merge (data5house, data5d, by = c("SA0010", "SA0100", "IM0100", "survey", "ID")) #We merge for household ID, country and house ID
data5house$HID <- data5house$ID #We create the house ID variable in this variable
data5house$ID <- NULL

#Note also that "survey" is all small letters in this wave...

data5personal <- merge (data5c, data5e, by = c("SA0010", "RA0010", "SA0100", "IM0100", "survey", "ID", "HID")) #We merge for household ID, country and individual ID
data5full <- merge (data5house, data5personal, by = c("SA0010", "SA0100", "IM0100", "survey", "HID"), sort=T) #We merge for household ID, which is called HID in the personal file and just ID in the other one.



#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE


setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/datasets") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/datasets") #Set Working Directory for the LAPTOP


saveRDS(data1full, file = "HFCS14_input1_full.rds")

saveRDS(data2full, file = "HFCS14_input2_full.rds")

saveRDS(data3full, file = "HFCS14_input3_full.rds")

saveRDS(data4full, file = "HFCS14_input4_full.rds")

saveRDS(data5full, file = "HFCS14_input5_full.rds")

#We could read the data to check
#data1full <- readRDS(file = "HFCS14_input1_full.rds") ##
#data2full <- readRDS(file = "HFCS14_input2_full.rds") ##
#data3full <- readRDS(file = "HFCS14_input3_full.rds") ##
#data4full <- readRDS(file = "HFCS14_input4_full.rds") ##
#data5full <- readRDS(file = "HFCS14_input5_full.rds") ##

















###### OPTIONAL STEP TO MERGE DIFFERENT DATASETS WITH THE FIVE IMPUTATIONS #########


#NOW WE MERGE IMPUTATIONS 1-5, FIRST ADDING TO DIFFERENCIATE THE VARIABLES THE SUFFIX .1 (OR .2, ETC) DEPENDING ON THE IMPUTATION TO EACH OF THE VARIABLES EXCEPT THE MATCHING ONES

colnames(data1full)[-which(names(data1full)%in%c("SA0010", "RA0010", "SA0100"))] <- paste(colnames(data1full)[-which(names(data1full)%in%c("SA0010", "RA0010", "SA0100"))], "1", sep = ".")

colnames(data2full)[-which(names(data2full)%in%c("SA0010", "RA0010", "SA0100"))] <- paste(colnames(data2full)[-which(names(data2full)%in%c("SA0010", "RA0010", "SA0100"))], "2", sep = ".")

colnames(data3full)[-which(names(data3full)%in%c("SA0010", "RA0010", "SA0100"))] <- paste(colnames(data3full)[-which(names(data3full)%in%c("SA0010", "RA0010", "SA0100"))], "2", sep = ".")

colnames(data4full)[-which(names(data4full)%in%c("SA0010", "RA0010", "SA0100"))] <- paste(colnames(data4full)[-which(names(data4full)%in%c("SA0010", "RA0010", "SA0100"))], "2", sep = ".")

colnames(data5full)[-which(names(data5full)%in%c("SA0010", "RA0010", "SA0100"))] <- paste(colnames(data5full)[-which(names(data5full)%in%c("SA0010", "RA0010", "SA0100"))], "2", sep = ".")


#WE COULD SEQUENTIALLY MERGE THE FULL MASSIVE DATASETS... (NOT RECOMMENDED BEFORE SUBSETTING THE NEEDED VARIABLES...)

HFCS2014mix12 <- merge (data1full, data2full, by = c("SA0010", "RA0010", "SA0100"), sort=T) #
#HFCS2014mix13 <- merge (HFCS2014mix12, data3full, by = c("SA0010", "RA0010", "SA0100"), sort=T) #
#HFCS2014mix14 <- merge (HFCS2014mix13, data4full, by = c("SA0010", "RA0010", "SA0100"), sort=T) #
#HFCS2014mix15 <- merge (HFCS2014mix15, data5full, by = c("SA0010", "RA0010", "SA0100"), sort=T) #

#WE CAN SAVE AT LEAST THE MERGER OF THE FIRST TWO IMPUTATIONS, FOR EXAMPLE...
saveRDS(HFCS2014mix12, file = "HFCS14_inputs1_2_full.rds")

#MORE THAN THAT THE COMPUTER IS GONNA COMPLAIN AND STOP MERGING...

