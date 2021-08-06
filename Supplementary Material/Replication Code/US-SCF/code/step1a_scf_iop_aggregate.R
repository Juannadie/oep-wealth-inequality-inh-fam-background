#STEP 1 FOR SFC US ANALYSIS
#PRELIMINARY THINGS.... load Stata Data, load libraries, scientific notation

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory for the LAPTOP

library(foreign)
library(readstata13)
options ("scipen"=100, "digits"=4)


#WE OPEN THE ORIGINAL DATASET FOR SCF 2016 WAVE
scf16<-read.dta13("datasets/p16i6.dta") #With Stata13 package

saveRDS(scf16, file = "datasets/p16i6.rds")

scf16 <- readRDS(file = "datasets/p16i6.rds")

#NOW WE OPEN THE DATASET WE HAVE RUN IN SAS WITH THE AGGREGATE VARIABLES

scf16agg<-read.dta("datasets/SCFP2016-real-agg.dta")

#WE CONVERT THEM TO R DATA

saveRDS(scf16agg, file = "datasets/SCFP2016-real-agg.rds")
scf16agg <- readRDS(file = "datasets/SCFP2016-real-agg.rds")

#WE CHECK BOTH DATABASES

head(scf16)

head(scf16agg)

summary(scf16agg$checking) #Money in checking accounts for example

summary(scf16agg$networth) #Money in networth

head(scf16$X8022) #Age in the original sample

head(scf16agg$age) #Age in the new sample

#It seems that we can merge the original DataBase and the Aggregated Variables from the Bulletin obtained with the SAS Macro can simply be added with cbind

scf16all <- cbind (scf16agg, scf16)

saveRDS(scf16all, file = "datasets/SCF-2016-all.rds")

scf16all<- readRDS(file = "datasets/SCF-2016-all.rds")

#We can also load the replicate weights file

repweightsscf2016 <-read.dta13("datasets/p16_rw1.dta") #With Stata13 package

saveRDS(repweightsscf2016, file = "datasets/repweightscf-2016.rds")


