#final variables creation

library(foreign)
library(tidyverse)
library(stats)
library(Hmisc)
library(reldist)
library(tidyverse)
library(IC2)
library(quantreg)
library(dineq)
options ("scipen"=100, "digits"=4)


dataukh <- readRDS(file = "data_rds/dataukh-v3-2-from-step7a-new-data.rds")



#### LET'S SEE IF WE FIND EDUCATION FOR THIS DATASET

table(dataukh$EdAttn1W1) #has qualifications with certificate (Y/n)
table(dataukh$EdAttn2W1) #has professionals qualifications with certificate (Y/n)
table(dataukh$EdAttn3W1) #has degree level education (1) or other type of qualifications (2)

####

table (dataukh$EdAttn1W3, dataukh$EdAttn2W3)

####

table (dataukh$EdLevelW3) ### We use this variable for education

#### Let's create a somewhat comparable eduhead variable

dataukh$eduhead <- NA
dataukh$eduhead[dataukh$EdLevelW3 == 4] <- 1 #No qualifications
dataukh$eduhead[dataukh$EdLevelW3 == 2] <- 2 #qualifications (no degree)
dataukh$eduhead[dataukh$EdLevelW3 == 1] <- 3 #degree qualifications

### LET'S SEE ABOUT OCCUPATION ####

#We use the derived variable with the occupation coding

table(dataukh$SOC2010W3)

#We have to substitute the W3 "does not apply" with the W2 or W1 value

dataukh$soc2digit <- dataukh$SOC2010W3

dataukh$soc2digit[dataukh$soc2digit == "-7"] <- dataukh$SOC2010_2digitW2[dataukh$soc2digit == "-7"]

dataukh$soc2digit[is.na(dataukh$soc2digit)] <- dataukh$SOC2010_2digitW1[is.na(dataukh$soc2digit)]

table(dataukh$soc2digit) #No missing or "not apply" values when using this variable


dataukh$socw3onedigit <- trunc(dataukh$soc2digit/10)

table(dataukh$socw3onedigit)

table(dataukh$SOC2010W3, dataukh$SOC2010_2digitW2) #We have filled many of the missing values

dataukh$occhead <- dataukh$socw3onedigit

#### LET'S TRY TO FIND OUT ABOUT SELF-EMPLOYMENT #####

table(dataukh$StatW1)
table(dataukh$StatW2_i)
summary(dataukh$Statw3_i)

dataukh$selfemployed <- 0
dataukh$selfemployed[dataukh$Statw3_i == 'Self-employed'] <- 1
dataukh$selfemployed[dataukh$StatW2_i == 2] <- 1
dataukh$selfemployed[dataukh$StatW1 == 'Self-employed'] <- 1

table(dataukh$selfemployed)

#### LET'S SEE ABOUT CAPITAL INCOME (all for households) ####

summary(dataukh$DVTotGIRw3) #Total Household Income Gross

summary(dataukh$DVGIEMPw3_AGGR) # Household employment income gross

summary(dataukh$DVGISEw3_AGGR) #Income from self-employment

summary(dataukh$DVTotAllBenAnnualw3_aggr) #Annual income for benefits

summary(dataukh$DVGIPPENw3_AGGR) #Annual income from pensions

summary(dataukh$DVGIINVw3_aggr) #Annual income from investments

summary(dataukh$DVGIothRw3_aggr ) #Irregular Income gross

dataukh$labourinc <- dataukh$DVGIEMPw3_AGGR
dataukh$capitalinc <- dataukh$DVGIINVw3_aggr + dataukh$DVGISEw3_AGGR
dataukh$pensioninc <- dataukh$DVGIPPENw3_AGGR
dataukh$benefitsinc <- dataukh$DVTotAllBenAnnualw3_aggr + dataukh$DVGIothRw3_aggr


dataukh$totalinc <- dataukh$DVGIEMPw3_AGGR + dataukh$DVGISEw3_AGGR + dataukh$DVTotAllBenAnnualw3_aggr + dataukh$DVGIPPENw3_AGGR + dataukh$DVGIINVw3_aggr + dataukh$DVGIothRw3_aggr


summary(dataukh$totalinc)
summary(dataukh$DVTotGIRw3)

## NOW WE DECOMPOSE IN DIFFERENT TYPES OF WEALTH ###

#Property wealth (net property wealth of real state) of MAIN RESIDENCE

summary(dataukh$DVHValueW3)
summary(dataukh$TotMortW3)

dataukh$mainreswealth <- dataukh$DVHValueW3 - dataukh$TotMortW3

dataukh$mainreswealthgross <- dataukh$DVHValueW3
dataukh$mainresliab <- dataukh$TotMortW3
dataukh$mainreswealthnet <- dataukh$DVHValueW3 - dataukh$TotMortW3

summary(dataukh$mainreswealthgross)
summary(dataukh$mainresliab)
summary(dataukh$mainreswealthnet)


### Property wealth

dataukh$otherpropertywealthgross <- dataukh$DVPropertyW3 - dataukh$DVHValueW3

dataukh$otherpropertyliab <- dataukh$HMORTGW3 - dataukh$TotMortW3

dataukh$otherpropertywealthnet <- dataukh$otherpropertywealthgross - dataukh$otherpropertyliab

summary(dataukh$otherpropertywealthgross)
summary(dataukh$otherpropertyliab)
summary(dataukh$otherpropertywealthnet)

#Financial wealth (net financial wealth: cash, accounts, shares, etc. - liabilities)
summary(dataukh$HFINWNTw3_sum)

dataukh$finwealthgross <- dataukh$HFINWW3_sum

dataukh$finliab <- dataukh$HFINLw3_aggr

dataukh$finwealthnet2 <- dataukh$finwealthgross - dataukh$finliab

dataukh$finwealthnet <- dataukh$HFINWNTw3_sum

summary(dataukh$finwealthgross)
summary(dataukh$finliab)
summary(dataukh$finwealthnet)
summary(dataukh$finwealthnet2)

#Business Wealth

summary(dataukh$fambusgrosswealth)
summary(dataukh$fambusdebt)
summary(dataukh$fambusnetwealth)

dataukh$buswealthgross <- dataukh$fambusgrosswealth
dataukh$buswealthnet <- dataukh$fambusnetwealth
dataukh$busliab <- dataukh$fambusdebt

#Valuables and Vehicles (gross)
#Physical wealth + Main Residence Wealth (Goods, contents of property, vehicles, valuables etc.) + Other Wealth which is not main residence

summary(dataukh$HPHYSWW3)

dataukh$physicalwealth <- dataukh$HPHYSWW3 - dataukh$AllGdW3 #We take out goods and contents from the house...

######

dataukh$physicalwealthgross <- dataukh$physicalwealth #this is the same in gross and net terms (no debt)
dataukh$physicalwealthnet <- dataukh$physicalwealth

#Pension wealth
summary(dataukh$TOTPENw3_aggr)

dataukh$pensionwealth <- dataukh$TOTPENw3_aggr


#Total net wealth added

dataukh$totalwealthnetwpensionandgoodsoriginal <-  dataukh$HPHYSWW3 + dataukh$HPROPWW3 + dataukh$HFINWNTw3_sum + dataukh$TOTPENw3_aggr

dataukh$totalwealthnetwpensionoriginal <-  dataukh$HPHYSWW3 + dataukh$HPROPWW3 + dataukh$HFINWNTw3_sum + dataukh$TOTPENw3_aggr - dataukh$AllGdW3

dataukh$totalwealthnetwpension <- dataukh$mainreswealthnet + dataukh$otherpropertywealthnet + dataukh$physicalwealth + dataukh$finwealthnet +  dataukh$pensionwealth

dataukh$totalwealthnetwpensionandbus <- dataukh$mainreswealthnet + dataukh$otherpropertywealthnet + dataukh$physicalwealth + dataukh$finwealthnet + dataukh$buswealthnet + dataukh$pensionwealth 


dataukh$totalwealthnetwpensionandbusandcontents <- dataukh$mainreswealthnet + dataukh$otherpropertywealthnet + dataukh$physicalwealth + dataukh$finwealthnet + dataukh$buswealthnet + dataukh$pensionwealth + dataukh$AllGdW3

#remember that business wealth is added by ourselves, not in original UK aggregates

summary (dataukh$totalwealthnetwpensionandgoodsoriginal)
summary (dataukh$totalwealthnetwpensionoriginal)
summary (dataukh$totalwealthnetwpension)
summary (dataukh$totalwealthnetwpensionandbus)
summary (dataukh$totalwealthnetwpensionandbusandcontents)
summary (dataukh$TotWlthw3) #To check

#Our version is totalwealthnet without pension and without household contents (not included in any other survey) and with business wealth

dataukh$totalwealthnet <- dataukh$mainreswealthnet + dataukh$otherpropertywealthnet + dataukh$physicalwealth + dataukh$finwealthnet + dataukh$buswealthnet

#From the original data
dataukh$networth <- dataukh$TotWlthw3 #Household wealth
dataukh$tnw <- dataukh$networth - dataukh$TOTPENw3_aggr #Taking out all pensions to compare with HFCS...
dataukh$wealth2 <- dataukh$tnw
dataukh$wealth3 <- dataukh$wealth2 + dataukh$fambuswealthnet - dataukh$AllGdW3

summary(dataukh$totalwealthnet)
summary(dataukh$wealth2)
summary(dataukh$wealth3)

#Final definition 

dataukh$wealth <- dataukh$TotWlthw3 - dataukh$TOTPENw3_aggr - dataukh$AllGdW3 #+ dataukh$buswealthnet

summary(dataukh$wealth)

###### And we export it #######

saveRDS(dataukh, file = "data_rds/datauk-new-final-no-cpt-new-data7b.rds")








