#now we aggregate inheritances from individuals to households

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)


alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step5c-gross-input-basic-new-data.rds")

casecheck <- alldatawas[alldatawas$CaseW3 == 276,]

write.csv(casecheck, file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/output_csv/casecheck276.csv")

## Some checks on the number of variables ##

summary(alldatawas$totalinhbaseline[alldatawas$totalinhbaseline>0]) #UPDATED NET, NO IMPUTATION
summary(alldatawas$totalinhbaselinegross[alldatawas$totalinhbaselinegross>0])#UPDATED GROSS, NO IMPUTATION
summary(alldatawas$totalinhbaselinegrossadjusted[alldatawas$totalinhbaselinegrossadjusted>0])#UPDATED GROSS, NO IMPUTATION

NROW(alldatawas$totalinhbaseline[alldatawas$totalinhbaseline>0])
NROW(alldatawas$totalinhbaselinegross[alldatawas$totalinhbaselinegross>0])
NROW(alldatawas$totalinhbaselinegrossadjusted[alldatawas$totalinhbaselinegrossadjusted>0])


summary(alldatawas$totalinhbasicinput[alldatawas$totalinhbasicinput>0])
summary(alldatawas$totalinhgrossinput[alldatawas$totalinhgrossinput>0])
summary(alldatawas$totalinhgrossadjustedinput[alldatawas$totalinhgrossadjustedinput>0])


NROW(alldatawas$totalinhbasicinput[alldatawas$totalinhbasicinput>0])
NROW(alldatawas$totalinhgrossinput[alldatawas$totalinhgrossinput>0])
NROW(alldatawas$totalinhgrossadjustedinput[alldatawas$totalinhgrossadjustedinput>0])




#WE CREATE A FLAG FOR IMPUTATION IF ANY MEMBER OF THE FAMILY HAS BEEN IMPUTED

alldatawas$flaginput<- 0

alldatawas$flaginput[alldatawas$flaginputlife1basic == 1 & !is.na(alldatawas$flaginputlife1basic)] <- 1
alldatawas$flaginput[alldatawas$flaginputlife2basic == 1 & !is.na(alldatawas$flaginputlife2basic)] <- 1
alldatawas$flaginput[alldatawas$flaginputrecent1basic == 1 & !is.na(alldatawas$flaginputrecent1basic)] <- 1
alldatawas$flaginput[alldatawas$flaginputrecent2basic == 1 & !is.na(alldatawas$flaginputrecent2basic)] <- 1


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(flaginputhouse = sum(flaginput)) #We check how many members have received inheritance
summary (alldatawas$flaginputhouse[alldatawas$flaginputhouse>0])
table (alldatawas$flaginputhouse)



#WE AGGREGATE FIRST THE NET VALUES

#THIS IS A VERY IMPORTANT STEP, WE ADD ALL THE INHERITANCES PER HOUSEHOLD
#FIRST FOR THE NET BASELINE MODEL (NO IMPUTATIONS, NET VALUES (NOT GROSSED BACK))


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhbaseline = sum(totalinhbaseline)) #We check how many members have received inheritance
summary (alldatawas$totalinhbaseline[alldatawas$totalinhbaseline>0])
summary (alldatawas$houseinhbaseline[alldatawas$houseinhbaseline>0])

#And we can do that also by inheritances and gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonly = sum(totalinhonly))
summary (alldatawas$totalinhonly[alldatawas$totalinhonly>0])
summary (alldatawas$houseinhonly[alldatawas$houseinhonly>0])

#Now gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonly = sum(totalgiftonly))
summary (alldatawas$totalgiftonly[alldatawas$totalgiftonly>0])
summary (alldatawas$housegiftonly[alldatawas$housegiftonly>0])

#####   BASIC INPUT #####

#Now we have to add the inheritances received by all the members in the family, in this case using the basic input

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhbasicinput = sum(totalinhbasicinput)) #We check how many members have received inheritance
summary (alldatawas$totalinhbasicinput[alldatawas$totalinhbasicinput>0])
summary (alldatawas$houseinhbasicinput[alldatawas$houseinhbasicinput>0])

#And we can do that also by inheritances and gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonlybasicinput = sum(totalinhonlybasicinput))
summary (alldatawas$totalinhonlybasicinput[alldatawas$totalinhonlybasicinput>0])
summary (alldatawas$houseinhonlybasicinput[alldatawas$houseinhonlybasicinput>0])

#Now gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonlybasicinput = sum(totalgiftonlybasicinput))
summary (alldatawas$totalgiftonlybasicinput[alldatawas$totalgiftonlybasicinput>0])
summary (alldatawas$housegiftonlybasicinput[alldatawas$housegiftonlybasicinput>0])


#THIS IS A VERY IMPORTANT STEP, WE ADD ALL THE INHERITANCES PER HOUSEHOLD
#FIRST FOR THE GROSS BASELINE MODEL (NO IMPUTATIONS)

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhbaselinegross = sum(totalinhbaselinegross)) #We check how many members have received inheritance
summary (alldatawas$totalinhbaselinegross[alldatawas$totalinhbaselinegross>0])
summary (alldatawas$houseinhbaselinegross[alldatawas$houseinhbaselinegross>0])

#And we can do that also by inheritances and gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonlygross = sum(totalinhonlygross))
summary (alldatawas$totalinhonlygross[alldatawas$totalinhonlygross>0])
summary (alldatawas$houseinhonlygross[alldatawas$houseinhonlygross>0])

#Now gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonly = sum(totalgiftonly))
summary (alldatawas$totalgiftonly[alldatawas$totalgiftonly>0])
summary (alldatawas$housegiftonly[alldatawas$housegiftonly>0])

#####   GROSS  IMPUT ##### #### THIS IS WHAT MATTERS, WHAT WE USE #####

#Now we have to add the inheritances received by all the members in the family, in this case using the basic input AND GROSS VALUES

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhgrossinput = sum(totalinhgrossinput)) #We check how many members have received inheritance
summary (alldatawas$totalinhgrossinput[alldatawas$totalinhgrossinput>0])
summary (alldatawas$houseinhgrossinput[alldatawas$houseinhgrossinput>0])

#And we can do that also by inheritances and gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonlygrossinput = sum(totalinhonlygrossinput))
summary (alldatawas$totalinhonlygrossinput[alldatawas$totalinhonlygrossinput>0])
summary (alldatawas$houseinhonlygrossinput[alldatawas$houseinhonlygrossinput>0])

#Now gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonlygrossinput = sum(totalgiftonlygrossinput))
summary (alldatawas$totalgiftonlygrossinput[alldatawas$totalgiftonlygrossinput>0])
summary (alldatawas$housegiftonlygrossinput[alldatawas$housegiftonlygrossinput>0])



####### NOW WE USE THE ADJUSTED GROSSING ##############

#FIRST FOR THE GROSS BASELINE MODEL (NO IMPUTATIONS)

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhbaselinegrossadjusted = sum(totalinhbaselinegrossadjusted)) #We check how many members have received inheritance
summary (alldatawas$totalinhbaselinegrossadjusted[alldatawas$totalinhbaselinegrossadjusted>0])
summary (alldatawas$houseinhbaselinegrossadjusted[alldatawas$houseinhbaselinegrossadjusted>0])

#And we can do that also by inheritances and gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonlygrossadjusted = sum(totalinhonlygrossadjusted))
summary (alldatawas$totalinhonlygrossadjusted[alldatawas$totalinhonlygrossadjusted>0])
summary (alldatawas$houseinhonlygrossadjusted[alldatawas$houseinhonlygrossadjusted>0])

#Now gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonly = sum(totalgiftonly))
summary (alldatawas$totalgiftonly[alldatawas$totalgiftonly>0])
summary (alldatawas$housegiftonly[alldatawas$housegiftonly>0])


#####   GROSS  INPUT ADJUSTED ##### #### THIS IS WHAT MATTERS, WHAT WE USE #####

#Now we have to add the inheritances received by all the members in the family, in this case using the basic input AND GROSS VALUES

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhgrossadjustedinput = sum(totalinhgrossadjustedinput)) #We check how many members have received inheritance
summary (alldatawas$totalinhgrossadjustedinput[alldatawas$totalinhgrossadjustedinput>0])
summary (alldatawas$houseinhgrossadjustedinput[alldatawas$houseinhgrossadjustedinput>0])

#And we can do that also by inheritances and gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonlygrossadjustedinput = sum(totalinhonlygrossadjustedinput))
summary (alldatawas$totalinhonlygrossadjustedinput[alldatawas$totalinhonlygrossadjustedinput>0])
summary (alldatawas$houseinhonlygrossadjustedinput[alldatawas$houseinhonlygrossadjustedinput>0])

#Now gifts separately

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonlygrossadjustedinput = sum(totalgiftonlygrossadjustedinput))
summary (alldatawas$totalgiftonlygrossadjustedinput[alldatawas$totalgiftonlygrossadjustedinput>0])
summary (alldatawas$housegiftonlygrossadjustedinput[alldatawas$housegiftonlygrossadjustedinput>0])






#Now trusts separately

#### They are the same for gross and basic input, they have not missing values ####

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housetrustonly = sum(brackedtotalupdtrust))
summary (alldatawas$brackedtotalupdtrust[alldatawas$brackedtotalupdtrust>0])
summary (alldatawas$housetrustonly[alldatawas$housetrustonly>0])


### We add the trust to all the aggregate measures equally, since they are equally valid:

alldatawas$houseinhonly <- alldatawas$houseinhonly + alldatawas$housetrustonly #Net no imput
alldatawas$houseinhonlygross <- alldatawas$houseinhonlygross + alldatawas$housetrustonly #Gross no imput
alldatawas$houseinhonlygrossadjusted <- alldatawas$houseinhonlygrossadjusted + alldatawas$housetrustonly #Gross no imput
alldatawas$houseinhonlybasicinput <- alldatawas$houseinhonlybasicinput + alldatawas$housetrustonly #Net with imput
alldatawas$houseinhonlygrossinput <- alldatawas$houseinhonlygrossinput + alldatawas$housetrustonly #Gross with imput
alldatawas$houseinhonlygrossadjustedinput <- alldatawas$houseinhonlygrossadjustedinput + alldatawas$housetrustonly #Gross with imput


alldatawas$houseinhbaseline <- alldatawas$houseinhbaseline + alldatawas$housetrustonly #Net no imput
alldatawas$houseinhbaselinegross <- alldatawas$houseinhbaselinegross + alldatawas$housetrustonly #Gross no imput
alldatawas$houseinhbaselinegrossadjusted <- alldatawas$houseinhbaselinegrossadjusted + alldatawas$housetrustonly #Gross no imput
alldatawas$houseinhbasicinput <- alldatawas$houseinhbasicinput + alldatawas$housetrustonly #Net with imput
alldatawas$houseinhgrossinput <- alldatawas$houseinhgrossinput + alldatawas$housetrustonly #Gross with imput
alldatawas$houseinhgrossadjustedinput <- alldatawas$houseinhgrossadjustedinput + alldatawas$housetrustonly #Gross with imput




#### WE CAN NOW INCLUDE HERE THE COUNTING OF THE INHERITANCES #####

#### COUNTING OF INHERITANCES BY INDIVIDUALS AND THEN AGGREGATION INTO HOUSEHOLDS ####

### First we aggregate per household each of the inheritances

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(pretaxinhlifeW1updhouse = sum(pretaxinhlifeW1upd)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(pretaxinhlife2W1updhouse = sum(pretaxinhlife2W1upd)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(pretaxinhlife3W1updhouse = sum(pretaxinhlife3W1upd)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(pretaxinhlifebW1updhouse = sum(pretaxinhlifebW1upd)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(pretaxinhlife2bW1updhouse = sum(pretaxinhlife2bW1upd)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(pretaxinhlife3bW1updhouse = sum(pretaxinhlife3bW1upd)) #

#### COUNTING ONLY THE ONES WITH VALUE OVER 0 ###

alldatawas$count1lifehouse <- ifelse(alldatawas$pretaxinhlifeW1updhouse>0,1,0) + ifelse(alldatawas$pretaxinhlife2W1updhouse>0,1,0) + ifelse(alldatawas$pretaxinhlife3W1updhouse>0,1,0) + ifelse(alldatawas$pretaxinhlifebW1updhouse>0,1,0) + ifelse(alldatawas$pretaxinhlife2bW1updhouse>0,1,0) + ifelse(alldatawas$pretaxinhlife3bW1updhouse>0,1,0)

summary(alldatawas$count1lifehouse)
table(alldatawas$count1lifehouse)


#FIRST WAVE, LIFE INHERITANCES (ACTUAL VALUES AND BRACKETS)

alldatawas$count1life <- ifelse(alldatawas$pretaxinhlifeW1upd>0,1,0) + ifelse(alldatawas$pretaxinhlife2W1upd>0,1,0) + ifelse(alldatawas$pretaxinhlife3W1upd>0,1,0) + ifelse(alldatawas$pretaxinhlifebW1upd>0,1,0) + ifelse(alldatawas$pretaxinhlife2bW1upd>0,1,0) + ifelse(alldatawas$pretaxinhlife3bW1upd>0,1,0)

summary(alldatawas$count1life)
table(alldatawas$count1life)

#FIRST WAVE, RECENT INHERITANCES

#Aggregation

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IValW1grosshouse = sum(IValW1gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IVal2W1grosshouse = sum(IVal2W1gross)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IVal3W1grosshouse = sum(IVal3W1gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked1inhW1grosshouse = sum(bracked1inhW1gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked2inhW1grosshouse = sum(bracked2inhW1gross)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked3inhW1grosshouse = sum(bracked3inhW1gross)) #


alldatawas$count1recenthouse <- ifelse(alldatawas$IValW1grosshouse>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal2W1grosshouse>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal3W1grosshouse>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked1inhW1grosshouse>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked2inhW1grosshouse>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked3inhW1grosshouse>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)",1,0)

summary(alldatawas$count1recenthouse)
table(alldatawas$count1recenthouse)

alldatawas$count1recent <- ifelse(alldatawas$IValW1gross>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal2W1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal3W1gross>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked1inhW1gross>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked2inhW1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked3inhW1gross>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)",1,0)

summary(alldatawas$count1recent)
table(alldatawas$count1recent)




#SECOND WAVE, RECENT INHERITANCES

#Aggregation

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IValW2grosshouse = sum(IValW2gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IVal2W2grosshouse = sum(IVal2W2gross)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IVal3W2grosshouse = sum(IVal3W2gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked1inhW2grosshouse = sum(bracked1inhW2gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked2inhW2grosshouse = sum(bracked2inhW2gross)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked3inhW2grosshouse = sum(bracked3inhW2gross)) #


alldatawas$count2recenthouse <- ifelse(alldatawas$IValW2grosshouse>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal2W2grosshouse>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal3W2grosshouse>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked1inhW2grosshouse>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked2inhW2grosshouse>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked3inhW2grosshouse>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)",1,0)

summary(alldatawas$count2recenthouse)
table(alldatawas$count2recenthouse)


alldatawas$count2recent <- ifelse(alldatawas$IValW2gross>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal2W2gross>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$IVal3W2gross>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked1inhW2gross>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked2inhW2gross>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)",1,0) + ifelse(alldatawas$bracked3inhW2gross>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)",1,0)

summary(alldatawas$count2recent)
table(alldatawas$count2recent)



#THIRD WAVE, RECENT INHERITANCES

#Aggregation

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IValW3grosshouse = sum(IValW3gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IVal2W3grosshouse = sum(IVal2W3gross)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IVal3W3grosshouse = sum(IVal3W3gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked1inhW3grosshouse = sum(bracked1inhW3gross)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked2inhW3grosshouse = sum(bracked2inhW3gross)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked3inhW3grosshouse = sum(bracked3inhW3gross)) #


alldatawas$count3recenthouse <- ifelse(alldatawas$IValW3grosshouse>0 & alldatawas$IWhoW3 != "Spouse or partner",1,0) + ifelse(alldatawas$IVal2W3grosshouse>0 & alldatawas$IWho2W3 != "Spouse or partner",1,0) + ifelse(alldatawas$IVal3W3grosshouse>0 & alldatawas$IWho3W3 != "Spouse or partner",1,0) + ifelse(alldatawas$bracked1inhW3grosshouse>0 & alldatawas$IWhoW3 != "Spouse or partner",1,0) + ifelse(alldatawas$bracked2inhW3grosshouse>0 & alldatawas$IWho2W3 != "Spouse or partner",1,0) + ifelse(alldatawas$bracked3inhW3grosshouse>0 & alldatawas$IWho3W3 != "Spouse or partner",1,0)

summary(alldatawas$count3recenthouse)
table(alldatawas$count3recenthouse)

alldatawas$count3recent <- ifelse(alldatawas$IValW3gross>0 & alldatawas$IWhoW3 != "Spouse or partner",1,0) + ifelse(alldatawas$IVal2W3gross>0 & alldatawas$IWho2W3 != "Spouse or partner",1,0) + ifelse(alldatawas$IVal3W3gross>0 & alldatawas$IWho3W3 != "Spouse or partner",1,0) + ifelse(alldatawas$bracked1inhW3gross>0 & alldatawas$IWhoW3 != "Spouse or partner",1,0) + ifelse(alldatawas$bracked2inhW3gross>0 & alldatawas$IWho2W3 != "Spouse or partner",1,0) + ifelse(alldatawas$bracked3inhW3gross>0 & alldatawas$IWho3W3 != "Spouse or partner",1,0)

summary(alldatawas$count3recent)
table(alldatawas$count3recent)

#NOW WE AGGREGATE THE COUNT TO GET A TOTAL NUMBER OF INHERITANCES RECEIVED

alldatawas$totalcountinhhouse <- alldatawas$count1lifehouse + alldatawas$count1recenthouse + alldatawas$count2recenthouse + alldatawas$count3recenthouse

summary(alldatawas$totalcountinhhouse)
table(alldatawas$totalcountinhhouse)

alldatawas$totalcountinhhouseall <- alldatawas$totalcountinhhouse
alldatawas$totalcountinhhouse[alldatawas$houseinhonlygrossinput < 5000] <- 0

summary(alldatawas$totalcountinhhouse)
table(alldatawas$totalcountinhhouse)



### NOW FOR GIFTS ####

#FIRST WAVE, RECENT INHERITANCES

#Aggregate first

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IGifvalW1house = sum(IGifvalW1)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(IgifvalW2house = sum(IgifvalW2)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(DVGiftAnnualw3house = sum(DVGiftAnnualw3)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked1giftW1house = sum(bracked1giftW1)) #

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked1giftW2house = sum(bracked1giftW2)) #


alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(bracked1giftW3house = sum(bracked1giftW3)) #


alldatawas$totalcountgifthouse <- ifelse(alldatawas$IGifvalW1house>0,1,0) + ifelse(alldatawas$IgifvalW2house>0,1,0) + ifelse(alldatawas$DVGiftAnnualw3house>0,1,0) + ifelse(alldatawas$bracked1giftW1house>0,1,0) + ifelse(alldatawas$bracked1giftW2house>0,1,0) + ifelse(alldatawas$bracked1giftW3house>0,1,0)

summary(alldatawas$totalcountgifthouse)
table(alldatawas$totalcountgifthouse)

alldatawas$totalcountgifthouseall <- alldatawas$totalcountgifthouse
alldatawas$totalcountgifthouse[alldatawas$housegiftonlygrossinput < 5000] <- 0

summary(alldatawas$totalcountgifthouse)
table(alldatawas$totalcountgifthouse)


##WE CAN ADD THE WEALTH OF BUSINESSES AT THE HOUSEHOLD LEVEL

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housetotbuswealth = sum(totbuswealth))
summary (alldatawas$totbuswealth[alldatawas$totbuswealth>0])
summary (alldatawas$housetotbuswealth[alldatawas$housetotbuswealth>0])

### AND WE AGGREGATE THE VALUES FROM WAVE 3 ONLY ####

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(houseinhonlyw3gross = sum(inhtotalw3gross))
summary (alldatawas$inhtotalw3gross[alldatawas$inhtotalw3gross>0])
summary (alldatawas$houseinhonlyw3gross[alldatawas$houseinhonlyw3gross>0])

alldatawas <- alldatawas %>%
  group_by(CaseW3) %>%
  mutate(housegiftonlyw3 = sum(giftotalw3))
summary (alldatawas$giftotalw3[alldatawas$giftotalw3>0])
summary (alldatawas$housegiftonlyw3[alldatawas$housegiftonlyw3>0])



saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step6a-Households-new-data.rds")
