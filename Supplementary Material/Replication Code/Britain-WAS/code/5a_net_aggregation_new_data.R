#now we aggregate inheritances for net values

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)

alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step4c-gross-updated-new-data.rds")


#we do not remove any outlier value



#### BASELINE PROCEDURE ##### NO IMPUTATION

##########     AGGREGATION OF INHERITANCES (WITH BRACKETS MIDDLE VALUES ASSIGNED AND UPDATED  ) ##############

#WAVE 1 -INHERITANCES OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhpastw1<- ifelse((alldatawas$IEValW1upd>0 & alldatawas$flaginputlife1basic == 0), alldatawas$IEValW1upd, 0) + (ifelse((alldatawas$IEVal2W1upd>0 & alldatawas$flaginputlife2basic == 0), alldatawas$IEVal2W1upd, 0)) + (ifelse(alldatawas$IEVal3W1upd>0, alldatawas$IEVal3W1upd, 0))

summary(alldatawas$inhpastw1)
summary(alldatawas$inhpastw1[alldatawas$inhpastw1>0])
NROW(alldatawas$inhpastw1[alldatawas$inhpastw1>0])

#WAVE 1 -  BRACKETED INHERITANCES EVER OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhbrackpastw1 <- ifelse(alldatawas$bracked1inhW1pastupd>0, alldatawas$bracked1inhW1pastupd, 0) + (ifelse(alldatawas$bracked2inhW1pastupd>0, alldatawas$bracked2inhW1pastupd, 0)) + (ifelse(alldatawas$bracked3inhW1pastupd>0, alldatawas$bracked3inhW1pastupd, 0))

summary(alldatawas$inhbrackpastw1)
summary(alldatawas$inhbrackpastw1[alldatawas$inhbrackpastw1>0])
NROW(alldatawas$inhbrackpastw1[alldatawas$inhbrackpastw1>0])


#WAVE 1 - INHERITANCES IN THE LAST 5 YEARS
#(Note we exclude those obtained from the previous partner, which reduces our cases from  512 to 494)

alldatawas$inh5yw1 <-0
alldatawas$inh5yw1 <- (ifelse((alldatawas$IValW1>0 & alldatawas$flaginputrecent1basic == 0), alldatawas$IValW1, 0)) + (ifelse((alldatawas$IVal2W1>0 & alldatawas$flaginputrecent2basic == 0), alldatawas$IVal2W1, 0)) + (ifelse(alldatawas$IVal3W1>0, alldatawas$IVal3W1, 0))

table(alldatawas$flaginputrecent1basic[alldatawas$inh5yw1>0])
#Only 1 case is imputed, and it is never excluded
table(alldatawas$IWhoW1, alldatawas$flaginputrecent1basic)



  alldatawas$inh5yw1 <-NULL
alldatawas$inh5yw1 <- (ifelse((alldatawas$IValW1>0 & (alldatawas$flaginputrecent1basic != 1 | is.na(alldatawas$flaginputrecent1basic)) & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$IValW1, 0)) + (ifelse((alldatawas$IVal2W1>0 & alldatawas$flaginputrecent2basic !=1 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$IVal2W1, 0)) + (ifelse((alldatawas$IVal3W1>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)"), alldatawas$IVal3W1, 0))

summary(alldatawas$inh5yw1)
summary(alldatawas$inh5yw1[alldatawas$inh5yw1>0])
NROW(alldatawas$inh5yw1[alldatawas$inh5yw1>0])

#NOTE WE HAVE TO UPDATE ALL OF THAT (OBTAINED IN 2008 OR 5 YEAR BEFORE) TO 2012 MONEY(IF WE USE WEALTH FROM THE THIRD WAVE). WE HAD NOT DONE THAT YES, ONLY THE ONES OBTAINED PRIOR TO 5 YEARS BEFORE THE SURVEY

alldatawas$inh5yw1upd <- alldatawas$inh5yw1
alldatawas$inh5yw1upd[alldatawas$inh5yw1>0]<- alldatawas$inh5yw1[alldatawas$inh5yw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#WAVE 1 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

#Reduced from 33 to 27 in the bracketed

alldatawas$inhbrackw1 <- NULL
alldatawas$inhbrackw1 <- ifelse((alldatawas$bracked1inhW1>0 ), alldatawas$bracked1inhW1, 0) + (ifelse((alldatawas$bracked2inhW1>0), alldatawas$bracked2inhW1, 0)) + (ifelse(alldatawas$bracked3inhW1>0, alldatawas$bracked3inhW1, 0))


alldatawas$inhbrackw1 <- ifelse((alldatawas$bracked1inhW1>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$bracked1inhW1, 0) + (ifelse((alldatawas$bracked2inhW1>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked2inhW1, 0)) + (ifelse((alldatawas$bracked3inhW1>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked3inhW1, 0))

summary(alldatawas$inhbrackw1)
summary(alldatawas$inhbrackw1[alldatawas$inhbrackw1>0])
NROW(alldatawas$inhbrackw1[alldatawas$inhbrackw1>0])

alldatawas$inhbrackw1upd <- alldatawas$inhbrackw1
alldatawas$inhbrackw1upd[alldatawas$inhbrackw1>0]<- alldatawas$inhbrackw1[alldatawas$inhbrackw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)


alldatawas$inhtotalw1 <- alldatawas$inhpastw1 + alldatawas$inh5yw1upd + alldatawas$inhbrackpastw1 + alldatawas$inhbrackw1upd

summary(alldatawas$inhtotalw1)
summary(alldatawas$inhtotalw1[alldatawas$inhtotalw1>0])
NROW(alldatawas$inhtotalw1[alldatawas$inhtotalw1>0])


#WAVE 2 - INHERITANCES IN THE LAST 5 YEARS

alldatawas$inh5yw2 <- (ifelse(alldatawas$IvalW2>0, alldatawas$IvalW2, 0)) + (ifelse(alldatawas$Ival2W2>0, alldatawas$Ival2W2, 0)) + (ifelse(alldatawas$Ival3W2>0, alldatawas$Ival3W2, 0))


alldatawas$inh5yw2 <- (ifelse((alldatawas$IvalW2>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$IvalW2, 0)) + (ifelse((alldatawas$Ival2W2>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$Ival2W2, 0)) + (ifelse((alldatawas$Ival3W2>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$Ival3W2, 0))

summary(alldatawas$inh5yw2)
summary(alldatawas$inh5yw2[alldatawas$inh5yw2>0])
NROW(alldatawas$inh5yw2[alldatawas$inh5yw2>0])

alldatawas$inh5yw2upd <- alldatawas$inh5yw2
alldatawas$inh5yw2upd[alldatawas$inh5yw2>0]<- alldatawas$inh5yw2[alldatawas$inh5yw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


#WAVE 2 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw2 <- ifelse(alldatawas$bracked1inhW2>0, alldatawas$bracked1inhW2, 0) + (ifelse(alldatawas$bracked2inhW2>0, alldatawas$bracked2inhW2, 0)) + (ifelse(alldatawas$bracked3inhW2>0, alldatawas$bracked3inhW2, 0))

alldatawas$inhbrackw2 <- ifelse((alldatawas$bracked1inhW2>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$bracked1inhW2, 0) + (ifelse((alldatawas$bracked2inhW2>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$bracked2inhW2, 0)) + (ifelse((alldatawas$bracked3inhW2>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$bracked3inhW2, 0))

summary(alldatawas$inhbrackw2)
summary(alldatawas$inhbrackw2[alldatawas$inhbrackw2>0])
NROW(alldatawas$inhbrackw2[alldatawas$inhbrackw2>0])

alldatawas$inhbrackw2upd <- alldatawas$inhbrackw2
alldatawas$inhbrackw2upd[alldatawas$inhbrackw2>0]<- alldatawas$inhbrackw2[alldatawas$inhbrackw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


alldatawas$inhtotalw2 <- alldatawas$inh5yw2upd + alldatawas$inhbrackw2upd

summary(alldatawas$inhtotalw2)
summary(alldatawas$inhtotalw2[alldatawas$inhtotalw2>0])
NROW(alldatawas$inhtotalw2[alldatawas$inhtotalw2>0])

#WAVE 3 - INHERITANCES IN THE LAST 2 YEARS

alldatawas$inh5yw3 <- (ifelse(alldatawas$IVal1W3>0, (alldatawas$IVal1W3), 0)) + (ifelse(alldatawas$IVal2w3_i>0, alldatawas$IVal2w3_i, 0)) + (ifelse(alldatawas$IVal3w3_i>0, alldatawas$IVal3w3_i, 0))

alldatawas$inh5yw3 <- (ifelse((alldatawas$IVal1W3>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$IVal1W3, 0)) + (ifelse((alldatawas$IVal2w3_i>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$IVal2w3_i, 0)) + (ifelse((alldatawas$IVal3w3_i>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$IVal3w3_i, 0))

summary(alldatawas$inh5yw3)
summary(alldatawas$inh5yw3[alldatawas$inh5yw3>0])
NROW(alldatawas$inh5yw3[alldatawas$inh5yw3>0])

#WAVE 3 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw3 <- ifelse((alldatawas$bracked1inhW3>0), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0), alldatawas$bracked3inhW3, 0))

alldatawas$inhbrackw3 <- ifelse((alldatawas$bracked1inhW3>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$bracked3inhW3, 0))

summary(alldatawas$inhbrackw3)
summary(alldatawas$inhbrackw3[alldatawas$inhbrackw3>0])
NROW(alldatawas$inhbrackw3[alldatawas$inhbrackw3>0])

alldatawas$inhtotalw3 <- alldatawas$inh5yw3 + alldatawas$inhbrackw3

summary(alldatawas$inhtotalw3)
summary(alldatawas$inhtotalw3[alldatawas$inhtotalw3>0])
NROW(alldatawas$inhtotalw3[alldatawas$inhtotalw3>0])


############################## GIFTS #######################

#WAVE 1

alldatawas$giftotalw1 <- (ifelse(alldatawas$IGifvalW1>0, alldatawas$IGifvalW1, 0))+ alldatawas$bracked1giftW1 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw1)
NROW(alldatawas$giftotalw1[alldatawas$giftotalw1>0])

#And we update gifts from wave 1 to wave 3

alldatawas$giftotalw1upd <- alldatawas$giftotalw1
alldatawas$giftotalw1upd[alldatawas$giftotalw1>0]<- alldatawas$giftotalw1[alldatawas$giftotalw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1

alldatawas$inhgiftw1 <- alldatawas$inhtotalw1 + alldatawas$giftotalw1upd
summary(alldatawas$inhgiftw1[alldatawas$inhgiftw1>0])
NROW(alldatawas$inhgiftw1[alldatawas$inhgiftw1>0])

#WAVE 2 #NOTE THAT IN WAVE TWO, RESPONDENT REPORTS ONLY THOSE RECEIVE IN THE LAST TWO YEARS, SO -UNLIKE INHERITANCES, WHO ARE ASKED IN THE LAST 5 YEARS - THERE IS NO OVERLAPPING WITH THE POSSIBLE GIFTS REPORTED IN WAVE 1 TWO YEARS BEFORE

alldatawas$giftotalw2 <- (ifelse(alldatawas$IgifvalW2>0, alldatawas$IgifvalW2, 0))+ alldatawas$bracked1giftW2 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw2)
NROW(alldatawas$giftotalw2[alldatawas$giftotalw2>0])

#And we update gifts from wave 2 to wave 3

alldatawas$giftotalw2upd <- alldatawas$giftotalw2
alldatawas$giftotalw2upd[alldatawas$giftotalw2>0]<- alldatawas$giftotalw2[alldatawas$giftotalw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1


alldatawas$inhgiftw2 <- alldatawas$inhtotalw2 + alldatawas$giftotalw2upd
summary(alldatawas$inhgiftw2[alldatawas$inhgiftw2>0])
NROW(alldatawas$inhgiftw2[alldatawas$inhgiftw2>0])

#WAVE 3

alldatawas$giftotalw3 <- (ifelse(alldatawas$DVGiftAnnualw3>0, alldatawas$DVGiftAnnualw3, 0))

#alldatawas$giftotalw3 <- (ifelse(alldatawas$giftotalw3==0, alldatawas$bracked3, alldatawas$giftotalw3))

# We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0])
NROW(alldatawas$giftotalw3[alldatawas$giftotalw3>0])

summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Imputed"])
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Not imputed"])

alldatawas$inhgiftw3 <- alldatawas$inhtotalw3 + alldatawas$giftotalw3
summary(alldatawas$inhgiftw3[alldatawas$inhgiftw3>0])
NROW(alldatawas$inhgiftw3[alldatawas$inhgiftw3>0])


################## FINALLY, LET US ACCUMULATE ALL INHERITANCES AND GIFTS FROM THE THREE WAVES ##########

######(NOTE THAT THIS INCLUDES INHERITANCES ACQUIRED EVER IN LIFE, FOR THAT WAS ASKED IN WAVE 1)#######

alldatawas$totalinhbaseline <- alldatawas$inhgiftw1 + alldatawas$inhgiftw2 + alldatawas$inhgiftw3

NROW(alldatawas$totalinhbaseline[alldatawas$totalinhbaseline>0])


##### ANOTHER OPTION IS TO SEPARATE INHERITANCES AND GIFTS ####

alldatawas$totalgiftonly <- alldatawas$giftotalw1upd + alldatawas$giftotalw2upd + alldatawas$giftotalw3

NROW(alldatawas$totalgiftonly[alldatawas$totalgiftonly>0])

alldatawas$totalinhonly <- alldatawas$inhtotalw1 + alldatawas$inhtotalw2 + alldatawas$inhtotalw3

NROW(alldatawas$totalinhonly[alldatawas$totalinhonly>0])





##### NOW LET US REPEAT THAT FOR THE BASIC IMPUTATION MODEL 1 ##########################################


#NOW WE DON'T EXCLUDE THE OBSERVATIONS FLAGGED AS IMPUTED


#WAVE 1 -INHERITANCES OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhpastw1<- ifelse(alldatawas$IEValW1upd>0, alldatawas$IEValW1upd, 0) + (ifelse(alldatawas$IEVal2W1upd>0, alldatawas$IEVal2W1upd, 0)) + (ifelse(alldatawas$IEVal3W1upd>0, alldatawas$IEVal3W1upd, 0))

summary(alldatawas$inhpastw1)
summary(alldatawas$inhpastw1[alldatawas$inhpastw1>0])
NROW(alldatawas$inhpastw1[alldatawas$inhpastw1>0])

#WAVE 1 -  BRACKETED INHERITANCES EVER OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhbrackpastw1 <- ifelse(alldatawas$bracked1inhW1pastupd>0, alldatawas$bracked1inhW1pastupd, 0) + (ifelse(alldatawas$bracked2inhW1pastupd>0, alldatawas$bracked2inhW1pastupd, 0)) + (ifelse(alldatawas$bracked3inhW1pastupd>0, alldatawas$bracked3inhW1pastupd, 0))

summary(alldatawas$inhbrackpastw1)
summary(alldatawas$inhbrackpastw1[alldatawas$inhbrackpastw1>0])
NROW(alldatawas$inhbrackpastw1[alldatawas$inhbrackpastw1>0])


#WAVE 1 - INHERITANCES IN THE LAST 5 YEARS
#(Note we exclude those obtained from the previous partner, which reduces our cases from  512 to 494)

alldatawas$inh5yw1 <-NULL
alldatawas$inh5yw1 <- (ifelse((alldatawas$IValW1>0), alldatawas$IValW1, 0)) + (ifelse(alldatawas$IVal2W1>0, alldatawas$IVal2W1, 0)) + (ifelse(alldatawas$IVal3W1>0, alldatawas$IVal3W1, 0))

table(alldatawas$flaginputrecent1basic[alldatawas$inh5yw1>0])
#Only 1 case is imputed, and it is never excluded
table(alldatawas$IWhoW1, alldatawas$flaginputrecent1basic)

alldatawas$inh5yw1 <-NULL
alldatawas$inh5yw1 <- (ifelse((alldatawas$IValW1>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$IValW1, 0)) + (ifelse((alldatawas$IVal2W1>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$IVal2W1, 0)) + (ifelse((alldatawas$IVal3W1>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)"), alldatawas$IVal3W1, 0))

summary(alldatawas$inh5yw1)
summary(alldatawas$inh5yw1[alldatawas$inh5yw1>0])
NROW(alldatawas$inh5yw1[alldatawas$inh5yw1>0])

#NOTE WE HAVE TO UPDATE ALL OF THAT (OBTAINED IN 2008 OR 5 YEAR BEFORE) TO 2012 MONEY(IF WE USE WEALTH FROM THE THIRD WAVE). WE HAD NOT DONE THAT YES, ONLY THE ONES OBTAINED PRIOR TO 5 YEARS BEFORE THE SURVEY

alldatawas$inh5yw1upd <- alldatawas$inh5yw1
alldatawas$inh5yw1upd[alldatawas$inh5yw1>0]<- alldatawas$inh5yw1[alldatawas$inh5yw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#WAVE 1 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

#Reduced from 33 to 27 in the bracketed

alldatawas$inhbrackw1 <- NULL
alldatawas$inhbrackw1 <- ifelse((alldatawas$bracked1inhW1>0 ), alldatawas$bracked1inhW1, 0) + (ifelse((alldatawas$bracked2inhW1>0), alldatawas$bracked2inhW1, 0)) + (ifelse(alldatawas$bracked3inhW1>0, alldatawas$bracked3inhW1, 0))


alldatawas$inhbrackw1 <- ifelse((alldatawas$bracked1inhW1>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$bracked1inhW1, 0) + (ifelse((alldatawas$bracked2inhW1>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked2inhW1, 0)) + (ifelse((alldatawas$bracked3inhW1>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked3inhW1, 0))

summary(alldatawas$inhbrackw1)
summary(alldatawas$inhbrackw1[alldatawas$inhbrackw1>0])
NROW(alldatawas$inhbrackw1[alldatawas$inhbrackw1>0])

alldatawas$inhbrackw1upd <- alldatawas$inhbrackw1
alldatawas$inhbrackw1upd[alldatawas$inhbrackw1>0]<- alldatawas$inhbrackw1[alldatawas$inhbrackw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)


alldatawas$inhtotalw1 <- alldatawas$inhpastw1 + alldatawas$inh5yw1upd + alldatawas$inhbrackpastw1 + alldatawas$inhbrackw1upd

summary(alldatawas$inhtotalw1)
summary(alldatawas$inhtotalw1[alldatawas$inhtotalw1>0])
NROW(alldatawas$inhtotalw1[alldatawas$inhtotalw1>0])


#WAVE 2 - INHERITANCES IN THE LAST 5 YEARS

alldatawas$inh5yw2 <- (ifelse(alldatawas$IvalW2>0, alldatawas$IvalW2, 0)) + (ifelse(alldatawas$Ival2W2>0, alldatawas$Ival2W2, 0)) + (ifelse(alldatawas$Ival3W2>0, alldatawas$Ival3W2, 0))


alldatawas$inh5yw2 <- (ifelse((alldatawas$IvalW2>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$IvalW2, 0)) + (ifelse((alldatawas$Ival2W2>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$Ival2W2, 0)) + (ifelse((alldatawas$Ival3W2>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$Ival3W2, 0))

summary(alldatawas$inh5yw2)
summary(alldatawas$inh5yw2[alldatawas$inh5yw2>0])
NROW(alldatawas$inh5yw2[alldatawas$inh5yw2>0])

alldatawas$inh5yw2upd <- alldatawas$inh5yw2
alldatawas$inh5yw2upd[alldatawas$inh5yw2>0]<- alldatawas$inh5yw2[alldatawas$inh5yw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


#WAVE 2 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw2 <- ifelse(alldatawas$bracked1inhW2>0, alldatawas$bracked1inhW2, 0) + (ifelse(alldatawas$bracked2inhW2>0, alldatawas$bracked2inhW2, 0)) + (ifelse(alldatawas$bracked3inhW2>0, alldatawas$bracked3inhW2, 0))

alldatawas$inhbrackw2 <- ifelse((alldatawas$bracked1inhW2>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$bracked1inhW2, 0) + (ifelse((alldatawas$bracked2inhW2>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$bracked2inhW2, 0)) + (ifelse((alldatawas$bracked3inhW2>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$bracked3inhW2, 0))

summary(alldatawas$inhbrackw2)
summary(alldatawas$inhbrackw2[alldatawas$inhbrackw2>0])
NROW(alldatawas$inhbrackw2[alldatawas$inhbrackw2>0])

alldatawas$inhbrackw2upd <- alldatawas$inhbrackw2
alldatawas$inhbrackw2upd[alldatawas$inhbrackw2>0]<- alldatawas$inhbrackw2[alldatawas$inhbrackw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


alldatawas$inhtotalw2 <- alldatawas$inh5yw2upd + alldatawas$inhbrackw2upd

summary(alldatawas$inhtotalw2)
summary(alldatawas$inhtotalw2[alldatawas$inhtotalw2>0])
NROW(alldatawas$inhtotalw2[alldatawas$inhtotalw2>0])

#WAVE 3 - INHERITANCES IN THE LAST 2 YEARS

alldatawas$inh5yw3 <- (ifelse(alldatawas$IVal1W3>0, (alldatawas$IVal1W3), 0)) + (ifelse(alldatawas$IVal2w3_i>0, alldatawas$IVal2w3_i, 0)) + (ifelse(alldatawas$IVal3w3_i>0, alldatawas$IVal3w3_i, 0))

alldatawas$inh5yw3 <- (ifelse((alldatawas$IVal1W3>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$IVal1W3, 0)) + (ifelse((alldatawas$IVal2w3_i>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$IVal2w3_i, 0)) + (ifelse((alldatawas$IVal3w3_i>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$IVal3w3_i, 0))

summary(alldatawas$inh5yw3)
summary(alldatawas$inh5yw3[alldatawas$inh5yw3>0])
NROW(alldatawas$inh5yw3[alldatawas$inh5yw3>0])

#WAVE 3 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw3 <- ifelse((alldatawas$bracked1inhW3>0), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0), alldatawas$bracked3inhW3, 0))

alldatawas$inhbrackw3 <- ifelse((alldatawas$bracked1inhW3>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$bracked3inhW3, 0))

summary(alldatawas$inhbrackw3)
summary(alldatawas$inhbrackw3[alldatawas$inhbrackw3>0])
NROW(alldatawas$inhbrackw3[alldatawas$inhbrackw3>0])

alldatawas$inhtotalw3 <- alldatawas$inh5yw3 + alldatawas$inhbrackw3

summary(alldatawas$inhtotalw3)
summary(alldatawas$inhtotalw3[alldatawas$inhtotalw3>0])
NROW(alldatawas$inhtotalw3[alldatawas$inhtotalw3>0])


############################## GIFTS #######################

#WAVE 1

alldatawas$giftotalw1 <- (ifelse(alldatawas$IGifvalW1>0, alldatawas$IGifvalW1, 0))+ alldatawas$bracked1giftW1 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw1)
NROW(alldatawas$giftotalw1[alldatawas$giftotalw1>0])

#And we update gifts from wave 1 to wave 3

alldatawas$giftotalw1upd <- alldatawas$giftotalw1
alldatawas$giftotalw1upd[alldatawas$giftotalw1>0]<- alldatawas$giftotalw1[alldatawas$giftotalw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1

alldatawas$inhgiftw1 <- alldatawas$inhtotalw1 + alldatawas$giftotalw1upd
summary(alldatawas$inhgiftw1[alldatawas$inhgiftw1>0])
NROW(alldatawas$inhgiftw1[alldatawas$inhgiftw1>0])

#WAVE 2 #NOTE THAT IN WAVE TWO, RESPONDENT REPORTS ONLY THOSE RECEIVE IN THE LAST TWO YEARS, SO -UNLIKE INHERITANCES, WHO ARE ASKED IN THE LAST 5 YEARS - THERE IS NO OVERLAPPING WITH THE POSSIBLE GIFTS REPORTED IN WAVE 1 TWO YEARS BEFORE

alldatawas$giftotalw2 <- (ifelse(alldatawas$IgifvalW2>0, alldatawas$IgifvalW2, 0))+ alldatawas$bracked1giftW2 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw2)
NROW(alldatawas$giftotalw2[alldatawas$giftotalw2>0])

#And we update gifts from wave 2 to wave 3

alldatawas$giftotalw2upd <- alldatawas$giftotalw2
alldatawas$giftotalw2upd[alldatawas$giftotalw2>0]<- alldatawas$giftotalw2[alldatawas$giftotalw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1


alldatawas$inhgiftw2 <- alldatawas$inhtotalw2 + alldatawas$giftotalw2upd
summary(alldatawas$inhgiftw2[alldatawas$inhgiftw2>0])
NROW(alldatawas$inhgiftw2[alldatawas$inhgiftw2>0])

#WAVE 3

alldatawas$giftotalw3 <- (ifelse(alldatawas$DVGiftAnnualw3>0, alldatawas$DVGiftAnnualw3, 0))

#alldatawas$giftotalw3 <- (ifelse(alldatawas$giftotalw3==0, alldatawas$bracked3, alldatawas$giftotalw3))

# We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0])
NROW(alldatawas$giftotalw3[alldatawas$giftotalw3>0])

summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Imputed"])
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Not imputed"])

alldatawas$inhgiftw3 <- alldatawas$inhtotalw3 + alldatawas$giftotalw3
summary(alldatawas$inhgiftw3[alldatawas$inhgiftw3>0])
NROW(alldatawas$inhgiftw3[alldatawas$inhgiftw3>0])


################## FINALLY, LET US ACCUMULATE ALL INHERITANCES AND GIFTS FROM THE THREE WAVES ##########

######(NOTE THAT THIS INCLUDES INHERITANCES ACQUIRED EVER IN LIFE, FOR THAT WAS ASKED IN WAVE 1)#######

alldatawas$totalinhbasicinput <- alldatawas$inhgiftw1 + alldatawas$inhgiftw2 + alldatawas$inhgiftw3

NROW(alldatawas$totalinhbasicinput[alldatawas$totalinhbasicinput>0])

##### ANOTHER OPTION IS TO SEPARATE INHERITANCES AND GIFTS ####

alldatawas$totalgiftonlybasicinput <- alldatawas$giftotalw1upd + alldatawas$giftotalw2upd + alldatawas$giftotalw3

NROW(alldatawas$totalgiftonlybasicinput[alldatawas$totalgiftonlybasicinput>0])

alldatawas$totalinhonlybasicinput <- alldatawas$inhtotalw1 + alldatawas$inhtotalw2 + alldatawas$inhtotalw3

NROW(alldatawas$totalinhonlybasicinput[alldatawas$totalinhonlybasicinput>0])

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step5a-net-aggregation-new-data.rds")



