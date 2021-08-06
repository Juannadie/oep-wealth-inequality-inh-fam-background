#now we aggregate inheritances for net values

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)

alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step5b-gross-input-basic-adjusted-new-data.rds")

##########     AGGREGATION OF INHERITANCES (WITH BRACKETS MIDDLE VALUES ASSIGNED AND UPDATED  ) ##############

#WAVE 1 -INHERITANCES OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)
#In this ones we can not exclude those ones from the spouse or partner, we don't have that info.

alldatawas$inhpastw1gross<- ifelse((alldatawas$pretaxinhlifeW1upd >0 & alldatawas$flaginputlife1basic == 0), alldatawas$pretaxinhlifeW1upd , 0) + (ifelse((alldatawas$pretaxinhlife2W1upd >0 & alldatawas$flaginputlife2basic == 0), alldatawas$pretaxinhlife2W1upd , 0)) + (ifelse(alldatawas$pretaxinhlife3W1upd >0, alldatawas$pretaxinhlife3W1upd , 0))

summary(alldatawas$inhpastw1gross)
summary(alldatawas$inhpastw1gross[alldatawas$inhpastw1gross>0])
NROW(alldatawas$inhpastw1gross[alldatawas$inhpastw1gross>0])

#WAVE 1 -  BRACKETED INHERITANCES EVER OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhbrackpastw1gross <- ifelse(alldatawas$pretaxinhlifebW1upd>0, alldatawas$pretaxinhlifebW1upd, 0) + (ifelse(alldatawas$pretaxinhlife2bW1upd>0, alldatawas$pretaxinhlife2bW1upd, 0)) + (ifelse(alldatawas$pretaxinhlife3bW1upd>0, alldatawas$pretaxinhlife3bW1upd, 0))

summary(alldatawas$inhbrackpastw1gross)
summary(alldatawas$inhbrackpastw1gross[alldatawas$inhbrackpastw1gross>0])
NROW(alldatawas$inhbrackpastw1gross[alldatawas$inhbrackpastw1gross>0])


#WAVE 1 - INHERITANCES IN THE LAST 5 YEARS
#(Note we exclude those obtained from the previous partner, which reduces our cases from  512 to 494)

alldatawas$inh5yw1gross <-NULL
alldatawas$inh5yw1gross <- (ifelse((alldatawas$IValW1gross>0 & (alldatawas$flaginputrecent1basic != 1 | is.na(alldatawas$flaginputrecent1basic))), alldatawas$IValW1gross, 0)) + (ifelse((alldatawas$IVal2W1gross>0 & alldatawas$flaginputrecent2basic == 0), alldatawas$IVal2W1gross, 0)) + (ifelse(alldatawas$IVal3W1gross>0, alldatawas$IVal3W1gross, 0))

table(alldatawas$flaginputrecent1basic[alldatawas$inh5yw1gross>0])
#Only 1 case is imputed, and it is never excluded
table(alldatawas$IWhoW1, alldatawas$flaginputrecent1basic)

alldatawas$inh5yw1gross <-NULL
alldatawas$inh5yw1gross <- (ifelse((alldatawas$IValW1gross>0 & (alldatawas$flaginputrecent1basic != 1 | is.na(alldatawas$flaginputrecent1basic)) & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$IValW1gross, 0)) + (ifelse((alldatawas$IVal2W1gross>0 & alldatawas$flaginputrecent2basic == 0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$IVal2W1gross, 0)) + (ifelse((alldatawas$IVal3W1gross>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)"), alldatawas$IVal3W1gross, 0))

summary(alldatawas$inh5yw1gross)
summary(alldatawas$inh5yw1gross[alldatawas$inh5yw1gross>0])
NROW(alldatawas$inh5yw1gross[alldatawas$inh5yw1gross>0])

#NOTE WE HAVE TO UPDATE ALL OF THAT (OBTAINED IN 2008 OR 5 YEAR BEFORE) TO 2012 MONEY(IF WE USE WEALTH FROM THE THIRD WAVE). WE HAD NOT DONE THAT YES, ONLY THE ONES OBTAINED PRIOR TO 5 YEARS BEFORE THE SURVEY

alldatawas$inh5yw1grossupd <- alldatawas$inh5yw1gross
alldatawas$inh5yw1grossupd[alldatawas$inh5yw1gross>0]<- alldatawas$inh5yw1gross[alldatawas$inh5yw1gross>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#WAVE 1 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

#Reduced from 33 to 27 in the bracketed

alldatawas$inhbrackw1gross <- NULL
alldatawas$inhbrackw1gross <- ifelse((alldatawas$bracked1inhW1gross>0 ), alldatawas$bracked1inhW1gross, 0) + (ifelse((alldatawas$bracked2inhW1gross>0), alldatawas$bracked2inhW1gross, 0)) + (ifelse(alldatawas$bracked3inhW1gross>0, alldatawas$bracked3inhW1gross, 0))


alldatawas$inhbrackw1gross <- ifelse((alldatawas$bracked1inhW1gross>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$bracked1inhW1gross, 0) + (ifelse((alldatawas$bracked2inhW1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked2inhW1gross, 0)) + (ifelse((alldatawas$bracked3inhW1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked3inhW1gross, 0))

summary(alldatawas$inhbrackw1gross)
summary(alldatawas$inhbrackw1gross[alldatawas$inhbrackw1gross>0])
NROW(alldatawas$inhbrackw1gross[alldatawas$inhbrackw1gross>0])

alldatawas$inhbrackw1grossupd <- alldatawas$inhbrackw1gross
alldatawas$inhbrackw1grossupd[alldatawas$inhbrackw1gross>0]<- alldatawas$inhbrackw1gross[alldatawas$inhbrackw1gross>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)


alldatawas$inhtotalw1gross <- alldatawas$inhpastw1gross + alldatawas$inh5yw1grossupd + alldatawas$inhbrackpastw1gross + alldatawas$inhbrackw1grossupd

summary(alldatawas$inhtotalw1gross)
summary(alldatawas$inhtotalw1gross[alldatawas$inhtotalw1gross>0])
NROW(alldatawas$inhtotalw1gross[alldatawas$inhtotalw1gross>0])


#WAVE 2 - INHERITANCES IN THE LAST 5 YEARS

alldatawas$inh5yw2gross <- (ifelse(alldatawas$IValW2gross>0, alldatawas$IValW2gross, 0)) + (ifelse(alldatawas$IVal2W2gross>0, alldatawas$IVal2W2gross, 0)) + (ifelse(alldatawas$IVal3W2gross>0, alldatawas$IVal3W2gross, 0))


alldatawas$inh5yw2gross <- (ifelse((alldatawas$IValW2gross>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$IValW2gross, 0)) + (ifelse((alldatawas$IVal2W2gross>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$IVal2W2gross, 0)) + (ifelse((alldatawas$IVal3W2gross>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$IVal3W2gross, 0))

summary(alldatawas$inh5yw2gross)
summary(alldatawas$inh5yw2gross[alldatawas$inh5yw2gross>0])
NROW(alldatawas$inh5yw2gross[alldatawas$inh5yw2gross>0])

alldatawas$inh5yw2grossupd <- alldatawas$inh5yw2gross
alldatawas$inh5yw2grossupd[alldatawas$inh5yw2gross>0]<- alldatawas$inh5yw2gross[alldatawas$inh5yw2gross>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


#WAVE 2 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw2gross <- ifelse(alldatawas$bracked1inhW2gross>0, alldatawas$bracked1inhW2gross, 0) + (ifelse(alldatawas$bracked2inhW2gross>0, alldatawas$bracked2inhW2gross, 0)) + (ifelse(alldatawas$bracked3inhW2gross>0, alldatawas$bracked3inhW2gross, 0))

alldatawas$inhbrackw2gross <- ifelse((alldatawas$bracked1inhW2gross>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$bracked1inhW2gross, 0) + (ifelse((alldatawas$bracked2inhW2gross>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$bracked2inhW2gross, 0)) + (ifelse((alldatawas$bracked3inhW2gross>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$bracked3inhW2gross, 0))

summary(alldatawas$inhbrackw2gross)
summary(alldatawas$inhbrackw2gross[alldatawas$inhbrackw2gross>0])
NROW(alldatawas$inhbrackw2gross[alldatawas$inhbrackw2gross>0])

alldatawas$inhbrackw2grossupd <- alldatawas$inhbrackw2gross
alldatawas$inhbrackw2grossupd[alldatawas$inhbrackw2gross>0]<- alldatawas$inhbrackw2gross[alldatawas$inhbrackw2gross>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


alldatawas$inhtotalw2gross <- alldatawas$inh5yw2grossupd + alldatawas$inhbrackw2grossupd

summary(alldatawas$inhtotalw2gross)
summary(alldatawas$inhtotalw2gross[alldatawas$inhtotalw2gross>0])
NROW(alldatawas$inhtotalw2gross[alldatawas$inhtotalw2gross>0])

#WAVE 3 - INHERITANCES IN THE LAST 2 YEARS

alldatawas$inh5yw3gross <- (ifelse(alldatawas$IValW3gross>0, (alldatawas$IValW3gross), 0)) + (ifelse(alldatawas$IVal2W3gross>0, alldatawas$IVal2W3gross, 0)) + (ifelse(alldatawas$IVal3W3gross>0, alldatawas$IVal3W3gross, 0))

alldatawas$inh5yw3gross <- (ifelse((alldatawas$IValW3gross>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$IValW3gross, 0)) + (ifelse((alldatawas$IVal2W3gross>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$IVal2W3gross, 0)) + (ifelse((alldatawas$IVal3W3gross>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$IVal3W3gross, 0))

summary(alldatawas$inh5yw3gross)
summary(alldatawas$inh5yw3gross[alldatawas$inh5yw3gross>0])
NROW(alldatawas$inh5yw3gross[alldatawas$inh5yw3gross>0])

#WAVE 3 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw3gross <- ifelse((alldatawas$bracked1inhW3>0), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0), alldatawas$bracked3inhW3, 0))

alldatawas$inhbrackw3gross <- ifelse((alldatawas$bracked1inhW3gross>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$bracked1inhW3gross, 0) + (ifelse((alldatawas$bracked2inhW3gross>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$bracked2inhW3gross, 0)) + (ifelse((alldatawas$bracked3inhW3gross>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$bracked3inhW3gross, 0))

summary(alldatawas$inhbrackw3gross)
summary(alldatawas$inhbrackw3gross[alldatawas$inhbrackw3gross>0])
NROW(alldatawas$inhbrackw3gross[alldatawas$inhbrackw3gross>0])

alldatawas$inhtotalw3gross <- alldatawas$inh5yw3gross + alldatawas$inhbrackw3gross

summary(alldatawas$inhtotalw3gross)
summary(alldatawas$inhtotalw3gross[alldatawas$inhtotalw3gross>0])
NROW(alldatawas$inhtotalw3gross[alldatawas$inhtotalw3gross>0])


############################## GIFTS #######################

#WAVE 1

alldatawas$giftotalw1 <- (ifelse(alldatawas$IGifvalW1>0, alldatawas$IGifvalW1, 0))+ alldatawas$bracked1giftW1 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw1)
NROW(alldatawas$giftotalw1[alldatawas$giftotalw1>0])

#And we update gifts from wave 1 to wave 3

alldatawas$giftotalw1upd <- alldatawas$giftotalw1
alldatawas$giftotalw1upd[alldatawas$giftotalw1>0]<- alldatawas$giftotalw1[alldatawas$giftotalw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1 GROSS

alldatawas$inhgiftw1gross <- alldatawas$inhtotalw1gross + alldatawas$giftotalw1upd
summary(alldatawas$inhgiftw1gross[alldatawas$inhgiftw1gross>0])
NROW(alldatawas$inhgiftw1gross[alldatawas$inhgiftw1gross>0])

#WAVE 2 #NOTE THAT IN WAVE TWO, RESPONDENT REPORTS ONLY THOSE RECEIVE IN THE LAST TWO YEARS, SO -UNLIKE INHERITANCES, WHO ARE ASKED IN THE LAST 5 YEARS - THERE IS NO OVERLAPPING WITH THE POSSIBLE GIFTS REPORTED IN WAVE 1 TWO YEARS BEFORE

alldatawas$giftotalw2 <- (ifelse(alldatawas$IgifvalW2>0, alldatawas$IgifvalW2, 0))+ alldatawas$bracked1giftW2 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw2)
NROW(alldatawas$giftotalw2[alldatawas$giftotalw2>0])

#And we update gifts from wave 2 to wave 3

alldatawas$giftotalw2upd <- alldatawas$giftotalw2
alldatawas$giftotalw2upd[alldatawas$giftotalw2>0]<- alldatawas$giftotalw2[alldatawas$giftotalw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1


alldatawas$inhgiftw2gross <- alldatawas$inhtotalw2gross + alldatawas$giftotalw2upd
summary(alldatawas$inhgiftw2gross[alldatawas$inhgiftw2gross>0])
NROW(alldatawas$inhgiftw2gross[alldatawas$inhgiftw2gross>0])

#WAVE 3

alldatawas$giftotalw3 <- (ifelse(alldatawas$DVGiftAnnualw3>0, alldatawas$DVGiftAnnualw3, 0))

alldatawas$giftotalw3 <- (ifelse(alldatawas$giftotalw3==0, alldatawas$bracked1giftW3, alldatawas$giftotalw3))

# We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0])
NROW(alldatawas$giftotalw3[alldatawas$giftotalw3>0])

summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Imputed"])
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Not imputed"])

alldatawas$inhgiftw3gross <- alldatawas$inhtotalw3gross + alldatawas$giftotalw3
summary(alldatawas$inhgiftw3gross[alldatawas$inhgiftw3gross>0])
NROW(alldatawas$inhgiftw3gross[alldatawas$inhgiftw3gross>0])


################## FINALLY, LET US ACCUMULATE ALL INHERITANCES AND GIFTS FROM THE THREE WAVES ##########

######(NOTE THAT THIS INCLUDES INHERITANCES ACQUIRED EVER IN LIFE, FOR THAT WAS ASKED IN WAVE 1)#######

alldatawas$totalinhbaselinegross <- alldatawas$inhgiftw1gross + alldatawas$inhgiftw2gross + alldatawas$inhgiftw3gross

NROW(alldatawas$totalinhbaselinegross[alldatawas$totalinhbaselinegross>0])

##### ANOTHER OPTION IS TO SEPARATE INHERITANCES AND GIFTS ####

alldatawas$totalgiftonlygross <- alldatawas$giftotalw1upd + alldatawas$giftotalw2upd + alldatawas$giftotalw3

NROW(alldatawas$totalgiftonlygross[alldatawas$totalgiftonlygross>0])

alldatawas$totalinhonlygross <- alldatawas$inhtotalw1gross + alldatawas$inhtotalw2gross + alldatawas$inhtotalw3gross

NROW(alldatawas$totalinhonlygross[alldatawas$totalinhonlygross>0])



##### NOW WITH IMPUTATIONS #### #WE DO NOT FILTER OUT WITH THE IMPUTATION FLAG ###

#WAVE 1 -INHERITANCES OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)
#In this ones we can not exclude those ones from the spouse or partner, we don't have that info.

alldatawas$inhpastw1gross<- ifelse((alldatawas$pretaxinhlifeW1upd >0), alldatawas$pretaxinhlifeW1upd , 0) + (ifelse((alldatawas$pretaxinhlife2W1upd >0), alldatawas$pretaxinhlife2W1upd , 0)) + (ifelse(alldatawas$pretaxinhlife3W1upd >0, alldatawas$pretaxinhlife3W1upd , 0))

summary(alldatawas$inhpastw1gross)
summary(alldatawas$inhpastw1gross[alldatawas$inhpastw1gross>0])
NROW(alldatawas$inhpastw1gross[alldatawas$inhpastw1gross>0])

#WAVE 1 -  BRACKETED INHERITANCES EVER OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhbrackpastw1gross <- ifelse(alldatawas$pretaxinhlifebW1upd>0, alldatawas$pretaxinhlifebW1upd, 0) + (ifelse(alldatawas$pretaxinhlife2bW1upd>0, alldatawas$pretaxinhlife2bW1upd, 0)) + (ifelse(alldatawas$pretaxinhlife3bW1upd>0, alldatawas$pretaxinhlife3bW1upd, 0))

summary(alldatawas$inhbrackpastw1gross)
summary(alldatawas$inhbrackpastw1gross[alldatawas$inhbrackpastw1gross>0])
NROW(alldatawas$inhbrackpastw1gross[alldatawas$inhbrackpastw1gross>0])


#WAVE 1 - INHERITANCES IN THE LAST 5 YEARS
#(Note we exclude those obtained from the previous partner, which reduces our cases from  512 to 494)

alldatawas$inh5yw1gross <-NULL
alldatawas$inh5yw1gross <- ifelse((alldatawas$IValW1gross>0), alldatawas$IValW1gross, 0) + (ifelse((alldatawas$IVal2W1gross>0), alldatawas$IVal2W1gross, 0)) + (ifelse(alldatawas$IVal3W1gross>0, alldatawas$IVal3W1gross, 0))


alldatawas$inh5yw1gross <-NULL
alldatawas$inh5yw1gross <- (ifelse((alldatawas$IValW1gross>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$IValW1gross, 0)) + (ifelse((alldatawas$IVal2W1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$IVal2W1gross, 0)) + (ifelse((alldatawas$IVal3W1gross>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)"), alldatawas$IVal3W1gross, 0))

summary(alldatawas$inh5yw1gross)
summary(alldatawas$inh5yw1gross[alldatawas$inh5yw1gross>0])
NROW(alldatawas$inh5yw1gross[alldatawas$inh5yw1gross>0])

#NOTE WE HAVE TO UPDATE ALL OF THAT (OBTAINED IN 2008 OR 5 YEAR BEFORE) TO 2012 MONEY(IF WE USE WEALTH FROM THE THIRD WAVE). WE HAD NOT DONE THAT YES, ONLY THE ONES OBTAINED PRIOR TO 5 YEARS BEFORE THE SURVEY

alldatawas$inh5yw1grossupd <- alldatawas$inh5yw1gross
alldatawas$inh5yw1grossupd[alldatawas$inh5yw1gross>0]<- alldatawas$inh5yw1gross[alldatawas$inh5yw1gross>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#WAVE 1 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

#Reduced from 33 to 27 in the bracketed

alldatawas$inhbrackw1gross <- NULL
alldatawas$inhbrackw1gross <- ifelse((alldatawas$bracked1inhW1gross>0 ), alldatawas$bracked1inhW1gross, 0) + (ifelse((alldatawas$bracked2inhW1gross>0), alldatawas$bracked2inhW1gross, 0)) + (ifelse(alldatawas$bracked3inhW1gross>0, alldatawas$bracked3inhW1gross, 0))


alldatawas$inhbrackw1gross <- ifelse((alldatawas$bracked1inhW1gross>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$bracked1inhW1gross, 0) + (ifelse((alldatawas$bracked2inhW1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked2inhW1gross, 0)) + (ifelse((alldatawas$bracked3inhW1gross>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$bracked3inhW1gross, 0))

summary(alldatawas$inhbrackw1gross)
summary(alldatawas$inhbrackw1gross[alldatawas$inhbrackw1gross>0])
NROW(alldatawas$inhbrackw1gross[alldatawas$inhbrackw1gross>0])

alldatawas$inhbrackw1grossupd <- alldatawas$inhbrackw1gross
alldatawas$inhbrackw1grossupd[alldatawas$inhbrackw1gross>0]<- alldatawas$inhbrackw1gross[alldatawas$inhbrackw1gross>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)


alldatawas$inhtotalw1gross <- alldatawas$inhpastw1gross + alldatawas$inh5yw1grossupd + alldatawas$inhbrackpastw1gross + alldatawas$inhbrackw1grossupd

summary(alldatawas$inhtotalw1gross)
summary(alldatawas$inhtotalw1gross[alldatawas$inhtotalw1gross>0])
NROW(alldatawas$inhtotalw1gross[alldatawas$inhtotalw1gross>0])


#WAVE 2 - INHERITANCES IN THE LAST 5 YEARS

alldatawas$inh5yw2gross <- (ifelse(alldatawas$IValW2gross>0, alldatawas$IValW2gross, 0)) + (ifelse(alldatawas$IVal2W2gross>0, alldatawas$IVal2W2gross, 0)) + (ifelse(alldatawas$IVal3W2gross>0, alldatawas$IVal3W2gross, 0))


alldatawas$inh5yw2gross <- (ifelse((alldatawas$IValW2gross>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$IValW2gross, 0)) + (ifelse((alldatawas$IVal2W2gross>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$IVal2W2gross, 0)) + (ifelse((alldatawas$IVal3W2gross>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$IVal3W2gross, 0))

summary(alldatawas$inh5yw2gross)
summary(alldatawas$inh5yw2gross[alldatawas$inh5yw2gross>0])
NROW(alldatawas$inh5yw2gross[alldatawas$inh5yw2gross>0])

alldatawas$inh5yw2grossupd <- alldatawas$inh5yw2gross
alldatawas$inh5yw2grossupd[alldatawas$inh5yw2gross>0]<- alldatawas$inh5yw2gross[alldatawas$inh5yw2gross>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


#WAVE 2 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw2gross <- ifelse(alldatawas$bracked1inhW2gross>0, alldatawas$bracked1inhW2gross, 0) + (ifelse(alldatawas$bracked2inhW2gross>0, alldatawas$bracked2inhW2gross, 0)) + (ifelse(alldatawas$bracked3inhW2gross>0, alldatawas$bracked3inhW2gross, 0))

alldatawas$inhbrackw2gross <- ifelse((alldatawas$bracked1inhW2gross>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$bracked1inhW2gross, 0) + (ifelse((alldatawas$bracked2inhW2gross>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$bracked2inhW2gross, 0)) + (ifelse((alldatawas$bracked3inhW2gross>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$bracked3inhW2gross, 0))

summary(alldatawas$inhbrackw2gross)
summary(alldatawas$inhbrackw2gross[alldatawas$inhbrackw2gross>0])
NROW(alldatawas$inhbrackw2gross[alldatawas$inhbrackw2gross>0])

alldatawas$inhbrackw2grossupd <- alldatawas$inhbrackw2gross
alldatawas$inhbrackw2grossupd[alldatawas$inhbrackw2gross>0]<- alldatawas$inhbrackw2gross[alldatawas$inhbrackw2gross>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


alldatawas$inhtotalw2gross <- alldatawas$inh5yw2grossupd + alldatawas$inhbrackw2grossupd

summary(alldatawas$inhtotalw2gross)
summary(alldatawas$inhtotalw2gross[alldatawas$inhtotalw2gross>0])
NROW(alldatawas$inhtotalw2gross[alldatawas$inhtotalw2gross>0])

#WAVE 3 - INHERITANCES IN THE LAST 2 YEARS

alldatawas$inh5yw3gross <- (ifelse(alldatawas$IValW3gross>0, (alldatawas$IValW3gross), 0)) + (ifelse(alldatawas$IVal2W3gross>0, alldatawas$IVal2W3gross, 0)) + (ifelse(alldatawas$IVal3W3gross>0, alldatawas$IVal3W3gross, 0))

alldatawas$inh5yw3gross <- (ifelse((alldatawas$IValW3gross>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$IValW3gross, 0)) + (ifelse((alldatawas$IVal2W3gross>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$IVal2W3gross, 0)) + (ifelse((alldatawas$IVal3W3gross>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$IVal3W3gross, 0))

summary(alldatawas$inh5yw3gross)
summary(alldatawas$inh5yw3gross[alldatawas$inh5yw3gross>0])
NROW(alldatawas$inh5yw3gross[alldatawas$inh5yw3gross>0])

#WAVE 3 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw3gross <- ifelse((alldatawas$bracked1inhW3>0), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0), alldatawas$bracked3inhW3, 0))

alldatawas$inhbrackw3gross <- ifelse((alldatawas$bracked1inhW3gross>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$bracked1inhW3gross, 0) + (ifelse((alldatawas$bracked2inhW3gross>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$bracked2inhW3gross, 0)) + (ifelse((alldatawas$bracked3inhW3gross>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$bracked3inhW3gross, 0))

summary(alldatawas$inhbrackw3gross)
summary(alldatawas$inhbrackw3gross[alldatawas$inhbrackw3gross>0])
NROW(alldatawas$inhbrackw3gross[alldatawas$inhbrackw3gross>0])

alldatawas$inhtotalw3gross <- alldatawas$inh5yw3gross + alldatawas$inhbrackw3gross

summary(alldatawas$inhtotalw3gross)
summary(alldatawas$inhtotalw3gross[alldatawas$inhtotalw3gross>0])
NROW(alldatawas$inhtotalw3gross[alldatawas$inhtotalw3gross>0])


############################## GIFTS #######################

#WAVE 1

alldatawas$giftotalw1 <- (ifelse(alldatawas$IGifvalW1>0, alldatawas$IGifvalW1, 0))+ alldatawas$bracked1giftW1 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw1)
NROW(alldatawas$giftotalw1[alldatawas$giftotalw1>0])

#And we update gifts from wave 1 to wave 3

alldatawas$giftotalw1upd <- alldatawas$giftotalw1
alldatawas$giftotalw1upd[alldatawas$giftotalw1>0]<- alldatawas$giftotalw1[alldatawas$giftotalw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1 GROSS

alldatawas$inhgiftw1gross <- alldatawas$inhtotalw1gross + alldatawas$giftotalw1upd
summary(alldatawas$inhgiftw1gross[alldatawas$inhgiftw1gross>0])
NROW(alldatawas$inhgiftw1gross[alldatawas$inhgiftw1gross>0])

#WAVE 2 #NOTE THAT IN WAVE TWO, RESPONDENT REPORTS ONLY THOSE RECEIVE IN THE LAST TWO YEARS, SO -UNLIKE INHERITANCES, WHO ARE ASKED IN THE LAST 5 YEARS - THERE IS NO OVERLAPPING WITH THE POSSIBLE GIFTS REPORTED IN WAVE 1 TWO YEARS BEFORE

alldatawas$giftotalw2 <- (ifelse(alldatawas$IgifvalW2>0, alldatawas$IgifvalW2, 0))+ alldatawas$bracked1giftW2 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw2)
NROW(alldatawas$giftotalw2[alldatawas$giftotalw2>0])

#And we update gifts from wave 2 to wave 3

alldatawas$giftotalw2upd <- alldatawas$giftotalw2
alldatawas$giftotalw2upd[alldatawas$giftotalw2>0]<- alldatawas$giftotalw2[alldatawas$giftotalw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1


alldatawas$inhgiftw2gross <- alldatawas$inhtotalw2gross + alldatawas$giftotalw2upd
summary(alldatawas$inhgiftw2gross[alldatawas$inhgiftw2gross>0])
NROW(alldatawas$inhgiftw2gross[alldatawas$inhgiftw2gross>0])

#WAVE 3

alldatawas$giftotalw3 <- (ifelse(alldatawas$DVGiftAnnualw3>0, alldatawas$DVGiftAnnualw3, 0))

alldatawas$giftotalw3 <- (ifelse(alldatawas$giftotalw3==0, alldatawas$bracked1giftW3, alldatawas$giftotalw3))

# We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0])
NROW(alldatawas$giftotalw3[alldatawas$giftotalw3>0])

summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Imputed"])
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Not imputed"])

alldatawas$inhgiftw3gross <- alldatawas$inhtotalw3gross + alldatawas$giftotalw3
summary(alldatawas$inhgiftw3gross[alldatawas$inhgiftw3gross>0])
NROW(alldatawas$inhgiftw3gross[alldatawas$inhgiftw3gross>0])



#### AGGREGATION OF TRUSTS ASWELL #####

#Let's first add bracked trusts from Wave 2 (no data for Wave 1 in terms of amounts) if the person that settles the trust is not Spouse or partner (including ex)

summary(alldatawas$bracked1trustW2)
summary(alldatawas$bracked1trustW2[alldatawas$bracked1trustW2>0])


table(alldatawas$IwhoW2)
table(alldatawas$Iwho2W2)
table(alldatawas$Iwho3W2)


alldatawas$brackedtotaltrustW2 <- NULL

#We add all the bracked trusts provided the person is not spouse/parter, brother, friend or unknown relative, so only intergenerational transmissions have been included.

alldatawas$brackedtotaltrustW2 <- (ifelse((alldatawas$bracked1trustW2>0 & !is.na(alldatawas$bracked1trustW2) & alldatawas$IwhoW2 != "spouse or partner (including ex)" & alldatawas$IwhoW2 != "brother/sister" &alldatawas$IwhoW2 != "other relative" & alldatawas$IwhoW2 != "non relatives (friend/neighbour)"), alldatawas$bracked1trustW2, 0)) + (ifelse((alldatawas$bracked2trustW2>0 & !is.na(alldatawas$bracked2trustW2) & alldatawas$Iwho2W2 != "spouse or partner (including ex)" & alldatawas$Iwho2W2 != "brother/sister" &alldatawas$Iwho2W2 != "other relative" & alldatawas$Iwho2W2 != "non relatives (friend/neighbour)"), alldatawas$bracked2trustW2, 0)) + (ifelse((alldatawas$bracked3trustW2>0 & !is.na(alldatawas$bracked3trustW2) & alldatawas$Iwho3W2 != "spouse or partner (including ex)" & alldatawas$Iwho3W2 != "brother/sister" &alldatawas$Iwho3W2 != "other relative" & alldatawas$Iwho3W2 != "non relatives (friend/neighbour)"), alldatawas$bracked3trustW2, 0))

summary(alldatawas$brackedtotaltrustW2)
summary(alldatawas$brackedtotaltrustW2[alldatawas$brackedtotaltrustW2>0])


##NOW WITH W3

summary(alldatawas$bracked1trustW3)
summary(alldatawas$bracked1trustW3[alldatawas$bracked1trustW3>0])


table(alldatawas$IWhoW3)
table(alldatawas$IWho2W3)
table(alldatawas$IWho3W3)


alldatawas$brackedtotaltrustW3 <- NULL

#We add all the bracked trusts provided the person is not spouse/parter, brother, friend or unknown relative, so only intergenerational transmissions have been included.

alldatawas$brackedtotaltrustW3 <- (ifelse((alldatawas$bracked1trustW3>0 & !is.na(alldatawas$bracked1trustW3) & alldatawas$IWhoW3 != "Spouse or partner" & alldatawas$IWhoW3 != "Brothersister" &alldatawas$IWhoW3 != "Other relative" & alldatawas$IWhoW3 != "Non relatives"), alldatawas$bracked1trustW3, 0)) + (ifelse((alldatawas$bracked2trustW3>0 & !is.na(alldatawas$bracked2trustW3) & alldatawas$IWho2W3 != "Spouse or partner" & alldatawas$IWho2W3 != "Brothersister" & alldatawas$IWho2W3 != "Other relative" & alldatawas$IWho2W3 != "Non relatives"), alldatawas$bracked2trustW3, 0)) + (ifelse((alldatawas$bracked3trustW3>0 & !is.na(alldatawas$bracked3trustW3) & alldatawas$IWho3W3 != "Spouse or partner" & alldatawas$IWho3W3 != "Brothersister" &alldatawas$IWho3W3 != "Other relative" & alldatawas$IWho3W3 != "Non relatives"), alldatawas$bracked3trustW3, 0))

summary(alldatawas$brackedtotaltrustW3)
summary(alldatawas$brackedtotaltrustW3[alldatawas$brackedtotaltrustW3>0])

### LET'S AGGREGATE TOTAL TRUSTS

#They do not seem to be so correlated
cor(alldatawas$brackedtotaltrustW2, alldatawas$brackedtotaltrustW3)
table(alldatawas$brackedtotaltrustW2, alldatawas$brackedtotaltrustW3)

#So we add them provided they are not equal...

NROW (alldatawas$brackedtotaltrustW3[alldatawas$brackedtotaltrustW3>0])
NROW (alldatawas$brackedtotaltrustW2[alldatawas$brackedtotaltrustW2>0])
NROW (alldatawas$brackedtotaltrustW2[alldatawas$brackedtotaltrustW2>0 & alldatawas$brackedtotaltrustW3==0]) #We add only the ones in W2 that do not have a value in W3

alldatawas$brackedtotalupdtrust <- alldatawas$brackedtotaltrustW3

alldatawas$brackedtotalupdtrust[alldatawas$brackedtotaltrustW2>0 & alldatawas$brackedtotaltrustW3==0] <- alldatawas$brackedtotalupdtrust [alldatawas$brackedtotaltrustW2>0 & alldatawas$brackedtotaltrustW3==0] + 1.1*alldatawas$brackedtotaltrustW2[alldatawas$brackedtotaltrustW2>0 & alldatawas$brackedtotaltrustW3==0] #Only add wave two when wave 3 is 0. Also we update wave 2 to wave 3.

NROW (alldatawas$brackedtotalupdtrust[alldatawas$brackedtotalupdtrust>0])


################## FINALLY, LET US ACCUMULATE ALL INHERITANCES AND GIFTS FROM THE THREE WAVES ##########

######(NOTE THAT THIS INCLUDES INHERITANCES ACQUIRED EVER IN LIFE, FOR THAT WAS ASKED IN WAVE 1)#######

alldatawas$totalinhgrossinput <- alldatawas$inhgiftw1gross + alldatawas$inhgiftw2gross + alldatawas$inhgiftw3gross

NROW(alldatawas$totalinhgrossinput[alldatawas$totalinhgrossinput>0])


##### ANOTHER OPTION IS TO SEPARATE INHERITANCES AND GIFTS ####

alldatawas$totalgiftonlygrossinput <- alldatawas$giftotalw1upd + alldatawas$giftotalw2upd + alldatawas$giftotalw3

NROW(alldatawas$totalgiftonlygrossinput[alldatawas$totalgiftonlygrossinput>0])

alldatawas$totalinhonlygrossinput <- alldatawas$inhtotalw1gross + alldatawas$inhtotalw2gross + alldatawas$inhtotalw3gross

NROW(alldatawas$totalinhonlygrossinput[alldatawas$totalinhonlygrossinput>0])



###### WE UPDATE HERE THE VARIABLES WE HAVE FOR THE BUSINESS WEALTH ######

#### LET'S US ADD THE NET MARKET ACTUAL VALUE OF THE BUSINESSES #### "If you sold your business/your share in this business today, including any debts or liabilities, about how much would you get? Please include the value of financial assets, accounts receivable, inventories, land, property, machinery, equipment, customer lists and intangible assets"

summary(alldatawas$Bval1W3[alldatawas$Bval1W3>0])
bv <- mean(alldatawas$Bval1W3[alldatawas$Bval1W3>5000000])


alldatawas$bracked1busW3 <- 0
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "Less than \xa3100 "] <-  50
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa3100 to \xa39,999"] <-  4950
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa310,000 to \xa349,999"] <-  30000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa350,000 to \xa399,999"] <-  75000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa3100,000 to \xa3249,999"] <-  175000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa3250,000 to \xa3499,999"] <-  375000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa3500,000 to \xa3999,999"] <-  750000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa31,000,000 to \xa31,999,999"] <-  1500000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa32,000,000 to \xa34,999,999"] <-  3500000
alldatawas$bracked1busW3[alldatawas$BvalB1W3== "\xa35 million or more"] <-  bv

summary(alldatawas$bracked1busW3)

summary(alldatawas$Bval2W3[alldatawas$Bval2W3>0])
bv <- mean(alldatawas$Bval2W3[alldatawas$Bval2W3>5000000])


alldatawas$bracked2busW3 <- 0
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "Less than \xa3100 "] <-  50
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa3100 to \xa39,999"] <-  4950
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa310,000 to \xa349,999"] <-  30000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa350,000 to \xa399,999"] <-  75000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa3100,000 to \xa3249,999"] <-  175000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa3250,000 to \xa3499,999"] <-  375000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa3500,000 to \xa3999,999"] <-  750000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa31,000,000 to \xa31,999,999"] <-  1500000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa32,000,000 to \xa34,999,999"] <-  3500000
alldatawas$bracked2busW3[alldatawas$BvalB2W3== "\xa35 million or more"] <-  bv

summary(alldatawas$bracked2busW3)

summary(alldatawas$Bval3W3[alldatawas$Bval3W3>0])
bv <- mean(alldatawas$Bval3W3[alldatawas$Bval3W3>5000000])


alldatawas$bracked3busW3 <- 0
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "Less than \xa3100 "] <-  50
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa3100 to \xa39,999"] <-  4950
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa310,000 to \xa349,999"] <-  30000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa350,000 to \xa399,999"] <-  75000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa3100,000 to \xa3249,999"] <-  175000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa3250,000 to \xa3499,999"] <-  375000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa3500,000 to \xa3999,999"] <-  750000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa31,000,000 to \xa31,999,999"] <-  1500000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa32,000,000 to \xa34,999,999"] <-  3500000
alldatawas$bracked3busW3[alldatawas$BvalB3W3== "\xa35 million or more"] <-  bv

summary(alldatawas$bracked3busW3)


##### AND NOW WE CAN ADD ALL THE VALUE FROM THE BUSINESS WEALTH ####

alldatawas$totbuswealth <- ifelse(alldatawas$Bval1W3>0, alldatawas$Bval1W3, alldatawas$bracked1busW3) + (ifelse(alldatawas$Bval2W3>0, alldatawas$Bval2W3, alldatawas$bracked2busW3)) + (ifelse(alldatawas$Bval3W3>0, alldatawas$Bval3W3, alldatawas$bracked3busW3))

summary(alldatawas$totbuswealth)

NROW(alldatawas$totbuswealth[alldatawas$totbuswealth>0])





######### AGGREGATION AT THE HOUSEHOLD LEVEL IN THE NEXT STEP 3C###################

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step5c-gross-input-basic-new-data.rds")




