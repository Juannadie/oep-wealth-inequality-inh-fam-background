#now we aggregate inheritances for net values

library(foreign)
library(tidyverse)
library(stats)
options ("scipen"=100, "digits"=4)


alldatawas <- readRDS(file = "data_rds/WAS-After-v3-2-Step5a-net-aggregation-new-data.rds")

##########     AGGREGATION OF INHERITANCES (WITH BRACKETS MIDDLE VALUES ASSIGNED AND UPDATED  ) ##############

#WAVE 1 -INHERITANCES OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)
#In this ones we can not exclude those ones from the spouse or partner, we don't have that info.

alldatawas$inhpastw1grossadjusted <- ifelse((alldatawas$pretaxinhlifeW1adjustedupd >0 & alldatawas$flaginputlife1basic == 0), alldatawas$pretaxinhlifeW1adjustedupd , 0) + (ifelse((alldatawas$pretaxinhlife2W1adjustedupd >0 & alldatawas$flaginputlife2basic == 0), alldatawas$pretaxinhlife2W1adjustedupd , 0)) + (ifelse(alldatawas$pretaxinhlife3W1adjustedupd >0, alldatawas$pretaxinhlife3W1adjustedupd , 0))

summary(alldatawas$inhpastw1grossadjusted)
summary(alldatawas$inhpastw1grossadjusted[alldatawas$inhpastw1grossadjusted>0])
NROW(alldatawas$inhpastw1grossadjusted[alldatawas$inhpastw1grossadjusted>0])

#WAVE 1 -  BRACKETED INHERITANCES EVER OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhbrackpastw1grossadjusted <- ifelse(alldatawas$pretaxinhlifebW1adjustedupd>0, alldatawas$pretaxinhlifebW1adjustedupd, 0) + (ifelse(alldatawas$pretaxinhlife2bW1adjustedupd>0, alldatawas$pretaxinhlife2bW1adjustedupd, 0)) + (ifelse(alldatawas$pretaxinhlife3bW1adjustedupd>0, alldatawas$pretaxinhlife3bW1adjustedupd, 0))

summary(alldatawas$inhbrackpastw1grossadjusted)
summary(alldatawas$inhbrackpastw1grossadjusted[alldatawas$inhbrackpastw1grossadjusted>0])
NROW(alldatawas$inhbrackpastw1grossadjusted[alldatawas$inhbrackpastw1grossadjusted>0])


#WAVE 1 - INHERITANCES IN THE LAST 5 YEARS
#(Note we exclude those obtained from the previous partner, which reduces our cases from  512 to 494)

alldatawas$inh5yw1grossadjusted <-NULL
alldatawas$inh5yw1grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W1adjusted>0 & (alldatawas$flaginputrecent1basic != 1 | is.na(alldatawas$flaginputrecent1basic))), alldatawas$pretaxinhrecent1W1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W1adjusted>0 & alldatawas$flaginputrecent2basic == 0), alldatawas$pretaxinhrecent2W1adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent3W1adjusted>0, alldatawas$pretaxinhrecent3W1adjusted, 0))

table(alldatawas$flaginputrecent1basic[alldatawas$inh5yw1grossadjusted>0])
#Only 1 case is imputed, and it is never excluded
table(alldatawas$IWhoW1, alldatawas$flaginputrecent1basicgrossadjusted)

alldatawas$inh5yw1grossadjusted <-NULL
alldatawas$inh5yw1grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W1adjusted>0 & (alldatawas$flaginputrecent1basic != 1 | is.na(alldatawas$flaginputrecent1basic)) & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecent1W1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W1adjusted>0 & alldatawas$flaginputrecent2basic == 0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecent2W1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent3W1adjusted>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecent3W1adjusted, 0))

summary(alldatawas$inh5yw1grossadjusted)
summary(alldatawas$inh5yw1grossadjusted[alldatawas$inh5yw1grossadjusted>0])
NROW(alldatawas$inh5yw1grossadjusted[alldatawas$inh5yw1grossadjusted>0])

#NOTE WE HAVE TO UPDATE ALL OF THAT (OBTAINED IN 2008 OR 5 YEAR BEFORE) TO 2012 MONEY(IF WE USE WEALTH FROM THE THIRD WAVE). WE HAD NOT DONE THAT YES, ONLY THE ONES OBTAINED PRIOR TO 5 YEARS BEFORE THE SURVEY

alldatawas$inh5yw1grossadjustedupd <- alldatawas$inh5yw1grossadjusted
alldatawas$inh5yw1grossadjustedupd[alldatawas$inh5yw1grossadjusted>0]<- alldatawas$inh5yw1grossadjusted[alldatawas$inh5yw1grossadjusted>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#WAVE 1 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

#Reduced from 33 to 27 in the bracketed

alldatawas$inhbrackw1grossadjusted <- NULL
alldatawas$inhbrackw1grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW1adjusted>0 ), alldatawas$pretaxinhrecentbracked1inhW1adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW1adjusted>0), alldatawas$pretaxinhrecentbracked2inhW1adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecentbracked3inhW1adjusted>0, alldatawas$pretaxinhrecentbracked3inhW1adjusted, 0))


alldatawas$inhbrackw1grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW1adjusted>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked1inhW1adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW1adjusted>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked2inhW1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecentbracked3inhW1adjusted>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked3inhW1adjusted, 0))

summary(alldatawas$inhbrackw1grossadjusted)
summary(alldatawas$inhbrackw1grossadjusted[alldatawas$inhbrackw1grossadjusted>0])
NROW(alldatawas$inhbrackw1grossadjusted[alldatawas$inhbrackw1grossadjusted>0])

alldatawas$inhbrackw1grossadjustedupd <- alldatawas$inhbrackw1grossadjusted
alldatawas$inhbrackw1grossadjustedupd[alldatawas$inhbrackw1grossadjusted>0]<- alldatawas$inhbrackw1grossadjusted[alldatawas$inhbrackw1grossadjusted>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)


alldatawas$inhtotalw1grossadjusted <- alldatawas$inhpastw1grossadjusted + alldatawas$inh5yw1grossadjustedupd + alldatawas$inhbrackpastw1grossadjusted + alldatawas$inhbrackw1grossadjustedupd

summary(alldatawas$inhtotalw1grossadjusted)
summary(alldatawas$inhtotalw1grossadjusted[alldatawas$inhtotalw1grossadjusted>0])
NROW(alldatawas$inhtotalw1grossadjusted[alldatawas$inhtotalw1grossadjusted>0])


#WAVE 2 - INHERITANCES IN THE LAST 5 YEARS

alldatawas$inh5yw2grossadjusted <- (ifelse(alldatawas$pretaxinhrecent1W2adjusted>0, alldatawas$pretaxinhrecent1W2adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent2W2adjusted>0, alldatawas$pretaxinhrecent2W2adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent3W2adjusted>0, alldatawas$pretaxinhrecent3W2adjusted, 0))


alldatawas$inh5yw2grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W2adjusted>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecent1W2adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W2adjusted>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecent2W2adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent3W2adjusted>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecent3W2adjusted, 0))

summary(alldatawas$inh5yw2grossadjusted)
summary(alldatawas$inh5yw2grossadjusted[alldatawas$inh5yw2grossadjusted>0])
NROW(alldatawas$inh5yw2grossadjusted[alldatawas$inh5yw2grossadjusted>0])

alldatawas$inh5yw2grossadjustedupd <- alldatawas$inh5yw2grossadjusted
alldatawas$inh5yw2grossadjustedupd[alldatawas$inh5yw2grossadjusted>0]<- alldatawas$inh5yw2grossadjusted[alldatawas$inh5yw2grossadjusted>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


#WAVE 2 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw2grossadjusted <- ifelse(alldatawas$pretaxinhrecentbracked1inhW2adjusted>0, alldatawas$pretaxinhrecentbracked1inhW2adjusted, 0) + (ifelse(alldatawas$pretaxinhrecentbracked2inhW2adjusted>0, alldatawas$pretaxinhrecentbracked2inhW2adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecentbracked3inhW2adjusted>0, alldatawas$pretaxinhrecentbracked3inhW2adjusted, 0))

alldatawas$inhbrackw2grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW2adjusted>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked1inhW2adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW2adjusted>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked2inhW2adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecentbracked3inhW2adjusted>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked3inhW2adjusted, 0))

summary(alldatawas$inhbrackw2grossadjusted)
summary(alldatawas$inhbrackw2grossadjusted[alldatawas$inhbrackw2grossadjusted>0])
NROW(alldatawas$inhbrackw2grossadjusted[alldatawas$inhbrackw2grossadjusted>0])

alldatawas$inhbrackw2grossadjustedupd <- alldatawas$inhbrackw2grossadjusted
alldatawas$inhbrackw2grossadjustedupd[alldatawas$inhbrackw2grossadjusted>0]<- alldatawas$inhbrackw2grossadjusted[alldatawas$inhbrackw2grossadjusted>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


alldatawas$inhtotalw2grossadjusted <- alldatawas$inh5yw2grossadjustedupd + alldatawas$inhbrackw2grossadjustedupd

summary(alldatawas$inhtotalw2grossadjusted)
summary(alldatawas$inhtotalw2grossadjusted[alldatawas$inhtotalw2grossadjusted>0])
NROW(alldatawas$inhtotalw2grossadjusted[alldatawas$inhtotalw2grossadjusted>0])

#WAVE 3 - INHERITANCES IN THE LAST 2 YEARS

alldatawas$inh5yw3grossadjusted <- (ifelse(alldatawas$pretaxinhrecent1W3adjusted>0, (alldatawas$pretaxinhrecent1W3adjusted), 0)) + (ifelse(alldatawas$pretaxinhrecent2W3adjusted>0, alldatawas$pretaxinhrecent2W3adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent3W3adjusted>0, alldatawas$pretaxinhrecent3W3adjusted, 0))

alldatawas$inh5yw3grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W3adjusted>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$pretaxinhrecent1W3adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W3adjusted>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$pretaxinhrecent2W3adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent3W3adjusted>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$pretaxinhrecent3W3adjusted, 0))

summary(alldatawas$inh5yw3grossadjusted)
summary(alldatawas$inh5yw3grossadjusted[alldatawas$inh5yw3grossadjusted>0])
NROW(alldatawas$inh5yw3grossadjusted[alldatawas$inh5yw3grossadjusted>0])

#WAVE 3 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw3grossadjusted <- ifelse((alldatawas$bracked1inhW3>0), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0), alldatawas$bracked3inhW3, 0))

alldatawas$inhbrackw3grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW3adjusted>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$pretaxinhrecentbracked1inhW3adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW3adjusted>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$pretaxinhrecentbracked2inhW3adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecentbracked3inhW3adjusted>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$pretaxinhrecentbracked3inhW3adjusted, 0))

summary(alldatawas$inhbrackw3grossadjusted)
summary(alldatawas$inhbrackw3grossadjusted[alldatawas$inhbrackw3grossadjusted>0])
NROW(alldatawas$inhbrackw3grossadjusted[alldatawas$inhbrackw3grossadjusted>0])

alldatawas$inhtotalw3grossadjusted <- alldatawas$inh5yw3grossadjusted + alldatawas$inhbrackw3grossadjusted

summary(alldatawas$inhtotalw3grossadjusted)
summary(alldatawas$inhtotalw3grossadjusted[alldatawas$inhtotalw3grossadjusted>0])
NROW(alldatawas$inhtotalw3grossadjusted[alldatawas$inhtotalw3grossadjusted>0])


############################## GIFTS #######################

#WAVE 1

alldatawas$giftotalw1 <- (ifelse(alldatawas$IGifvalW1>0, alldatawas$IGifvalW1, 0))+ alldatawas$bracked1giftW1 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw1)
NROW(alldatawas$giftotalw1[alldatawas$giftotalw1>0])

#And we update gifts from wave 1 to wave 3

alldatawas$giftotalW1upd <- alldatawas$giftotalw1
alldatawas$giftotalw1upd[alldatawas$giftotalw1>0]<- alldatawas$giftotalw1[alldatawas$giftotalw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1 GROSS

alldatawas$inhgiftw1grossadjusted <- alldatawas$inhtotalw1grossadjusted + alldatawas$giftotalw1upd
summary(alldatawas$inhgiftw1grossadjusted[alldatawas$inhgiftw1grossadjusted>0])
NROW(alldatawas$inhgiftw1grossadjusted[alldatawas$inhgiftw1grossadjusted>0])

#WAVE 2 #NOTE THAT IN WAVE TWO, RESPONDENT REPORTS ONLY THOSE RECEIVE IN THE LAST TWO YEARS, SO -UNLIKE INHERITANCES, WHO ARE ASKED IN THE LAST 5 YEARS - THERE IS NO OVERLAPPING WITH THE POSSIBLE GIFTS REPORTED IN WAVE 1 TWO YEARS BEFORE

alldatawas$giftotalw2 <- (ifelse(alldatawas$IgifvalW2>0, alldatawas$IgifvalW2, 0))+ alldatawas$bracked1giftW2 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw2)
NROW(alldatawas$giftotalw2[alldatawas$giftotalw2>0])

#And we update gifts from wave 2 to wave 3

alldatawas$giftotalw2upd <- alldatawas$giftotalw2
alldatawas$giftotalw2upd[alldatawas$giftotalw2>0]<- alldatawas$giftotalw2[alldatawas$giftotalw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1


alldatawas$inhgiftw2grossadjusted <- alldatawas$inhtotalw2grossadjusted + alldatawas$giftotalw2upd
summary(alldatawas$inhgiftw2grossadjusted[alldatawas$inhgiftw2grossadjusted>0])
NROW(alldatawas$inhgiftw2grossadjusted[alldatawas$inhgiftw2grossadjusted>0])

#WAVE 3

alldatawas$giftotalw3 <- (ifelse(alldatawas$DVGiftAnnualw3>0, alldatawas$DVGiftAnnualw3, 0))

alldatawas$giftotalw3 <- (ifelse(alldatawas$giftotalw3==0, alldatawas$bracked1giftW3, alldatawas$giftotalw3))

# We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0])
NROW(alldatawas$giftotalw3[alldatawas$giftotalw3>0])

summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Imputed"])
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Not imputed"])

alldatawas$inhgiftw3grossadjusted <- alldatawas$inhtotalw3grossadjusted + alldatawas$giftotalw3
summary(alldatawas$inhgiftw3grossadjusted[alldatawas$inhgiftw3grossadjusted>0])
NROW(alldatawas$inhgiftw3grossadjusted[alldatawas$inhgiftw3grossadjusted>0])


################## FINALLY, LET US ACCUMULATE ALL INHERITANCES AND GIFTS FROM THE THREE WAVES ##########

######(NOTE THAT THIS INCLUDES INHERITANCES ACQUIRED EVER IN LIFE, FOR THAT WAS ASKED IN WAVE 1)#######

alldatawas$totalinhbaselinegrossadjusted <- alldatawas$inhgiftw1grossadjusted + alldatawas$inhgiftw2grossadjusted + alldatawas$inhgiftw3grossadjusted

NROW(alldatawas$totalinhbaselinegrossadjusted[alldatawas$totalinhbaselinegrossadjusted>0])

##### ANOTHER OPTION IS TO SEPARATE INHERITANCES AND GIFTS ####

alldatawas$totalgiftonlygrossadjusted <- alldatawas$giftotalw1upd + alldatawas$giftotalw2upd + alldatawas$giftotalw3

NROW(alldatawas$totalgiftonlygrossadjusted[alldatawas$totalgiftonlygrossadjusted>0])

alldatawas$totalinhonlygrossadjusted <- alldatawas$inhtotalw1grossadjusted + alldatawas$inhtotalw2grossadjusted + alldatawas$inhtotalw3grossadjusted

NROW(alldatawas$totalinhonlygrossadjusted[alldatawas$totalinhonlygrossadjusted>0])





##### NOW WITH IMPUTATIONS #### #WE DO NOT FILTER OUT WITH THE IMPUTATION FLAG ###

#WAVE 1 -INHERITANCES OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)
#In this ones we can not exclude those ones from the spouse or partner, we don't have that info.

alldatawas$inhpastw1grossadjusted<- ifelse((alldatawas$pretaxinhlifeW1adjustedupd >0), alldatawas$pretaxinhlifeW1adjustedupd , 0) + (ifelse((alldatawas$pretaxinhlife2W1adjustedupd >0), alldatawas$pretaxinhlife2W1adjustedupd , 0)) + (ifelse(alldatawas$pretaxinhlife3W1adjustedupd >0, alldatawas$pretaxinhlife3W1adjustedupd , 0))

summary(alldatawas$inhpastw1grossadjusted)
summary(alldatawas$inhpastw1grossadjusted[alldatawas$inhpastw1grossadjusted>0])
NROW(alldatawas$inhpastw1grossadjusted[alldatawas$inhpastw1grossadjusted>0])

#WAVE 1 -  BRACKETED INHERITANCES EVER OBTAINED IN THE PAST (PRIOR TO 5 YEARS BEFORE THE SURVEY)

alldatawas$inhbrackpastw1grossadjusted <- ifelse(alldatawas$pretaxinhlifebW1adjustedupd>0, alldatawas$pretaxinhlifebW1adjustedupd, 0) + (ifelse(alldatawas$pretaxinhlife2bW1adjustedupd>0, alldatawas$pretaxinhlife2bW1adjustedupd, 0)) + (ifelse(alldatawas$pretaxinhlife3bW1adjustedupd>0, alldatawas$pretaxinhlife3bW1adjustedupd, 0))

summary(alldatawas$inhbrackpastw1grossadjusted)
summary(alldatawas$inhbrackpastw1grossadjusted[alldatawas$inhbrackpastw1grossadjusted>0])
NROW(alldatawas$inhbrackpastw1grossadjusted[alldatawas$inhbrackpastw1grossadjusted>0])


#WAVE 1 - INHERITANCES IN THE LAST 5 YEARS
#(Note we exclude those obtained from the previous partner, which reduces our cases from  512 to 494)

alldatawas$inh5yw1grossadjusted <-NULL
alldatawas$inh5yw1grossadjusted <- ifelse((alldatawas$pretaxinhrecent1W1adjusted>0), alldatawas$pretaxinhrecent1W1adjusted, 0) + (ifelse((alldatawas$pretaxinhrecent2W1adjusted>0), alldatawas$pretaxinhrecent2W1adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent3W1adjusted>0, alldatawas$pretaxinhrecent3W1adjusted, 0))


alldatawas$inh5yw1grossadjusted <-NULL
alldatawas$inh5yw1grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W1adjusted>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecent1W1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W1adjusted>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecent2W1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent3W1adjusted>0 & alldatawas$IWho3W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecent3W1adjusted, 0))

summary(alldatawas$inh5yw1grossadjusted)
summary(alldatawas$inh5yw1grossadjusted[alldatawas$inh5yw1grossadjusted>0])
NROW(alldatawas$inh5yw1grossadjusted[alldatawas$inh5yw1grossadjusted>0])

#NOTE WE HAVE TO UPDATE ALL OF THAT (OBTAINED IN 2008 OR 5 YEAR BEFORE) TO 2012 MONEY(IF WE USE WEALTH FROM THE THIRD WAVE). WE HAD NOT DONE THAT YES, ONLY THE ONES OBTAINED PRIOR TO 5 YEARS BEFORE THE SURVEY

alldatawas$inh5yw1grossadjustedupd <- alldatawas$inh5yw1grossadjusted
alldatawas$inh5yw1grossadjustedupd[alldatawas$inh5yw1grossadjusted>0]<- alldatawas$inh5yw1grossadjusted[alldatawas$inh5yw1grossadjusted>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#WAVE 1 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

#Reduced from 33 to 27 in the bracketed

alldatawas$inhbrackw1grossadjusted <- NULL
alldatawas$inhbrackw1grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW1adjusted>0 ), alldatawas$pretaxinhrecentbracked1inhW1adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW1adjusted>0), alldatawas$pretaxinhrecentbracked2inhW1adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecentbracked3inhW1adjusted>0, alldatawas$pretaxinhrecentbracked3inhW1adjusted, 0))


alldatawas$inhbrackw1grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW1adjusted>0 & alldatawas$IWhoW1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked1inhW1adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW1adjusted>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked2inhW1adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecentbracked3inhW1adjusted>0 & alldatawas$IWho2W1 != "Spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked3inhW1adjusted, 0))

summary(alldatawas$inhbrackw1grossadjusted)
summary(alldatawas$inhbrackw1grossadjusted[alldatawas$inhbrackw1grossadjusted>0])
NROW(alldatawas$inhbrackw1grossadjusted[alldatawas$inhbrackw1grossadjusted>0])

alldatawas$inhbrackw1grossadjustedupd <- alldatawas$inhbrackw1grossadjusted
alldatawas$inhbrackw1grossadjustedupd[alldatawas$inhbrackw1grossadjusted>0]<- alldatawas$inhbrackw1grossadjusted[alldatawas$inhbrackw1grossadjusted>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)


alldatawas$inhtotalw1grossadjusted <- alldatawas$inhpastw1grossadjusted + alldatawas$inh5yw1grossadjustedupd + alldatawas$inhbrackpastw1grossadjusted + alldatawas$inhbrackw1grossadjustedupd

summary(alldatawas$inhtotalw1grossadjusted)
summary(alldatawas$inhtotalw1grossadjusted[alldatawas$inhtotalw1grossadjusted>0])
NROW(alldatawas$inhtotalw1grossadjusted[alldatawas$inhtotalw1grossadjusted>0])


#WAVE 2 - INHERITANCES IN THE LAST 5 YEARS

alldatawas$inh5yw2grossadjusted <- (ifelse(alldatawas$pretaxinhrecent1W2adjusted>0, alldatawas$pretaxinhrecent1W2adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent2W2adjusted>0, alldatawas$pretaxinhrecent2W2adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent3W2adjusted>0, alldatawas$pretaxinhrecent3W2adjusted, 0))


alldatawas$inh5yw2grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W2adjusted>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecent1W2adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W2adjusted>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecent2W2adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent3W2adjusted>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecent3W2adjusted, 0))

summary(alldatawas$inh5yw2grossadjusted)
summary(alldatawas$inh5yw2grossadjusted[alldatawas$inh5yw2grossadjusted>0])
NROW(alldatawas$inh5yw2grossadjusted[alldatawas$inh5yw2grossadjusted>0])

alldatawas$inh5yw2grossadjustedupd <- alldatawas$inh5yw2grossadjusted
alldatawas$inh5yw2grossadjustedupd[alldatawas$inh5yw2grossadjusted>0]<- alldatawas$inh5yw2grossadjusted[alldatawas$inh5yw2grossadjusted>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


#WAVE 2 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw2grossadjusted <- ifelse(alldatawas$pretaxinhrecentbracked1inhW2adjusted>0, alldatawas$pretaxinhrecentbracked1inhW2adjusted, 0) + (ifelse(alldatawas$pretaxinhrecentbracked2inhW2adjusted>0, alldatawas$pretaxinhrecentbracked2inhW2adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecentbracked3inhW2adjusted>0, alldatawas$pretaxinhrecentbracked3inhW2adjusted, 0))

alldatawas$inhbrackw2grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW2adjusted>0 & alldatawas$IwhoW2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked1inhW2adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW2adjusted>0 & alldatawas$Iwho2W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked2inhW2adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecentbracked3inhW2adjusted>0 & alldatawas$Iwho3W2 != "spouse or partner (including ex)"), alldatawas$pretaxinhrecentbracked3inhW2adjusted, 0))

summary(alldatawas$inhbrackw2grossadjusted)
summary(alldatawas$inhbrackw2grossadjusted[alldatawas$inhbrackw2grossadjusted>0])
NROW(alldatawas$inhbrackw2grossadjusted[alldatawas$inhbrackw2grossadjusted>0])

alldatawas$inhbrackw2grossadjustedupd <- alldatawas$inhbrackw2grossadjusted
alldatawas$inhbrackw2grossadjustedupd[alldatawas$inhbrackw2grossadjusted>0]<- alldatawas$inhbrackw2grossadjusted[alldatawas$inhbrackw2grossadjusted>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)


alldatawas$inhtotalw2grossadjusted <- alldatawas$inh5yw2grossadjustedupd + alldatawas$inhbrackw2grossadjustedupd

summary(alldatawas$inhtotalw2grossadjusted)
summary(alldatawas$inhtotalw2grossadjusted[alldatawas$inhtotalw2grossadjusted>0])
NROW(alldatawas$inhtotalw2grossadjusted[alldatawas$inhtotalw2grossadjusted>0])

#WAVE 3 - INHERITANCES IN THE LAST 2 YEARS

alldatawas$inh5yw3grossadjusted <- (ifelse(alldatawas$pretaxinhrecent1W3adjusted>0, (alldatawas$pretaxinhrecent1W3adjusted), 0)) + (ifelse(alldatawas$pretaxinhrecent2W3adjusted>0, alldatawas$pretaxinhrecent2W3adjusted, 0)) + (ifelse(alldatawas$pretaxinhrecent3W3adjusted>0, alldatawas$pretaxinhrecent3W3adjusted, 0))

alldatawas$inh5yw3grossadjusted <- (ifelse((alldatawas$pretaxinhrecent1W3adjusted>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$pretaxinhrecent1W3adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent2W3adjusted>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$pretaxinhrecent2W3adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecent3W3adjusted>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$pretaxinhrecent3W3adjusted, 0))

summary(alldatawas$inh5yw3grossadjusted)
summary(alldatawas$inh5yw3grossadjusted[alldatawas$inh5yw3grossadjusted>0])
NROW(alldatawas$inh5yw3grossadjusted[alldatawas$inh5yw3grossadjusted>0])

#WAVE 3 - BRACKETED INHERITANCES FROM THE LAST 5 YEARS - SUM

alldatawas$inhbrackw3grossadjusted <- ifelse((alldatawas$bracked1inhW3>0), alldatawas$bracked1inhW3, 0) + (ifelse((alldatawas$bracked2inhW3>0), alldatawas$bracked2inhW3, 0)) + (ifelse((alldatawas$bracked3inhW3>0), alldatawas$bracked3inhW3, 0))

alldatawas$inhbrackw3grossadjusted <- ifelse((alldatawas$pretaxinhrecentbracked1inhW3adjusted>0 & alldatawas$IWhoW3 != "Spouse or partner"), alldatawas$pretaxinhrecentbracked1inhW3adjusted, 0) + (ifelse((alldatawas$pretaxinhrecentbracked2inhW3adjusted>0 & alldatawas$IWho2W3 != "Spouse or partner"), alldatawas$pretaxinhrecentbracked2inhW3adjusted, 0)) + (ifelse((alldatawas$pretaxinhrecentbracked3inhW3adjusted>0 & alldatawas$IWho3W3 != "Spouse or partner"), alldatawas$pretaxinhrecentbracked3inhW3adjusted, 0))

summary(alldatawas$inhbrackw3grossadjusted)
summary(alldatawas$inhbrackw3grossadjusted[alldatawas$inhbrackw3grossadjusted>0])
NROW(alldatawas$inhbrackw3grossadjusted[alldatawas$inhbrackw3grossadjusted>0])

alldatawas$inhtotalw3grossadjusted <- alldatawas$inh5yw3grossadjusted + alldatawas$inhbrackw3grossadjusted

summary(alldatawas$inhtotalw3grossadjusted)
summary(alldatawas$inhtotalw3grossadjusted[alldatawas$inhtotalw3grossadjusted>0])
NROW(alldatawas$inhtotalw3grossadjusted[alldatawas$inhtotalw3grossadjusted>0])


############################## GIFTS #######################

#WAVE 1

alldatawas$giftotalw1 <- (ifelse(alldatawas$IGifvalW1>0, alldatawas$IGifvalW1, 0))+ alldatawas$bracked1giftW1 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw1)
NROW(alldatawas$giftotalw1[alldatawas$giftotalw1>0])

#And we update gifts from wave 1 to wave 3

alldatawas$giftotalw1upd <- alldatawas$giftotalw1
alldatawas$giftotalw1upd[alldatawas$giftotalw1>0]<- alldatawas$giftotalw1[alldatawas$giftotalw1>0]*1.14 #WE UPDATE USING THE 2008-2012 MULTIPLIER (1.14)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1 GROSS

alldatawas$inhgiftw1grossadjusted <- alldatawas$inhtotalw1grossadjusted + alldatawas$giftotalw1upd
summary(alldatawas$inhgiftw1grossadjusted[alldatawas$inhgiftw1grossadjusted>0])
NROW(alldatawas$inhgiftw1grossadjusted[alldatawas$inhgiftw1grossadjusted>0])

#WAVE 2 #NOTE THAT IN WAVE TWO, RESPONDENT REPORTS ONLY THOSE RECEIVE IN THE LAST TWO YEARS, SO -UNLIKE INHERITANCES, WHO ARE ASKED IN THE LAST 5 YEARS - THERE IS NO OVERLAPPING WITH THE POSSIBLE GIFTS REPORTED IN WAVE 1 TWO YEARS BEFORE

alldatawas$giftotalw2 <- (ifelse(alldatawas$IgifvalW2>0, alldatawas$IgifvalW2, 0))+ alldatawas$bracked1giftW2 # We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw2)
NROW(alldatawas$giftotalw2[alldatawas$giftotalw2>0])

#And we update gifts from wave 2 to wave 3

alldatawas$giftotalw2upd <- alldatawas$giftotalw2
alldatawas$giftotalw2upd[alldatawas$giftotalw2>0]<- alldatawas$giftotalw2[alldatawas$giftotalw2>0]*1.1 #WE UPDATE USING THE 2010-2012 MULTIPLIER (1.1)

#AND HERE IS THE TOTAL OF INHERITANCES AND GIFTS UPDATED FROM WAVE 1


alldatawas$inhgiftw2grossadjusted <- alldatawas$inhtotalw2grossadjusted + alldatawas$giftotalw2upd
summary(alldatawas$inhgiftw2grossadjusted[alldatawas$inhgiftw2grossadjusted>0])
NROW(alldatawas$inhgiftw2grossadjusted[alldatawas$inhgiftw2grossadjusted>0])

#WAVE 3

alldatawas$giftotalw3 <- (ifelse(alldatawas$DVGiftAnnualw3>0, alldatawas$DVGiftAnnualw3, 0))

alldatawas$giftotalw3 <- (ifelse(alldatawas$giftotalw3==0, alldatawas$bracked1giftW3, alldatawas$giftotalw3))

# We add GifVal only if value > 0 to avoid negative coding of other answers and only valid values.
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0])
NROW(alldatawas$giftotalw3[alldatawas$giftotalw3>0])

summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Imputed"])
summary(alldatawas$giftotalw3[alldatawas$giftotalw3>0 & alldatawas$ILGiftw3_iflag == "Not imputed"])

alldatawas$inhgiftw3grossadjusted <- alldatawas$inhtotalw3grossadjusted + alldatawas$giftotalw3
summary(alldatawas$inhgiftw3grossadjusted[alldatawas$inhgiftw3grossadjusted>0])
NROW(alldatawas$inhgiftw3grossadjusted[alldatawas$inhgiftw3grossadjusted>0])




################## FINALLY, LET US ACCUMULATE ALL INHERITANCES AND GIFTS FROM THE THREE WAVES ##########

######(NOTE THAT THIS INCLUDES INHERITANCES ACQUIRED EVER IN LIFE, FOR THAT WAS ASKED IN WAVE 1)#######

alldatawas$totalinhgrossadjustedinput <- alldatawas$inhgiftw1grossadjusted + alldatawas$inhgiftw2grossadjusted + alldatawas$inhgiftw3grossadjusted

NROW(alldatawas$totalinhgrossadjustedinput[alldatawas$totalinhgrossadjustedinput>0])


##### ANOTHER OPTION IS TO SEPARATE INHERITANCES AND GIFTS AND TRUSTS####

alldatawas$totalgiftonlygrossadjustedinput <- alldatawas$giftotalw1upd + alldatawas$giftotalw2upd + alldatawas$giftotalw3

NROW(alldatawas$totalgiftonlygrossadjustedinput[alldatawas$totalgiftonlygrossadjustedinput>0])

alldatawas$totalinhonlygrossadjustedinput <- alldatawas$inhtotalw1grossadjusted + alldatawas$inhtotalw2grossadjusted + alldatawas$inhtotalw3grossadjusted

NROW(alldatawas$totalinhonlygrossadjustedinput[alldatawas$totalinhonlygrossadjustedinput>0])








######### AGGREGATION AT THE HOUSEHOLD LEVEL ###################

saveRDS(alldatawas, file = "data_rds/WAS-After-v3-2-Step5b-gross-input-basic-adjusted-new-data.rds")




