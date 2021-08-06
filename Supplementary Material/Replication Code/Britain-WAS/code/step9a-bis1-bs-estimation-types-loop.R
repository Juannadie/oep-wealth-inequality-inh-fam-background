
library(Hmisc)
library(reldist)
library(tidyverse)
library(np)
library(IC2)
library(dplyr)

options(scipen=100, digits=12)

#LET US LOAD THE DATA

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/data_rds") #Set Working Directory # LAPTOP DIRECTORY

temp = list.files(pattern="WAS-After-Step-v2-IO-8b-bis1-imp*") #creates a list with all the personal files in the eu-silc directory directory
imps <- lapply(temp, readRDS) #reads the list (note, alphabetical order)

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code") #Set Working Directory # LAPTOP DIRECTORY


#START LOOP

for (i in 1:length(imps)) {
 dataukh <-  imps[[i]]


dataukh$wealthoriginal <- dataukh$wealth #

dataukh$wealth <- dataukh$wealthpredictexp #We use the value of wealth without using taking into account age




#WE CREATE TYPES FOR OCCUPATION

dataukh$types4 <- NA

#low level parental EDU --> Type 1
dataukh$types4 [dataukh$maxeduparent == 1] <- 1
#mid level parental EDU--> Type 2
dataukh$types4 [dataukh$maxeduparent == 2] <- 2
#mid-high level parental EDU--> Type 3
dataukh$types4 [dataukh$maxeduparent == 3] <- 3
#high-level parental EDU  --> Type 4
dataukh$types4 [dataukh$maxeduparent == 4] <- 4


table(dataukh$types4)
summary (dataukh$types4)



#### WE NOW MAKE THE TYPES OF INHERITANCE LEVELS##
## ONLY 6 TYPES, BASED ON INHERITANCES ####

dataukh$types6 <- NA

#no inheritance --> Type 1
dataukh$types6 [dataukh$eqinhcat == 1] <- 1
#no inheritance but expecting--> Type 2
dataukh$types6 [dataukh$eqinhcat == 2] <- 2
#inheritance 1q --> Type 3
dataukh$types6 [dataukh$eqinhcat == 3] <- 3
#inheritance 2q --> Type 4
dataukh$types6 [dataukh$eqinhcat == 4] <- 4
#inheritance 3q  --> Type 5
dataukh$types6 [dataukh$eqinhcat == 5] <- 5
#inheritance 4q  --> Type 6
dataukh$types6 [dataukh$eqinhcat == 6] <- 6

table(dataukh$types6)
summary (dataukh$types6)


#### NOW OCC AND INHERITANCES ####

#### NOW OCC AND INHERITANCES, 24 TYPES ####

dataukh$types24 <- NA

# --> Type 1
dataukh$types24 [dataukh$maxeduparent == "1" & dataukh$eqinhcat == 1] <- 1 #
# --> Type 2
dataukh$types24 [dataukh$maxeduparent == "1" & dataukh$eqinhcat == 2] <- 2 #
# --> Type 3
dataukh$types24 [dataukh$maxeduparent == "1" & dataukh$eqinhcat == 3] <- 3 #
# --> Type 4
dataukh$types24 [dataukh$maxeduparent == "1" & dataukh$eqinhcat == 4] <- 4 #
# --> Type 5
dataukh$types24 [dataukh$maxeduparent == "1" & dataukh$eqinhcat == 5] <- 5 #
# --> Type 6
dataukh$types24 [dataukh$maxeduparent == "1" & dataukh$eqinhcat == 6] <- 6 #
# --> Type 7
dataukh$types24 [dataukh$maxeduparent == "2" & dataukh$eqinhcat == 1] <- 7 #
# --> Type 8
dataukh$types24 [dataukh$maxeduparent == "2" & dataukh$eqinhcat == 2] <- 8 #
# --> Type 9
dataukh$types24 [dataukh$maxeduparent == "2" & dataukh$eqinhcat == 3] <- 9 #
# --> Type 10
dataukh$types24 [dataukh$maxeduparent == "2" & dataukh$eqinhcat == 4] <- 10 #
# --> Type 11
dataukh$types24 [dataukh$maxeduparent == "2" & dataukh$eqinhcat == 5] <- 11 #
# --> Type 12
dataukh$types24 [dataukh$maxeduparent == "2" & dataukh$eqinhcat == 6] <- 12 #
# --> Type 13
dataukh$types24 [dataukh$maxeduparent == "3" & dataukh$eqinhcat == 1] <- 13 #
# --> Type 14
dataukh$types24 [dataukh$maxeduparent == "3" & dataukh$eqinhcat == 2] <- 14 #
# --> Type 15
dataukh$types24 [dataukh$maxeduparent == "3" & dataukh$eqinhcat == 3] <- 15 #
# --> Type 16
dataukh$types24 [dataukh$maxeduparent == "3" & dataukh$eqinhcat == 4] <- 16 #
# --> Type 17
dataukh$types24 [dataukh$maxeduparent == "3" & dataukh$eqinhcat == 5] <- 17 #
# --> Type 18
dataukh$types24 [dataukh$maxeduparent == "3" & dataukh$eqinhcat == 6] <- 18 #
# --> Type 19
dataukh$types24 [dataukh$maxeduparent == "4" & dataukh$eqinhcat == 1] <- 19 #
# --> Type 20
dataukh$types24 [dataukh$maxeduparent == "4" & dataukh$eqinhcat == 2] <- 20 #
# --> Type 21
dataukh$types24 [dataukh$maxeduparent == "4" & dataukh$eqinhcat == 3] <- 21 #
# --> Type 22
dataukh$types24 [dataukh$maxeduparent == "4" & dataukh$eqinhcat == 4] <- 22 #
# --> Type 23
dataukh$types24 [dataukh$maxeduparent == "4" & dataukh$eqinhcat == 5] <- 23 #
# --> Type 24
dataukh$types24 [dataukh$maxeduparent == "4" & dataukh$eqinhcat == 6] <- 24 #

table(dataukh$types24)
summary (dataukh$types24)



#Let's try to obtain a bootstrap stimation of the Theil Index
theil4tboot <- vector()
theil4tIEboot <- vector()
share4tboot <- vector()

#Let's try to obtain a bootstrap stimation of the Theil Index
theil6tboot <- vector()
theil6tIEboot <- vector()
share6tboot <- vector()

#Let's try to obtain a bootstrap stimation of the Theil Index
theil24tboot <- vector()
theil24tIEboot <- vector()
share24tboot <- vector()

marginal_inh <- vector()
marginal_bk <- vector()

interactive_bk_inh <- vector()

shapley_inh <- vector()
shapley_bk <- vector()


for (n in 1:10)
{datasimpleukboot <- dataukh [sample(nrow(dataukh), replace=TRUE),]

#We obtain the ranking variable as the ranking (normalized to 1) within each type
datasimpleukboot4 <- datasimpleukboot %>%
  group_by(types4) %>%
  mutate(my_rank = (rank(wealth, ties.method = 'random')/length(wealth)))

#First we can create a simple data frame that would only uke concerned variables and therefore be perhaps faster with computation #####

#We include only the types, the rank, the weight (longitudinal W1-W3), the outcome variable and the ID. Recall in Step 1b we renamed the variable w1w3wgt as weight

datasimpleuk <- datasimpleukboot4[,c("wealth", "my_rank", "weight", "types4", "CaseW3")]

names (datasimpleuk) <- c("wealth", "rank", "weight", "types", "id")

attach(datasimpleuk)


#First we get the cross-validated bandwidth without weights

bw.noweight <- npregbw(xdat=rank, ydat=wealth)

#Now the cross-validated bandwidth with weights

weightmatrix <- as.matrix(datasimpleuk$weight)

w <- datasimpleuk$weight

txdat <- as.data.frame(datasimpleuk$rank)

y <- datasimpleuk$wealth

num.ydat <- datasimpleuk$wealth*datasimpleuk$weight

#ALL OF THE ESTIMATIONS OF OPTIMAL BANDWIDTH YIELD VERY CLOSE ESTIMATES

# Write an R function that returns the average leave-one-out sum of
# squared residuals for the local constant estimator based upon
# npksum(). This function accepts one argument and presumes that
# txdat and tydat have been defined already.

#AND NOW A POSSIBLE WAY OF ESTIMATING USING THE REGULAR FORMULA FOR KERNEL ESTIMATION, IN THIS CASE ADDING THE WEIGHTS ADDITIONALLY IN THE KERNEL FORMULA

ss3 <- function(h) {

  # Test for valid (non-negative) bandwidths - return infinite penalty
  # when this occurs

  if(min(h)<=0) {

    return(.Machine$double.xmax)

  } else {

    mhat <-  npksum(txdat,
                    tydat=y,
                    leave.one.out=TRUE,
                    bandwidth.divide=TRUE,
                    weights=weightmatrix,
                    bws=h)$ksum/
      npksum(txdat,
             leave.one.out=TRUE,
             bandwidth.divide=TRUE,
             weights=weightmatrix,
             bws=h)$ksum

    fv <- (sum(w*((y-mhat)^2))/sum(w)) #Weighted average error in the estimation

    return(fv)

  }

}

nlm.return.3 <- nlm(ss3, runif(length(txdat)))

# Bandwidths from nlm()

bw.cv.w.3 <- nlm.return.3$estimate
bandwidth4t <- bw.cv.w.3

#THE RESULTS WITH ALL THE WAYS OF ESTIMATING THE OPTIMAL BANDWITH (CROSS VALIDATION) ARE SIMILAR

### COMPUTE WITH WEIGHTS THE SMOOTHING, USING ONE OF THE OBTAINED BANDWIDTHS ############

fit.lc.1 <- npksum(txdat=rank, tydat=wealth, bws=bw.cv.w.3, weights=weightmatrix)$ksum/
  npksum(txdat=rank, bws=bw.cv.w.3, weights=weightmatrix)$ksum

plot(rank, wealth, xlab="Rank", ylab="Wealth")
plot (rank, fit.lc.1)

smoothnetwealth <- as.vector(unlist(fit.lc.1)[]) #We convert the non-parametric fit from list to vector

datasimpleuk <- cbind.data.frame (datasimpleuk, smoothnetwealth)

theil4tlistIE <- calcGEI (datasimpleuk$smoothnetwealth, w=datasimpleuk$weight, alpha = 0)

theil4tIEboot[n] <- as.numeric(theil4tlistIE$ineq$index)

theil4tIEscalar <- as.numeric(theil4tlistIE$ineq$index)

theil4tlist <- calcGEI (datasimpleuk$wealth, w=datasimpleuk$weight, alpha = 0)

theil4tboot[n] <- as.numeric(theil4tlist$ineq$index)

theil4tscalar <- as.numeric(theil4tlist$ineq$index)

share4tboot[n]<-  (theil4tscalar-theil4tIEscalar)/theil4tscalar


#Bootstrap Standard Error
theil4tbootvar <- var(theil4tboot)
theil4tbootmean <- mean (theil4tboot)
theil4tbootsd <- sd(theil4tboot)
theil4tsdup <- mean(theil4tboot) + 1.96*theil4tbootsd
theil4tsddown <- mean(theil4tboot) - 1.96*theil4tbootsd
#Confidence Interval
theil4tlowci <- quantile(theil4tboot, probs = 0.05)
theil4thighci <- quantile(theil4tboot, probs = 0.95)
indextheil4t <- cbind (theil4tbootmean, theil4tbootvar, theil4tbootsd, theil4tsdup, theil4tsddown, theil4tlowci, theil4thighci)

#Bootstrap Standard Error
theil4tIEbootvar <- var(theil4tIEboot)
theil4tIEbootmean <- mean (theil4tIEboot)
theil4tIEbootsd <- sd(theil4tIEboot)
theil4tIEsdup <- mean(theil4tIEboot) + 1.96*theil4tIEbootsd
theil4tIEsddown <- mean(theil4tIEboot) - 1.96*theil4tIEbootsd
#Confidence Interval
theil4tIElowci <- quantile(theil4tIEboot, probs = 0.05)
theil4tIEhighci <- quantile(theil4tIEboot, probs = 0.95)

indextheil4tIE <- cbind (theil4tIEbootmean, theil4tIEbootvar, theil4tIEbootsd, theil4tIEsdup, theil4tIEsddown, theil4tIElowci, theil4tIEhighci)

#Bootstrap Standard Error
share4tbootvar <- var(share4tboot)
share4tbootmean <- mean (share4tboot)
share4tbootsd <- sd(share4tboot)
share4tsdup <- mean(share4tboot) + 1.96*share4tbootsd
share4tsddown <- mean(share4tboot) - 1.96*share4tbootsd
#Confidence Interval
share4tlowci <- quantile(share4tboot, probs = 0.05)
share4thighci <- quantile(share4tboot, probs = 0.95)

indexshare4t <- cbind (share4tbootmean, share4tbootvar, share4tbootsd, share4tsdup, share4tsddown, share4tlowci, share4thighci)


detach(datasimpleuk)


#We obtain the ranking variable as the ranking (normalized to 1) within each type (6 types)

datasimpleukboot6 <- datasimpleukboot %>%
  group_by(types6) %>%
  mutate(my_rank = (rank(wealth, ties.method = 'random')/length(wealth)))

#First we can create a simple data frame that would only uke concerned variables and therefore be perhaps faster with computation #####

#We include only the types, the rank, the weight (longitudinal W1-W3), the outcome variable and the ID. Recall in Step 1b we renamed the variable w1w3wgt as weight

datasimpleuk <- datasimpleukboot6[,c("wealth", "my_rank", "weight", "types6", "CaseW3")]

names (datasimpleuk) <- c("wealth", "rank", "weight", "types", "id")

attach(datasimpleuk)


#First we get the cross-validated bandwidth without weights

bw.noweight <- npregbw(xdat=rank, ydat=wealth)

#Now the cross-validated bandwidth with weights

weightmatrix <- as.matrix(datasimpleuk$weight)

w <- datasimpleuk$weight

txdat <- as.data.frame(datasimpleuk$rank)

y <- datasimpleuk$wealth

num.ydat <- datasimpleuk$wealth*datasimpleuk$weight

#ALL OF THE ESTIMATIONS OF OPTIMAL BANDWIDTH YIELD VERY CLOSE ESTIMATES

# Write an R function that returns the average leave-one-out sum of
# squared residuals for the local constant estimator based upon
# npksum(). This function accepts one argument and presumes that
# txdat and tydat have been defined already.

#AND NOW A POSSIBLE WAY OF ESTIMATING USING THE REGULAR FORMULA FOR KERNEL ESTIMATION, IN THIS CASE ADDING THE WEIGHTS ADDITIONALLY IN THE KERNEL FORMULA

ss3 <- function(h) {

  # Test for valid (non-negative) bandwidths - return infinite penalty
  # when this occurs

  if(min(h)<=0) {

    return(.Machine$double.xmax)

  } else {

    mhat <-  npksum(txdat,
                    tydat=y,
                    leave.one.out=TRUE,
                    bandwidth.divide=TRUE,
                    weights=weightmatrix,
                    bws=h)$ksum/
      npksum(txdat,
             leave.one.out=TRUE,
             bandwidth.divide=TRUE,
             weights=weightmatrix,
             bws=h)$ksum

    fv <- (sum(w*((y-mhat)^2))/sum(w)) #Weighted average error in the estimation

    return(fv)

  }

}

nlm.return.3 <- nlm(ss3, runif(length(txdat)))

# Bandwidths from nlm()

bw.cv.w.3 <- nlm.return.3$estimate

bandwidth6t <- bw.cv.w.3

#THE RESULTS WITH ALL THE WAYS OF ESTIMATING THE OPTIMAL BANDWITH (CROSS VALIDATION) ARE SIMILAR

### COMPUTE WITH WEIGHTS THE SMOOTHING, USING ONE OF THE OBTAINED BANDWIDTHS ############

fit.lc.1 <- npksum(txdat=rank, tydat=wealth, bws=bw.cv.w.3, weights=weightmatrix)$ksum/
  npksum(txdat=rank, bws=bw.cv.w.3, weights=weightmatrix)$ksum

plot(rank, wealth, xlab="Rank", ylab="Wealth")
plot (rank, fit.lc.1)

smoothnetwealth <- as.vector(unlist(fit.lc.1)[]) #We convert the non-parametric fit from list to vector

datasimpleuk <- cbind.data.frame (datasimpleuk, smoothnetwealth)

theil6tlistIE <- calcGEI (datasimpleuk$smoothnetwealth, w=datasimpleuk$weight, alpha = 0)

theil6tIEboot[n] <- as.numeric(theil6tlistIE$ineq$index)

theil6tIEscalar <- as.numeric(theil6tlistIE$ineq$index)

theil6tlist <- calcGEI (datasimpleuk$wealth, w=datasimpleuk$weight, alpha = 0)

theil6tboot[n] <- as.numeric(theil6tlist$ineq$index)

theil6tscalar <- as.numeric(theil6tlist$ineq$index)

share6tboot[n]<-  (theil6tscalar-theil6tIEscalar)/theil6tscalar


#Bootstrap Standard Error
theil6tbootvar <- var(theil6tboot)
theil6tbootmean <- mean (theil6tboot)
theil6tbootsd <- sd(theil6tboot)
theil6tsdup <- mean(theil6tboot) + 1.96*theil6tbootsd
theil6tsddown <- mean(theil6tboot) - 1.96*theil6tbootsd
#Confidence Interval
theil6tlowci <- quantile(theil6tboot, probs = 0.05)
theil6thighci <- quantile(theil6tboot, probs = 0.95)
indextheil6t <- cbind (theil6tbootmean, theil6tbootvar, theil6tbootsd, theil6tsdup, theil6tsddown, theil6tlowci, theil6thighci)

#Bootstrap Standard Error
theil6tIEbootvar <- var(theil6tIEboot)
theil6tIEbootmean <- mean (theil6tIEboot)
theil6tIEbootsd <- sd(theil6tIEboot)
theil6tIEsdup <- mean(theil6tIEboot) + 1.96*theil6tIEbootsd
theil6tIEsddown <- mean(theil6tIEboot) - 1.96*theil6tIEbootsd
#Confidence Interval
theil6tIElowci <- quantile(theil6tIEboot, probs = 0.05)
theil6tIEhighci <- quantile(theil6tIEboot, probs = 0.95)

indextheil6tIE <- cbind (theil6tIEbootmean, theil6tIEbootvar, theil6tIEbootsd, theil6tIEsdup, theil6tIEsddown, theil6tIElowci, theil6tIEhighci)

#Bootstrap Standard Error
share6tbootvar <- var(share6tboot)
share6tbootmean <- mean (share6tboot)
share6tbootsd <- sd(share6tboot)
share6tsdup <- mean(share6tboot) + 1.96*share6tbootsd
share6tsddown <- mean(share6tboot) - 1.96*share6tbootsd
#Confidence Interval
share6tlowci <- quantile(share6tboot, probs = 0.05)
share6thighci <- quantile(share6tboot, probs = 0.95)

indexshare6t <- cbind (share6tbootmean, share6tbootvar, share6tbootsd, share6tsdup, share6tsddown, share6tlowci, share6thighci)


detach(datasimpleuk)


#We obtain the ranking variable as the ranking (normalized to 1) within each type (24 types)

datasimpleukboot24 <- datasimpleukboot %>%
  group_by(types24) %>%
  mutate(my_rank = (rank(wealth, ties.method = 'random')/length(wealth)))

#First we can create a simple data frame that would only uke concerned variables and therefore be perhaps faster with computation #####

#We include only the types, the rank, the weight (longitudinal W1-W3), the outcome variable and the ID. Recall in Step 1b we renamed the variable w1w3wgt as weight

datasimpleuk <- datasimpleukboot24[,c("wealth", "my_rank", "weight", "types24", "CaseW3")]

names (datasimpleuk) <- c("wealth", "rank", "weight", "types", "id")

attach(datasimpleuk)


#First we get the cross-validated bandwidth without weights

bw.noweight <- npregbw(xdat=rank, ydat=wealth)

#Now the cross-validated bandwidth with weights

weightmatrix <- as.matrix(datasimpleuk$weight)

w <- datasimpleuk$weight

txdat <- as.data.frame(datasimpleuk$rank)

y <- datasimpleuk$wealth

num.ydat <- datasimpleuk$wealth*datasimpleuk$weight

#ALL OF THE ESTIMATIONS OF OPTIMAL BANDWIDTH YIELD VERY CLOSE ESTIMATES

# Write an R function that returns the average leave-one-out sum of
# squared residuals for the local constant estimator based upon
# npksum(). This function accepts one argument and presumes that
# txdat and tydat have been defined already.

#AND NOW A POSSIBLE WAY OF ESTIMATING USING THE REGULAR FORMULA FOR KERNEL ESTIMATION, IN THIS CASE ADDING THE WEIGHTS ADDITIONALLY IN THE KERNEL FORMULA

ss3 <- function(h) {

  # Test for valid (non-negative) bandwidths - return infinite penalty
  # when this occurs

  if(min(h)<=0) {

    return(.Machine$double.xmax)

  } else {

    mhat <-  npksum(txdat,
                    tydat=y,
                    leave.one.out=TRUE,
                    bandwidth.divide=TRUE,
                    weights=weightmatrix,
                    bws=h)$ksum/
      npksum(txdat,
             leave.one.out=TRUE,
             bandwidth.divide=TRUE,
             weights=weightmatrix,
             bws=h)$ksum

    fv <- (sum(w*((y-mhat)^2))/sum(w)) #Weighted average error in the estimation

    return(fv)

  }

}

nlm.return.3 <- nlm(ss3, runif(length(txdat)))

# Bandwidths from nlm()

bw.cv.w.3 <- nlm.return.3$estimate

bandwidth24t <- bw.cv.w.3

#THE RESULTS WITH ALL THE WAYS OF ESTIMATING THE OPTIMAL BANDWITH (CROSS VALIDATION) ARE SIMILAR

### COMPUTE WITH WEIGHTS THE SMOOTHING, USING ONE OF THE OBTAINED BANDWIDTHS ############

fit.lc.1 <- npksum(txdat=rank, tydat=wealth, bws=bw.cv.w.3, weights=weightmatrix)$ksum/
  npksum(txdat=rank, bws=bw.cv.w.3, weights=weightmatrix)$ksum

plot(rank, wealth, xlab="Rank", ylab="Wealth")
plot (rank, fit.lc.1)

smoothnetwealth <- as.vector(unlist(fit.lc.1)[]) #We convert the non-parametric fit from list to vector

datasimpleuk <- cbind.data.frame (datasimpleuk, smoothnetwealth)

theil24tlistIE <- calcGEI (datasimpleuk$smoothnetwealth, w=datasimpleuk$weight, alpha = 0)

theil24tIEboot[n] <- as.numeric(theil24tlistIE$ineq$index)

theil24tIEscalar <- as.numeric(theil24tlistIE$ineq$index)

theil24tlist <- calcGEI (datasimpleuk$wealth, w=datasimpleuk$weight, alpha = 0)

theil24tboot[n] <- as.numeric(theil24tlist$ineq$index)

theil24tscalar <- as.numeric(theil24tlist$ineq$index)

share24tboot[n]<-  (theil24tscalar-theil24tIEscalar)/theil24tscalar


#Bootstrap Standard Error
theil24tbootvar <- var(theil24tboot)
theil24tbootmean <- mean (theil24tboot)
theil24tbootsd <- sd(theil24tboot)
theil24tsdup <- mean(theil24tboot) + 1.96*theil24tbootsd
theil24tsddown <- mean(theil24tboot) - 1.96*theil24tbootsd
#Confidence Interval
theil24tlowci <- quantile(theil24tboot, probs = 0.05)
theil24thighci <- quantile(theil24tboot, probs = 0.95)
indextheil24t <- cbind (theil24tbootmean, theil24tbootvar, theil24tbootsd, theil24tsdup, theil24tsddown, theil24tlowci, theil24thighci)

#Bootstrap Standard Error
theil24tIEbootvar <- var(theil24tIEboot)
theil24tIEbootmean <- mean (theil24tIEboot)
theil24tIEbootsd <- sd(theil24tIEboot)
theil24tIEsdup <- mean(theil24tIEboot) + 1.96*theil24tIEbootsd
theil24tIEsddown <- mean(theil24tIEboot) - 1.96*theil24tIEbootsd
#Confidence Interval
theil24tIElowci <- quantile(theil24tIEboot, probs = 0.05)
theil24tIEhighci <- quantile(theil24tIEboot, probs = 0.95)

indextheil24tIE <- cbind (theil24tIEbootmean, theil24tIEbootvar, theil24tIEbootsd, theil24tIEsdup, theil24tIEsddown, theil24tIElowci, theil24tIEhighci)

#Bootstrap Standard Error
share24tbootvar <- var(share24tboot)
share24tbootmean <- mean (share24tboot)
share24tbootsd <- sd(share24tboot)
share24tsdup <- mean(share24tboot) + 1.96*share24tbootsd
share24tsddown <- mean(share24tboot) - 1.96*share24tbootsd
#Confidence Interval
share24tlowci <- quantile(share24tboot, probs = 0.05)
share24thighci <- quantile(share24tboot, probs = 0.95)

indexshare24t <- cbind (share24tbootmean, share24tbootvar, share24tbootsd, share24tsdup, share24tsddown, share24tlowci, share24thighci)

detach(datasimpleuk)

#marginal effects

marginal_bk[n] <- ((theil24tscalar-theil24tIEscalar)/theil24tscalar)-((theil6tscalar-theil6tIEscalar)/theil6tscalar)



#Bootstrap Standard Error
marginal_bkvar <- var(marginal_bk)
marginal_bkmean <- mean (marginal_bk)
marginal_bksd <- sd(marginal_bk)
marginal_bksdup <- mean(marginal_bk) + 1.96*marginal_bksd
marginal_bksddown <- mean(marginal_bk) - 1.96*marginal_bksd
#Confidence Interval
marginal_bklowci <- quantile(marginal_bk, probs = 0.05)
marginal_bkhighci <- quantile(marginal_bk, probs = 0.95)

indexmarginal_bk <- cbind (marginal_bkmean, marginal_bkvar, marginal_bksd, marginal_bksdup, marginal_bksddown, marginal_bklowci, marginal_bkhighci)



marginal_inh[n] <- ((theil24tscalar-theil24tIEscalar)/theil24tscalar)-((theil4tscalar-theil4tIEscalar)/theil4tscalar)


#Bootstrap Standard Error
marginal_inhvar <- var(marginal_inh)
marginal_inhmean <- mean (marginal_inh)
marginal_inhsd <- sd(marginal_inh)
marginal_inhsdup <- mean(marginal_inh) + 1.96*marginal_inhsd
marginal_inhsddown <- mean(marginal_inh) - 1.96*marginal_inhsd
#Confidence Interval
marginal_inhlowci <- quantile(marginal_inh, probs = 0.05)
marginal_inhhighci <- quantile(marginal_inh, probs = 0.95)

indexmarginal_inh <- cbind (marginal_inhmean, marginal_inhvar, marginal_inhsd, marginal_inhsdup, marginal_inhsddown, marginal_inhlowci, marginal_inhhighci)


#interactive effects


interactive_bk_inh[n] <- ((theil6tscalar-theil6tIEscalar)/theil6tscalar) + ((theil4tscalar-theil4tIEscalar)/theil4tscalar) - ((theil24tscalar-theil24tIEscalar)/theil24tscalar)



#Bootstrap Standard Error
interactive_bk_inhvar <- var(interactive_bk_inh)
interactive_bk_inhmean <- mean (interactive_bk_inh)
interactive_bk_inhsd <- sd(interactive_bk_inh)
interactive_bk_inhsdup <- mean(interactive_bk_inh) + 1.96*interactive_bk_inhsd
interactive_bk_inhsddown <- mean(interactive_bk_inh) - 1.96*interactive_bk_inhsd
#Confidence Interval
interactive_bk_inhlowci <- quantile(interactive_bk_inh, probs = 0.05)
interactive_bk_inhhighci <- quantile(interactive_bk_inh, probs = 0.95)

indexinteractive_bk_inh <- cbind (interactive_bk_inhmean, interactive_bk_inhvar, interactive_bk_inhsd, interactive_bk_inhsdup, interactive_bk_inhsddown, interactive_bk_inhlowci, interactive_bk_inhhighci)




#shapley
shapley_bk[n] <- (((theil4tscalar-theil4tIEscalar)/theil4tscalar) + (((theil24tscalar-theil24tIEscalar)/theil24tscalar)-((theil6tscalar-theil6tIEscalar)/theil6tscalar)))/2

#Bootstrap Standard Error
shapley_bkvar <- var(shapley_bk)
shapley_bkmean <- mean (shapley_bk)
shapley_bksd <- sd(shapley_bk)
shapley_bksdup <- mean(shapley_bk) + 1.96*shapley_bksd
shapley_bksddown <- mean(shapley_bk) - 1.96*shapley_bksd
#Confidence Interval
shapley_bklowci <- quantile(shapley_bk, probs = 0.05)
shapley_bkhighci <- quantile(shapley_bk, probs = 0.95)

indexshapley_bk <- cbind (shapley_bkmean, shapley_bkvar, shapley_bksd, shapley_bksdup, shapley_bksddown, shapley_bklowci, shapley_bkhighci)



shapley_inh[n] <- (((theil6tscalar-theil6tIEscalar)/theil6tscalar) + (((theil24tscalar-theil24tIEscalar)/theil24tscalar)-((theil4tscalar-theil4tIEscalar)/theil4tscalar)))/2


#Bootstrap Standard Error
shapley_inhvar <- var(shapley_inh)
shapley_inhmean <- mean (shapley_inh)
shapley_inhsd <- sd(shapley_inh)
shapley_inhsdup <- mean(shapley_inh) + 1.96*shapley_inhsd
shapley_inhsddown <- mean(shapley_inh) - 1.96*shapley_inhsd
#Confidence Interval
shapley_inhlowci <- quantile(shapley_inh, probs = 0.05)
shapley_inhhighci <- quantile(shapley_inh, probs = 0.95)

indexshapley_inh <- cbind (shapley_inhmean, shapley_inhvar, shapley_inhsd, shapley_inhsdup, shapley_inhsddown, shapley_inhlowci, shapley_inhhighci)

}

results4typesbsUKfull <- rbind(indextheil4t, indextheil4tIE, indexshare4t)

write.csv(results4typesbsUKfull, file = "results/uk_bis1/uk-results4types-full-imp1.csv")
write.table(results4typesbsUKfull, file = "results/uk_bis1/uk-results4types-full-imps.csv", append = T, sep = ",", row.names = T)

results6typesbsUKfull <- rbind(indextheil6t, indextheil6tIE, indexshare6t)

write.csv(results6typesbsUKfull, file = "results/uk_bis1/uk-results6types-full-imp1.csv")
write.table(results6typesbsUKfull, file = "results/uk_bis1/uk-results6types-full-imps.csv", append = T, sep = ",", row.names = T)



results24typesbsUKfull <- rbind(indextheil24t, indextheil24tIE, indexshare24t)

write.csv(results24typesbsUKfull, file = "results/uk_bis1/uk-results24types-full-imp1.csv")
write.table(results24typesbsUKfull, file = "results/uk_bis1/uk-results24types-full-imps.csv", append = T, sep = ",", row.names = T)


resultsmarginalUK <- rbind(indexmarginal_bk, indexmarginal_inh )

write.csv(resultsmarginalUK, file = "results/uk_bis1/uk-results-marginal-full-imp1.csv")
write.table(resultsmarginalUK, file = "results/uk_bis1/uk-results-marginal-full-imps.csv", append = T, sep = ",", row.names = T)


resultsinteractiveUK <- indexinteractive_bk_inh

write.csv(resultsinteractiveUK, file = "results/uk_bis1/uk-results-interactive-full-imp1.csv")
write.table(resultsinteractiveUK, file = "results/uk_bis1/uk-results-interactive-full-imps.csv", append = T, sep = ",", row.names = T)


resultsshapleyUK <- rbind(indexshapley_bk, indexshapley_inh )

write.csv(resultsshapleyUK, file = "results/uk_bis1/uk-results-shapley-full-imp1.csv")
write.table(resultsshapleyUK, file = "results/uk_bis1/uk-results-shapley-full-imps.csv", append = T, sep = ",", row.names = T)

}

