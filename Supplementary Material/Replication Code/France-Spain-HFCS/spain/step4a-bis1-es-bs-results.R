#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

library(foreign)
options ("scipen"=100, "digits"=4)

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory # LAPTOP DIRECTORY


#WE NOW MEASURE THE RESULTS FOR EACH CIRCUMSTANCE

#SES

sesimps <- read.csv(file = "results/spain/bis1/es-results4types-full-imps.csv", row.names = NULL)

sesimps <- sesimps[,-1] #we eliminate first column of names
sesimps <- sesimps[-c(4,8,12,16),] #we eliminate rows of repeated names

colnames(sesimps) <- c("mean", "var", "sd", "sdup", "sddown", "lowci", "highci")
rownames(sesimps) <- NULL

sesimps[] <- lapply(sesimps, function(x) as.numeric(as.character(x))) #convert all to numeric

w_ineq_imps <- sesimps[c(1,4,7,10,13),]
ses_ineq_imps <- sesimps[c(2,5,8,11,14),]
ses_share_imps <- sesimps[c(3,6,9,12,15),]


#First for wealth ineq

w_ineq_mi_est <- mean(w_ineq_imps$mean)
w_ineq_imps$varestmi <- (w_ineq_imps$mean - w_ineq_mi_est)^2 
w_ineq_mi_var <- (mean(w_ineq_imps$var)) + ((5+1)/(5*(5-1)))*(sum(w_ineq_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*w_ineq_mi_var)/((5+1)*w_ineq_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

w_ineq_mi_se <- (w_ineq_mi_var)^0.5#/(5^0.5)
w_ineq_mi_low_ci <- w_ineq_mi_est - (tvalue * w_ineq_mi_se)
w_ineq_mi_high_ci <- w_ineq_mi_est + (tvalue * w_ineq_mi_se)



#First for smooth wealth ineq (ses)

ses_ineq_mi_est <- mean(ses_ineq_imps$mean)
ses_ineq_imps$varestmi <- (ses_ineq_imps$mean - ses_ineq_mi_est)^2 
ses_ineq_mi_var <- (mean(ses_ineq_imps$var)) + ((5+1)/(5*(5-1)))*(sum(ses_ineq_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*ses_ineq_mi_var)/((5+1)*ses_ineq_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

ses_ineq_mi_se <- (ses_ineq_mi_var)^0.5#/(5^0.5)
ses_ineq_mi_low_ci <- ses_ineq_mi_est - (tvalue * ses_ineq_mi_se)
ses_ineq_mi_high_ci <- ses_ineq_mi_est + (tvalue * ses_ineq_mi_se)

#Now for ses gross contribution (ses)

ses_share_mi_est <- mean(ses_share_imps$mean)
ses_share_imps$varestmi <- (ses_share_imps$mean - ses_share_mi_est)^2 
ses_share_mi_var <- (mean(ses_share_imps$var)) + ((5+1)/(5*(5-1)))*(sum(ses_share_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*ses_share_mi_var)/((5+1)*ses_share_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

ses_share_mi_se <- (ses_share_mi_var)^0.5#/(5^0.5)
ses_share_mi_low_ci <- ses_share_mi_est - (tvalue * ses_share_mi_se)
ses_share_mi_high_ci <- ses_share_mi_est + (tvalue * ses_share_mi_se)


#INH

inhimps <- read.csv(file = "results/spain/bis1/es-results6types-full-imps.csv", row.names = NULL)

inhimps <- inhimps[,-1] #we eliminate first column of names
inhimps <- inhimps[-c(4,8,12,16),] #we eliminate rows of repeated names

colnames(inhimps) <- c("mean", "var", "sd", "sdup", "sddown", "lowci", "highci")
rownames(inhimps) <- NULL

inhimps[] <- lapply(inhimps, function(x) as.numeric(as.character(x))) #convert all to numeric

w_ineq_imps <- inhimps[c(1,4,7,10,13),]
inh_ineq_imps <- inhimps[c(2,5,8,11,14),]
inh_share_imps <- inhimps[c(3,6,9,12,15),]


#First for wealth ineq

w_ineq_mi_est <- mean(w_ineq_imps$mean)
w_ineq_imps$varestmi <- (w_ineq_imps$mean - w_ineq_mi_est)^2 
w_ineq_mi_var <- (mean(w_ineq_imps$var)) + ((5+1)/(5*(5-1)))*(sum(w_ineq_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*w_ineq_mi_var)/((5+1)*w_ineq_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

w_ineq_mi_se <- (w_ineq_mi_var)^0.5#/(5^0.5)
w_ineq_mi_low_ci <- w_ineq_mi_est - (tvalue * w_ineq_mi_se)
w_ineq_mi_high_ci <- w_ineq_mi_est + (tvalue * w_ineq_mi_se)



#First for smooth wealth ineq (inh)

inh_ineq_mi_est <- mean(inh_ineq_imps$mean)
inh_ineq_imps$varestmi <- (inh_ineq_imps$mean - inh_ineq_mi_est)^2 
inh_ineq_mi_var <- (mean(inh_ineq_imps$var)) + ((5+1)/(5*(5-1)))*(sum(inh_ineq_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*inh_ineq_mi_var)/((5+1)*inh_ineq_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

inh_ineq_mi_se <- (inh_ineq_mi_var)^0.5#/(5^0.5)
inh_ineq_mi_low_ci <- inh_ineq_mi_est - (tvalue * inh_ineq_mi_se)
inh_ineq_mi_high_ci <- inh_ineq_mi_est + (tvalue * inh_ineq_mi_se)

#Now for inh gross contribution (inh)

inh_share_mi_est <- mean(inh_share_imps$mean)
inh_share_imps$varestmi <- (inh_share_imps$mean - inh_share_mi_est)^2 
inh_share_mi_var <- (mean(inh_share_imps$var)) + ((5+1)/(5*(5-1)))*(sum(inh_share_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*inh_share_mi_var)/((5+1)*inh_share_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

inh_share_mi_se <- (inh_share_mi_var)^0.5#/(5^0.5)
inh_share_mi_low_ci <- inh_share_mi_est - (tvalue * inh_share_mi_se)
inh_share_mi_high_ci <- inh_share_mi_est + (tvalue * inh_share_mi_se)


#SES + INH

jointimps <- read.csv(file = "results/spain/bis1/es-results24types-full-imps.csv", row.names = NULL)

jointimps <- jointimps[,-1] #we eliminate first column of names
jointimps <- jointimps[-c(4,8,12,16),] #we eliminate rows of repeated names

colnames(jointimps) <- c("mean", "var", "sd", "sdup", "sddown", "lowci", "highci")
rownames(jointimps) <- NULL

jointimps[] <- lapply(jointimps, function(x) as.numeric(as.character(x))) #convert all to numeric

w_ineq_imps <- jointimps[c(1,4,7,10,13),]
joint_ineq_imps <- jointimps[c(2,5,8,11,14),]
joint_share_imps <- jointimps[c(3,6,9,12,15),]


#First for wealth ineq

w_ineq_mi_est <- mean(w_ineq_imps$mean)
w_ineq_imps$varestmi <- (w_ineq_imps$mean - w_ineq_mi_est)^2 
w_ineq_mi_var <- (mean(w_ineq_imps$var)) + ((5+1)/(5*(5-1)))*(sum(w_ineq_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*w_ineq_mi_var)/((5+1)*w_ineq_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

w_ineq_mi_se <- (w_ineq_mi_var)^0.5#/(5^0.5)
w_ineq_mi_low_ci <- w_ineq_mi_est - (tvalue * w_ineq_mi_se)
w_ineq_mi_high_ci <- w_ineq_mi_est + (tvalue * w_ineq_mi_se)



#First for smooth wealth ineq (joint)

joint_ineq_mi_est <- mean(joint_ineq_imps$mean)
joint_ineq_imps$varestmi <- (joint_ineq_imps$mean - joint_ineq_mi_est)^2 
joint_ineq_mi_var <- (mean(joint_ineq_imps$var)) + ((5+1)/(5*(5-1)))*(sum(joint_ineq_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*joint_ineq_mi_var)/((5+1)*joint_ineq_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

joint_ineq_mi_se <- (joint_ineq_mi_var)^0.5#/(5^0.5)
joint_ineq_mi_low_ci <- joint_ineq_mi_est - (tvalue * joint_ineq_mi_se)
joint_ineq_mi_high_ci <- joint_ineq_mi_est + (tvalue * joint_ineq_mi_se)

#Now for joint gross contribution (joint)

joint_share_mi_est <- mean(joint_share_imps$mean)
joint_share_imps$varestmi <- (joint_share_imps$mean - joint_share_mi_est)^2 
joint_share_mi_var <- (mean(joint_share_imps$var)) + ((5+1)/(5*(5-1)))*(sum(joint_share_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*joint_share_mi_var)/((5+1)*joint_share_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

joint_share_mi_se <- (joint_share_mi_var)^0.5#/(5^0.5)
joint_share_mi_low_ci <- joint_share_mi_est - (tvalue * joint_share_mi_se)
joint_share_mi_high_ci <- joint_share_mi_est + (tvalue * joint_share_mi_se)



#Now we put together the results

w_ineq_res <- c(w_ineq_mi_est, w_ineq_mi_se, w_ineq_mi_low_ci, w_ineq_mi_high_ci)
ses_ineq_res <- c(ses_ineq_mi_est, ses_ineq_mi_se, ses_ineq_mi_low_ci, ses_ineq_mi_high_ci)
ses_share_res <- c(ses_share_mi_est, ses_share_mi_se, ses_share_mi_low_ci, ses_share_mi_high_ci)
inh_ineq_res <- c(inh_ineq_mi_est, inh_ineq_mi_se, inh_ineq_mi_low_ci, inh_ineq_mi_high_ci)
inh_share_res <- c(inh_share_mi_est, inh_share_mi_se, inh_share_mi_low_ci, inh_share_mi_high_ci)
joint_ineq_res <- c(joint_ineq_mi_est, joint_ineq_mi_se, joint_ineq_mi_low_ci, joint_ineq_mi_high_ci)
joint_share_res <- c(joint_share_mi_est, joint_share_mi_se, joint_share_mi_low_ci, joint_share_mi_high_ci)

spain_res <- rbind (w_ineq_res, ses_ineq_res, ses_share_res, inh_ineq_res, inh_share_res, joint_ineq_res, joint_share_res)

colnames(spain_res) <- c("estimate", "se", "low_ci", "high_ci")

write.csv (spain_res, file = "results/spain/bis1/spain_res.csv")
