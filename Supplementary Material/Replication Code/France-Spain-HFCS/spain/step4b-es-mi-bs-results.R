#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

library(foreign)
options ("scipen"=100, "digits"=4)

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code/") #Set Working Directory # LAPTOP DIRECTORY

shapleyimps <- read.csv(file = "results/spain/es-results-shapley-full-imps.csv", row.names = NULL)

shapleyimps <- shapleyimps[,-1] #we eliminate first column of names
shapleyimps <- shapleyimps[-c(3,6,9,12),] #we eliminate rows of repeated names

colnames(shapleyimps) <- c("mean", "var", "sd", "sdup", "sddown", "lowci", "highci")
rownames(shapleyimps) <- NULL

shapleyimps[] <- lapply(shapleyimps, function(x) as.numeric(as.character(x))) #convert all to numeric

shapley_ses_imps <- shapleyimps[c(1,3,5,7,9),]
shapley_inh_imps <- shapleyimps[c(2,4,6,8,10),]


#First for SES

shapley_ses_mi_est <- mean(shapley_ses_imps$mean)
shapley_ses_imps$varestmi <- (shapley_ses_imps$mean - shapley_ses_mi_est)^2 
shapley_ses_mi_var <- (mean(shapley_ses_imps$var)) + ((5+1)/(5*(5-1)))*(sum(shapley_ses_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*shapley_ses_mi_var)/((5+1)*shapley_ses_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

shapley_ses_mi_se <- (shapley_ses_mi_var)^0.5#/(5^0.5)
shapley_ses_mi_low_ci <- shapley_ses_mi_est - (tvalue * shapley_ses_mi_se)
shapley_ses_mi_high_ci <- shapley_ses_mi_est + (tvalue * shapley_ses_mi_se)


### NOW FOR INH 

shapley_inh_mi_est <- mean(shapley_inh_imps$mean)
shapley_inh_imps$varestmi <- (shapley_inh_imps$mean - shapley_inh_mi_est)^2 
shapley_inh_mi_var <- (mean(shapley_inh_imps$var)) + ((5+1)/(5*(5-1)))*(sum(shapley_inh_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*shapley_inh_mi_var)/((5+1)*shapley_inh_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

shapley_inh_mi_se <- (shapley_inh_mi_var)^0.5#/(5^0.5)
shapley_inh_mi_low_ci <- shapley_inh_mi_est - (tvalue * shapley_inh_mi_se)
shapley_inh_mi_high_ci <- shapley_inh_mi_est + (tvalue * shapley_inh_mi_se)

##### NOW FOR MARGINAL CONTRIBUTION 

marginalimps <- read.csv(file = "results/spain/es-results-marginal-full-imps.csv", row.names = NULL)

marginalimps <- marginalimps[,-1] #we eliminate first column of names
marginalimps <- marginalimps[-c(3,6,9,12),] #we eliminate rows of repeated names

colnames(marginalimps) <- c("mean", "var", "sd", "sdup", "sddown", "lowci", "highci")
rownames(marginalimps) <- NULL

marginalimps[] <- lapply(marginalimps, function(x) as.numeric(as.character(x))) #convert all to numeric

marginal_ses_imps <- marginalimps[c(1,3,5,7,9),]
marginal_inh_imps <- marginalimps[c(2,4,6,8,10),]


#First for SES

marginal_ses_mi_est <- mean(marginal_ses_imps$mean)
marginal_ses_imps$varestmi <- (marginal_ses_imps$mean - marginal_ses_mi_est)^2 
marginal_ses_mi_var <- (mean(marginal_ses_imps$var)) + ((5+1)/(5*(5-1)))*(sum(marginal_ses_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*marginal_ses_mi_var)/((5+1)*marginal_ses_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

marginal_ses_mi_se <- (marginal_ses_mi_var)^0.5#/(5^0.5)
marginal_ses_mi_low_ci <- marginal_ses_mi_est - (tvalue * marginal_ses_mi_se)
marginal_ses_mi_high_ci <- marginal_ses_mi_est + (tvalue * marginal_ses_mi_se)


### NOW FOR INH 

marginal_inh_mi_est <- mean(marginal_inh_imps$mean)
marginal_inh_imps$varestmi <- (marginal_inh_imps$mean - marginal_inh_mi_est)^2 
marginal_inh_mi_var <- (mean(marginal_inh_imps$var)) + ((5+1)/(5*(5-1)))*(sum(marginal_inh_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*marginal_inh_mi_var)/((5+1)*marginal_inh_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

marginal_inh_mi_se <- (marginal_inh_mi_var)^0.5#/(5^0.5)
marginal_inh_mi_low_ci <- marginal_inh_mi_est - (tvalue * marginal_inh_mi_se)
marginal_inh_mi_high_ci <- marginal_inh_mi_est + (tvalue * marginal_inh_mi_se)


##### NOW FOR MARGINAL CONTRIBUTION 

interactiveimps <- read.csv(file = "results/spain/es-results-interactive-full-imps.csv", row.names = NULL)

interactiveimps <- interactiveimps[,-1] #we eliminate first column of names
interactiveimps <- interactiveimps[-c(2,4,6,8),] #we eliminate rows of repeated names

colnames(interactiveimps) <- c("mean", "var", "sd", "sdup", "sddown", "lowci", "highci")
rownames(interactiveimps) <- NULL

interactiveimps[] <- lapply(interactiveimps, function(x) as.numeric(as.character(x))) #convert all to numeric

interactive_imps <- interactiveimps

#First for SES

interactive_mi_est <- mean(interactive_imps$mean)
interactive_imps$varestmi <- (interactive_imps$mean - interactive_mi_est)^2 
interactive_mi_var <- (mean(interactive_imps$var)) + ((5+1)/(5*(5-1)))*(sum(interactive_imps$varestmi)) #mi_variance


#Confidence Intervals

#t distribution degrees of freedom

r <- (5-1)*(1+((5*interactive_mi_var)/((5+1)*interactive_mi_var)))^2
tvalue <- abs(qt(0.05/2, r))

interactive_mi_se <- (interactive_mi_var)^0.5#/(5^0.5)
interactive_mi_low_ci <- interactive_mi_est - (tvalue * interactive_mi_se)
interactive_mi_high_ci <- interactive_mi_est + (tvalue * interactive_mi_se)

#Now the results 

marginal_ses_res <- c(marginal_ses_mi_est, marginal_ses_mi_se, marginal_ses_mi_low_ci, marginal_ses_mi_high_ci)
marginal_inh_res <- c(marginal_inh_mi_est, marginal_inh_mi_se, marginal_inh_mi_low_ci, marginal_inh_mi_high_ci)
shapley_ses_res <- c(shapley_ses_mi_est, shapley_ses_mi_se, shapley_ses_mi_low_ci, shapley_ses_mi_high_ci)
shapley_inh_res <- c(shapley_inh_mi_est, shapley_inh_mi_se, shapley_inh_mi_low_ci, shapley_inh_mi_high_ci)
interactive_res <- c(interactive_mi_est, interactive_mi_se, interactive_mi_low_ci, interactive_mi_high_ci)

spain_res_2 <- rbind (marginal_ses_res, marginal_inh_res, shapley_ses_res, shapley_inh_res, interactive_res)

colnames(spain_res_2) <- c("estimate", "se", "low_ci", "high_ci")

write.csv (spain_res_2, file = "results/spain/spain_res_2.csv")
