#THIS V2 INDICATES THAT WE ARE USING THE NEW CONFIGURATION - YEARS 35 - 85 AND THEN AGE CONTROL

#CHANGE THE WORKING DIRECTORY TO MOVE THE DATA THERE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory

setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/HFCS-IOp/code") #Set Working Directory for the LAPTOP

library(Hmisc)
library(reldist)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(snakecase)

options ("scipen"=100, "digits"=10)


#### WE LOAD THE AGGREGATION FROM THE PREVIOUS FILE,  ###############

#datafrh <- readRDS(file = "HFCS14_France_after_step_2c.rds")

datafrh <- readRDS(file = "datasets/v2-HFCS14_France_after_step_2c-no-pension-ad-eq_imp2.rds")




#THEN WE CONVERT THE WEALTH DATA INTO NET TERMS OF AGE AND GENDER

datafrh$agedif <- datafrh$age - 65
datafrh$agedif2 <- (datafrh$agedif)^2
datafrh$agedif3 <- (datafrh$agedif)^3
datafrh$agedif4 <- (datafrh$agedif)^4


datafrh$femaledummy <- 0
datafrh$femaledummy[datafrh$sex == "2"] <- 1

#We convert sex to factor for the graph
datafrh$sexfactor <- as.factor (datafrh$sex)
levels(datafrh$sexfactor) <- c("Male","Female")


datafrh$femaleagedif <- datafrh$agedif * datafrh$femaledummy
datafrh$femaleagedif2 <- (datafrh$femaleagedif)^2
datafrh$femaleagedif3 <- (datafrh$femaleagedif)^3
datafrh$femaleagedif4 <- (datafrh$femaleagedif)^4

datafrh$wealth <- datafrh$eqwealth


modelwealth <- lm(log(wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = datafrh, weights = weight)
#modelwealth <- lm((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = datafrh, weights = weight)

summary(modelwealth)

tab_model(modelwealth, digits = 3, digits.p = 3, show.se = T, show.fstat = T, show.aic = T)

#datafrh$wealthpredict <- predict(modelwealth)

datafrh$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid #Alpha (termino independiente) + Residuals

datafrh$wealthstandard <- datafrh$wealth - datafrh$wealthpredict

summary(datafrh$wealthstandard)
summary(datafrh$wealth)

summary(datafrh$wealthpredict)
summary(modelwealth$residuals)
summary(modelwealth$resid)
summary(modelwealth$coefficients[1])


datafrh$wealthpredictexp <- exp(datafrh$wealthpredict)


summary(datafrh$wealth)
summary(datafrh$wealthpredictexp)


### Now we do some graphical test of age and wealth after the adjustment


##### OK NOW WE DO A SIMPLE ESTIMATION SMOOTHING  ####

yr_range = c(-4:4)   # same as c(-1, 0, 1)

#Make a copy of each row for each entry in yr_range using tidyr::uncount, then create a dummy age_adj that adjusts each row's age to move it into a bucket for summarization:

df2 <- datafrh %>%
  uncount(length(yr_range)) %>%
  mutate(age_adj = rep(yr_range, length.out = n()),
         age_bucket  = age + age_adj) %>%
  # At this point it looks like:
  #   income age type age_adj age_bucket
  #1    1000  41    1      -1         40
  #2    1000  41    1       0         41
  #3    1000  41    1       1         42
  #4    2000  42    2      -1         41
  #5    2000  42    2       0         42
  #6    2000  42    2       1         43
  #filter(relativewealthalltypes<quantile(relativewealth, probs = .99 )) %>%
  group_by(age_bucket) %>%
  mutate(mean_wealth = weighted.mean(log(wealth), w=weight)) %>%
  mutate(median_wealth = wtd.quantile(log(wealth), q=0.5, weight=weight)) %>%
  # optional, to prune edge years beyond orig data
  filter(age_adj == 0)
#filter(age_bucket >= min(dataesplot$age),
#age_bucket <= max(dataesplot$age))

df2gender <- datafrh %>%
  uncount(length(yr_range)) %>%
  mutate(age_adj = rep(yr_range, length.out = n()),
         age_bucket  = age + age_adj) %>%
  # At this point it looks like:
  #   income age type age_adj age_bucket
  #1    1000  41    1      -1         40
  #2    1000  41    1       0         41
  #3    1000  41    1       1         42
  #4    2000  42    2      -1         41
  #5    2000  42    2       0         42
  #6    2000  42    2       1         43
  #filter(relativewealthalltypes<quantile(relativewealth, probs = .99 )) %>%
  group_by(age_bucket, sex) %>%
  mutate(mean_wealth = weighted.mean(log(wealth), w=weight)) %>%
  mutate(median_wealth = wtd.quantile(log(wealth), q=0.5, weight=weight)) %>%
  # optional, to prune edge years beyond orig data
  filter(age_adj == 0)
#filter(age_bucket >= min(dataesplot$age),
#age_bucket <= max(dataesplot$age))



df3 <- df2 %>%
  uncount(length(yr_range)) %>%
  mutate(age_adj = rep(yr_range, length.out = n()),
         age_bucket2  = age + age_adj) %>%
  # At this point it looks like:
  #   income age type age_adj age_bucket
  #1    1000  41    1      -1         40
  #2    1000  41    1       0         41
  #3    1000  41    1       1         42
  #4    2000  42    2      -1         41
  #5    2000  42    2       0         42
  #6    2000  42    2       1         43
  #filter(relativewealthalltypes<quantile(relativewealth, probs = .99 )) %>%
  group_by(age_bucket2) %>%
  mutate(mean_pred_wealth = weighted.mean(wealthpredict, w=weight)) %>%
  mutate(median_pred_wealth = wtd.quantile(wealthpredict, q=0.5, weight=weight)) %>% #WE have to rename weights first
  # optional, to prune edge years beyond orig data
  filter(age_adj == 0)
#filter(age_bucket >= min(dataesplot$age),
#age_bucket <= max(dataesplot$age))

df3gender <- df2gender %>%
  uncount(length(yr_range)) %>%
  mutate(age_adj = rep(yr_range, length.out = n()),
         age_bucket2  = age + age_adj) %>%
  # At this point it looks like:
  #   income age type age_adj age_bucket
  #1    1000  41    1      -1         40
  #2    1000  41    1       0         41
  #3    1000  41    1       1         42
  #4    2000  42    2      -1         41
  #5    2000  42    2       0         42
  #6    2000  42    2       1         43
  #filter(relativewealthalltypes<quantile(relativewealth, probs = .99 )) %>%
  group_by(age_bucket2, sex) %>%
  mutate(mean_pred_wealth = weighted.mean(wealthpredict, w=weight)) %>%
  mutate(median_pred_wealth = wtd.quantile(wealthpredict, q=0.5, weight=weight)) %>% #WE have to rename weights first
  # optional, to prune edge years beyond orig data
  filter(age_adj == 0)
#filter(age_bucket >= min(dataesplot$age),
#age_bucket <= max(dataesplot$age))

#NOW THE GRAPH

#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph1 <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=median_wealth, weight=df2$weight))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Share of median wealth")+
  ggtitle("Age-Wealth profiles Wealth")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

graph1gender <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=median_wealth, weight=df2$weight, colour=sexfactor))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles France")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Wealth-Age-Profile-by-Gender-FR-no-pension-ad-eq-log-log-Oct19-imp2.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


graph1gendermean <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=mean_wealth, weight=df2$weight, colour=sexfactor))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Mean Wealth")+
  ggtitle("Age-Wealth profiles France")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Wealth-Age-Profile-by-Gender-Mean-FR-no-pension-ad-eq-log-Oct19-imp2.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)

#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph2 <- ggplot()+
  geom_point(data=df3, aes(x=age, y=median_pred_wealth, weight=df3$weight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3, aes(x=age, y=median_wealth, weight=df3$weight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles FR")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph2male <- ggplot()+
  geom_point(data=df3gender[df3gender$sexfactor=='Male',], aes(x=age, y=median_pred_wealth, weight=weight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Male',], aes(x=age, y=median_wealth, weight=weight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles FR")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.6, 0.3),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )


#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph2female <- ggplot()+
  geom_point(data=df3gender[df3gender$sexfactor=='Female',], aes(x=age, y=median_pred_wealth, weight=weight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Female',], aes(x=age, y=median_wealth, weight=weight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles FR")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.60, 0.3),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )


#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph2gendermedian <- ggplot()+
  geom_point(data=df3gender[df3gender$sexfactor=='Female',], aes(x=age, y=median_pred_wealth, weight=weight, colour = '4. Adjusted Wealth Women'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Male',], aes(x=age, y=median_pred_wealth, weight=weight, colour = '2. Adjusted Wealth Men'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Female',], aes(x=age, y=median_wealth, weight=weight, colour = '3. Wealth Women'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Male',], aes(x=age, y=median_wealth, weight=weight, colour = '1. Wealth Men'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Adjusted and Original Wealth-Age profiles France")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.6, 0.3),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Adjusted-Wealth-Age-Profile-by-Gender-Median-FR-no-pension-ad-eq-log-Oct19-imp2.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph2gendermean <- ggplot()+
  geom_point(data=df3gender[df3gender$sexfactor=='Female',], aes(x=age, y=mean_pred_wealth, weight=weight, colour = '4. Adjusted Wealth Women'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Male',], aes(x=age, y=mean_pred_wealth, weight=weight, colour = '2. Adjusted Wealth Men'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Female',], aes(x=age, y=mean_wealth, weight=weight, colour = '3. Wealth Women'))+
  geom_point(data=df3gender[df3gender$sexfactor=='Male',], aes(x=age, y=mean_wealth, weight=weight, colour = '1. Wealth Men'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Mean wealth")+
  ggtitle("Adjusted and Original Wealth-Age profiles France")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.6, 0.3),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Adjusted-Wealth-Age-Profile-by-Gender-Mean-FR-no-pension-ad-eq-log-Oct19-imp2.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph3 <- ggplot()+
  geom_point(data=df3, aes(x=age, y=mean_pred_wealth, weight=df3$weight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3, aes(x=age, y=mean_wealth, weight=df3$weight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Mean wealth")+
  ggtitle("Age-Wealth profiles FR")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )



####



saveRDS(datafrh, file = "datasets/v2-HFCS14_France_after_step_2d-no-pension-ad-eq_imp2.rds")








