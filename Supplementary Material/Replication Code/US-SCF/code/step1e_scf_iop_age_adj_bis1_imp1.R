#NOW WE PROCEED TO WORK WITH THE AGGREGATE DATABASE

setwd("/Users/mac/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory
setwd("/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/SCF-IOp/code") #Set Working Directory for the LAPTOP

options ("scipen"=100, "digits"=10)

library(Hmisc)
library(reldist)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(snakecase)
library(survey)
library(quantreg)


#LET US LOAD THE DATA
dataush <- readRDS(file = "datasets/SCF-2016-all-after-1d-bis1-imp1.rds")


#THEN WE CONVERT THE WEALTH DATA INTO NET TERMS OF AGE AND GENDER

dataush$agedif <- dataush$age - 65
dataush$agedif2 <- (dataush$agedif)^2
dataush$agedif3 <- (dataush$agedif)^3
dataush$agedif4 <- (dataush$agedif)^4


dataush$femaledummy <- 0
dataush$femaledummy[dataush$sex == "2"] <- 1

#We convert sex to factor for the graph

dataush$sexfactor <- as.factor (dataush$sex)
levels(dataush$sexfactor) <- c("Male","Female")


#

dataush$femaleagedif <- dataush$agedif * dataush$femaledummy
dataush$femaleagedif2 <- (dataush$femaleagedif)^2
dataush$femaleagedif3 <- (dataush$femaleagedif)^3
dataush$femaleagedif4 <- (dataush$femaleagedif)^4

dataush$wealth <- dataush$eqwealth

NROW(dataush$wealth[dataush$wealth <= 0])


modelwealth <- lm(log(wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataush, weights = weight)

#modelwealth <- lm((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataush, weights = weight)
#modelwealth <- rq((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4,tau = .5, data = dataush, weights = weight)

#The model
#modelwealthpoisson <- svyglm(formula = (wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, design = svydesign(ids = ~1, weights = dataush$weight, data = dataush), rescale = T,family = poisson)

#summodelwealthpoisson<- summary (svyinhpoisson3)

#modelwealthus <- tab_model(modelwealthpoisson, digits = 3, digits.p = 3, show.fstat = T, show.se = T, show.aic = T)

#modelwealthus

#dataush$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid





#modelwealth <- lm((wealth) ~ log(agedif^2) + femaledummy + log(femaleagedif^2), data = dataush, weights = weight)

summary(modelwealth)

modelwealthus <- tab_model(modelwealth, digits = 3, digits.p = 3, show.fstat = T, show.se = T, show.aic = T)

modelwealthus

#dataush$wealthpredict <- predict(modelwealth)

dataush$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid

dataush$wealthstandard <- dataush$wealth - dataush$wealthpredict

summary(dataush$wealthstandard)
summary(dataush$wealth)

summary(dataush$wealthpredict)
summary(modelwealth$residuals)
summary(modelwealth$resid)
summary(modelwealth$coefficients[1])


#dataush$wealthpredictexp <- (dataush$wealthpredict)
dataush$wealthpredictexp <- exp(dataush$wealthpredict)

summary(dataush$wealth)
summary(dataush$wealthpredictexp)

NROW(dataush$wealthpredictexp[dataush$wealthpredictexp <= 0])

#Less number of negative values, so we keep that


### Now we do some graphical test of age and wealth after the adjustment

##### OK NOW WE DO A SIMPLE ESTIMATION SMOOTHING  ####

yr_range = c(-4:4)   # same as c(-1, 0, 1)

#Make a copy of each row for each entry in yr_range using tidyr::uncount, then create a dummy age_adj that adjusts each row's age to move it into a bucket for summarization:

df2 <- dataush %>%
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

df2gender <- dataush %>%
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
  ggtitle("Age-Wealth profiles US")+
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

ggsave(file="graphs/Wealth-Age-Profile-by-Gender-US-ad-eq-bis1-imp1.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


graph1gendermean <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=mean_wealth, weight=df2$weight, colour=sexfactor))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Mean Wealth")+
  ggtitle("Age-Wealth profiles US")+
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

ggsave(file="graphs/Wealth-Age-Profile-by-Gender-Mean-US-ad-eq-bis1-imp1.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)

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
  ggtitle("Age-Wealth profiles UK")+
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
  geom_point(data=df3gender[df3gender$sex=='1',], aes(x=age, y=median_pred_wealth, weight=weight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3gender[df3gender$sex=='1',], aes(x=age, y=median_wealth, weight=weight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles UK")+
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
  geom_point(data=df3gender[df3gender$sex=='Feale',], aes(x=age, y=median_pred_wealth, weight=weight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3gender[df3gender$sex=='2',], aes(x=age, y=median_wealth, weight=weight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles UK")+
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

graph2gender <- ggplot()+
  geom_point(data=df3gender[df3gender$sex=='2',], aes(x=age, y=median_pred_wealth, weight=weight, colour = 'Adjusted Wealth Women'))+
  geom_point(data=df3gender[df3gender$sex=='1',], aes(x=age, y=median_pred_wealth, weight=weight, colour = 'Adjusted Wealth Men'))+
  geom_point(data=df3gender[df3gender$sex=='2',], aes(x=age, y=median_wealth, weight=weight, colour = 'Wealth Women'))+
  geom_point(data=df3gender[df3gender$sex=='1',], aes(x=age, y=median_wealth, weight=weight, colour = 'Wealth Men'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles US")+
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
  ggtitle("Adjusted and Original Wealth-Age profiles US")+
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

ggsave(file="graphs/Adjusted-Wealth-Age-Profile-by-Gender-Median-US-ad-eq-bis1-imp1.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


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
  ggtitle("Adjusted and Original Wealth-Age profiles US")+
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

ggsave(file="graphs/Adjusted-Wealth-Age-Profile-by-Gender-Mean-US-ad-eq-bis1-imp1.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)



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
  ggtitle("Age-Wealth profiles UK")+
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




saveRDS(dataush, file = "datasets/SCF-2016-all-after-1e-bis1-imp1.rds")

#####





