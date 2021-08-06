
library(Hmisc)
#library(reldist)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(snakecase)

options ("scipen"=100, "digits"=6)


#### WE LOAD THE AGGREGATION FROM THE PREVIOUS FILE,  ###############

dataukh <- readRDS(file = "data_rds/datauk-new-final-no-cpt-new-data8a.rds")

#THEN WE CONVERT THE WEALTH DATA INTO NET TERMS OF AGE AND GENDER

dataukh$agedif <- dataukh$age - 65
dataukh$agedif2 <- (dataukh$agedif)^2
dataukh$agedif3 <- (dataukh$agedif)^3
dataukh$agedif4 <- (dataukh$agedif)^4

dataukh$femaledummy <- 0
dataukh$femaledummy[dataukh$sex == "Female"] <- 1

dataukh$sexfactor <- dataukh$sex #Just to have same name as in HFCS for the graphs

dataukh$femaleagedif <- dataukh$agedif * dataukh$femaledummy
dataukh$femaleagedif2 <- (dataukh$femaleagedif)^2
dataukh$femaleagedif3 <- (dataukh$femaleagedif)^3
dataukh$femaleagedif4 <- (dataukh$femaleagedif)^4

#We adjust already using equivalent wealth
dataukh$wealth <- dataukh$eqwealth


#modelwealth <- lm((wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataukh, weights = hholdweight)

modelwealth <- lm(log(wealth) ~ agedif + agedif2 + agedif3 + agedif4 + femaledummy + femaleagedif  + femaleagedif2 + femaleagedif3  + femaleagedif4, data = dataukh, weights = hholdweight)

summary(modelwealth)

modelagegenderadjstUK<- tab_model(modelwealth, digits = 3, digits.p = 3, show.fstat = T, show.se = T, show.ci = F)
modelagegenderadjstUK


#dataukh$wealthpredict <- predict(modelwealth)

dataukh$wealthpredict <- modelwealth$coefficients[1] + modelwealth$resid

summary(dataukh$wealthpredict)
summary(modelwealth$residuals)
summary(modelwealth$resid)
summary(modelwealth$coefficients[1])

NROW (dataukh$wealthpredict[dataukh$wealthpredict <= 0]) #No negative values

dataukh$wealthpredictexp <- exp(dataukh$wealthpredict)


summary(dataukh$wealth)
summary(dataukh$wealthpredictexp)


### Now we do some graphical test of age and wealth after the adjustment

#We use the prediction in logs for the fit

##### OK NOW WE DO A SIMPLE ESTIMATION SMOOTHING  ####

yr_range = c(-4:4)   # same as c(-1, 0, 1)

#Make a copy of each row for each entry in yr_range using tidyr::uncount, then create a dummy age_adj that adjusts each row's age to move it into a bucket for summarization:

df2 <- dataukh %>%
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
  mutate(mean_wealth = weighted.mean(log(wealth), w=hholdweight)) %>%
  mutate(median_wealth = reldist::wtd.quantile(log(wealth), q=0.5, weight=hholdweight)) %>%
  # optional, to prune edge years beyond orig data
  filter(age_adj == 0)
#filter(age_bucket >= min(dataesplot$age),
#age_bucket <= max(dataesplot$age))

df2gender <- dataukh %>%
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
  mutate(mean_wealth = weighted.mean(log(wealth), w=hholdweight)) %>%
  mutate(median_wealth = reldist::wtd.quantile(log(wealth), q=0.5, weight=hholdweight)) %>%
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
  mutate(mean_pred_wealth = weighted.mean(wealthpredict, w=hholdweight)) %>%
  mutate(median_pred_wealth = reldist::wtd.quantile(wealthpredict, q=0.5, weight=hholdweight)) %>% #WE have to rename weights first
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
  mutate(mean_pred_wealth = weighted.mean(wealthpredict, w=hholdweight)) %>%
  mutate(median_pred_wealth = reldist::wtd.quantile(wealthpredict, q=0.5, weight=hholdweight)) %>% #WE have to rename weights first
  # optional, to prune edge years beyond orig data
  filter(age_adj == 0)
#filter(age_bucket >= min(dataesplot$age),
#age_bucket <= max(dataesplot$age))

#NOW THE GRAPH

#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph1 <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=median_wealth, weight=df2$hholdweight))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Share of median wealth")+
  ggtitle("Age-Wealth profiles Wealth")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal()+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

graph1gender <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=median_wealth, weight=df2$hholdweight, colour=sex))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles Britain")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  ##theme_minimal()+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Wealth-Age-Profile-by-Gender-UK-log-imp3.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


graph1gendermean <- ggplot()+
  geom_point(data=df2gender, aes(x=age, y=mean_wealth, weight=df2$hholdweight, colour=sex))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Mean Wealth")+
  ggtitle("Age-Wealth profiles Britain")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal()+
  theme(
    legend.position = c(.05, 0.95),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Wealth-Age-Profile-by-Gender-Mean-UK-log-imp3.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)

#And now we do the plot again for the median type...
#col=Type2, group=Type2,

graph2 <- ggplot()+
  geom_point(data=df3, aes(x=age, y=median_pred_wealth, weight=df3$hholdweight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3, aes(x=age, y=median_wealth, weight=df3$hholdweight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles Britain")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal()+
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
  geom_point(data=df3gender[df3gender$sex=='Male',], aes(x=age, y=median_pred_wealth, weight=hholdweight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3gender[df3gender$sex=='Male',], aes(x=age, y=median_wealth, weight=hholdweight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles Britain")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal()+
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
  geom_point(data=df3gender[df3gender$sex=='Female',], aes(x=age, y=median_pred_wealth, weight=hholdweight, colour = 'Adjusted Wealth'))+
  geom_point(data=df3gender[df3gender$sex=='Female',], aes(x=age, y=median_wealth, weight=hholdweight, colour = 'Wealth'))+
  #geom_point(data=dataesplot, aes(x=age, y=wealth, group=Type), pch=16, size=1)+
  #scale_y_continuous(limits = c(0, 100))+
  #coord_cartesian(ylim = c(0, 3)) +
  coord_cartesian(xlim = c(35, 80)) +
  xlab("Age")+
  ylab("Median wealth")+
  ggtitle("Age-Wealth profiles Britain")+
  #theme_minimal()+
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
  ggtitle("Adjusted and Original Wealth-Age profiles Britain")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal()+
  theme(
    legend.position = c(.6, 0.3),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Adjusted-Wealth-Age-Profile-by-Gender-Median-UK-log-imp3.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)


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
  ggtitle("Adjusted and Original Wealth-Age profiles Britain")+
  guides(colour = guide_legend(override.aes = list(size=3.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal()+
  theme(
    legend.position = c(.6, 0.3),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "NA", colour = "transparent"),
    legend.box.just = "right",
    legend.margin = margin(5, 5, 5, 5),
    legend.title=element_blank()
  )

ggsave(file="graphs/Adjusted-Wealth-Age-Profile-by-Gender-Mean-Britain-log-imp3.pdf", device = "pdf", scale = 1, width = 7.5, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)





####


saveRDS(dataukh, file = "data_rds/WAS-After-Step-v2-IO-8b-imp3.rds")
saveRDS(dataukh, file = "/Users/Juan/Google Drive/A-UK-Research/IO-Wealth-All-Countries/WAS-IOp/code/data_rds/WAS-After-Step-v2-IO-8b-imp3.rds")
#####





