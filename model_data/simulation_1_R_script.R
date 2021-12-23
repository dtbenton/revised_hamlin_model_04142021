# Simulation 1 Analysis #

# Load all relevant libraries
# Install packages
library(lme4)
library(nlme)
library(boot)
library(car) 
library(reshape2)
library(ggplot2)
library(ez)
library(plyr)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
library(BayesFactor)
library(foreign)
library(dplyr)
library(lattice)
library(openxlsx)
options(scipen=9999)

#######################
## EXPERIMENTER DATA ##
#######################

# load data
D = read.csv(file.choose(), header = TRUE)

# reshape the data
D_tall = reshape(D, varying = 2:7, v.names = "measure", 
                 timevar = "test_trial", idvar = "ID", 
                 new.row.names = 1:96, direction = "long")

# order data
D_tall = D_tall[order(D_tall$ID),]

# rename levels of test_trial variable 
D_tall$test_trial = revalue(x = as.factor(D_tall$test_trial), c("1" = "helper1", "2"="hinderer1", "3" = "helper2", 
                                                                "4"="hinderer2", "5" = "helper3", "6"="hinderer3"))

# add a helper/hinderer columns

D_tall$row.names = NULL
D_tall$test_trial = NULL
D_tall$help_hind_col = NULL


D_tall$test_trial = rep(c(0,1), times = 48)

D_tall$test_trial = revalue(x = as.factor(D_tall$test_trial), 
                               c("0" = "helper", "1"="hinderer"))
# reorder columns
D_tall = as.data.frame(D_tall[,c(1,2,4,3)])
names(D_tall)

###################################
## FOLLOW-UP PLANNED COMPARISONS ##
###################################
#  permutation global function
set.seed(2021)
b = rep(0,4000) 
for(i in 1:4000){
  y = sample(D_tall$measure, replace=TRUE)
  lm_1 = lme(y~test_trial, random=~1|ID, data = D_tall) 
  b[i] = fixed.effects(lm_1)[[2]]
}

lme.fit = lme(measure~test_trial, random=~1|ID, data = D_tall)
beta_actual = fixed.effects(lme.fit)[[2]]

beta_actual
sum(abs(b) > beta_actual)/4000



# bootstrap global function
bbB.lme.fit  <- function(d,i) {  
  d <- d[i,]
  thecoef <- tryCatch({bootfm.lme <- lme(measure ~ test_trial, data=d,
                                         random=~1|ID)
  fixef(bootfm.lme)},
  error = function(e) {print(e)
    return(rep(NA,length(fixef(bootfm.lme))))})
  return(thecoef)
}

set.seed(2021)
bbB.lme.Bootobj = boot(D_tall, bbB.lme.fit , R=4000)
bbB.lme.Bootobj

difBootconfint = 2.033229  + 1.96*c(-0.1864418, 0.1864418)
difBootconfint


# assumptions checks 
## NORMALITY CHECKS

par(mfrow=c(1,2)) 
for (ii in c("helper","hinderer"))  hist(D_tall$measure[D_tall$test_trial==ii], breaks=5)
par(mfrow=c(1,1)) 

# formal test of normality
shapiro.ps = rep(0,2)
for(i in c("helper","hinderer")) {
  shap.calc = shapiro.test(D_tall$measure[D_tall$test_trial==i])
  shapiro.ps[i] = shap.calc$p.value
}
shapiro.ps


# main analysis
lme.fit.main = lme(measure~test_trial, random=~1|ID, data = D_tall)
anova.lme(lme.fit.main)


mean(D_tall$measure[D_tall$test_trial=="helper"])
sd(D_tall$measure[D_tall$test_trial=="helper"])

mean(D_tall$measure[D_tall$test_trial=="hinderer"])
sd(D_tall$measure[D_tall$test_trial=="hinderer"])


# figures
condition_barplot = ggplot(D_tall, aes(test_trial, measure, fill=test_trial)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Choice Preference") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 8)) +
  scale_fill_manual(values = c("black", "azure3")) +
  theme_bw() 


##################
## CONTROL DATA ##
##################

# load data
D = read.csv(file.choose(), header = TRUE)

# reshape the data
D_tall = reshape(D, varying = 2:7, v.names = "measure", 
                 timevar = "test_trial", idvar = "ID", 
                 new.row.names = 1:96, direction = "long")

# order data
D_tall = D_tall[order(D_tall$ID),]

# rename levels of test_trial variable 
D_tall$test_trial = revalue(x = as.factor(D_tall$test_trial), c("1" = "helper1", "2"="hinderer1", "3" = "helper2", 
                                                                "4"="hinderer2", "5" = "helper3", "6"="hinderer3"))

# add a helper/hinderer columns 
D_tall$help_hind_col = rep(c(0,1), times = 48)

D_tall$help_hind_col = revalue(x = as.factor(D_tall$help_hind_col), 
                               c("0" = "helper", "1"="hinderer"))

D_tall$row.names = NULL
D_tall$test_trial = NULL

D_tall$test_trial = rep(c(0,1), times = 48)

D_tall$test_trial = revalue(x = as.factor(D_tall$test_trial), 
                               c("0" = "helper", "1"="hinderer"))

D_tall$help_hind_col = NULL

# reorder columns 
D_tall = as.data.frame(D_tall[,c(1,3,2)])

# main analysis
lme.fit.main = lme(measure~test_trial, random=~1|ID, data = D_tall)
anova.lme(lme.fit.main)

lm.fit.main = lm(measure~test_trial, data=D_tall)
anova_stats(lm.fit.main)

# determine value of using mixed-effects model
anova.lme(lme.fit.main, lm.fit.main)


mean(D_tall$measure[D_tall$test_trial=="helper"])
sd(D_tall$measure[D_tall$test_trial=="helper"])

mean(D_tall$measure[D_tall$test_trial=="hinderer"])
sd(D_tall$measure[D_tall$test_trial=="hinderer"])

# figures
condition_barplot = ggplot(D_tall, aes(test_trial, measure, fill=test_trial)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Choice Preference") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 8)) +
  scale_fill_manual(values = c("black", "azure3")) +
  theme_bw() 
