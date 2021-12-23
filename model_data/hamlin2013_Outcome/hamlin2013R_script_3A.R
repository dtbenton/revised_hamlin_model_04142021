########################################################
########################################################
########################################################
#############                              #############
#############         HAMLIN (2013)        #############
#############                              #############
########################################################
########################################################
########################################################
# load all relevant libraries:
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
options(scipen=9999)

###################
## SIMULATION 1A ##
###################

## ORGANIZE DATA ##

# load data
D = read.csv(file.choose(), header = TRUE)


# reshape the data
D_tall = reshape(D, varying = 3:6, v.names = "measure", 
                 timevar = "unit_type", idvar = "ID", 
                 new.row.names = 1:64, direction = "long")

# order data
D_tall = D_tall[order(D_tall$ID),]


# names D_tall
names(D_tall)

# rename the levels of the following factors: c(D_tall$event, D_tall$unit_type)
D_tall$event = revalue(x = as.factor(D_tall$event), 
                       c("1" = "First Trial", 
                         "2"="Second Trial"))

D_tall$unit_type = as.factor(rep(c(0,1), each = 2, times = 16))
D_tall$unit_type = revalue(x = as.factor(D_tall$unit_type), 
                           c("0" = "Interaction Unit", 
                             "1"="NonInteraction Unit"))

D_tall$char_type = as.factor(rep(c(0,1), each = 4, times = 8))
D_tall$char_type = revalue(x = as.factor(D_tall$char_type), 
                           c("0" = "Successful Hinderer", 
                             "1"="Failed Hinderer"))

# remove extraneous columns
D_tall$row.names = NULL

# reorder columns in 'D_tall' dataframe
D_tall = D_tall[,c(1,2,3,5,4)]


## DETERMINE WHETHER RANDOM EFFECT OF "SUBJ" IS NECESSARY ##
model_1 = lme(measure ~ 1, random=~1|ID,
              data=D_tall)

model_2 = lm(measure~1, data = D_tall)

anova(model_1,model_2)

## Descriptive statistics ##
mean(D_tall$measure)

# Helper
mean(D_tall$measure[D_tall$unit_type=="Interaction Unit" & D_tall$char_type=="Successful Hinderer"])
sd(D_tall$measure[D_tall$unit_type=="Interaction Unit" & D_tall$char_type=="Successful Hinderer"])

mean(D_tall$measure[D_tall$unit_type=="NonInteraction Unit" & D_tall$char_type=="Successful Hinderer"])
sd(D_tall$measure[D_tall$unit_type=="NonInteraction Unit" & D_tall$char_type=="Successful Hinderer"])

# Hinderer
mean(D_tall$measure[D_tall$unit_type=="Interaction Unit" & D_tall$char_type=="Failed Hinderer"])
sd(D_tall$measure[D_tall$unit_type=="Interaction Unit" & D_tall$char_type=="Failed Hinderer"])

mean(D_tall$measure[D_tall$unit_type=="NonInteraction Unit" & D_tall$char_type=="Failed Hinderer"])
sd(D_tall$measure[D_tall$unit_type=="NonInteraction Unit" & D_tall$char_type=="Failed Hinderer"])
# unit
mean(D_tall$measure[D_tall$unit_type=="Interaction Unit"])
sd(D_tall$measure[D_tall$unit_type=="Interaction Unit"])

mean(D_tall$measure[D_tall$unit_type=="NonInteraction Unit"])
sd(D_tall$measure[D_tall$unit_type=="NonInteraction Unit"])
# character
mean(D_tall$measure[D_tall$char_type=="Helper"])
mean(D_tall$measure[D_tall$char_type=="Hinderer"])

## RUN MODELS ##
lm.fit = lm(measure~(unit_type + char_type)^2, data = D_tall)
anova(lm.fit)

## POSTHOC MODELS ##
t.test(D_tall$measure[D_tall$unit_type=="Interaction Unit"],
       D_tall$measure[D_tall$unit_type=="NonInteraction Unit"],
       alternative = c("two.sided"), paired = TRUE)


## PLOTS ##
condition_barplot = ggplot(D_tall, aes(char_type, measure, fill = unit_type)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Interaction Expectation") + # change the label of the y-axis
  theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + # remove the major and minor grids
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black", "gray", "black", "gray")) +
  theme_classic() + # this changes the size and potentially weight of the facet labels
  labs(x = "Character Type") +
  labs(fill = "Unit Type")
