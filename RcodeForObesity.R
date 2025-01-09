#########################################The socioeconomic and demographic vars correlation test##################################################
install.packages("RCurl")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/OritMazza/childhood_obesity_Israel/refs/heads/main/SocioEconomicand2014obesityPercentageGitHub.csv")
cohort.df <- read.csv(text = x)
cohort.df

summary(cohort.df)

cohort.df$X.1<-as.factor(cohort.df$X.1)

#Corelation table
# ensure the results are repeatable
set.seed(7)
# load the library
new.cohort.df<-cohort.df

library(caret)
library(ggplot2)
library(lattice)
# calculate correlation matrix
str(new.cohort.df)
correlationMatrix <- cor(new.cohort.df[,5:15])
?cor
# summarize the correlation matrix
print(correlationMatrix)

??correlationMatrix

# find attributes that are highly corrected (ideally >0.7)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

class(highlyCorrelated)

highlyCorrelated<-c(highlyCorrelated)

#Print the name of the columns
#Cor>0.7: columns in new.cohort.df are +4 than the correlation matrix

names(new.cohort.df[,c(9, 12  ,15  ,6 ,11, 14)])
#####################The highly correlated vars#############################
#> names(new.cohort.df[,c(9, 12  ,15  ,6 ,11, 14)])
#[1] "AVERAGE.MONTHLY.INCOME.PER.CAPITA"                                
#[2] "PERCENT.OF.WAGE.AND.INCOME.EARNERS...ABOVE.TWICE.THE.AVERAGE.WAGE"
#[3] "PERCENT.OF.ACADEMIC.DEGREE.HOLDERS..OF.AGED.25.54"                
#[4] "AVERAGE.NUMBER.OF.DAYS.ABOURD"                                    
#[5] "PERCENT.OF.WAGE.AND.INCOME.EARNERS...BELOW.THE.MINIMUM.WAGE"      
#[6] "PERCENT.OF.WAGE.AND.INCOME.EARNERS..OF.AGED.15.AND.OVER" 
############################################################################

##############################################ANOVA test for the pbesity prevalence from 2014 to 2018#########################################
x <- getURL("https://raw.githubusercontent.com/OritMazza/childhood_obesity_Israel/refs/heads/main/ForANOVAGithub.csv")
OBESITY.df <- read.csv(text = x)
OBESITY.df
summary(OBESITY.df)
OBESITY.df<-OBESITY.df[,-1]

library(ggpubr)#ggpubr for creating easily publication ready plots
library(rstatix)#rstatix provides pipe-friendly R functions for easy statistical analyses
library(tidyverse)#tidyverse for data manipulation and visualization
#get_summary_stats(weight, type = "mean_sd")

# Gather columns t1, t2 and t3 into long format
# Convert id and time into factor variables
OBESITY.df <- OBESITY.df %>%
  gather(key = "time", value = "score", X2014, X2015, X2016 , X2017 ,X2018) %>%
  convert_as_factor(ID, time)
class(OBESITY.df$score)

#Compute some summary statistics of the score by groups (time): mean and sd (standard deviation)
OBESITY.df %>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")

bxp <- ggboxplot(OBESITY.df, x = "time", y = "score", add = "point")
bxp

#The normality assumption can be checked by computing Shapiro-Wilk test for each time point. If the data is normally distributed, the p-value should be greater than 0.05
OBESITY.df %>%
  group_by(time) %>%
  shapiro_test(score)

qqplot(OBESITY.df, "score", facet.by = "time")

res.aov <- anova_test(data = OBESITY.df, dv = score, wid = ID, within = time)
get_anova_table(res.aov)

#Post-hoc tests
# pairwise comparisons
pwc <- OBESITY.df %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
