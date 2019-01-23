library(ggplot2)
load("rdas/research_funding_rates.rda") 
research_funding_rates
boxplot(research_funding_rates$success_rates_total,research_funding_rates$success_rates_men, 
     research_funding_rates$success_rates_women, xlab=c("Total, Men, Women"), 
     main ="Research funding success rates")
ggsave("figs/plot001.png")
summary(research_funding_rates)

# Compute the totals that were successful and the totals that were not as follows:
library(dplyr)
totals <- research_funding_rates %>%  select(-discipline) %>% summarize_all(funs(sum)) %>%  
  summarize(yes_men = awards_men, no_men = applications_men - awards_men, yes_women = awards_women,              
            no_women = applications_women - awards_women) 
totals
# Larger percent of men than women received awards
totals %>% summarize(percent_men = yes_men/(yes_men+no_men),                      
                       percent_women = yes_women/(yes_women+no_women)) 

# Percent funding when randomly assigned
funding_rate <- totals %>%  summarize(percent_total = 
                    (yes_men + yes_women)/(yes_men + no_men +yes_women + no_women)) %>% .$percent_total
funding_rate

# Chi-square test is to compare any signficant difference between the groups with the help of a two-by-two table
# Create the two-by-two Observed data table:
  Observed_data <- data.frame(awarded = c("no", "yes"),                       
                           men = c(totals$no_men, totals$yes_men),                      
                           women = c(totals$no_women, totals$yes_women))
  Observed_data

# Have a look at the two-by-two expected data table:
  Expected_data <- data.frame(awarded = c("no", "yes"),      
             men = (totals$no_men + totals$yes_men) * c(1 - funding_rate, funding_rate),        
             women = (totals$no_women + totals$yes_women)*c(1 - funding_rate, funding_rate))
Expected_data 
# The idea is to compare the observed to expected.

chisq_test <- Observed_data %>%    
  select(-awarded) %>%   
  chisq.test()
chisq_test
qchisq(0.95,df=1)
# H0 is There is no difference beween the groups
# H1 is there is difference between the groups
# Pvalue is >0.05 at 95% significance level. So Ho cannot be rejected.
# The Chisquare value of 3.8111 wont cross the critical value at 95% signifcance level for df=1 is 3.841459, 
# so H0 cannot be rejected
# Conclusion is that ther is no difference between the groups
# Odds Ratio
odds_men <- (Observed_data$men[2] / sum(Observed_data$men)) / 
  (Observed_data$men[1] / sum(Observed_data$men))
odds_women <- (Observed_data$women[2] / sum(Observed_data$women)) / 
  (Observed_data$women[1] / sum(Observed_data$women))
odds_ratio <- odds_men / odds_women
odds_ratio

# Larger sample size will give lesser p-value but odds ratio remains same 
# Confidence interval for Odds Ratio
log_or <- log( odds_men / odds_women ) 
se <- Observed_data %>%  select(-awarded) %>% summarize(se = sqrt(sum(1/men) + sum(1/women))) %>%  .$se 
ci <- log_or + c(-1,1) * qnorm(0.975) * se 
ci

######################################################################################################

# Fisher test with lady tasing tea for milk poured before or after tea was poured.
tab <- matrix(c(3,1,1,3),2,2) 
rownames(tab)<-c("Poured Before","Poured After") 
colnames(tab)<-c("Guessed before","Guessed after") 
tab
# idea is test actual to guess really works or was it just random happening
fisher.test(tab)
fisher.test(tab, alternative="greater") 
# H0 is that there is no difference
# H1 is that there s difference between groups
# p-value is >0.05 so H0 cannot be rejected. It cannot be said with statistical sig that the lady can actucally guess.
# when sample size increase p-vlaue gets reduced but odd ratio remains same.


############################################################################################################
# Mcnemar test for paired data

x<-matrix(c(21,9,2,12),2,2)
mcnemar.test(x)
mcnemar.test(x,correct=FALSE) #a logical indicating whether to apply continuity correction when computing the 
# test statistic.
library(exact2x2)
mcnemar.exact(x)

###################################################################################################################3

# Binomial test "an exact test of a simple null hypothesis about the probability of success in a Bernoulli experiment"
# An experiment crossing flowers of two genotypes that produce progeny with white flowers (recessive) of assumed 
# proportion ¼ and progeny with purple flowers (dominant) of assumed proportion ¾.
# Test the assumption (null hypothesis) that these proportions are correct given that we have empirical data 
# from 900 plants, 625 of which have purple flowers, and the remainder (275) have white flowers. 
# x=number of successes (purple flowers), n=total in sample, and p=proportion of successes to be tested (3/4).
# H0 = proportion =3/4, H1= proportion not equal to 3/4
binom.test(x=625,n=900,p=3/4) #exact test of a binomial hypothesis, usually done when 
# sample with small sample size.
# the 95% confidence interval for the proportion of successes being between 0.663 and 0.724, with the hypothesized 
# value of 0.75 lying outside the 95% confidence interval. p-value <0.05, H0 is rejected. says there is a statistical 
# diff in the observed and expected proportions of flowers.
prop.test(x=625, n=900, p = 3/4, alternative = "two.sided",
          correct = TRUE) # when N>30
# The function returns the value of Pearson's chi-squared test statistic, a p-value, a 95% 
# confidence intervals, an estimated probability of success
###################################################################################################################
# Two proportion sample test
# Compare the proportions of smokers in the two groups of individuals: different or same
# lung cancer = 500, smokers = 490
# healthy = 500, smokers = 400
prop.test(x = c(490, 400), n = c(500, 500)) # p-value<0.05, there is difference between groups

prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "less")  # p-value>0.05, cancer group is not having less proportion of 
# smokers than healthy

prop.test(x = c(490, 400), n = c(500, 500),
          alternative = "greater") # p-value<0.05 cancer group has more proportion of smokers
# than healthy groups. It can be concluded that smoking increase the chances for lung cancer.
# standard chi-square test in chisq.test() is exactly equivalent to prop.test() but it works 
# with data in matrix form.

#############################################

load("rdas/geHTdata.rda")
#create data vector of all control data 
controls<-c(geHTdata$c1,geHTdata$c2,geHTdata$c3,geHTdata$c4) 
#perform one-sample t test that true mean is 2000 
t.test(controls,mu=2000) 

# One Sample t-test 
# data:  controls  
# t = -2.174, df = 39, p-value = 0.03583 
# alternative hypothesis: true mean is not equal to 2000 
# 95 percent confidence interval:  1715.028 1989.722  
# sample estimates: mean of x   1852.375 
#  p-value of 0.03583 rejects the null hypothesis at a critical value (alpha level) of 0.05

##########################################

# Two tailed t testBioCConnection
treatments<-c(geHTdata$t1, geHTdata$t2, geHTdata$t3, geHTdata$t4)
t.test(controls,treatments) 
# Welch Two Sample t-test 
# data:  controls and treatments  
# t = -3.6163, df = 70.732, p-value = 0.0005564 
# alternative hypothesis: true difference in means is not equal to 0  
# 95 percent confidence interval:  -653.6098 -188.9902  
# sample estimates: mean of x mean of y   1852.375  2273.675
# p-value for this test very strongly rejects the null hypothesis 
# difference between the mean of the treatment group and control group
# some genes exhibit significantly different gene expression levels in treatments n control groups

################################################
# Paired T test

t.test(controls,treatments, paired = TRUE) 

# Paired t-test
# data:  controls and treatments
# t = -6.3945, df = 39, p-value = 1.468e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -554.5654 -288.0346
# sample estimates:
#  mean of the differences -421.3

# p-value for the test indicates there is a significant difference in gene 
# reject the null hypothesis
####################################################################################

# ANOVA (one way ANOVA)
# Compare more than 2 groups, parametric
load("rdas/protStruct.rda")
str(protStruct)

compareMethod <- protStruct[, 2:3]
compareMethod 

anova(lm(Correct~Method,data=protStruct)) 
# Residuals refers to the sum of squares between by the group name (method row) and 
# the sum of squares within as part of the "residuals" row. 

# ANOVA analysis tells us whether there is a significant difference between 3 or more groups 
# (treatments, factor levels).
# determine which groups differ with pairewise.t.test function. 

pairwise.t.test(protStruct$Correct,protStruct$Method)
# no difference between the GOR and PHD methods but that both of these differ significantly 
# from the CF AVG method. 

pairwise.t.test(protStruct$Correct,protStruct$Method,p.adj="fdr") # Using false discovery rate

# Graph for ANOVA
stripchart(Correct~Method, vert=T, data= protStruct)
title("Comparing Secondary Structure Prediction Methods") 
#Calculating group means, sd, and sem 
xbar<-tapply(protStruct$Correct,protStruct$Method,mean) 
s<-tapply(protStruct$Correct,protStruct$Method,sd) 
sem<-s/sqrt(12) 
arrows(1:3,xbar+2*sem,1:3,xbar-2*sem,angle=90,code=3) 
ggsave("figs/plot002.png")
#Two factor ANOVA
anova(lm(Correct~Method+Protein, data=protStruct)) 

# Based on this analysis the method factor is significant but the protein factor is not. 
#The F-values are the mean square for the sum of squares for that method divided by the sum of 
#squares error.  An additional concern with ANOVA models incorporating two or more factors is 
#the possibility that the factors interact and the interaction of factors is a significant 
#concern.  The analysis of interactions is not presented here, but this is a point of interest 
#to note. 
##################################################################################
# One sample Wilcoxon Test

# Wilcoxon test
# Wilcoxon's rank sum test (also known as the unpaired Wilcoxon rank sum test or 
# the Mann-Whitney U test)	It is the test for ordinal or continuous data. In contrast to 
# Student's t-test, does not require the data to be normally distributed. This test too can be used 
# for paired or unpaired data.
wilcox.test(protStruct$Correct,mu=0.5) 
# The results of this test concur with the results of the t-test that the central measure (mean or median) of the data 
# differs significantly from 0.5. 

#####################################################################################
# Unpaired 2 sample Wilcoxon Rank Sum Tests
load("rdas/medrank.rda")

group_by(medrank, Drug) %>%
  summarise(
    count = n(),
    medi_score = median(Score, na.rm = TRUE),
    IQR_score = IQR(Score, na.rm = TRUE),
    medi_rank = median(Rank, na.rm = TRUE),
    IQR_rank = IQR(Rank, na.rm = TRUE)
  )

oxycodone_rank <- medrank$Rank[medrank$Drug=="Oxycodone"]
ibuprofen_rank <- medrank$Rank[medrank$Drug=="Ibuprofen"]

relation <- wilcox.test(oxycodone_rank, ibuprofen_rank)
relation
# No difference between groups

########################################################################################################
# Unpaired Wilcoxon Rank Sum Tests
load("rdas/my_data.rda")

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

#  Compute two-samples Wilcoxon test - Method 1
women_weight <- my_data$weight[my_data$group=="Woman"]
men_weight <- my_data$weight[my_data$group=="Man"]
res <- wilcox.test(women_weight, men_weight)
res

# Compute two-samples Wilcoxon test - Method 2
res <- wilcox.test(weight ~ group, data = my_data,
                   exact = FALSE)
res

#to test whether the median men's weight is less than the median women's weight, type this:
wilcox.test(weight ~ group, data = my_data, 
            exact = FALSE, alternative = "less")
#to test whether the median men's weight is greater than the median women's weight, type this
wilcox.test(weight ~ group, data = my_data,
            exact = FALSE, alternative = "greater")

##############################################################################

# Pairwise Wilcoxon Rank Sum Tests
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
## These give warnings because of ties :
pairwise.wilcox.test(Ozone, Month)
pairwise.wilcox.test(Ozone, Month, p.adj = "bonf")
detach()

########################################################################

#Krusal Wallis Test

load("rdas/reactionR.rda")
str(reactionR)
tapply(reactionR$ReactionTime, reactionR$Drink, median)
#Alcohol  Coffee   Water 
#2.250   1.445   0.845
Water_summary<-summary(reactionR$ReactionTime[reactionR$Drink=='Water'])
Coffee_summary<-summary(reactionR$ReactionTime[reactionR$Drink=='Coffee'])
Alcohol_summary<-summary(reactionR$ReactionTime[reactionR$Drink=='Alcohol'])

compare1<-cbind(Water_summary,Coffee_summary,Alcohol_summary)
round(compare1,2)

boxplot(reactionR$ReactionTime~reactionR$Drink)

kruskal.test(reactionR$ReactionTime~reactionR$Drink)
# The simplest adjustment is the Bonferroni adjustment p.adj='bonferroni' which multiplies each Wilcoxon signed 
# rank p-value by the total number of Wilcoxon tests being carried out (here it is 3) while the exact=F stands for 
# the asymptotic test which allows tied ranks.

pairwise.wilcox.test(reactionR$ReactionTime,reactionR$Drink,p.adj='bonferroni',exact=F)
# Alcohol Coffee
# Coffee 0.042   -     
# Water  0.002   0.027

#########################################################################
# Friedman Test
load("rdas/Data.rda")
str(Data)
Data$Likert.f <- as.factor(Data$Likert)
XT <- xtabs( ~ Instructor + Likert.f,
             data = Data)
XT
#                  Likert
#Instructor       4 5 6 7 8 9 10
#'Bob Belcher'    2 1 4 0 0 0  1
#'Gene Belcher'   1 4 3 0 0 0  0
#'Linda Belcher'  0 0 1 1 4 1  1
#'Louise Belcher' 0 0 0 1 4 2  1
#'Tina Belcher'   0 1 0 2 2 2  1
prop.table(XT, 
           margin = 1)

library(lattice)

histogram(~ Likert.f | Instructor,
          data=Data,
          layout=c(1,5)      #  columns and rows of individual plots
)


friedman.test(Likert ~ Instructor | Rater,
              data = Data)
# OR
friedman.test(Data$Likert, Data$Instructor, Data$Rater)
#Friedman rank sum test
#data:  Likert and Instructor and Rater
#Friedman chi-squared = 23.139, df = 4, p-value = 0.0001188

Ratertable <- xtabs(Likert ~ Instructor + Rater, 
                    data = Data)
# can do rowmeans of ratertable to see the difference in mean ratings,the diff is tested statistically by friedmantest
### Conover test

library(PMCMR)

PT = posthoc.friedman.conover.test(y      = Data$Likert,
                                   groups = Data$Instructor,
                                   blocks = Data$Rater,
                                   p.adjust.method="fdr")
PT
#Pairwise comparisons using Conover's test for a two-way	
#                    balanced complete block design 
#data:  Data$Likert , Data$Instructor and Data$Rater 
#             'Bob Belcher' 'Gene Belcher' 'Linda Belcher' 'Louise Belcher'
#'Gene Belcher'   0.17328       -              -               -               
#'Linda Belcher'  2.8e-05       1.2e-06        -               -               
#'Louise Belcher' 1.9e-06       1.1e-07        0.27303         -               
#'Tina Belcher'   0.00037       8.8e-06        0.31821         0.05154         
#P value adjustment method: fdr 

#########################################################################

# Survival Analysis
# suvival analysis in R Ani Katchova
library(survival)
load("rdas/survival.rda")

head(survival)
str(survival)
summary(survival)

# Kaplan-Meier non-parametric analysis 
kmsurvival <- survfit(Surv(survival$Time, survival$Outcome) ~1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
     
# Kaplan-Meier non-parametric analysis by group
kmsurvival_grp <- survfit(Surv(survival$Time, survival$Outcome) ~ survival$Group)
summary(kmsurvival_grp)
plot(kmsurvival_grp,conf.int=FALSE,col=c("Red","Blue"),xlab="Time", ylab="Survival Probability")
legend("bottomleft", c("Group0", "Group1"), col=c("Red","Blue"), lty = 1)

# Cox proprtional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(survival$Time, survival$Outcome) ~ survival$Group, method = "breslow")
summary(coxph)
