# BIO 380 Lab Week 5
# Janani Krishnakumar
rm(list=ls())
setwd("C:/Users/crush/Desktop/Janani/College/Wake Forest/2024-2025/
      Biostatistics/Week 5")

########################################################################

### REVIEW: SAMPLING DISTRIBUTIONS ###

# Population Properties:
###

# Central Tendency: Mean, Median
# Variability: St. dev., Quantiles, Min, Max
# Measures of Association: Correlation, Covariance, Functional relationships

# Binomial Distribution:

# Small sample size distribution

num.frogs.sampled <- 5
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, 
                         size=num.frogs.sampled, p=prob.inf)
plot(poss.outcomes, outcomes.probs, pch = 19,
     xlab='# Infected Frogs in Sample', ylab='Probability',
     main='Sampling Distribution for Number of Infected Frogs')

# Medium sample size distribution w/ Infection Prevalence
num.frogs.sampled <- 30
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, 
                         size=num.frogs.sampled, p=prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled #***
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab='# Infected Frogs in Sample', ylab='Probability',
     main='Sampling Distribution for Number of Infected Frogs')

# Checkpoint 1: #########################
num.frogs.sampled <- 100
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, 
                         size=num.frogs.sampled, p=prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled #***
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab='# Infected Frogs in Sample', ylab='Probability',
     main='Sampling Distribution for Number of Infected Frogs')
# Bounds: ~0.4 - ~0.8
################# NK Comments #################
# Nice job.
################# NK Comments #################



# Calculating The Most Likely Outcomes
###

cumul.prob <- pbinom(poss.outcomes,
                     size=num.frogs.sampled, p=prob.inf)
# Plot range of 50% most likely outcomes
plot(inf.prevalence, cumul.prob, pch=19, 
     xlab='Infection Prevalence in a Sample', ylab='Cumulative Probability')
qbinom(0.25, size=num.frogs.sampled, prob=prob.inf) # 16
qbinom(0.75, size=num.frogs.sampled, prob=prob.inf) # 20
abline(v=c(16,20)/num.frogs.sampled,
       lty=2,
       col='red')
# Plot range of 95% most likely outcomes
plot(inf.prevalence, cumul.prob, pch=19, 
     xlab='Infection Prevalence in a Sample', ylab='Cumulative Probability')
qbinom(0.025, size=num.frogs.sampled, prob=prob.inf) # 13
qbinom(0.975, size=num.frogs.sampled, prob=prob.inf) # 23
abline(v=c(13,22)/num.frogs.sampled,
       lty=2,
       col='red')

# Checkpoint 2 + 3: #########################
# From checkpoint 1:
# num.frogs.sampled <- 100
# poss.outcomes <- 0:num.frogs.sampled
# outcomes.probs <- dbinom(poss.outcomes, 
#                         size=num.frogs.sampled, p=prob.inf)
# inf.Prevalence <- poss.outcomes/num.frogs.sampled #***
# plot(inf.prevalence, outcomes.probs, pch = 19,
#      xlab='# Infected Frogs in Sample', ylab='Probability',
#      main='Sampling Distribution for Number of Infected Frogs')

# 60% region
qbinom(0.20, size=num.frogs.sampled, prob=prob.inf) # 56
qbinom(0.80, size=num.frogs.sampled, prob=prob.inf) # 64
abline(v=c(56,64)/num.frogs.sampled,
       lty=2,
       col='red')

# 95% region
qbinom(0.025, size=num.frogs.sampled, prob=prob.inf) # 50
qbinom(0.975, size=num.frogs.sampled, prob=prob.inf) # 69
abline(v=c(50,69)/num.frogs.sampled,
       lty=2,
       col='red')

################# NK Comments #################
# Great job. Very neat and clear. 
################# NK Comments #################


# Normal Distribution:

# Checkpoint 4: #########################
# This distribution is a lot more symmetrical about the middle.
# Additionally, the peak is a bit sharper.

################# NK Comments #################
# The most important difference is how much narrower the distribution is than 
# the one with 10 individuals. Look at the values on the x-axis in each case. 
# This is a sign that you are very unlikely to get sample means very far away 
# from the population mean when you sample 100 individuals.  
################# NK Comments #################


########################################################################

### STANDARD ERROR: ###

# AKA Standard Deviation

# St. dev. of mean (CLT): St. dev. of random variable applying to an
#       individual / sqrt(num individuals in the sample)

fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size # 2.236068
# Mean fish size of a sample of 20 fish changes by 2.4cm on avg,

# Checkpoint 5: #########################
fish.size.sd <- 10
# Sample of 30 fish
sample.size <- 30
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size # 1.825742
# Sample of 50 fish
sample.size <- 50
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size # 1.414214
# Sample of 100 fish
sample.size <- 100
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size # 1
# **SE goes down if the sample size is larger
################# NK Comments #################
# Yeppers. 
################# NK Comments #################




########################################################################

### DIABETES: CONFIDENCE INTERVALS OF THE MEAN: ###

# Read Diabetes data
diabetes.df <- read.csv(file = "diabetes.csv")
str(diabetes.df)

# Testing the data
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
#   Min.  1st Qu. Median  Mean    3rd Qu. Max.    NA's 
#   78.0  179.0   204.0   207.8   230.0   443.0   1
new.diabetes.df <- subset(diabetes.df, subset=!(is.na(chol)))
summary(new.diabetes.df$chol)
#   Min.  1st Qu. Median  Mean    3rd Qu. Max.
#   78.0  179.0   204.0   207.8   230.0   443.0

# Checkpoint 6: #########################
# It operates as a nested if-statement, where for each row in diabetes.df,
#   if a row has a value of NA for chol, it is not added to the new dataframe.


# Confidence Intervals Basics:

# Calculating SE
chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
chol.se <- sd(diabetes.df$chol, na.rm=T)/sqrt(chol.n)
chol.se # 2.216743

# Creating Normal dist to approx sampling dist
possible.mean.chol <- seq(from=150, to=250, length=10000)
poss.mean.prob <- dnorm(possible.mean.chol,
                        mean=chol.mean, sd=chol.se)
plot(possible.mean.chol, poss.mean.prob, typ='l',
     xlab='Cholesterol Means', lwd=3,
     ylab='Probability', xlim=c(195,220))

# Define norm dist quantiles + add to plot
sampl.quantiles <- qnorm(c(0.025,0.975),
                         mean=chol.mean, sd=chol.se)
sampl.quantiles # 203.5010 212.1905
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)

# Checkpoint 7: #########################
new.sampl.quantiles <- qnorm(c(0.05,0.95),
                         mean=chol.mean, sd=chol.se)
new.sampl.quantiles # 204.1996 211.4920
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
################# NK Comments #################
# All good, except that you plotted the 90% confidence interval again. You 
# needed "new.sampl.quantiles" in abline(), not "sampl.quantiles". Honest 
# mistake.
################# NK Comments #################



# Calculating Confidence Intervals:

our.model <- lm(chol~1, data=new.diabetes.df)
confint(our.model, level=0.95)
#              2.5 %   97.5 %
#  (Intercept) 203.4879 212.2037

# Checkpoint 9: #########################
confint(our.model, level=0.95)
#              2.5 %   97.5 %
#  (Intercept) 203.4879 212.2037
################# NK Comments #################
# Looks good to me. 
################# NK Comments #################




# Checkpoint 10: #########################
height.model <- lm(height~1, data=new.diabetes.df)
confint(height.model, level=0.90)
confint(height.model, level=0.95)
confint(height.model, level=0.99)
# Run Results:
#              5 %      95 %
#  (Intercept) 65.69057 66.33966
#              2.5 %    97.5 %
#  (Intercept) 65.62811 66.40211
#              0.5 %    99.5 %
#  (Intercept) 65.50561 66.52462
################# NK Comments #################
# Very clear and efficient. 
################# NK Comments #################





