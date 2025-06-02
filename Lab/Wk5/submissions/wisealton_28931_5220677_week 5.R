## BIO 380 Week 5 lab
## Alton Wise
################# NK Comments #################
# Remember that it is good coding practice to clear your envrionment at the top
# of every script with the line rm(list = ls())
################# NK Comments #################


setwd("/Users/altonwise/Downloads")
read.csv('diabetes.csv')


num.frogs.sampled <- 5
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
(outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf))
plot(poss.outcomes, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Number of Infected Frogs')
num.frogs.sampled <- 30
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = '# Infected Frogs in Sample', ylab = 'Probability',main = 'Sampling Distribution for Infection Prevalence')

##Checkpoint 1: Plot the sampling distribution of infection prevalence when we sample 100 frogs. What are the bounds on reasonable sample prevalence in this case?
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = '# Infected Frogs in Sample', ylab = 'Probability',main = 'Sampling Distribution for Infection Prevalence')
## The reasonable sample prevalence would be between 50 and 68 %
################# NK Comments #################
# Looks good. 
################# NK Comments #################




num.frogs.sampled <- 30
cumul.prob <- pbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf)
plot(inf.prevalence, cumul.prob, pch = 19, xlab = 'Infection Prevalence in a Sample',ylab = 'Cumulative Probability')
qbinom(0.25, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.75, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = '# Infected Frogs in Sample', ylab = 'Probability',main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(16,20)/num.frogs.sampled,lty = 2,col = 'red')
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = '# Infected Frogs in Sample', ylab = 'Probability',main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(13,23)/num.frogs.sampled,lty = 2,col = 'red')

##Checkpoint 2: Find the region of 60% of the most likely prevalence values for a sample of 100 frogs. Plot the boundaries on your probability distribution from checkpoint 1.
num.frogs.sampled <- 100
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = '# Infected Frogs in Sample', ylab = 'Probability',main = 'Sampling Distribution for Infection Prevalence')
qbinom(0.60, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.40, size = num.frogs.sampled, prob = prob.inf)
abline(v = c(59,61)/num.frogs.sampled,lty = 2,col = 'red')
## It says the region is between 59-61 frogs
################# NK Comments #################
# You've chosen the range that has only 20% of likely values, not 60%. You chose
# the 60% and 40% quantiles. That means 40% of data points are below this region
# and 100%-60% = 40% are above this region. 
# To find the 60% region, note that this mean 40% is outside the region: 20% 
# above and 20% below. So you need to find the 20% and 80% quantiles instead. 
################# NK Comments #################




## Checkpoint 3: Now do the same for the region of 95% of the most likely sample prevalences.
qbinom(0.95, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.05, size = num.frogs.sampled, prob = prob.inf)
abline(v = c(52,68)/num.frogs.sampled,lty = 2,col = 'red')
################# NK Comments #################
# Same issue here as in checkpoint 2. You found the 90% of most likely sample
# prevalences. 
################# NK Comments #################



##Checkpoint 4: How does this distribution differ from the distribution when we only sampled 10 individuals?
## This sample is more acurate and is not swayed as much by the extremes. The sample with a 100 has a bit higher average
################# NK Comments #################
# One of the most important differences is that the distribution with more 
# samples is narrower. 
################# NK Comments #################


fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size


## Checkpoint 5: What is SE  ̄X when we sample 30 fish, 50 fish, and 100 fish?
sample.size <- 30
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
## 1.8257
sample.size <- 50
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
## 1.414
sample.size <- 100
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
## 1
################# NK Comments #################
# Looks good. If you want to do this all in one line, use the following code:
sample.size <- c(30,50,100)
fish.size.sd/sqrt(sample.size)
################# NK Comments #################



sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = 'o', pch = 19,xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean',ylim = c(0,5))
abline(h = 0, lty = 2)
diabetes.df <- read.csv('diabetes.csv')
str(diabetes.csv)
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)
##Checkpoint 6: How does the subset command !is.na(chol) work? 
##  This command works by filtering out any rows where cholestrol does not have a value
################# NK Comments #################
# is.na(chol) asks which of the entries in the column of chol is NA and spits 
# out TRUE if it is and FALSE if it is not. The symbol "!" turns TRUEs to FALSEs 
# and FALSEs to TRUEs.
################# NK Comments #################


chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = 'l',xlab = 'Cholesterol Means', lwd = 3,ylab = 'Probability', xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.025, 0.975),mean = chol.mean, sd = chol.se)                        
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)


##Checkpoint 7: Find the 90% confidence interval for the mean of cholesterol in this population.
sampl.quantiles <- qnorm(c(0.10, 0.90))
abline(v = sampl.quantiles, col = 'red', lty = 2)
sampl.quantiles_90 <- qnorm(c(0.05, 0.95), mean = chol.mean,  sd = chol.se)
## checkpoint 7:  204.1996 211.4920
################# NK Comments #################
# Note that this is an 80% interval because it removes the 10% low values and 
# the 10% (100%-90%) of high values. That's 2 10% removals = 80%. This range is
# too narrow. 
################# NK Comments #################



## Checkpoint 8:
sampl.quantiles_99 <- qnorm(c(0.005, 0.995),  mean = chol.mean, sd = chol.se)
sampl.quantiles_99
## checkpoint 8 : 202.1358 213.5557 this is larger than the 95% confidence interval


## checkpoint 9:
height.mean <- mean(diabetes.df$height)
height.sd <- sd(diabetes.df$height)
height.se <- height.sd / sqrt(length(diabetes.df$height))
height.quantiles_90 <- qnorm(c(0.05, 0.95), mean = height.mean, sd = height.se)
height.quantiles_95 <- qnorm(c(0.025, 0.975), mean = height.mean, sd = height.se)
height.quantiles_99 <- qnorm(c(0.005, 0.995), mean = height.mean, sd = height.se)
height.quantiles_90
height.quantiles_95
height.quantiles_99
################# NK Comments #################
# All the code looks right, but the confidence intervals are all NA's. That is 
# because there are missing entries for height just like there was for 
# cholesterol. The way to fix this is to remove missing values before you do
# your calculations, just like we did with cholesterol. 
################# NK Comments #################