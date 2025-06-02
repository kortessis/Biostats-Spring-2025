################# NK Comments #################
# Remember that it is good coding practice to clear your envrionment at the top
# of every script with the line rm(list = ls())
################# NK Comments #################


setwd("/Users/hopenitsche/Desktop/Spring 2025/Biostats/Lab/Wk5")
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
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
#Checkpoint 1: Plot the sampling distribution of infection prevalence when we sample 100 frogs. What are the bounds on reasonable sample prevalence in this case?
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
#In this case, the bounds of the reasonable sample prevalance is between 0.5 and 0.7 based on the shape of the sampling distribution. 
################# NK Comments #################
# Looks good. 
################# NK Comments #################



cumul.prob <- pbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf)
plot(inf.prevalence, cumul.prob, pch = 19, xlab = 'Infection Prevalence in a Sample',
     ylab = 'Cumulative Probability')
qbinom(0.25, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.75, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(16,20)/num.frogs.sampled,
       lty = 2,
       col = 'red')
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(13,23)/num.frogs.sampled,
       lty = 2,
       col = 'red')

#Checkpoint 2: Find the region of 60% of the most likely prevalence values for a sample of 100 frogs. Plot the boundaries on your probability distribution from checkpoint 1.
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
qbinom(0.20, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.80, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(56,64)/num.frogs.sampled,
       lty = 2,
       col = 'red')
################# NK Comments #################
# Very clear.
################# NK Comments #################


#Checkpoint 3: Now do the same for the region of 95% of the most likely sample prevalences
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(50,69)/num.frogs.sampled,
       lty = 2,
       col = 'red')
################# NK Comments #################
# Looks good. 
################# NK Comments #################




#Checkpoint 4: How does this distribution differ from the distribution when we only sampled 10 individuals?
#When you sample 100 individuals, the distribution looks more like a normal distribution and we see more values closer to the average than when we sampled 10 indivduals. This supports the idea of the central limit theorem, as the more individuals we sample, the closer we get to a normal distribution. 
################# NK Comments #################
# Yes, but the most important part is the width of the distribution. Look at the
# x-axis in both cases. It is much narrower in the case with 100 individuals in
# the sample. This is a sign that the standard error is samller. 
################# NK Comments #################




fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#Checkpoint 5: What is SE¯X when we sample 30 fish, 50 fish, and 100 fish?
#For 30 fish: SE-X=1.83
fish.size.sd <- 10
sample.size <- 30
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#For 50 Fish: SE-X=1.41 
fish.size.sd <- 10
sample.size <- 50
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#For 100 fish: SE-X= 1
fish.size.sd <- 10
sample.size <- 100
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
################# NK Comments #################
# Looks good. If you want to do this all in one line, use the following code
sample.size <- c(30,50,100)
fish.size.sd/sqrt(sample.size)
################# NK Comments #################


sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = 'o', pch = 19,
     xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean',
     ylim = c(0,5))
abline(h = 0, lty = 2)
diabetes.df <- read.csv("diabetes.csv")
str(diabetes.df)
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)
#Checkpoint 6: How does the subset command !is.na(chol) work?
#It looks for all of the places where there are not NAs/ where data points are located, in order to separate them from the NAs in the data set. 
################# NK Comments #################
# Very good. 
################# NK Comments #################



chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.025, 0.975),  
                         mean = chol.mean, 
                         sd = chol.se) 
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)


#Checkpoint 7: Find the 90% confidence interval for the mean of cholesterol in this population.
sampl.quantiles <- qnorm(c(0.05, 0.95),  
                         mean = chol.mean, 
                         sd = chol.se) 
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
################# NK Comments #################
# Looks good. 
################# NK Comments #################



our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)

#Checkpoint 9: Find the 99% confidence interval for cholesterol. Is it larger or smaller than the 95% confidence interval?
our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)
#           0.5 %  99.5 %
#(Intercept) 202.1085 213.583
#The 99% Confidence Interval is larger than the 95% confidence interva for this data set, since it encompasses more data. 
################# NK Comments #################
# Not more data, but more probable outcomes. Remember, confidence intervals 
# reflect a probabilistic statement about where the population parameter lies. 
# Since a 99% CI only leaves a 1% chance of being wrong, then it needs to 
# encompass a larger range of possible means. Sample variances and standard 
# deviations give us information about where the data lies.
################# NK Comments #################



#Checkpoint 10: Find the 90%, the 95%, and the 99% for height in this dataset.
our.model.height <- lm(height ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.90)
our.model <- lm(height ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)
our.model <- lm(height ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)
################# NK Comments #################
# Looks good. Note that you only need to define the model once. Once the model 
# is defined, you can extract all kinds of information from it. The function
# lm() just fits a model. No need to refit the same model three times. 
################# NK Comments #################
