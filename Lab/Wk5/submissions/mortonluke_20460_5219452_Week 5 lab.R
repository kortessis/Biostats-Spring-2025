################# NK Comments #################
# Remember that it is good coding practice to clear your envrionment at the top
# of every script with the line rm(list = ls())
################# NK Comments #################


setwd("/Users/lukemorton/Desktop/Bio stats/week 5")
diabetes.df <- read.csv(file = 'diabetes.csv')

# Start

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
# Checkpoint 1: Code Below. Bound are .5 to .7
################# NK Comments #################
# Looks good. 
################# NK Comments #################

num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')

num.frogs.sampled <- 30
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
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
# Checkpoint 2: 
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
lower.bound.60 <- qbinom(0.20, size = num.frogs.sampled, prob = prob.inf) / num.frogs.sampled
upper.bound.60 <- qbinom(0.80, size = num.frogs.sampled, prob = prob.inf) / num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', 
     ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(lower.bound.60, upper.bound.60), 
       lty = 2, 
       col = 'red')
# Checkpoint 3
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
lower.bound.95 <- qbinom(0.025, size = num.frogs.sampled, prob = prob.inf) / num.frogs.sampled
upper.bound.95 <- qbinom(0.975, size = num.frogs.sampled, prob = prob.inf) / num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', 
     ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(lower.bound.95, upper.bound.95), 
       lty = 2, 
       col = 'red')
################# NK Comments #################
# Looks good. 
################# NK Comments #################




# Checkpoint 4: In a smaller sample size the data is more spread out and there is more variability. In a larger sample size the data is closer together and there is less variability. The error also decreases with a larger sample size. 
################# NK Comments #################
# The reduced variability is the most important part of the effect of sample 
# size on the sampling distribution of the mean. 
################# NK Comments #################




fish.size.sd <- 10
sample.size <- 100
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
# Checkpoint 5: At 30 = 1.825742. at 50 = 1.414214, at 100 = 1
################# NK Comments #################
# You've got the right numbers here. Show the code next time and then you won't
# have to transcribe the output. It will just show when I run the code.  
# You can do it all in one line like this. 
# sample.size <- c(30,50,100)
# fish.size.sd/sqrt(sample.size)
################# NK Comments #################



# Let's pick a bunch of sample sizes.
sample.sizes <- seq(from = 1, to = 100, by = 1)
# State the variability in the population in fish length
sd.fish.length <- 5
# Now calculate the SE of the mean for every sample size
SE.mean <- sd.fish.length/sqrt(sample.sizes)
# Let's take a look
plot(sample.sizes, SE.mean, typ = 'o', pch = 19,
     xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean',
     ylim = c(0,5))
abline(h = 0, lty = 2)
str(diabetes.df)
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)
# Checkpoint 6: The command removes the rows where the cholesterol is missing. It checks to see if there is a value for cholesterol then filters out the values. 

chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
# Create a bunch of possible means we could calcualte
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
# Find their associated probabilities of occurrence
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
# And plot them
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.025, 0.975), # Ask for the 2.5% and 97.5% quantile
                         mean = chol.mean, # Specify the mean of the sampling distribution
                         sd = chol.se) # Specify the standard deviation of the sampling dist.
# This is the standard error.
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)


# Checkpoint 7: 
chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
# Create a bunch of possible means we could calcualte
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
# Find their associated probabilities of occurrence
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
# And plot them
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.05, 0.95), 
                         mean = chol.mean, # Specify the mean of the sampling distribution
                         sd = chol.se) # Specify the standard deviation of the sampling dist.
# This is the standard error.
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'blue', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
################# NK Comments #################
# Looks good. 
################# NK Comments #################



our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)
# Checkpoint 9: Code below for 99% confidence interval. The 99% confidence interval is larger than the 95%. 
our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99) 
################# NK Comments #################
# Yes, when the confidence interval is larger, it reflects less uncertainty in 
# the estimate, and so the region needs to be larger. 
################# NK Comments #################



# Checkpoint 10: Code Below.
our.model <- lm(height ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.90)
confint(our.model, level = 0.95)
confint(our.model, level = 0.99)
################# NK Comments #################
# Nice work. 
################# NK Comments #################
