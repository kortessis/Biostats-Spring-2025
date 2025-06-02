# BIO 380 Lab; Week 5
# Katie Sung

rm(list=ls())

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

# Checkpoint 1: The bounds for reasonable sample prevalence in this case are .5 and .7
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
################# NK Comments #################
# Looks good. 
################# NK Comments #################

num.frogs.sampled <- 30
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
qbinom(0.20, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.80, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(56,64)/num.frogs.sampled,
       lty = 2,
       col = 'red')
################# NK Comments #################
# Looks good. 
################# NK Comments #################


# Checkpoint 3: 
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
qbinom(.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(50,69)/num.frogs.sampled,
       lty = 2,
       col = 'red')


# Checkpoint 4: This distributions differs when the sample size is 100 vs 10 because it is even closer to a normal distribution, in line with central limit theory. 
################# NK Comments #################
# Most importantly, the range of sample means is much narrower with 100 
# individuals in the sample rather than 10. This narrower range of possible 
# means reflects the idea that there is less uncertainty in the population mean. 
################# NK Comments #################



fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size

# Checkpoint 5: For a sample size of 30- standard error is 1.825742, sample size of 50- standard error is 1.414214, sample size of 100- standard error is 1
fish.size.sd <- 10
sample.size1 <- 30
sample.size2 <- 50
sample.size3 <- 100
SE.mean.fish.size1 <- fish.size.sd/sqrt(sample.size1)
SE.mean.fish.size2 <- fish.size.sd/sqrt(sample.size2)
SE.mean.fish.size3 <- fish.size.sd/sqrt(sample.size3)
SE.mean.fish.size1
SE.mean.fish.size2
SE.mean.fish.size3
################# NK Comments #################
# Looks good. You could do this all in one line if you'd like. Use the following
# code:
sample.size <- c(30,50,10)
fish.size.sd/sqrt(sample.size)
################# NK Comments #################




sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = 'o', pch = 19,
     xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean',
     ylim = c(0,5))
abline(h = 0, lty = 2)

setwd("~/Desktop/BIO380/Wk5")
diabetes.df <- read.csv(file="diabetes.csv")
str(diabetes.df)
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)

# Checkpoint 6: !is.na(chol) works to selects all NA values in the data set. The ! switches to values in the set that are not NA
################# NK Comments #################
# Yep.
################# NK Comments #################



summary(new.diabetes.df$chol)
chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))

sampl.quantiles <- qnorm(c(0.025, 0.975), mean = chol.mean, sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)

# Checkpoint 7: the 90% confidence interval for the distribution is 204.1996 to 211.4920
sampl.quantiles <- qnorm(c(0.05, 0.95), mean = chol.mean, sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)

our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)



# Checkpoint 9: the 99% confidence interval is 202.11 to 213.58. This is larger than the 90% confidence interval since there is a wider range of data that the mean could fall into. 
our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)
################# NK Comments #################
# Looks good. 
################# NK Comments #################



# Checkpoint 10: The 90% confidence interval is 65.69627 to 66.34393, the 95% confidence interval is 65.63395 to 66.40625, the 99% confidence interval is 65.51172 to 66.52848
summary(diabetes.df$height)
new.diabetesforheight.df <- subset(diabetes.df, subset = !(is.na(height)))
summary(new.diabetesforheight.df$height)
height.model <- lm(height ~ 1, data = new.diabetesforheight.df)
confint(height.model, level = 0.90)
confint(height.model, level = 0.95)
confint(height.model, level = 0.99)

################# NK Comments #################
# Nice work.  
################# NK Comments #################
