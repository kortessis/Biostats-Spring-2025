################# NK Comments #################
# Remember that it is good coding practice to clear your envrionment at the top
# of every script with the line rm(list = ls())
################# NK Comments #################


num.frogs.sampled <- 5
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
(outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf))
plot(poss.outcomes, outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Number of Infected Frogs')
num.frogs.sampled <- 30
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')


#Start of Checkpoint 1
num.frogs.sampled.1 <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled.1
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled.1, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled.1
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
#It appears that in any given sample, it is most likely that 60% of frogs are infected, but it also seems that we could reasonably see as low as 50% or as high as 70% infected.
#End of Checkpoint 1
################# NK Comments #################
# Good. 
################# NK Comments #################



plot(inf.prevalence, outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(16,20)/num.frogs.sampled, lty = 2, col = 'red')
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(13,23)/num.frogs.sampled, lty = 2, col = 'red')



#Start of Checkpoint 2
qbinom(0.20, size = num.frogs.sampled.1, prob = prob.inf)
qbinom(0.80, size = num.frogs.sampled.1, prob = prob.inf)
#The region of 60% of the most prevalent values ranges from 56 to 64.
poss.outcomes.1 <- 0:num.frogs.sampled.1
outcomes.probs.1 <- dbinom(poss.outcomes.1, size = num.frogs.sampled.1, prob = prob.inf)
inf.prevalence.1 <- poss.outcomes.1/num.frogs.sampled.1
plot(inf.prevalence.1, outcomes.probs.1, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(56,64)/num.frogs.sampled.1, lty = 2, col = 'red')
#End of Checkpoint 2
################# NK Comments #################
# Nice figure.
################# NK Comments #################


#Start of Checkpoint 3
qbinom(0.025, size = num.frogs.sampled.1, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled.1, prob = prob.inf)
#The region of 95% of the most prevalent values ranges from 50 to 69.
poss.outcomes.1 <- 0:num.frogs.sampled.1
outcomes.probs.1 <- dbinom(poss.outcomes.1, size = num.frogs.sampled.1, prob = prob.inf)
inf.prevalence.1 <- poss.outcomes.1/num.frogs.sampled.1
plot(inf.prevalence.1, outcomes.probs.1, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(50,69)/num.frogs.sampled.1, lty = 2, col = 'red')
################# NK Comments #################
# Looks good. 
################# NK Comments #################


#Checkpoint 4: In general, as sample size of any population increases, the sampling distribution approaches a normal graph. The same is true in this population when sampling for average speed. With 100 individuals in the sample, the graph becomes more symmetrical and looks more like a normal plot than the graph of 10 individuals did. Additionally, the spread becomes narrower in the distribution for a sample of 100 individuals. This means that the standard error will also become smaller with more individuals.
################# NK Comments #################
# The narrower spread is one of the most important differences. 
################# NK Comments #################


fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#Start of checkpoint 5:
fish.size.sd <- 10
sample.size <- 30
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#SE with a sample size of 30 is 1.825742
fish.size.sd <- 10
sample.size <- 50
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#SE with a sample size of 50 is 1.414214
fish.size.sd <- 10
sample.size <- 100
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#SE with a sample size of 100 is 1
#End of Checkpoint 5
################# NK Comments #################
# You could do this all in three lines. Like this:
fish.size.sd <- 10
sample.size <- c(30,50,100)
(SE.mean <- fish.size.sd/sqrt(sample.size))
################# NK Comments #################


sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = 'o', pch = 19, xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean', ylim = c(0,5))
abline(h = 0, lty = 2)
setwd("/Users/natewhitworth/Desktop/Bio 380/Wk5")
diabetes.df <- read.csv(file = 'diabetes.df.csv')
str(diabetes.df)
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)
#Checkpoint 6: First, the portion of the command "is.na(chol)" checks for values that are missing for the variable "chol." Then, the "!" reverses the command, meaning that the whole command keeps the values that are present for the variable "chol"
################# NK Comments #################
# Exactly.
################# NK Comments #################


chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = 'l', xlab = 'Cholesterol Means', lwd = 3, ylab = 'Probability', xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.025, 0.975), mean = chol.mean, sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l', xlab = 'Cholesterol Means', lwd = 3, ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)


#Start of Checkpoint 7
sampl.quantiles.1 <- qnorm(c(0.05, 0.95), mean = chol.mean, sd = chol.se)
sampl.quantiles.1
#The 90% confidence interval for the mean of cholestrol in this population ranges from 204.1996 to 211.4920.
plot(possible.mean.chol, poss.mean.prob, typ = 'l', xlab = 'Cholesterol Means', lwd = 3, ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles.1, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
#End of Checkpoint 7
################# NK Comments #################
# Nice figure. 
################# NK Comments #################



our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)


#Start of Checkpoint 9
our.model.9 <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model.9, level = 0.99)
#The confidence interval is 202.1085, 213.583
#This 99% confidence interval is larger than our 95% confidence interval
#End of Checkpoint 9


#Start of Checkpoint 10
height.diabetes.df <- subset(diabetes.df, subset = !(is.na(height)))
summary(height.diabetes.df$height)
our.model.9 <- lm(height ~ 1, data = height.diabetes.df)
confint(our.model.9, level = 0.90)
#The 90% confidence interval is 65.69627 to 66.34393
confint(our.model.9, level = 0.95)
#The 90% confidence interval is 65.63395 to 66.40625
confint(our.model.9, level = 0.99)
#The 90% confidence interval is 65.51172 to 66.52848
#End of Checkpoint 10

################# NK Comments #################
# Nice work. 
################# NK Comments #################
