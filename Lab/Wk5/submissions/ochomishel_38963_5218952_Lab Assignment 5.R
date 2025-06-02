# BIO 380 Lab; Week 5
# <Mishel Ocho>
rm(list = ls())
setwd("C:/Users/ochom/Downloads/BIO380/Wk2")

num.frogs.sampled <- 5
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
(outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf))
plot(poss.outcomes, outcomes.probs, pch = 19, xlab = "# Infected Frogs in Sample", ylab = "Probability", main = "Sampling Distribution for Number of Infected Frogs")
num.frogs.sampled <- 30
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = "# Infected Frogs in Sample", ylab = "Probability", main = "Sampling Distribution for Number of Infected Frogs")

#Checkpoint 1
num.frogs.sampled <- 100
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = "# Infected Frogs in Sample", ylab = "Probability", main = "Sampling Distribution for Number of Infected Frogs")
#30% and 90% are reasonalble bounds
################# NK Comments #################
# 30% and 90% are a bit more than reasonable. The chance of getting something 
# outside this range (<30 and >90) is 0.0009
1 - (pbinom(0.9*num.frogs.sampled, num.frogs.sampled, prob.inf)-pbinom(0.3*num.frogs.sampled, num.frogs.sampled, prob.inf))
################# NK Comments #################


num.frogs.sampled <- 30
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
cumul.prob <- pbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf)
plot(inf.prevalence, cumul.prob, pch = 19, xlab = "Infection Prevalence in a Sample", ylab = "Cumulative Probability")
qbinom(0.25, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.75, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = "# Infected Frogs in Sample', ylab = 'Probability", main = "Sampling Distribution for Infection Prevalence")
abline(v = c(16,20)/num.frogs.sampled, lty = 2, col = "red")
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,xlab = "# Infected Frogs in Sample', ylab = 'Probability", main = "Sampling Distribution for Infection Prevalence")
abline(v = c(13,23)/num.frogs.sampled, lty = 2, col = "red")

#Checkpoint 2
num.frogs.sampled <- 100
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = "# Infected Frogs in Sample", ylab = "Probability", main = "Sampling Distribution for Infection Prevalence")
p.20.frogs <- qbinom(0.2, size = num.frogs.sampled, prob = prob.inf)
p.80.frogs <- qbinom(0.8, size = num.frogs.sampled, prob = prob.inf)
abline(v = c(p.20.frogs,p.80.frogs)/num.frogs.sampled, lty = 2, col = "red")
plot(inf.prevalence, outcomes.probs, pch = 19, xlab = "# Infected Frogs in Sample", ylab = "Probability", main = "Sampling Distribution for Infection Prevalence")
p.2.5.frogs <- qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
p.97.5.frogs <- qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
abline(v = c(p.2.5.frogs,p.97.5.frogs)/num.frogs.sampled, lty = 2, col = "red")
################# NK Comments #################
# Figures look good. 
################# NK Comments #################


#Checkpoint 4: THe distribution is more even on either side and the most extreme values are less extreme.
################# NK Comments #################
# That's right. The outcomes are much less variable and closer to the mean.
################# NK Comments #################

fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size

#Checkpoint 5
fish.size.sd/sqrt(30)
fish.size.sd/sqrt(50)
fish.size.sd/sqrt(100)

sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = "o", pch = 19, xlab = "Number of Fish In Sample", ylab = "Standard Error of the Mean", ylim = c(0,5))
abline(h = 0, lty = 2)
diabetes.df <- read.csv(file = "diabetes.csv")
str(diabetes.df)
hist(diabetes.df$chol, xlab = "Cholesterol")
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)

#Checkpoint 6: The command checks the data points to see if any of them are na, them creates a new data frame with all the values that are not na.


chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = "l", xlab = "Cholesterol Means", lwd = 3, ylab = "Probability", xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.025, 0.975), mean = chol.mean, sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = "l", xlab = "Cholesterol Means", lwd = 3, ylab = "Probability", xlim = c(195,220))
abline(v = sampl.quantiles, col = "red", lty = 2)
abline(v = chol.mean, col = "black", lty = 2, lwd = 3)

#Checkpoint 7
plot(possible.mean.chol, poss.mean.prob, typ = "l", xlab = "Cholesterol Means", lwd = 3, ylab = "Probability", xlim = c(195,220))
sampl.quantiles.90 <- qnorm(c(0.05, 0.95), mean = chol.mean, sd = chol.se)
abline(v = sampl.quantiles.90, col = "red", lty = 2)
abline(v = chol.mean, col = "black", lty = 2, lwd = 3)

our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)

#Checkpoint 9
confint(our.model, level = 0.99)
#It is larger.




our.model.height <- lm(height ~ 1, data = new.diabetes.df)
confint(our.model.height, level = 0.90)
confint(our.model.height, level = 0.95)
confint(our.model.height, level = 0.99)
################# NK Comments #################
# Nice work. 
################# NK Comments #################