################# NK Comments #################
# Remember to include rm(list = ls()) to clear your environment
################# NK Comments #################
num.frogs <- 100
prob.inf <- 0.6
poss.frog.outcomes <- 0:num.frogs
frog.outcomes.probs <- dbinom(poss.frog.outcomes, size = num.frogs, prob = prob.inf)
inf.prevalence <- poss.frog.outcomes/num.frogs
plot(inf.prevalence, frog.outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
# Checkpoint 1 lines of code 1-6 reasonable bounds would be 0.4-0.8 # of infected frogs in sample
qbinom(0.2, size = num.frogs, prob = prob.inf)
qbinom(0.8, size =num.frogs, prob = prob.inf)
plot(inf.prevalence, frog.outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(56,64)/num.frogs, lty = 2, col = 'red')
# Checkpoint 2 lines of code 8-11
################# NK Comments #################
# Good.
################# NK Comments #################


qbinom(0.025, size = num.frogs, prob = prob.inf)
qbinom(0.975, size = num.frogs, prob = prob.inf)
plot(inf.prevalence, frog.outcomes.probs, pch = 19, xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(50,69)/num.frogs, lty = 2, col = 'red')
# Checkpoint 3 lines of code 13-16
################# NK Comments #################
# Very clear
################# NK Comments #################


# Checkpoint 4 the distribution of 100 individuals has a much higher normal character then the distribution of 10 individuals, the mean of speed in the sample also appears to be much closer to 3 (albeit both are close) the rest of the possible means demonstrated in the sampling distribution of 200 individuals also follows a more traditional normal distribution curve
################# NK Comments #################
# Another important difference is that the range of outcomes (i.e., sample 
# means) is much narrower. 
################# NK Comments #################



fish.size.sd <- 10
sample.size <- 30
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
fish.size.sd <- 10
sample.size <- 50
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
fish.size.sd <- 10
sample.size <- 100
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
# Checkpoint 5 30 fish (1.82 cm), 50 fish (1.41), 100 fish (1 cm)
################# NK Comments #################
# Yep
################# NK Comments #################



setwd('/Users/dominicamato/Desktop/Biostatistics Labs/Wk5')
diabetes.df <- read.csv("diabetes.csv")
str(diabetes.df)
?!is.na
# Checkpoint 6 this subset command acts as a logical negation, which means that for any data which does not contain a value for a particular characteristic, this data will be excluded 
# ################# NK Comments #################
# Yeppers
################# NK Comments #################



new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df)
chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n)
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
sampl.quantiles <- qnorm(c(0.05, 0.95), mean = chol.mean, sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l', xlab = 'Cholesterol Means', lwd = 3, ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd =3)
# Checkpoint 7 lines of code 37-48
################# NK Comments #################
# Looks good. 
################# NK Comments #################


our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)
# Checkpoint 9 This confidence interval, as expected, is larger than that of 95%, this means that the range of data is larger, (smallest value smaller and largest value larger)
################# NK Comments #################
# Not range of data, but range of average cholesterol!
################# NK Comments #################


quantile(new.diabetes.df$height, probs = c(0.90, 0.95, 0.99), na.rm = TRUE)
# Checkpoint 10 71 (90%), 72 (95%), 75 (99%)

################# NK Comments #################
# This gives the quantiles of height. Not the confidence intervals!
################# NK Comments #################
