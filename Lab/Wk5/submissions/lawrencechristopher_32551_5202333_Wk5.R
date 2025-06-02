################# NK Comments #################
# Remember that it is good coding practice to clear your envrionment at the top
# of every script with the line rm(list = ls())
################# NK Comments #################


#Checkpoint 1

num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')

#Bounds of reasonable sample prevalence seem to be from 48 to 75 #of infected frogs

#Checkpoint 2

qbinom(0.2, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.8, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(56,64)/num.frogs.sampled,
       lty = 2,
       col = 'red')

################# NK Comments #################
# Looks good.
################# NK Comments #################


#Checkpoint 3
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(50,69)/num.frogs.sampled,
       lty = 2,
       col = 'red')

#Checkpoint 4
#This distribution has a tighter mean of sample and a more uniform normal distribution. This is expected because there is a higher sample size and more data to pull the mean of speed sample to the average. With only sampling 10 people, each speed sample has a very high relative weighting for calculating mean of speed in sample, so that small boost in probability from speed 4-6 can get over represented in smaller sampled data sets whereas the higher sampled size dataset pulls the means to the highest probility speed which is 3.
################# NK Comments #################
# Good argument.
################# NK Comments #################


#Checkpoint 5
fish.size.sd <- 10
sample.size <- c(30, 50, 100)
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size

# 30 - 1.825742 / 50 - 1.414214 / 100 - 1.000000 

#Checkpoint 6
#The !is.na(chol) just checks if any columns have NA values, if there is a value it will put TRUE and if not it will put FALSE. Value can be numerical or letters

#Checkpoint 7

# Create a bunch of possible means we could calcualte
diabetes.df <- read.csv("/Users/chris/Desktop/BIO 380/Wk5/diabetes.csv")
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
# Find their associated probabilities of occurrence
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
sampl.quantiles <- qnorm(c(0.05, 0.95), # Ask for the 2.5% and 97.5% quantile
                         mean = chol.mean, # Specify the mean of the sampling distribution
                         sd = chol.se) # Specify the standard deviation of the sampling dist.
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)

#Checkpoint 8

our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)

our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)

#99 - (Intercept) 202.1085 213.583 / 95 - (Intercept) 203.4879 212.2037
#99% confidence interval has a larger range

#Checkpoint 9
our.model <- lm(height ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)

#90 - (Intercept) 65.69057 66.33966 / 95 - (Intercept) 65.62811 66.40211 / 99 - (Intercept) 65.50561 66.52462

################# NK Comments #################
# Correct answers. Next time, just leave in the code to calculate these things.
################# NK Comments #################



