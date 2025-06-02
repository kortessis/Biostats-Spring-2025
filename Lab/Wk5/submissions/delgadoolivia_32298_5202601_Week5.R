################# NK Comments #################
# Remember that it's good practice to clear your environment at the top of your 
# script using rm(list = ls())
################# NK Comments #################


setwd("~/Desktop/BIO380/Week 5")
diabetes.df <- read.csv(file = 'diabetes.csv')

##### CHECKPOINT 1 #####
num.frogs.sampled <- 100
prob.inf <- 0.6 

poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes,size=num.frogs.sampled,prob=prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled

plot(inf.prevalence,outcomes.probs,pch=19, xlab='#InfectedFrogsinSample',ylab='Probability', main='Sampling Distribution for Infection Prevalence')

# We are most likely to see a sample where 60% of the frogs are infected, but we could see as high as 70% or as low as 50% infected. 
################# NK Comments #################
# Sounds good to me. 
################# NK Comments #################


##### CHECKPOINT 2 #####
qbinom(0.20, size = num.frogs.sampled, prob = prob.inf) #56
qbinom(0.8, size = num.frogs.sampled, prob = prob.inf) #64
plot(inf.prevalence,outcomes.probs,
     pch=19, xlab='#InfectedFrogsinSample',ylab='Probability',
     main='Sampling Distribution for Infection Prevalence (60%)',)
abline(v = c(56,64)/num.frogs.sampled, lty = 2, col = 'red')
################# NK Comments #################
# Looks good.
################# NK Comments #################



##### CHECKPOINT 3 #####
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf) #50
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf) #69
plot(inf.prevalence,outcomes.probs,
     pch=19, xlab='#InfectedFrogsinSample',ylab='Probability',
     main='Sampling Distribution for Infection Prevalence (95%)',)
abline(v = c(50,69)/num.frogs.sampled, lty = 2, col = 'red')
################# NK Comments #################
# Nice figure
################# NK Comments #################





##### CHECKPOINT 4 #####
# This distribution is different because there are more bins in the histogram and the mean estimate is more definitely 3 because the most central bins are equal to each other. 
################# NK Comments #################
# One of the most important differences is that the distribution is much 
# narrower. That means the probability of sampling a mean very different from 
# 3 is extremely small. You can see this by putting both on the same scale. 
################# NK Comments #################



##### CHECKPOINT 5 #####
fish.size.sd <- 10 
sample.size <- 100 
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
# SE30 = 1.83, SE50 = 1.41, SE = 1
################# NK Comments #################
# Looks good, but show the code next time. 
################# NK Comments #################



##### CHECKPOINT 6 #####
new.diabetes.df <- subset(diabetes.df,subset= !(is.na(chol)))
# The is.na(chol) subset part finds the row that has the NA value and creates a subset of it. However, by putting the ! in front of it, the code does the opposite and returns all the rows that don't have the NA into the subset. 
################# NK Comments #################
# Yeppers.
################# NK Comments #################




##### CHECKPOINT 7 #####
chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
chol.se <- sd(diabetes.df$chol, na.rm=T) / sqrt(chol.n)
possible.mean.chol <- seq(from=150, to=250, length=10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean= chol.mean, sd= chol.se)
sampl.quantiles <- qnorm(c(0.05, 0.95), mean = chol.mean, sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ= 'l', xlab='Cholesterol Means',
     lwd =3, ylab='Probability', xlim= c(195,220))
abline(v=sampl.quantiles, col= 'red',lty=2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
################# NK Comments #################
# Looks great. 
################# NK Comments #################


##### CHECKPOINT 9 #####
our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.99)
# The interval is (202, 213) which is slightly larger than the previous confidence interval. 
################# NK Comments #################
# That's correct. 
################# NK Comments #################


##### CHECKPOINT 10 #####
confint(our.model, level = 0.90) # CI = (204.2, 211,5)
confint(our.model, level = 0.95) # CI = (203.5, 212.2)
confint(our.model, level = 0.99) # CI = (202.1, 213.6)
################# NK Comments #################
# Everything here is good, except this is for cholesterol. The question asks for
# you to do this with the heights of people in this dataset. 
################# NK Comments #################
