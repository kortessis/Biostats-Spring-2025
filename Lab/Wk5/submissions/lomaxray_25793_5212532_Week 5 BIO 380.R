rm(list = ls())
num.frogs.sampled <- 5
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
(outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf))
plot(poss.outcomes, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Number of Infected Frogs')
num.frogs.sampled <- 30
prob.inf <- 0.6 
(poss.outcomes <- 0:num.frogs.sampled)
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
#Checkpoint 1: The bounds on reasonable sample prevalence are again as high as 70% infected or as low as 45% infected. 


num.frogs.sampled <- 100
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
#Done with checkpoint 

cumul.prob <- pbinom(poss.outcomes, size = num.frogs.sampled, p = prob.inf)
plot(inf.prevalence, cumul.prob, pch = 19, xlab = 'Infection Prevalence in a Sample',
     ylab = 'Cumulative Probability')
num.frogs.sampled <- 30
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
qbinom(0.25, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.75, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability', main = 'Sampling Distribution for Infection Prevalence')
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
#Checkpoint 2: 
num.frogs.sampled <- 100
prob.inf <- 0.6
(poss.outcomes <- 0:num.frogs.sampled)
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
#Checkpoint 3: 
qbinom(0.025, size = num.frogs.sampled, prob = prob.inf)
qbinom(0.975, size = num.frogs.sampled, prob = prob.inf)
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
abline(v = c(50,69)/num.frogs.sampled,
       lty = 2,
       col = 'red')
################# NK Comments #################
# Nice figures. 
################# NK Comments #################




#Checkpoint 4: This distribution is more symmetrical and the range of both axes has decreased. The mean stays the same however. 
################# NK Comments #################
# Yes. The narrower range, but no change in the mean is the main effect of 
# increasing sample size. 
################# NK Comments #################

fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
#Checkpoint 5: For 30 the SE mean is 1.825742, for 50 it's 1.414214, for 100 it's 1. 
sample.size.fish.30 <- 30
SE.mean.fish.size.30 <- fish.size.sd/sqrt(sample.size.fish.30)
SE.mean.fish.size.30
sample.size.fish.50 <- 50
SE.mean.fish.size.50 <- fish.size.sd/sqrt(sample.size.fish.50)
SE.mean.fish.size.50
sample.size.fish.100 <- 100
SE.mean.fish.size.100 <-fish.size.sd/sqrt(sample.size.fish.100) 
SE.mean.fish.size.100
################# NK Comments #################
# You could do this all at once with the code
# sample.size <- c(30,50,100)
# fish.size.sd/sqrt(sample.size)
################# NK Comments #################
# Done with checkpoint



fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = 'o', pch = 19,
     xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean',
     ylim = c(0,5))
abline(h = 0, lty = 2)
getwd()
setwd("/Users/raymondlomax/Desktop/BIO380/Wk 5")
diabetes.df <- read.csv(file = 'diabetes.csv')
str(diabetes.df)
hist(diabetes.df$chol, xlab = 'Cholestorol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)


#Checkpoint 6: Subset command !is.na(chol) finds the row that corresponds to an NA and it gets rid of the specific individual that has NA data.
################# NK Comments #################
# Yes. is.na() finds the NAs and the "!" turns the TRUEs (i.e., the NAs) to 
# FALSEs. 
################# NK Comments #################



chol.mean <- mean(new.diabetes.df$chol)
chol.n <- length(new.diabetes.df$chol)
(chol.se <- sd(diabetes.df$chol, na.rm = T)/sqrt(chol.n))
possible.mean.chol <- seq(from = 150, to = 250, length = 10000)
poss.mean.prob <- dnorm(possible.mean.chol, mean = chol.mean, sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
    xlab = 'Cholesterol Means', lwd = 3,
    ylab = 'Probability', xlim = c(195,220))
sampl.quantiles <- qnorm(c(0.025,0.975),
                         mean = chol.mean,
                         sd = chol.se)
sampl.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = sampl.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)



#Checkpoint 7: 5% 204.1996 and 95% 211.4920
confidence.interval.90 <- qnorm(c(0.05,0.95),
                        mean = chol.mean,
                        sd = chol.se)
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = confidence.interval.90, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
#Done with checkpoint 
################# NK Comments #################
# Looks good.
################# NK Comments #################


our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)
#Checkpoint 9: It has wider range than the 95% confident interval so that means it is larger.
our.model.2 <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model.2, level = 0.99)





#Checkpoint 10: 
our.model.3 <-  lm(height ~ 1, data = new.diabetes.df)
confint(our.model.3, level = 0.90)
confint(our.model.3, level = 0.95)
confint(our.model.3, level = 0.99)
################# NK Comments #################
# Good job.
################# NK Comments #################