################# NK Comments #################
# Remember that it is good coding practice to clear your envrionment at the top
# of every script with the line rm(list = ls())
################# NK Comments #################


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

# Checkpoint 1: See the eight lines of code below. As the graph shows, the bounds on reasonable sample prevalence are approximately .5 and .7.
num.frogs.sampled <- 100
prob.inf <- 0.6
poss.outcomes <- 0:num.frogs.sampled
outcomes.probs <- dbinom(poss.outcomes, size = num.frogs.sampled, prob = prob.inf)
inf.prevalence <- poss.outcomes/num.frogs.sampled
plot(inf.prevalence, outcomes.probs, pch = 19,
     xlab = '# Infected Frogs in Sample', ylab = 'Probability',
     main = 'Sampling Distribution for Infection Prevalence')
################# NK Comments #################
# Looks good to me. 
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

# Checkpoint 2: See the 13 lines of code below. 
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
# Nice job.
################# NK Comments #################



# Checkpoint 3: See the 13 lines of code below. 
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
################# NK Comments #################
# Nice job.
################# NK Comments #################




# Checkpoint 4: This distribution looks even more "normal", with the mean right on 3 instead of slightly below 3. This makes sense, as the CLT tells us that the DSM approaches a normal distribution as the sample size increases.
################# NK Comments #################
# Even more important is the range of possible outcomes. When the sample size is
# 100 instead of 10, the distribution is much narrower. This is a sign that the 
# chance of getting sample means very different from the population mean is very
# small. 
################# NK Comments #################


fish.size.sd <- 10
sample.size <- 20
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size

# Checkpoint 5: See the 4 lines of code below. When the sample size is 30 fish, the SEx is 1.825742, when the sample size is 50 fish, the SEx is 1.414214, and when the sample size is 100 fish, the SEx is 1.
fish.size.sd <- 10
sample.size <- c(30, 50, 100)
SE.mean.fish.size <- fish.size.sd/sqrt(sample.size)
SE.mean.fish.size
################# NK Comments #################
# Looks good.
################# NK Comments #################



sample.sizes <- seq(from = 1, to = 100, by = 1)
sd.fish.length <- 5
SE.mean <- sd.fish.length/sqrt(sample.sizes)
plot(sample.sizes, SE.mean, typ = 'o', pch = 19,
     xlab = 'Number of Fish In Sample', ylab = 'Standard Error of the Mean',
     ylim = c(0,5))
abline(h = 0, lty = 2)

setwd("/Users/lilygetter/Documents/Biology380")
diabetes.df <- read.csv(file="diabetes.csv")
str(diabetes.df)
hist(diabetes.df$chol, xlab = 'Cholesterol')
summary(diabetes.df$chol)
new.diabetes.df <- subset(diabetes.df, subset = !(is.na(chol)))
summary(new.diabetes.df$chol)

# Checkpoint 6: The function "is.na" calls up all values that are NA. By putting the exclamation point in front, we tell it to call up anything that is not NA. So, we are making avariable of all data points whose variables are not NA. 
################# NK Comments #################
# Yeppers
################# NK Comments #################



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

# Checkpoint 7: See lines of code below. The 90% confidence interval for the mean of cholesterol in this population is 204-211. This is illustrated in the graoh produced by the lines of code below.
my.quantiles <- qnorm(c(0.05, 0.95), mean = chol.mean, sd = chol.se)
my.quantiles
plot(possible.mean.chol, poss.mean.prob, typ = 'l',
     xlab = 'Cholesterol Means', lwd = 3,
     ylab = 'Probability', xlim = c(195,220))
abline(v = my.quantiles, col = 'red', lty = 2)
abline(v = chol.mean, col = 'black', lty = 2, lwd = 3)
################# NK Comments #################
# Nice plot.
################# NK Comments #################


our.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(our.model, level = 0.95)



# Checkpoint 9: See lines of code below. The 99% confidence interval is 202.1085-213.583. This is a larger range than the 95% confidence interval. 

my.model <- lm(chol ~ 1, data = new.diabetes.df)
confint(my.model, level = 0.99)
################# NK Comments #################
# Very clear.
################# NK Comments #################


# Checkpoint 10: See lines of code below. The 90% confidence interval is 65.69627-66.34393 inches, the 95% confidence interval is 65.63395-66.40625 inches, and the 99% confidence interval is 65.51172-66.52848 inches.

summary(diabetes.df$height)
new.heightdiabetes.df <- subset(diabetes.df, subset = !(is.na(height)))
summary(new.heightdiabetes.df$height)
model.height <- lm(height ~ 1, data = new.heightdiabetes.df)
confint(model.height, level = 0.90)
confint(model.height, level = 0.95)
confint(model.height, level = 0.99)
################# NK Comments #################
# Great job. 
################# NK Comments #################


























