rm(list = ls())
##################### Power analysis ####################################
# 
# Power analysis answers the question: What is the sample size that is needed to 
# detect a particular effect?

# Imagine that we are sampling a population for males and females and we are 
# interested in whether males and females equally common in the population. 
# Male versus female is a binary, categorical characteristic. So long as we 
# sample randomly, we can use the binomial as a sampling distribution. The 
# binomial has the parameters p (probability of "male") and n (sample size).
# 
# If we are interested in whether males and females are equally likely, our null 
# hypothesis is defined on the parameter p such that p = 0.5. That is a 
# statement that males and females are equally likely in the population. 
null.p <- 0.5 

# To do a hypothesis test, we find the sampling distribution for the male/female
# sex ratio under the null hypothesis. Let's first pick a sample size.  
sample.size <- 60

# We will use the binomial and convert from # of males to fraction male by 
# simply dividing by the sample size. This tells us the probaiblity of seeing 
# a certain fraction of males in our smaple under the null hypothesis of equal
# numbers of males and females.

# Here is a visual of the sampling distribution under the null hypothesis.
poss.num.males <- 0:sample.size
prob <- dbinom(poss.num.males, sample.size, null.p)
poss.frac.males <- poss.num.males/sample.size
plot(poss.frac.males, prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = 'Sampling Distribution under H0: Prob(Male) = 0.5')

# Now we pick a type I error level test. 
alpha <- 0.05

# Find the critical values for the test. 
crit.test.values <- qbinom(c(alpha/2, 1 - alpha/2), 
                           size = sample.size, 
                           prob = null.p)/sample.size
# Plot the critical values
abline(v = crit.test.values, col = 'red')
# Find the rejection region and label points in red.
rej.indx <- poss.frac.males < crit.test.values[1] | poss.frac.males > crit.test.values[2]
rej.region <- poss.frac.males[rej.indx]
# Label the rejection region points in red.
points(rej.region, prob[rej.indx], pch = 19, col = 'red')

# At this point, our test is set up. We now sample 60 individuals, count the 
# number of males, and then evaluate whether it is in the rejection region. 
# If it is, we reject the null hypothesis. If it is not, we fail to reject it. 

# But what if we have a suspicion that the actual fraction that is male is 55%, 
# what is our chance of rejecting the null? This is the essence of power. We 
# want to know the probability of correctly rejecting the null given the actual 
# fraction male is something different than 0.5. To do this, we need to pick a 
# possible alternative to the null. Here, we will pick prob(male) = 0.55. 
# 
# What we are doing now is eavluating the power to detect a particular "effect 
# size". An effect size is the difference between the null and what you aim to 
# detect. Let's calculate it. 

hyp.real.p <- 0.55 # Presumed real p value
(effect.size <- hyp.real.p - null.p)


# Now we create what the sampling distribution would be if the male fraction is 
# actually 55%. We do that again with a binomial distribution, but we need to 
# now use a different probability, prob(Male) = 0.55. 
alt.prob <- dbinom(poss.num.males, sample.size, hyp.real.p)

# Let's plot it on a paired plot. 
par(mfcol = c(2,1))
# First, the sampling distribution under the null.
plot(poss.frac.males, prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = 'Sampling Distribution under H0: Prob(Male) = 0.5')
points(rej.region, prob[rej.indx], pch = 19, col = 'red')

# Now, the sampling distribution under the alternative of a particular effect 
# size. 
plot(poss.frac.males, alt.prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = paste('Sampling Distribution under HA: Prob(Male) = ',hyp.real.p,
                  sep = ''))
# Now let's plot the rejection region of the null hypothesis test on this 
# sampling distribution. By doing so, we are getting a picture of how often we 
# would reject the null IF THE ALTERNATIVE OF A SPECIFIC EFFECT WERE TRUE. 
# This is the essence of a power analysis. 
points(rej.region, alt.prob[rej.indx], pch = 19, col = 'red')

# The power is the probability of rejecting the null under the alternative. 
# To calculate that probability, we just sum of the probability of the red 
# values of the sampling distribution under the alternative. 

(power <- sum(alt.prob[rej.indx]))


################## Increasing Power ######################################
# In this case, there are three ways to increase power. Really all we have here 
# is a statement about what the world is like (the effect size), our sampling
# scheme (the sample size), and our choice about Type I error. Changing any one
# alters the power. 
# 
# For the same alpha value and the same sample size, power is higher when 
# detecting larger effects. 
sample.size <- 60
effect.size <- 0.15
alpha = 0.05
hyp.real.p <- null.p + effect.size
alt.prob <- dbinom(poss.num.males, sample.size, hyp.real.p)
par(mfcol = c(2,1))
plot(poss.frac.males, prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = 'Sampling Distribution under H0: Prob(Male) = 0.5')
points(rej.region, prob[rej.indx], pch = 19, col = 'red')
plot(poss.frac.males, alt.prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = paste('Sampling Distribution under HA: Prob(Male) = ',hyp.real.p,
                  sep = ''))
points(rej.region, alt.prob[rej.indx], pch = 19, col = 'red')
(power <- sum(alt.prob[rej.indx]))

# For the same alpha value and the same effect size, power is higher when you 
# have a larger sample size. 
effect.size <- 0.05
sample.size <- 100
alpha <- 0.05
hyp.real.p <- null.p + effect.size
poss.num.males <- 0:sample.size; poss.frac.males <- poss.num.males/sample.size
null.prob <- dbinom(poss.num.males, sample.size, null.p)
alt.prob <- dbinom(poss.num.males, sample.size, hyp.real.p)
par(mfcol = c(2,1))
plot(poss.frac.males, null.prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = 'Sampling Distribution under H0: Prob(Male) = 0.5')
crit.test.values <- qbinom(c(alpha/2, 1 - alpha/2), 
                           size = sample.size, 
                           prob = null.p)/sample.size
rej.indx <- poss.frac.males < crit.test.values[1] | poss.frac.males > crit.test.values[2]
rej.region <- poss.frac.males[rej.indx]
points(rej.region, null.prob[rej.indx], pch = 19, col = 'red')
plot(poss.frac.males, alt.prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = paste('Sampling Distribution under HA: Prob(Male) = ',hyp.real.p,
                  sep = ''))
points(rej.region, alt.prob[rej.indx], pch = 19, col = 'red')
(power <- sum(alt.prob[rej.indx]))
# Increasing the sample size here almost doubled the power. 



# Last, increasing the type I error with the same sample size can also increase
# the power. But, of course, this trades off with Type I error. Let's increase 
# Type I error to 0.1
effect.size <- 0.05
sample.size <- 60
alpha <- 0.1
hyp.real.p <- null.p + effect.size
poss.num.males <- 0:sample.size; poss.frac.males <- poss.num.males/sample.size
null.prob <- dbinom(poss.num.males, sample.size, null.p)
alt.prob <- dbinom(poss.num.males, sample.size, hyp.real.p)
par(mfcol = c(2,1))
plot(poss.frac.males, null.prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = 'Sampling Distribution under H0: Prob(Male) = 0.5')
crit.test.values <- qbinom(c(alpha/2, 1 - alpha/2), 
                           size = sample.size, 
                           prob = null.p)/sample.size
rej.indx <- poss.frac.males < crit.test.values[1] | poss.frac.males > crit.test.values[2]
rej.region <- poss.frac.males[rej.indx]
points(rej.region, null.prob[rej.indx], pch = 19, col = 'red')
plot(poss.frac.males, alt.prob, pch = 19,
     xlab = 'Fraction Male', ylab = 'Probability',
     main = paste('Sampling Distribution under HA: Prob(Male) = ',hyp.real.p,
                  sep = ''))
points(rej.region, alt.prob[rej.indx], pch = 19, col = 'red')
(power <- sum(alt.prob[rej.indx]))
# Power in this case is about 19%. 