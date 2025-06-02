rm(list = ls())
# Summary of power analysis.

# Binomial exact test
# Sample size effect
sample.size <- seq(from = 5, to = 400, by = 15)
# Pick effect size and alpha value
null.p <- 0.5
effect.size <- 0.05
hyp.p <- null.p + effect.size
alpha <- 0.05
power <- rep(NA, length(sample.size))
for (i in 1:length(sample.size)){
  crit.test.values <- qbinom(c(alpha/2, 1 - alpha/2), 
                             size = sample.size[i], 
                             prob = null.p)
  poss.out <- 0:sample.size[i]
  rej.indx <- poss.out<crit.test.values[1] | poss.out > crit.test.values[2]
  # Find probability of getting a sample in the rejection region given the real
  # hypothesis
  power[i] <- sum(dbinom(poss.out[rej.indx], 
                         sample.size[i], 
                         hyp.p))
}
par(mfcol = c(1,1))
plot(sample.size, power, xlab = 'Sample Size', 
     ylab = 'Probability of Rejecting Null (Power)',
     main = paste('Binomial Test Power Analysis
    H0: p = ', null.p, ', HA: p = ', hyp.p, sep = ''), 
     typ = 'o', pch = 19,
     ylim = c(0,1))
abline(h = seq(from = 0, to = 1, by = 0.05), lty = 2, col = 'gray')








# Effect size 
sample.size <- 60
# Pick effect size and alpha value
null.p <- 0.5
effect.size <- seq(from = 0.01, to = 0.5, by = 0.01)
hyp.p <- null.p + effect.size
alpha <- 0.05
power <- rep(NA, length(effect.size))
crit.test.values <- qbinom(c(alpha/2, 1 - alpha/2), 
                           size = sample.size, 
                           prob = null.p)
poss.out <- 0:sample.size
rej.indx <- poss.out<crit.test.values[1] | poss.out > crit.test.values[2]

for (i in 1:length(effect.size)){
  # Find probability of getting a sample in the rejection region given the real
  # hypothesis
  power[i] <- sum(dbinom(poss.out[rej.indx], 
                         sample.size, 
                         hyp.p[i]))
}
par(mfcol = c(1,1))
plot(effect.size, power, xlab = 'Effect Size', 
     ylab = 'Probability of Rejecting Null (Power)',
     main = paste('Binomial Test Power Analysis
    H0: p = ', null.p, ', with n = ',sample.size, sep = ''), 
     typ = 'o', pch = 19,
     ylim = c(0,1))
abline(h = seq(from = 0, to = 1, by = 0.05), lty = 2, col = 'gray')




############################ Type I error ##################################
# Pick effect size and sample size
sample.size <- 60
null.p <- 0.5
effect.size <- 0.05
hyp.p <- null.p + effect.size
alpha <- seq(from = 0.0001, to = 0.2, length = 40)
power <- rep(NA, length(alpha))

for (i in 1:length(alpha)){
  crit.test.values <- qbinom(c(alpha[i]/2, 1 - alpha[i]/2), 
                             size = sample.size, 
                             prob = null.p)
  poss.out <- 0:sample.size
  rej.indx <- poss.out<crit.test.values[1] | poss.out > crit.test.values[2]
  
  # Find probability of getting a sample in the rejection region given the real
  # hypothesis
  power[i] <- sum(dbinom(poss.out[rej.indx], 
                         sample.size, 
                         hyp.p))
}
par(mfcol = c(1,1))
plot(alpha, power, xlab = 'Type I Error', 
     ylab = 'Probability of Rejecting Null (Power)',
     main = paste('Binomial Test Power Analysis
    H0: p = ', null.p, ', HA: p = ', hyp.p, ' and n = ', sample.size, sep = ''), 
     typ = 'o', pch = 19,
     ylim = c(0,1))
abline(h = seq(from = 0, to = 1, by = 0.05), lty = 2, col = 'gray')
