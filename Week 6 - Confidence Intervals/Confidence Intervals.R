n <- 1000 # Sample size
p0 <- 0.5 # Null hypothesis
alpha.val <- 0.05
alt.p <- 0.55
observed.fraction <-  round(0.55*n)

# Possible outcomes
num.males <- 0:n
# Sampling distribution probabilities
prob <- dbinom(num.males, size = n, prob = p0)

(crit.values <- qbinom(c(alpha.val/2, 1-alpha.val/2), size = n, prob = p0))

library(greekLetters)

par(mfcol = c(2,1))
plot(num.males, prob, xlab = 'Number of Males in Sample',
     ylab = 'Probability', pch = 19,
     main = paste('Sampling Distribution of H_0: p = ', p0, sep = ''))
abline(v = crit.values, lty = 2, col = 'red')
points(num.males[num.males<crit.values[1]], prob[num.males<crit.values[1]], 
       pch = 19, col = 'red')
points(num.males[num.males>crit.values[2]], prob[num.males>crit.values[2]], 
       pch = 19, col = 'red')
abline(v = observed.fraction, lty = 1, col = 'blue', lwd = 3)
text(0, 0.75*max(prob), 
     labels = paste('Type I Error Probability
',greeks("alpha"), '=', alpha.val, sep = ' '),
     cex = 1.25,
     pos = 4)

alt.prob <- dbinom(num.males, size = n, prob = alt.p)
plot(num.males, alt.prob, pch = 19, xlab = 'Number of Males in Sample',
     ylab = 'Probabiltiy', 
     main = paste('Sampling Distribution of H_A: p = ', alt.p, sep = ''))
points(num.males[num.males<crit.values[1]],
       alt.prob[num.males<crit.values[1]], 
       pch = 19, col = 'red')
points(num.males[num.males>crit.values[2]],
       alt.prob[num.males>crit.values[2]], 
       pch = 19, col = 'red')
abline(v = crit.values, lty = 2, col = 'red')
power <- pbinom(crit.values[1], size = n, prob = alt.p) + 1 - 
  pbinom(crit.values[2], size = n, prob = alt.p)
text(0, 0.75*max(prob),
     labels = paste('Power
', greeks("beta"), '=', round(power,3), '
1 - Type II Error Probability', sep = ' '),
     cex = 1.25, pos = 4)
