# State the population parameters
mu = 7
sigma = sqrt(2.6)

# Make a figure of the population distribution
x <- seq(from = 0, to = 11, length = 1000)
pdf.x <- dnorm(x, mu, sigma)
plot(x,pdf.x)

# Create a sample
n <-10; data <- rnorm(n, mu, sigma)
# Look at it
data
hist(data)

# Calculate sample properties (these are a bit different than the population
# parameters)
mean(data)
var(data)


# Simulate the process of sampling many many times
samples <- 10000
# Create an object to store estimates of the variance
s.stored <- rep(NA, samples)

# Sample the population and calcualte variance multiple times
for (i in 1:samples){
  data <- rnorm(n, mu, sigma)
  s.stored[i] <- var(data)
}

# Look at the distribution we created
hist(s.stored/sigma^2*(n-1), prob = T)

# Caclulate the corresponding chi-square distribution
chi.sq.values <- seq(from = 0, to = 35, length = 1000)
chisq.prob <- dchisq(chi.sq.values, n-1)

# Plot it on our histogram. 
lines(chi.sq.values, chisq.prob)

#Viola! They match. Proof that S^2*(n-1)/sigma^2 is distributed as a chi-square
#distribution with n-1 degrees of freeedom. 