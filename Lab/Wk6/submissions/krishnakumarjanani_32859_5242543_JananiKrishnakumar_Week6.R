# BIO 380 Lab Week 6
# Janani Krishnakumar
rm(list=ls())
setwd("C:/Users/crush/Desktop/Janani/College/Wake Forest/2024-2025/Biostatistics/Week 6")

########################################################################

### REVIEW: OBJECT TYPES ###

# Vectors:
###

vec <- c("Janani's", "BIO 380", "Lab") # Simple vector
my.vec <- seq(from=10, to=300, length=5) # Sequence vector
another.vec <- rep(c("Success","Failure"), times=c(30,70)) # Frequency vector
# Indexing
vec[2] 
my.vec[3]
another.vec[100]
another.vec[50:60] # Slice
vec[-1]; vec[-c(2:3)] # Exclude slice

# Matrices:
###

a.matrix <- matrix(data = LETTERS[1:20],
                   ncol = 4,
                   nrow = 5,
                   byrow = T) # 1st 20 letters, 4 cols, 5 rows
# Indexing
a.matrix[2,3] # Row 2, Column 3
a.matrix[2,] # Row 2
a.matrix[,3] # Column 3
a.matrix[-3,c(3:4)] # without row 3, only columns 3-4

# Data Frames:
###

# Read file
cod.df <- read.csv(file="cod.csv")
str(cod.df)
# Indexing
cod.df$Prevalence # All inst. whether cod had parasite
# Replace values within dafaframe
cod.df[cod.df$Sex == 2,"Sex"] <- "Male"
cod.df[cod.df$Sex == 1,"Sex"] <- "Female"
cod.df[cod.df$Sex == 0,"Sex"] <- "Unknown"
head(cod.df$Sex)

# Logical Statements:
###

# is.na
# <
# <=
# &
# |

# Subsetting:
###

Male1.df <- subset(cod.df, subset=(Sex=="Male")) # Using subset
Male2.df <- cod.df[cod.df$Sex=="Male",] # Using index
identical(Male1.df, Male2.df) # Same result

# Checkpoint 1: #########################
clean.cod.df <- subset(cod.df, subset=(Sex!="Unknown"))

# Additional methods
str(clean.cod.df)
dim(clean.cod.df)
unique(clean.cod.df)
table(clean.cod.df$Sex)
table(clean.cod.df$Sex, clean.cod.df$Stage)
aggregate(Length ~ Sex, data = clean.cod.df, sd)

# Checkpoint 2: #########################
# a ###
dim(cod.df)
# There are 1254 fish in the dataset
# b ###
prop.table(table(cod.df$Sex))
#     Female       Male    Unknown 
# 0.45773525 0.47687400 0.06539075 
# c ###
unique(cod.df$Area)
# There are 4 areas (1, 2, 3, and 4) in the dataset
# d ###
aggregate(Weight ~ Sex, data=cod.df, mean)
# Female = 1857.852, Male = 1593.789, Unknown = 1443.805
# e ###
aggregate(Prevalence ~ Sex, data=cod.df, mean)
#       Sex Prevalence
# 1  Female  0.4756098
# 2    Male  0.4481605
# 3 Unknown  0.5000000
# Parasite infections are slightly more common in females than males, although
#   the probability appears to be roughly similar
# f ###
aggregate(Prevalence ~ Area, data=cod.df, mean)
table(cod.df$Prevalence, cod.df$Area)
#   Area Prevalence
# 1    1  0.4889706
# 2    2  0.3254902
# 3    3  0.3518072
# 4    4  0.7051282
# Parasite infections are significantly more common in Area 4, and it is not
#   due to a small sample size.

########################################################################

### REVIEW: PLOTTING ###

# Plotting Amounts:
###

# Basic Amount Plots:

# Bar plot
barplot(table(cod.df$Sex),
        xlab = 'Sex',
        ylab = 'Number of Fish')
# Mosaic plot
mosaicplot(Sex ~ 1, data = cod.df,
           main = 'Sex Distribution in Cod')
# Colour bar plot
barplot(table(cod.df$Sex),
        xlab = 'Sex',
        ylab = 'Number of Fish',
        col = 'red')

# Add Some Formatting:

# Using ViridisLite for colouring
install.packages("viridisLite") # Install
library(viridisLite) # Load
# Generate colours for each bar, limit range of colours selected
our.colors <- mako(length(unique(cod.df$Sex)),
                    begin = 0.25,
                    end = 0.75)
# New coloured bar plot
barplot(table(cod.df$Sex),
        xlab = 'Sex',
        ylab = 'Number of Fish',
        col = our.colors,
        main = 'Sex Distribution of Norwegian Cod')
# Increase axis label size
barplot(table(cod.df$Sex),
        xlab = 'Sex',
        ylab = 'Number of Fish',
        col = our.colors,
        main = 'Sex Distribution of Norwegian Cod',
        cex.lab = 1.5)

# Plotting Distributions:
###

par(mfrow = c(2,2)) # 4x4 chart display

# Basic Density Distributions:

# Box plot
boxplot(cod.df$Weight)
# Density plot
plot(density(cod.df$Weight, na.rm = TRUE))
# Histogram
hist(cod.df$Weight)
# Strip chart
stripchart(cod.df$Weight,
           method = "jitter",
           pch = 19)

# Add Some Formatting:

# Box plot
boxplot(cod.df$Weight, horizontal = TRUE) # Make horizontal
# Density plot
plot(density(cod.df$Weight, na.rm = TRUE)) # Didn't change
# Histogram
hist(cod.df$Weight) # Didn't change
# Strip chart
stripchart(cod.df$Weight,
           method = "jitter",
           pch = 19,
           col = rgb(0,0,0, alpha = 0.05)) # Make more transparent to see dots

# Summarize data
summary(cod.df$Weight)

# Plot As A Function Of:

# Box plot of fish weight as a function of area; weights of each area
par(mfrow = c(1,1))
boxplot(cod.df$Weight ~ cod.df$Area,
        xlab = 'Area', ylab = 'Fish Weight',
        col = mako(n=length(unique(cod.df$Area)), begin=0.3, end=0.7))


# Strip Chart Jitter:
par(mfrow = c(1,2))

# No jitter, low transparency
plot(cod.df$Area, cod.df$Weight,
     xlab = 'Area', ylab = 'Fish Weight',
     pch = 19, col = rgb(0,0,0,alpha = 0.5),
     main = "Without Jitter")
# With formatting
plot(jitter(cod.df$Area), cod.df$Weight,
     xlab = 'Area', ylab = 'Fish Weight',
     pch = 19, col = rgb(0,0,0,alpha = 0.05),
     main = 'With Jitter')
# How does this work?
head(cod.df$Area) # Exact values
head(jitter(cod.df$Area)) # Slightly off mark

# Scatter plots:
###

# Basic Scatter Plot:

# Ex: Fish length vs. Weight
par(mfrow = c(1,1))
plot(cod.df$Length, cod.df$Weight,
     pch = 19,
     xlab = 'Length', ylab = 'Weight',
     col = rgb(0,0,0, alpha = 0.15))

# Add Some Formatting:

# Colours
my.colors <- viridis(2, begin = 0.3, end = 0.7, alpha = 0.25)
# Separate data
male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
# Plot females
plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length', ylab = 'Weight',
     col = my.colors[1])
# Points function, Add males
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2]) # Use lines(x,y) for lines instead of points
# Legend
legend('topleft', # Position
       legend = c("Females","Males"), # Labels
       col = my.colors, # Label Colours
       pch = 19, cex = 1.5, # Draw circle colour dots for label + dot size
       bty = 'n') # box type (no box around legend)
# Straight line: abline(h=y, v=x, a=intercept, b=slope)
# pch=19 is a filled circle
# cex = label/point size
# If it's a plot, can use rgb or viridis for colour
# Can add text to a figure using text function

# Export To PDF:

# Create file to write to
pdf(file = 'MyFishPlot.pdf')
# Make plot (Repeat steps from earlier)
plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length', ylab = 'Weight',
     col = my.colors[1])
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft', legend = c("Females","Males"),
       col = my.colors, pch = 19, cex = 1.5,
       bty = 'n')
# Stop plotting
dev.off()

# Checkpoint 3: #########################
# sex.parasite.df <- subset(cod.df, subset=(Prevalence=="1"), select=Sex)
sex.parasite.df <- cod.df[, c("Sex","Prevalence")]
sp.df.prop <- prop.table(table(sex.parasite.df))
barplot(sp.df.prop[,2],
        xlab = 'Fish Sex',
        ylab = 'Proportion of Sex Infected',
        col = mako(length(unique(sex.parasite.df$Sex)), begin=0.25, end=0.75),
        main = 'Parasitic Infection of Norwegian Cod by Sex',
        cex.lab = 1.5)

# Checkpoint 4: #########################
# area.parasite.df <- subset(cod.df, subset=(Prevalence=="1"), select=Area)
area.parasite.df <- cod.df[, c("Area", "Prevalence")]
ap.df.prop <- prop.table(table(area.parasite.df))
barplot(ap.df.prop[,2],
        xlab = 'Area',
        ylab = 'Proportion of Area Infected',
        col = mako(length(unique(area.parasite.df$Area)), begin=0.25, end=0.75),
        main = 'Parasitic Infection of Norwegian Cod by Area',
        cex.lab = 1.5)

# Checkpoint 5: #########################
boxplot(cod.df$Length ~ cod.df$Prevalence,
        xlab = 'Prevalence (1=Is infected)', ylab = 'Fish Length',
        col = mako(n=length(unique(cod.df$Prevalence)), begin=0.3, end=0.7))
# Fish of large and small size are at about the same risk of parasite
#     infection.

########################################################################

### REVIEW: PROBABILITY AND RANDOM SAMPLING ###

# Binomial Distribution:
###

# Taking A Sample:

sample.size <- 10
prob.success <- 0.6
possible.outcomes <- 0:sample.size
par(mfcol = c(2,2), mar = c(4,4,3,1))

# Modeling Distribution:
# (Use Binom to create sample curves for a binomal assumption)

alphabet <- letters[1:26]
sample(alphabet, size = 10, replace = T) # With replacement
sample(alphabet, size = 10, replace = F) # Without replacement

# R binom: Random # samples taken of certain size given prob success
# (What outcomes do you get if you keep sampling? FREQUENCY)
rbinom(5, size = 10, prob = 0.6)
hist(rbinom(100, size = sample.size, prob = prob.success),
     xlab = '# Successes',
     main = 'Random Sample')

# D binom: Samp dist of outcomes given prob success
# (What is the likelihood of each outcome if you keep sampling? PROBABILITY)
dbinom(4:7, size = 10, prob = 0.6)
probabilities <- dbinom(possible.outcomes,
                        size = sample.size,
                        prob = prob.success)
plot(possible.outcomes, probabilities,
     xlab = '# Successes',
     ylab = 'Probability',
     main = 'Probability Distribution Function',
     pch = 19)

# P binom: Cumulative samp dist of outcomes given prob success
# (Integrate D binom curve)
pbinom(4:7, size = 10, prob = 0.6)
cumulative.prob <- pbinom(possible.outcomes,
                          size = sample.size,
                          prob = prob.success)
plot(possible.outcomes, cumulative.prob,
     xlab = '# Successes',
     ylab = 'Cumulative Probability',
     main = 'Cumulative Distribution Function',
     pch = 19)

# Q binom: Quantiles of cumulative distribution
# (Segment P binom curve based on where bulk of samples are likely to be)
qbinom(c(0.5,0.9), size = 10, prob = 0.6)
quantile.probs <- seq(from = 0, to = 1, length = 100)
quantiles <- qbinom(quantile.probs,
                    size = sample.size,
                    prob = prob.success)
plot(quantile.probs, quantiles,
     xlab = 'Cumulative Probability',
     ylab = 'Quantile',
     main = 'Quantile Function',
     typ = 'l')

# Normal Distribution:
###

# Taking A Sample:

# Population characteristics
mu <- 10 # Population mean
sigma <- 2 # Standard deviation
# Sample characteristics
n <- 30 # Sample size
xbar.mu <- mu # Samp dist mean
xbar.sd <- sigma/sqrt(n) # Samp dist sd (se)

# Modeling Distribution::
# (Use Norm to Calculate Probability Curves that Model a Population)
# (Assumes CLT that many samples of lg size will model norm pop)

# Can get out any number from 5-15 when sampling (remember mean=10)
pot.outcomes <- seq(from = 5, to = 15, length = 10000)
par(mfcol = c(1,2), cex.lab = 0.8, cex.axis = 0.8)

# D Norm: Probability dist of different outcomes given a Normal population
sam.prob <- dnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
plot(pot.outcomes, sam.prob, typ = 'l',
     xlab = 'Sample Outcome',
     ylab = 'Probability',
     main = 'Sampling Distribution')

# P Norm: Cumul've prob dist of diff. outcomes given a Normal population
sam.cumul.prob <- pnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
plot(pot.outcomes, sam.cumul.prob, typ = 'l',
     xlab = 'Sample Mean',
     ylab = 'Cumulative Probability',
     main = 'Sampling Distribution')

########################################################################

### T-DISTRIBUTIONS ###

# Calculate from Sample: t = (x-bar-mu)/se

# Functions:
# rt(n,df) - t-vaues of n random samples w/ df degrees of freedom
# dt(t,df) - Probability of diff t-values if have certain df
# pt(t,df) - Cumul've prob of diff t-vals if have certain df
# qt(cumul.prob,df) - Quantiles for cuml've probs of diff t-vals if certain df

par(mfcol = c(1,1))

# Checkpoint 6: #########################
# Calculate degrees of freedom
samp.size <- 5
df <- samp.size-1
# Set n values
n <- seq(from = -5, to = 5, length = 100)
# Calculate t-dist
t.dist.y <- dt(n, df)
# Plot
plot(n,t.dist.y, pch = 19, col=rgb(0,1,0))
# Calc. quantiles
cum.prob.low <- .025
cum.prob.high <- 0.975
qt(cum.prob.low,df) # -2.776445
qt(cum.prob.high,df) # 2.776445
# 95% of all values in this t-dist fall b/w -2.776 and 2.776

# Checkpoint 7: #########################
samp.size.2 <- 50
df.2 <- samp.size.2-1
# n is same as last time
t.dist.y.2 <- dt(n,df.2)
# We can compare this plot to the last one
points(n, t.dist.y.2,
       pch = 19, col = rgb(1,0,0)) # It's steeper!
qt(cum.prob.low,df.2) # -2.009575
qt(cum.prob.high,df.2) # 2.009575
# 95% of all values in this t-dist fall b/w -2.01 and 2.01 (narrower than last)

########################################################################

### STD ERROR & CONFIDENCE INTERVALS ###

# Can calculate SE & CI in 2 ways
# 1. Manually (what we've done): Use
#     A. Categorical outcomes: Binomial Q
#     ba. Normal dist using CLT: Normal Q
#     B. Normal dist: T-Dist Q
# 2. Linear model structures based on t-dist

# 1A: For Categorical Characters: Using Q Binom
###

# Ex: Cod infection prevalence

# Sample information about cod prevalence
n <- length(cod.df$Prevalence) # NEED
num.infected <- sum(cod.df$Prevalence == 1)
p.inf <- num.infected/n # NEED

# Characteristics of a 95% Confidence Interval
alpha.95 <- 0.05
conf.levels.95 <- c(alpha.95/2, 1 - alpha.95/2)

# Calculate CI
conf.int <- qbinom(conf.levels.95, size=n, prob=p.inf)/n
# 0.4362041 0.4920255 --> 
# 95% chance that the true prevalence is b/w 43.6 and 49.2 percentiles

# Checkpoint 8: #########################
alpha.99 <- 1-(99/100)
conf.levels.99 <- c(alpha.99/2, 1 - alpha.99/2)
qbinom(conf.levels.99, size=n, prob=p.inf)/n
# 0.4282297 0.5007974
# 99% chance that the true prevalence is b/w 42.8 and 50.1 percentile

# 1B: For Normal Model Characters: Use QT
###

# Ex: Cod length

# Is it a normal dist (approx.)?
hist(cod.df$Length) 

# Sample information about cod length
mean.length <- mean(cod.df$Length, na.rm = T)
n.length <- sum(!is.na(cod.df$Length))
sd.length <- sd(cod.df$Length, na.rm = T)
se.length <- sd.length/sqrt(n.length)

# Characteristics of a 99% Confidence Interval
alpha.99 <- 1-(99/100)
conf.levels.99 <- c(alpha.99/2, 1 - alpha.99/2)

# Calculate quantiles
quantiles.99 <- qt(conf.levels.99, df = n.length -1)

# Calculate Confidence Intervals
mean.length + quantiles.99*se.length
# 52.41542 54.47561

# 2: Use Linear Model
###

length.mdl <- lm(Length ~ 1, data = cod.df)
confint(length.mdl, level = 0.99)
#                 0.5 %   99.5 %
#  (Intercept) 52.41542 54.47561
# Very straightforward

# Checkpoint 9: #########################
# Find years
all.years <- unique(cod.df$Year)
all.years <- na.omit(all.years)
all.years <- sort(all.years, decreasing = FALSE)
# Calculate conf int for each year
for (year in all.years) {
  select <- subset(cod.df, subset=(Year==year))
  length.mdl.new <- lm(Length ~ 1, data = select)
  print(confint(length.mdl.new, level = 0.95))
}
# 1999: 50.35874, 52.49232
# 2000: 53.79142, 57.30028
# 2001: 53.46606 56.31856

########################################################################

### SYNTHESIS CHALLENGE ###

# Checkpoint 10: #########################

# Plot All Data:

# Colour
my.col = mako(n=length(unique(cod.df$Area)), begin=0.25, end=0.75)

# Characteristics of a 95% Confidence Interval
alpha.95 <- 0.05
conf.levels.95 <- c(alpha.95/2, 1 - alpha.95/2)

# Plot main + conf int:

# Sample size
sample.size <- length(cod.df$Prevalence) # n
total.num.infected <- sum(cod.df$Prevalence == 1)
total.p.inf <- total.num.infected/sample.size # Prob

possible.outcomes <- 0:sample.size

probabilities <- dbinom(possible.outcomes,
                        size = sample.size,
                        prob = total.p.inf)

# Plot
plot(possible.outcomes, probabilities,
     xlab = 'Number of cod infected',
     ylab = 'Probability',
     main = 'Probability Distribution of Cod Infection Prevalence',
     pch = 19, cex = 0.75)
c.int <- qbinom(conf.levels.95, size=n, prob=p.inf)
abline(v=c(0+c.int[1], sample.size-c.int[2]), lwd=2)

# Find confidence intervals for each area + Plot:

# Find what locations are present
all.areas <- unique(cod.df$Area)
all.areas <- all.areas[order(all.areas)]

# Find prevalence for each location
count=1
prev.areas.cint <- data.frame(matrix(nrow=2,ncol=length(all.areas)))
for (area in all.areas) {
  
  # Subset
  cod.prev.in.area <- subset(cod.df, subset=(Area==area))
  
  # Sample information about cod prevalence
  n <- length(cod.prev.in.area$Prevalence) # NEED
  num.infected <- sum(cod.prev.in.area$Prevalence == 1)
  p.inf <- num.infected/n # NEED
  poss.outcomes <- 0:n
  probs <- dbinom(poss.outcomes,
                          size = n,
                          prob = p.inf)
  
  # Plot Area
  points(poss.outcomes, probs,
       pch = 19, cex = 0.75, col=my.col[count])
  
  # Find confidence interval for area
  cint <- qbinom(conf.levels.95, size=n, prob=p.inf)
  prev.areas.cint[count] <- cint
  
  print(c(cint[1],cint[2]))
  
  # Add conf int lines to plot
  abline(v=c(0+cint[1],n-cint[2]), col=my.col[count], lwd=2)
  
  count <- count+1
}

#   Area Prevalence
# 1    1  0.4889706
# 2    2  0.3254902
# 3    3  0.3518072
# 4    4  0.7051282

# Legend
all.areas.char <- as.character(all.areas)
legend('topright', # Position
       legend = all.areas.char,
       col = my.col,
       pch = 19, cex = 1.5,
       title = "95% Confidence\nIntervals\nby Area",
       bg="transparent")

# The majority of infections occur in Area 4, while the least number
#     occur in area 2
