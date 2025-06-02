vec <- c("Lily's", "BIO 380", "Lab")
my.vec <- seq(from = 10, to = 300, length = 5)
another.vec <- rep(c("Success", "Failure"), times = c(30, 70))
vec[2]
my.vec[3]
another.vec[100]
another.vec[50:60]
vec[-2]
vec[-c(2:3)]
a.matrix <- matrix(data = LETTERS[1:20], ncol = 4, nrow = 5, byrow = T)
a.matrix[2, 3]
a.matrix[2, ]
a.matrix[, 3]
a.matrix[-3, c(3:4)]
setwd("/Users/lilygetter/Documents/Biology380/Wk3")
cod.df <- read.csv(file="Cod.csv")
str(cod.df)
cod.df$Prevalence
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"
head(cod.df$Sex)
Male1.df <- subset(cod.df, subset = (Sex == "Male"))
Male2.df <- cod.df[cod.df$Sex == "Male", ]
identical(Male1.df, Male2.df)
# Checkpoint 1: See code below
clean.cod.df <- cod.df[cod.df$Sex != "Unknown", ]
str(clean.cod.df)
dim(clean.cod.df)
unique(clean.cod.df$Sex)
table(clean.cod.df$Sex)
table(clean.cod.df$Sex, clean.cod.df$Stage)
aggregate(Length ~ Sex, data = clean.cod.df, sd)
# Checkpoint 2: See code 
# A. There are 1254 fish in the entire data set
dim(cod.df)
# B. 45.77% of the fish are female, 47.69% are male, and 6.54% are unknown. 
sex <- table(cod.df$Sex)
percentsex <- sex/1254 * 100
percentsex
# C. There are four different areas labeled 1-4.
unique(cod.df$Area)
# D. The average weight of a female is 1857.852 grams, the average weight of a male is 1593.79 grams, and the average weight of an unknown is 1443.81 grams.
aggregate(Weight ~ Sex, data = cod.df, mean)
# E. 273 female fish have parasite infections, or 47.5% of total female fish. 268 male fish, or 44.8% of total male fish have a parasite infection. 41 fish of unknown sex, or 50% of total fish of unknown sex have a parasite infection. Thus, parasite infections are most common in fish of unknown sex, followed by females, and finally males. 
sick.cod.df <- cod.df[cod.df$Prevalence != 0, ]
table(sick.cod.df$Sex)
table(sick.cod.df$Sex)/table(cod.df$Sex)
# F. 48% (133 fish) in area 1 have parasite infections, compared with 32% (83 fish) in area 2, 35% (146 fish) in area 3, and 71% (220 fish) in area 4. Therefore, parastie infections are most common in area 4.
table(sick.cod.df$Area)
table(sick.cod.df$Area)/table(cod.df$Area)

barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish")
mosaicplot(Sex ~ 1, data = cod.df, main = "Sex Distribution in Cod")
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish", col = "red")
install.packages("viridisLite")
library(viridisLite)
(our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25, end = 0.75))
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish", col = our.colors,
        main = "Sex Distribution of Norwegian Cod")
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish", col = our.colors,
        main = "Sex Distribution of Norwegian Cod", cex.lab = 1.5)
par(mfrow = c(2, 2))
boxplot(cod.df$Weight)
plot(density(cod.df$Weight, na.rm = TRUE))
hist(cod.df$Weight)
stripchart(cod.df$Weight, method = "jitter", pch = 19)
par(mfrow = c(2, 2))
boxplot(cod.df$Weight, horizontal = TRUE)
plot(density(cod.df$Weight, na.rm = TRUE))
hist(cod.df$Weight)
stripchart(cod.df$Weight, method = "jitter", pch = 19, col = rgb(0, 0, 0, alpha = 0.05))
summary(cod.df$Weight)

par(mfrow = c(1, 2))
plot(cod.df$Area, cod.df$Weight, xlab = "Area", ylab = "Fish Weight", pch = 19, col = rgb(0, 0, 0, alpha = 0.5), main = "Without Jitter")
plot(jitter(cod.df$Area), cod.df$Weight, xlab = "Area", ylab = "Fish Weight", pch = 19, col = rgb(0, 0, 0, alpha = 0.05), main = "With Jitter")
head(cod.df$Area)
head(jitter(cod.df$Area))
plot(cod.df$Length, cod.df$Weight, pch = 19, xlab = "Length", ylab = "Weight", col = rgb(0, 0, 0, alpha = 0.15))
my.colors <- viridis(2, begin = 0.3, end = 0.7, alpha = 0.25)


male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length', ylab = 'Weight',
     col = my.colors[1])
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft',
       legend = c("Females","Males"),
       col = my.colors, 
       pch = 19, cex = 1.5, 
       bty = 'n')
pdf(file = 'MyFishPlot.pdf')

plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length', ylab = 'Weight',
     col = my.colors[1])
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft',
       legend = c("Females","Males"), 
       col = my.colors, 
       pch = 19, cex = 1.5,
       bty = 'n') 
dev.off()

# Checkpoint 3: This graph shows the number of sick fish by sex (sick.cod.df was created in an earlier checkpoint)
barplot(table(sick.cod.df$Sex), xlab = "Sex", ylab = "Number of Fish with Parasite Infections", col = "red")
# Checkpoint 4: This graph shows the number of sick fish by area (sick.cod.df was created in an earlier checkpoint)
barplot(table(cod.df$Area), xlab = "Area", ylab = "Number of Fish with Parasite Infections", col = "blue")
# Checkpoint 5: This graph shows the length of fish with infections compared to fish without infections
plot(jitter(cod.df$Prevalence), cod.df$Length, xlab = "Infection Status", ylab = "Fish Length (cm)", pch = 19, col = rgb(0, 0, 0, alpha = 0.05))



alphabet <- letters[1:26]
sample(alphabet, size = 10, replace = T)
sample(alphabet, size = 10, replace = F)
rbinom(5, size = 10, prob = 0.6)
dbinom(4:7, size = 10, prob = 0.6)
pbinom(4:7, size = 10, prob = 0.6)
qbinom(c(0.5,0.9), size = 10, prob = 0.6)
sample.size <- 10
prob.success <- 0.6
possible.outcomes <- 0:sample.size
probabilities <- dbinom(possible.outcomes,
                        size = sample.size,
                        prob = prob.success)
par(mfcol = c(2,2), mar = c(4,4,3,1))
plot(possible.outcomes, probabilities,
     xlab = '# Successes',
     ylab = 'Probability',
     main = 'Probability Distribution Function',
     pch = 19)
cumulative.prob <- pbinom(possible.outcomes,
                          size = sample.size,
                          prob = prob.success)
plot(possible.outcomes, cumulative.prob,
     xlab = '# Successes',
     ylab = 'Cumulative Probability',
     main = 'Cumulative Distribution Function',
     pch = 19)
hist(rbinom(100, size = sample.size, prob = prob.success),
     xlab = '# Successes',
     main = 'Random Sample')
quantile.probs <- seq(from = 0, to = 1, length = 100)
quantiles <- qbinom(quantile.probs,
                    size = sample.size,
                    prob = prob.success)
plot(quantile.probs, quantiles,
     xlab = 'Cumulative Probability',
     ylab = 'Quantile',
     main = 'Quantile Function',
     typ = 'l')

mu <- 10
sigma <- 2
n <- 30
xbar.mu <- mu
xbar.sd <- sigma/sqrt(n)
pot.outcomes <- seq(from = 5, to = 15, length = 10000)
sam.prob <- dnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
sam.cumul.prob <- pnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
par(mfcol = c(1,2), cex.lab = 0.8, cex.axis = 0.8)
plot(pot.outcomes, sam.prob, typ = 'l',
     xlab = 'Sample Outcome',
     ylab = 'Probability',
     main = 'Sampling Distribution')
plot(pot.outcomes, sam.cumul.prob, typ = 'l',
     xlab = 'Sample Mean',
     ylab = 'Cumulative Probability',
     main = 'Sampling Distribution')

# Checkpoint 6:
df1 <- 4
t1 <- seq(-4, 4)
tprob1 <- dt(t1, df1)
plot(t1, tprob1, type = "l", lwd = 2, col = "blue",
     xlab = "T-values", ylab = "Density",
     main = "T-Distribution, df =  4")
quantiles <- qt(c(0.025, 0.975), 4)
abline(v = quantiles, col = "red", lty = 2)
# Checkpoint 7:
df2 <- 49
t2 <- seq(-20, 20)
tprob2 <- dt(t2, df2)
plot(t2, tprob2, type = "l", lwd = 2, col = "blue",
     xlab = "T-values", ylab = "Density",
     main = "T-Distribution, df =  49")
quantiles <- qt(c(0.025, 0.975), 49)
abline(v = quantiles, col = "red", lty = 2)


n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.05
conf.levels <- c(alpha/2, 1 - alpha/2)
(conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n

# Checkpoint 8: The probability that the true prevalence is between 42.8% and 50.1% is 99%. 
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.01
conf.levels <- c(alpha/2, 1 - alpha/2)
(conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n

hist(cod.df$Length)
mean.length <- mean(cod.df$Length, na.rm = T)
n.length <- sum(!is.na(cod.df$Length))
sd.length <- sd(cod.df$Length, na.rm = T)
se.length <- sd.length/sqrt(n.length)
n.length; mean.length; sd.length; se.length

alpha <- 0.01
conf.levels <- c(alpha/2, 1-alpha/2)
qt(conf.levels, df = n.length -1)
(fish.conf.int <- mean.length + qt(conf.levels, df = n.length - 1)*se.length)
length.mdl <- lm(Length ~ 1, data = cod.df)
confint(length.mdl, level = 0.99)

# Checkpoint 9: In 1999, the 95% confidence interval for length is 50.35-52.49, in 2000 it is 53.79-57.30, and in 2001 it is 53.47-56.32.
unique(cod.df$Year)

Year1.df <- cod.df[cod.df$Year == 1999, ]
length.mdl1 <- lm(Length ~ 1, data = Year1.df)
confint(length.mdl1, level = 0.95)


Year2.df <- cod.df[cod.df$Year == 2000, ]
length.mdl2 <- lm(Length ~ 1, data = Year2.df)
confint(length.mdl2, level = 0.95)

Year3.df <- cod.df[cod.df$Year == 2001, ]
length.mdl3 <- lm(Length ~ 1, data = Year3.df)
confint(length.mdl3, level = 0.95)


# Checkpoint 10: Area 4 has the highest prevalence, followed by area 1, then 3, then 2.
Area1.df <- cod.df[cod.df$Area == 1, ]
TI1 <- sum(!is.na(Area1.df$Prevalence))
Area2.df <- cod.df[cod.df$Area == 2, ]
TI2 <- sum(!is.na(Area1.df$Prevalence))
Area3.df <- cod.df[cod.df$Area == 3, ]
TI3 <- sum(!is.na(Area1.df$Prevalence))
Area4.df <- cod.df[cod.df$Area == 4, ]
TI4 <- sum(!is.na(Area1.df$Prevalence))
table(cod.df$Area, cod.df$Prevalence)
barplot(table(cod.df$Area, cod.df$Prevalence)[, 2] / table(cod.df$Area), 
        col = "blue", 
        xlab = "Area", 
        ylab = "Prevalence",)



















































