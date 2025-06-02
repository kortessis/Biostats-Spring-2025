rm(list = ls())
vec <- c("Nicks", "BIO 380", "Lab")
my.vec <- seq(from = 10, to = 300, length = 5)
another.vec <- rep(c("Success", "Failure"), times = c(30,70))
vec[2]
my.vec[3]
another.vec[100]
another.vec[50:60]
vec[-2]
vec[-c(2:3)]
a.matrix <- matrix(data = LETTERS[1:20], ncol = 4, nrow = 5, byrow = T)
a.matrix[2,3]
a.matrix[2,]
a.matrix[, 3]
a.matrix [-3, c(3:4)]
getwd()
setwd("/Users/raymondlomax/Desktop/BIO380/Wk 6")
cod.df <- read.csv(file = 'cod.csv')
str(cod.df)
cod.df$Prevalence
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"
head(cod.df$Sex)
Male1.df <- subset(cod.df, subset = (Sex == "Male"))
Male2.df <- cod.df[cod.df$Sex == "Male", ]
identical(Male1.df, Male2.df)
#Checkpoint 1:
clean.cod.df <- subset(cod.df, subset = (Sex != "Unknown"))
str(clean.cod.df)
dim(clean.cod.df)
unique(clean.cod.df$Sex)
table(clean.cod.df$Sex)
table(clean.cod.df$Sex, clean.cod.df$Stage)
aggregate(Length ~ Sex, data = clean.cod.df, sd)
#Chekpoint 2: 
#a Female = 578, Male = 598, and Unknown = 82
table(cod.df$Sex)
#b 
Female = 574/1254
Male = 598/1254
Unknown = 82/1254
#c 4 
unique(cod.df$Area)
#d 
aggregate(Weight ~ Sex, data = cod.df, mean)
#e I looked this one up online because I was stuck so that's why there is some new function here. The parasite is slightly more common in male cod. 
table.prevalence.1 <- table(cod.df$Prevalence)
table.prevalence.1 
newtable <- table(cod.df$Prevalence, cod.df$Sex)
newtable
prop.table(newtable)
#f The parasite is more common in area 3 and 4. 
table.prevalence.2 <- table(cod.df$Prevalence)
table.prevalence.2
newtable.2 <- table(cod.df$Prevalence, cod.df$Area)
newtable.2
prop.table(newtable.2)
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish")
mosaicplot(Sex ~ 1, data = cod.df, main = "Sex Distribution in Cod")
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish", col = "red")
install.packages("virdisLite")
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
boxplot(cod.df$Weight ~ cod.df$Area,
        xlab = 'Area', ylab = 'Fish Weight',
        col = mako(n = length(unique(cod.df$Area)), begin = 0.3, end = 0.7))                              
par(mfrow = c(1,2))
plot(cod.df$Area, cod.df$Weight,
     xlab = 'Area', ylab = 'Fish Weight',
     pch = 19, col = rgb(0,0,0,alpha = 0.5),
     main = "Without Jitter")
plot(jitter(cod.df$Area), cod.df$Weight,
     xlab = 'Area', ylab = 'Fish Weight',
     pch = 19, col = rgb(0,0,0,alpha = 0.05),
     main = 'With Jitter')                       
head(cod.df$Area)                           
head(jitter(cod.df$Area)) 
plot(cod.df$Length, cod.df$Weight,
     pch = 19,
     xlab = 'Length', ylab = 'Weight',
     col = rgb(0,0,0, alpha = 0.15))
my.colors <- viridis(2, begin = 0.3, end = 0.7, alpha = 0.25)
male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
par(mfrow = c(1,1))
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
#Checkpoint 3: 
plot(female.df$Prevalence, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Prevalence', ylab = 'Weight',
     col = my.colors[1])
points(male.df$Prevalence, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft', 
       legend = c("Females","Males"), 
       col = my.colors, 
       pch = 19, cex = 1.5, 
       bty = 'n') 
#Checkpoint 4: 
plot(cod.df$Area, cod.df$Prevalence,
     pch = 19,
     xlab = 'Area', ylab = 'Prevalence',
     col = rgb(0,0,0, alpha = 0.15))
#Checkpoint 5: The smaller the fish the more at risk it will be to parasite infection. 
plot(jitter(cod.df$Prevalence), cod.df$Weight,
     xlab = 'Prevalence', ylab = 'Fish Weight',
     pch = 19, col = rgb(0,0,0,alpha = 0.05),
     main = 'With Jitter')
12

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

#Checkpoint 6:
n <- 5
df <- 4
x <- seq(from = 1, to = 5, length = 10000)
par(mfcol = c(1,1), cex.lab = 0.8, cex.axis = 0.8)
dt(x, df = 5)
curve(dt(x, df = 4), from = -5, to = 5)
lower.quantile <- qt(0.025, df)
lower.quantile
upper.quantile <- qt(0.975, df)
upper.quantile

#Checkpoint 7:
n <- 50
df <- 49
x <- seq(from = 1, to = 5, length = 10000)
par(mfcol = c(1,1), cex.lab = 0.8, cex.axis = 0.8)
dt(x, df = 49)
curve(dt(x, df = 49), from = -5, to = 5)
lower.quantile.2 <- qt(0.025,df)
upper.quantile.2 <- qt(0.975,df)
lower.quantile.2
upper.quantile.2
#Done with checkpoint 6 & 7
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.05
conf.levels <- c(alpha/2,1 - alpha/2)
(conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n

#Checkpoint 8 
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.01
conf.levels <- c(alpha/2,1 - alpha/2)
(conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n

hist(cod.df$Length)
mean.length <- mean(cod.df$Length, na.rm = T)
n.length <- sum(!is.na(cod.df$Length))
sd.length <- sd(cod.df$Length, na.rm = T)
se.length <- sd.length/sqrt(n.length)
n.length; mean.length; sd.length; se.length;
alpha <- 0.01
conf.levels <- c(alpha/2, 1-alpha/2)
qt(conf.levels, df = n.length -1)
(fish.conf.int <- mean.length + qt(conf.levels, df = n.length - 1)*se.length)
length.mdl <- lm(Length ~ 1, data = cod.df)
confint(length.mdl, level = 0.99)

#Checkpoint 9: 
length.mdl <- lm(Length ~ 1, data = cod.df)
confint(length.mdl, level = 0.95)

#Checkpoint 10: Prevalence is pretty evenly distributed across each area. 
parasite.prevalence <- length(cod.df$Prevalence & cod.df$Area)
num.infected <- sum(cod.df$Prevalence == 1, cod.df$Area == 1,2,3,4)
num.infected
par(mfrow = c(1,2))
plot(cod.df$Area, cod.df$Prevalence,
     xlab = 'Area', ylab = 'Prevalence',
     pch = 19, col = rgb(0,0,0,alpha = 0.5),
     main = "Without Jitter")
plot(jitter(cod.df$Area), cod.df$Prevalence,
     xlab = 'Area', ylab = 'Prevalence',
     pch = 19, col = rgb(0,0,0,alpha = 0.05),
     main = 'With Jitter')

