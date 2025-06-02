# BIO 380 Lab; Week 6
# <Mishel Ocho>
rm(list = ls())
setwd("C:/Users/ochom/Downloads/BIO380/Wk2")
cod.df <- read.csv(file = "cod.csv")
str(cod.df)
cod.df$Prevalence
cod.df[cod.df$Sex == 2,"Sex"] <- "Male"
cod.df[cod.df$Sex == 1,"Sex"] <- "Female"
cod.df[cod.df$Sex == 0,"Sex"] <- "Unknown"
head(cod.df$Sex)
Male1.df <- subset(cod.df, subset = (Sex == "Male"))

#Checkpoint 1:
clean.cod.df <- subset(cod.df, subset = (Sex != "Unknown"))

#Checkpoint 2:
#Number of fish in data set:
dim(cod.df)[1]

#Sex percentages
table(cod.df$Sex)/dim(cod.df)[1]*100

#Number of studied areas
max(unique(cod.df$Area))

#Average weight by sex
aggregate(Weight ~ Sex, data = cod.df, mean)

#Parasite infection rates by sex
aggregate(Prevalence ~ Sex, data = cod.df, mean)
#infection more common in females

#Parasite infection rates by area
aggregate(Prevalence ~ Area, data = cod.df, mean)
#Infection most common in area 4

barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish")
mosaicplot(Sex ~ 1, data = cod.df, main = "Sex Distribution in Cod")
install.packages("viridisLite")
library(viridisLite)
(our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25, end = 0.75))
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish", col = our.colors, main = "Sex Distribution of Norwegian Cod", cex.lab = 1.5)
par(mfrow = c(2,2))
boxplot(cod.df$Weight, horizontal = TRUE)
plot(density(cod.df$Weight, na.rm = TRUE))
hist(cod.df$Weight)
stripchart(cod.df$Weight, method = "jitter", pch = 19, col = rgb(0,0,0, alpha = 0.05))
summary(cod.df$Weight)
par(mfrow = c(1,1))
boxplot(cod.df$Weight ~ cod.df$Area, xlab = "Area", ylab = "Fish Weight", col = mako(n = length(unique(cod.df$Area)), begin = 0.3, end = 0.7))
par(mfrow = c(1,2))
plot(cod.df$Area, cod.df$Weight, xlab = "Area", ylab = "Fish Weight", pch = 19, col = rgb(0,0,0,alpha = 0.5), main = "Without Jitter")
plot(jitter(cod.df$Area), cod.df$Weight, xlab = "Area", ylab = "Fish Weight", pch = 19, col = rgb(0,0,0,alpha = 0.05), main = "With Jitter")
par(mfrow = c(1,1))
plot(cod.df$Length, cod.df$Weight, pch = 19, xlab = "Length", ylab = "Weight", col = rgb(0,0,0, alpha = 0.15))
my.colors <- viridis(2, begin = 0.3, end = 0.7, alpha = 0.25)
male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
plot(female.df$Length, female.df$Weight, pch = 19, cex = 1.5, xlab = "Length", ylab = "Weight", col = my.colors[1])
points(male.df$Length, male.df$Weight, pch = 19, cex = 1.5, col = my.colors[2])
legend("topleft", legend = c("Females", "Males"), col = my.colors, pch = 19, cex = 1.5, bty = "n")
pdf(file = "MyFishPlot.pdf")
plot(female.df$Length, female.df$Weight, pch = 19, cex = 1.5, xlab = "Length", ylab = "Weight", col = my.colors[1])
points(male.df$Length, male.df$Weight, pch = 19, cex = 1.5, col = my.colors[2])
legend("topleft", legend = c("Females", "Males"), col = my.colors, pch = 19, cex = 1.5, bty = "n")
dev.off()

#Checkpoint 3
barplot(table(cod.df$Prevalence, cod.df$Sex), xlab = "Sex", ylab = "Infection Prevalence", col = my.colors, main = "Infection Prevalence of Norwegian Cod by Sex", cex.lab = 1.5)
legend("topright", legend = c("Non-Infected", "Infected"), col = my.colors, pch = 19, cex = 1.5, bty = "n")

#Checkpoint 4
barplot(table(cod.df$Prevalence, cod.df$Area), xlab = "Area", ylab = "Infection Prevalence", col = my.colors, main = "Infection Prevalence of Norwegian Cod by Area", cex.lab = 1.5)
legend("topleft", legend = c("Non-Infected", "Infected"), col = my.colors, pch = 19, cex = 1.5, bty = "n")

#Checkpoint 5
boxplot(cod.df$Length ~ cod.df$Prevalence,  xlab = "Infection Prevalence", ylab = "Fish Size", col = mako(n = length(unique(cod.df$Area)), begin = 0.3, end = 0.7))
#There doesnt seem to be a strond difference in infection prevalence caused by fish size.

#Checkpoint 6
sample.size <- 5
possible.outcomes <- -2:2
df <- sample.size - 1
plot(possible.outcomes, dt(possible.outcomes, df), xlab = "Sample Outcome", ylab = "Probability", main = "T-distribution of Sample Size 5")
qt(c(.025, .975), df)

#Checkpoint 7
sample.size <- 50
possible.outcomes <- -25:25
df <- sample.size - 1
plot(possible.outcomes, dt(possible.outcomes, df), xlab = "Sample Outcome", ylab = "Probability", main = "T-distribution of Sample Size 5")
qt(c(.025, .975), df)

#Checkpoint 8
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.01
conf.levels <- c(alpha/2, 1 - alpha/2)
(conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n

#Checkpoint 9
cod.df.1999 <- subset(cod.df, subset = (Year == "1999"))
confint(lm(Length ~ 1, data = cod.df.1999), level = 0.95)
cod.df.2000 <- subset(cod.df, subset = (Year == "2000"))
confint(lm(Length ~ 1, data = cod.df.2000), level = 0.95)
cod.df.2001 <- subset(cod.df, subset = (Year == "2001"))
confint(lm(Length ~ 1, data = cod.df.2001), level = 0.95)

#Checkpoint 10
Area1.df <- subset(cod.df, subset = (Area == "1"))
Area2.df <- subset(cod.df, subset = (Area == "2"))
Area3.df <- subset(cod.df, subset = (Area == "3"))
Area4.df <- subset(cod.df, subset = (Area == "4"))
(Area1.infected <- sum(Area1.df$Prevalence == 1) / length(Area1.df$Prevalence))
(Area2.infected <- sum(Area2.df$Prevalence == 1) / length(Area2.df$Prevalence))
(Area3.infected <- sum(Area3.df$Prevalence == 1) / length(Area3.df$Prevalence))
(Area4.infected <- sum(Area4.df$Prevalence == 1) / length(Area4.df$Prevalence))
barplot(table(cod.df$Prevalence, cod.df$Area), xlab = "Area", ylab = "Infection Prevalence", col = my.colors, main = "Parasite Infection Rates by Area", cex.lab = 1.5)
legend("topleft", legend = c("Non-Infected", "Infected"), col = my.colors, pch = 19, cex = 1.5, bty = "n")
#The prevalence of parasite infections seems to be highest in Area 4 and lowest in Area 2.