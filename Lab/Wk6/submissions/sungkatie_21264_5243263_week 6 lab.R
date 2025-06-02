# BIO 380 Lab; Week 6
# Katie Sung

rm(list=ls())
setwd("~/Desktop/BIO380/Wk6")

# Checkpoint 1
cod.df <- read.csv("cod.csv")
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"
clean.cod.df <- cod.df[cod.df$Sex != "Unknown", ]
head(clean.cod.df)
Male1.df <- subset(cod.df, subset = (Sex == "Male"))
Male2.df <- cod.df[cod.df$Sex == "Male", ]
identical(Male1.df, Male2.df)

clean.cod.df <- cod.df[cod.df$Sex != "Unknown", ]
str(clean.cod.df)
dim(clean.cod.df)
unique(clean.cod.df$Sex)
table(clean.cod.df$Sex)
table(clean.cod.df$Sex, clean.cod.df$Stage)
aggregate(Length ~ Sex, data = clean.cod.df, sd)

# Checkpoint 2
#a there are 1254 fish in the data set
total_fish <- nrow(cod.df)
total_fish
#b 45.77% are female, 47.68% are male and 6.54% are unknown
sex_counts <- table(cod.df$Sex)
sex_fractions <- sex_counts / total_fish
sex_fractions
#c there are 4 different areas 
unique(cod.df$Area)
#d the average weight of a female is 1857.9 grane, average weight of a male is 1593.79 grams, average weight of an unknown is 1443.81 grams
aggregate(Weight ~ Sex, data = cod.df, mean)
#e 273 females are sick, 47.5% of total female fish. 268 male fish are sick, 44.8% of male fish. 41 fish of unknown or 50% are sick
sick.cod.df <- cod.df[cod.df$Prevalence != 0, ]
table(sick.cod.df$Sex)
table(sick.cod.df$Sex)/table(cod.df$Sex)
#f 48% of fish in area 1 have parasite infections compared with 32% in area 2, 35% in area 3, and 71% in area 4 meaning they are most common in area 4
table(sick.cod.df$Area)
table(sick.cod.df$Area)/table(cod.df$Area)

install.packages("viridisLite")
library(viridisLite)
(our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25,
                    end = 0.75))
# Checkpoint 3
barplot(table(sick.cod.df$Sex), xlab = "Sex", ylab = "Number of Fish with Parasite Infections", col = "red")

# Checkpoint 4
barplot(table(cod.df$Area), xlab = "Area", ylab = "Number of Fish with Parasite Infections", col = "blue")

# Checkpoint 5
plot(jitter(cod.df$Prevalence), cod.df$Length, xlab = "Infection Status", ylab = "Fish Length (cm)", pch = 19, col = rgb(0, 0, 0, alpha = 0.05))

# Checkpoint 6
n <- 5
df <- n - 1
t_values <- seq(-4, 4, length = 100)
t_probabilities <- dt(t_values, df = df)
plot(t_values, t_probabilities, type = "l", lwd = 2, col = "blue",
     xlab = "t-values", ylab = "Density",
     main = paste("t-Distribution (df =", df, ")"))
quantiles <- qt(c(0.025, 0.975), df = df)
abline(v = quantiles, col = "red", lty = 2)
print(quantiles)

# Checkpoint 7
n <- 50
df <- n - 1
t_values <- seq(-4, 4, length = 100)
t_probabilities <- dt(t_values, df = df)
plot(t_values, t_probabilities, type = "l", lwd = 2, col = "blue",
     xlab = "t-values", ylab = "Density",
     main = paste("t-Distribution (df =", df, ")"))
quantiles <- qt(c(0.025, 0.975), df = df)
abline(v = quantiles, col = "red", lty = 2)
print(quantiles)

# Checkpoint 8 the probaibility that the true prevelence is between 42.8& and 50.1% is 99%
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

# Checkpoint 9 in 1999 the 95% confidence interval is 50.35-52.49. in 200 it is 53.79-58.30 and in 2001 it is 53.47-56.32
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

# Checkpoint 10 
# parasite prevalence (with uncertainty) in each location
Area1.df <- cod.df[cod.df$Area == 1, ]
TotalInfected1 <- sum(!is.na(Area1.df$Prevalence))
Area2.df <- cod.df[cod.df$Area == 2, ]
Area3.df <- cod.df[cod.df$Area == 3, ]
Area4.df <- cod.df[cod.df$Area == 4, ]
table(cod.df$Area, cod.df$Prevalence)

areas <- unique(cod.df$Area)
prevalence_data <- data.frame(Area = numeric(), Prevalence = numeric(), Lower_CI = numeric(), Upper_CI = numeric())

# visualization to show the data that support this estimate
prevalence_by_area <- table(cod.df$Area, cod.df$Prevalence)[, 2] / table(cod.df$Area)
barplot(prevalence_by_area, 
        col = "lightblue", 
        xlab = "Area", 
        ylab = "Prevalence", 
        main = "Parasite Prevalence Across Locations in Norwegian Cod")

# interpretation: area 4 has the highest prevalence which might mean more parasites, area 1 has higher but 2 and 3 show low prevelance, confidence intervals may vary by location due to different levels of uncertainty due to area and sample size

