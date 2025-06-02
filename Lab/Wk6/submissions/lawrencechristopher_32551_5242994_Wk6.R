cod.df <- read.csv('/Users/chris/Desktop/BIO 380/Wk6/Cod.csv')

#Checkpoint 1

cod.df$Prevalence
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"
clean.cod.df <- cod.df[cod.df$Sex != "Unknown",]
print(clean.cod.df)

#Checkpoint 2

table(cod.df$Area)
aggregate(Weight ~ Sex, data = cod.df, mean)
aggregate(Prevalence ~ Sex, data = cod.df, mean)
aggregate(Prevalence ~ Area, data = cod.df, mean)
table(cod.df$Prevalence, cod.df$Area)

#1254 fish
#48% female, 46% male, 6% unknown
#4 areas
# d) Avg weight --> female(1857.85), Unknown(1443.8), male(1593.8),
#45% infection in males, 50% infection in unknown, 48% infection in females, unknown is most common, then female then males
#Area1 - 49%, Area2 - 33%, Area 3 - 35%, Area4 - 71%; larger prevalence percetnages in areas 1 and 4


#Checkpoint 3, 4 and 5
install.packages("viridisLite")
library(viridisLite)
our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25, end = 0.75)
male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
boxplot(cod.df$Prevalence ~ cod.df$Sex, xlab = 'Sex', ylab = 'Prevalence')
boxplot(cod.df$Prevalence ~ cod.df$Area, xlab = 'Area', ylab = 'Prevalence')
plot(jitter(cod.df$Length), cod.df$Prevalence, xlab = 'Length', ylab = 'Parasite Prevalence', pch = 19)

.value <- seq(-4,4, length = 100)
t.distribution <- dt(t.value, 4)
plot(t.distribution, xlab = 'T-values', ylab = 'Probability', pch = 19)
t.quantiles <- qt(c(0.025, 0.975), df=4)
t.quantiles

#Checkpoint 6
t.value.2 <- seq(-49,49, length = 1000)
t.distribution.2 <- dt(t.value.2, 4)
plot(t.distribution.2, xlab = 'T-values', ylab = 'Probability', pch = 19)
t.quantiles.2 <- qt(c(0.025, 0.975), df=49)
t.quantiles.2

#Checkpoint 7
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.01
conf.levels <- c(alpha/2, 1 - alpha/2)
conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n)/n
conf.int

#Checkpoint 8

length.fish.99 <- lm(Length ~ 1, data = cod.df, Year == 1999)
confint(length.fish.99, level = 0.95)
length.fish.00 <- lm(Length ~ 1, data = cod.df, cod.df$Year == 2000)
confint(length.fish.99, level = 0.95)
length.fish.01 <- lm(Length ~ 1, data = cod.df, Year == 2001)
confint(length.fish.99, level = 0.95)


#Checkpoint 9

mean_lengths_by_year <- tapply(cod.df$Length, cod.df$Year, mean, na.rm = T)
print(mean_lengths_by_year)

mean.lengths_1999 <- mean(subset(cod.df, Year == 1999)$Length, na.rm = T)
mean.lengths_2000 <- mean(subset(cod.df, Year == 2000)$Length, na.rm = T)
mean.lengths_2001 <- mean(subset(cod.df, Year == 2001)$Length, na.rm = T)

stats_by_year <- tapply(cod.df$Length, cod.df$Year, function(x) {
  n.length <- sum(!is.na(x))
  mean.length <- mean(x, na.rm = T)
  sd.length <- sd(x, na.rm = T)
  se.length <- sd.length / sqrt(n.length)
  c(n.length = n.length, mean.length = mean.length, sd.length = sd.length, se.length = se.length)
})

alpha <- 0.05
conf.levels <- c(alpha / 2, 1 - alpha / 2)

print(confints_by_year)

#Checkpoint 10
prevalence_by_area <- tapply(cod.df$Prevalence, cod.df$Area, mean, na.rm = T)
print(prevalence_by_area)
names(cod.df)

confint_by_area <- tapply(cod.df$Prevalence, cod.df$Area, function(x) {
  num_infected_area <- sum(x == 1)
  n_area <- length(x)
  alpha <- 0.05
  conf.levels <- c(alpha/2, 1 - alpha/2)
  conf.int <- qbinom(conf.levels, size = n_area, prob = num_infected_area / n_area) / n_area
  return(conf.int)
})

barplot(prevalence_by_area, ylim = c(0, 2), xlab = "Area", ylab = "Prevalence", main = "Parasite Prevalence by Area")

mean_prevalence_by_area <- tapply(cod.df$Prevalence, cod.df$Area, mean, na.rm = T)

conf_intervals <- tapply(cod.df$Prevalence, cod.df$Area, function(x) {
  t_test <- t.test(x)
  confint <- t_test$conf.int
  return(confint)
})


lower_bounds <- sapply(conf_intervals, function(x) x[1])
upper_bounds <- sapply(conf_intervals, function(x) x[2])

plot(mean_prevalence_by_area, type = "b", pch = 19, ylim = c(min(lower_bounds), max(upper_bounds)), 
     xlab = "Area", ylab = "Prevalence", main = "Prevalence by Area with 95% CI")

arrows(1:length(mean_prevalence_by_area), lower_bounds, 
       1:length(mean_prevalence_by_area), upper_bounds, 
       angle = 90, code = 3, length = 0.1)

