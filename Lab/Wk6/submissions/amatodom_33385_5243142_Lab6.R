setwd('/Users/dominicamato/desktop/Biostatistics Labs/Wk6')
cod.df <- read.csv('cod.csv')
str(cod.df)
cod.df$Prevalence
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"
head(cod.df$Sex)
clean.cod.df <- cod.df[cod.df$Sex != "Unknown",]
str(clean.cod.df)
#Checkpoint 1 lines 9-10
table(cod.df$Sex)
table(cod.df$Area)
aggregate(Weight ~ Sex, data = cod.df, mean)
aggregate(Prevalence ~ Sex, data = cod.df, mean)
aggregate(Prevalence ~ Area, data = cod.df, mean)
table(cod.df$Prevalence, cod.df$Area)
#Checkpoint 2 
# a) 1254 fish
# b) 46% male, 48% female, 6% unknown
# c) 4 areas
# d) Average weight: male(1593.8), female(1857.85), Unknown(1443.8)
# e) 48% infection in females, 45% infection in males, 50% infection in unknown, it seems unknown is the most common followed by females
# f) Area 1: 49%, Area 2: 33%, Area 3: 35%, Area 4: 71%; significantly higher prevalence in areas 1 and 4
install.packages("viridisLite")
library(viridisLite)
our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25, end = 0.75)
male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
boxplot(cod.df$Prevalence ~ cod.df$Sex, xlab = 'Sex', ylab = 'Prevalence')
boxplot(cod.df$Prevalence ~ cod.df$Area, xlab = 'Area', ylab = 'Prevalence')
plot(jitter(cod.df$Length), cod.df$Prevalence, xlab = 'Length', ylab = 'Parasite Prevalence', pch = 19)
#Checkpoint 3-5 lines of code 30-32
t.value <- seq(-4,4, length = 100)
t.distribution <- dt(t.value, 4)
plot(t.distribution, xlab = 'T-values', ylab = 'Probability', pch = 19)
t.quantiles <- qt(c(0.025, 0.975), df=4)
t.quantiles
#Checkpoint 6 lines 34-38
t.value.2 <- seq(-49,49, length = 1000)
t.distribution.2 <- dt(t.value.2, 4)
plot(t.distribution.2, xlab = 'T-values', ylab = 'Probability', pch = 19)
t.quantiles.2 <- qt(c(0.025, 0.975), df=49)
t.quantiles.2
#Checkpoint 7 lines 40-44
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.01
conf.levels <- c(alpha/2, 1 - alpha/2)
conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n)/n
conf.int
#Checkpoint 8 lines 47-51
length.fish.99 <- lm(Length ~ 1, data = cod.df, Year == 1999)
confint(length.fish.99, level = 0.95)
length.fish.00 <- lm(Length ~ 1, data = cod.df, cod.df$Year == 2000)
confint(length.fish.99, level = 0.95)
length.fish.01 <- lm(Length ~ 1, data = cod.df, Year == 2001)
confint(length.fish.99, level = 0.95)
#Checkpoint 9 lines 53-58
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
#Checkpoint 10 lines of code 60-84, ChatGPT was used. I could not figure out how to determine uncertainty in each location while simultaneously estimating prevalence, similarly, I struggled with creating a visualization for the data. Between the 4 areas, area 4 has the largest prevalence of cod, followed by area 1. The prevalence of cod in both areas 2 and 3 have overlapping distributions and very similar prevalences of cod.