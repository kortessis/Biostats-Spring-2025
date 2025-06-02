setwd('~/Desktop/BIO380/Week 6')
cod.df <- read.csv('Cod.csv')

##### CHECKPOINT 1 #####
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"
clean.cod.df <- subset(cod.df, subset = (Sex != "Unknown"))

##### CHECKPOINT 2 #####
table(cod.df$Sex)
574 + 598 + 82
574/1254 # 46% female
598/1254 # 48% male
82/1254 # 6% unknown
unique(cod.df$Area) #4
aggregate(Weight ~ Sex, data = clean.cod.df, mean)
table(clean.cod.df$Sex, clean.cod.df$Prevalence)
# Parasite infection is not more common in one sex. 
table(clean.cod.df$Area, clean.cod.df$Prevalence)
# Parasite infection is most common in area 4, but is also common in areas 1 and 2.

install.packages("viridisLite")
library(viridisLite)
(our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25, end = 0.75))

par(mar= c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1, 1))

##### CHECKPOINT 3 #####
# maybe do mosaic instead
barplot(table(clean.cod.df$Sex, clean.cod.df$Prevalence), xlab = 'Sex', ylab = 'Prevalence', col = our.colors, main = 'Sex Distribution of Infection Prevalence')
legend('topright', legend = c("Females","Males"), col = our.colors, pch = 19, cex = 1,  bty = 'n') 

##### CHECKPOINT 4 #####
barplot(table(clean.cod.df$Area, clean.cod.df$Prevalence), xlab = 'Areas', ylab = 'Prevalence', col = our.colors, main = 'Area Distribution of Infection Prevalence')
legend('topright', legend = c("3","1", "4", "2"), col = our.colors, pch = 19, cex = 1,  bty = 'n') 

##### CHECKPOINT 5 #####
as.factor(cod.df$Prevalence) #makes it a "category" instead of #
plot(jitter(cod.df$Prevalence), cod.df$Weight, xaxt = 'n', xlab = 'Prevalence', ylab = 'Fish Weight', pch = 19, col = rgb(0,0,0,alpha = 0.05), main = 'Fish Weight v Prevalence')
axis(1, at = c(0,1), labels = c('Not Infected', 'Infected'))
#could also do box plots, overlapping histograms, or overlapping density plots 

##### CHECKPOINT 6 #####
n <- 5
pot.outcomes <- seq(from = -5, to = 5, length = 100)
samp.prob <- dt(pot.outcomes, 4)
plot(pot.outcomes, samp.prob, typ = 'l', xlab = 'Sample Outcome', ylab = 'Probability', main = 'Sample t-Distribution')
cum.prob <- pt(pot.outcomes, 4)
quantile <- qt(c(0.025, 0.975), 4) #-2.78, 2.78

##### CHECKPOINT 7 #####
n <- 50
pot.outcomes2 <- seq(from = -50, to = 50, length = 100)
samp.prob2 <- dt(pot.outcomes2, 49)
plot(pot.outcomes2, samp.prob2, typ = 'l', xlab = 'Sample Outcome', ylab = 'Probability', main = 'Sample t-Distribution')
cum.prob2 <- pt(pot.outcomes2, 49)
quantile2 <- qt(c(0.025, 0.975), 49) #-2.01, 2.01

##### CHECKPOINT 8 #####
n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence == 1)
alpha <- 0.01
conf.levels <- c(alpha/2, 1 - alpha/2)
conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n)
answer <- conf.int / n # CI = 42.8%-50.1%

##### CHECKPOINT 9 #####
cod.1999 <- subset(cod.df, subset = (cod.df$Year == 1999), select = 'Length')
mean.length <- mean(cod.1999[[1]], na.rm = T)
n.length <- sum(!is.na(cod.1999[[1]]))
sd.length <- sd(cod.1999[[1]], na.rm = T)
se.length <- sd.length/sqrt(n.length)
alpha <- 0.05
conf.levels <- c(alpha/2, 1-alpha/2)
qt(conf.levels, df = n.length -1)
(fish.conf.int <- mean.length + qt(conf.levels, df = n.length - 1)*se.length) 

cod.2000 <- subset(cod.df, subset = (cod.df$Year == 2000), select = 'Length')
mean.length <- mean(cod.2000[[1]], na.rm = T)
n.length <- sum(!is.na(cod.2000[[1]]))
sd.length <- sd(cod.2000[[1]], na.rm = T)
se.length <- sd.length/sqrt(n.length)
alpha <- 0.05
conf.levels <- c(alpha/2, 1-alpha/2)
qt(conf.levels, df = n.length -1)
(fish.conf.int <- mean.length + qt(conf.levels, df = n.length - 1)*se.length) 

cod.2001 <- subset(cod.df, subset = (cod.df$Year == 2001), select = 'Length')
mean.length <- mean(cod.2001[[1]], na.rm = T)
n.length <- sum(!is.na(cod.2001[[1]]))
sd.length <- sd(cod.2001[[1]], na.rm = T)
se.length <- sd.length/sqrt(n.length)
alpha <- 0.05
conf.levels <- c(alpha/2, 1-alpha/2)
qt(conf.levels, df = n.length -1)
(fish.conf.int <- mean.length + qt(conf.levels, df = n.length - 1)*se.length) 

# 1999 = 50.4 - 52.5
# 2000 = 53.8 - 57.3
# 2001 = 53.5 - 56.3

##### CHECKPOINT 10 #####
alpha <- 0.05
conf.levels <- c(alpha/2, 1 - alpha/2)

area1 <- subset(cod.df, subset = (cod.df$Area == 1), select = 'Prevalence')[[1]]
n.area1 <- length(area1)
infected.area1 <- sum(area1 == 1)
CI.area1 <- qbinom(conf.levels, size = n.area1, prob = infected.area1/n.area1)
answer.area1 <- CI.area1 / n.area1 #0.4301471 0.5477941

area2 <- subset(cod.df, subset = (cod.df$Area == 2), select = 'Prevalence')[[1]]
n.area2 <- length(area2)
infected.area2 <- sum(area2 == 1)
CI.area2 <- qbinom(conf.levels, size = n.area2, prob = infected.area2/n.area2)
answer.area2 <- CI.area2 / n.area2 #0.271 0.384

area3 <- subset(cod.df, subset = (cod.df$Area == 3), select = 'Prevalence')[[1]]
n.area3 <- length(area3)
infected.area3 <- sum(area3 == 1)
CI.area3 <- qbinom(conf.levels, size = n.area3, prob = infected.area3/n.area3)
answer.area3 <- CI.area3 / n.area3 #0.306 0.398

area4 <- subset(cod.df, subset = (cod.df$Area == 4), select = 'Prevalence')[[1]]
n.area4 <- length(area4)
infected.area4 <- sum(area4 == 1)
CI.area4 <- qbinom(conf.levels, size = n.area4, prob = infected.area4/n.area4)
answer.area4 <- CI.area4 / n.area4 #0.654 0.756

prev.a1 <- infected.area1 / n.area1
prev.a2 <- infected.area2 / n.area2
prev.a3 <- infected.area3 / n.area3
prev.a4 <- infected.area4 / n.area4
vec.values <- c(prev.a1, prev.a2, prev.a3, prev.a4)
categories <- c("Area 1", "Area 2", "Area 3", "Area 4")

barplot(vec.values, names.arg = categories, main = 'Parasite Prevelance in Each Location', ylab = 'Prevelance')

table(vec.area, vec.values)
barplot(vec.values)
axis(1, at = c(0,1, 2, 3), labels = c('Not Infected', 'Infected'))