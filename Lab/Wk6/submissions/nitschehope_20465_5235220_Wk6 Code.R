setwd("/Users/hopenitsche/Desktop/Spring 2025/Biostats/Lab/Wk6")
cod.df <- read.csv('cod.csv')
str(cod.df)
cod.df$Prevalence 
cod.df[cod.df$Sex == 2, "Sex"] <- "Male"
cod.df[cod.df$Sex == 1, "Sex"] <- "Female"
cod.df[cod.df$Sex == 0, "Sex"] <- "Unknown"

head(cod.df$Sex)
Male1.df <- subset(cod.df, subset = (Sex == "Male"))
Male2.df <- cod.df[cod.df$Sex == "Male", ]
identical(Male1.df, Male2.df)

#Checkpoint 1: Create a dataset that does not include the individuals with the Unknown sex. Call this clean.cod.df.
clean.cod.df <- subset(cod.df, subset=(Sex !="Unknown"))

str(clean.cod.df)
dim(clean.cod.df)
unique(clean.cod.df$Sex)
table(clean.cod.df$Sex)
table(clean.cod.df$Sex, clean.cod.df$Stage)
aggregate(Length ~ Sex, data = clean.cod.df, sd)

#Checkpoint 2: Use these functions to write code that answers the following question. 
#a. How many fish are in the total data set (i.e., including the fish with unknown sex)?
dim(cod.df)
#There are 1254 fish in the total data set, since this is the number of rows/obs in the data set. 
#b. What fraction are male, female, and unknown?
table(cod.df$Sex)
#The fraction of females in the study is 574/1254. The fraction of males is 598/1254. The fraction of fish with an unknown gender is 82/1264. 
#c. How many areas are in the study?
unique(cod.df$Area)
#There are 4 different areas in this study (2,3,4,1). 
#d. What are the average weights of fish for each different sex?
aggregate(Weight ~ Sex, data= cod.df,mean)
#The table generated shows the average weight of fish in each different sex. 
#e. What is the prevalence of parasite infections by fish age? Is parasite infection more common in one sex?
aggregate(Prevalence ~ Age, data=cod.df, mean)
aggregate(Prevalence ~ Sex, data=cod.df, mean)
#These tables show the differences in parasite infection in different ages (apart from age 1), and across sexes. It does not appear that there is a sex difference between the sexes in parasite infection. 
#f. What is the prevalence of parasite infections by area? Is parasite infection more common in some areas? Which ones?
table(cod.df$ Prevalence, cod.df$Area)
prop.table(table(cod.df$ Prevalence, cod.df$Area))
# It appears that the parasite is more common to area 4 than all of the other areas. It is least common to area 2 out of all of the different areas included in this sample. 

barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish")
mosaicplot(Sex ~ 1, data = cod.df, main = "Sex Distribution in Cod")
barplot(table(cod.df$Sex), xlab = "Sex", ylab = "Number of Fish", col = "red")
install.packages("viridisLite")
library (viridisLite)
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

boxplot(cod.df$Weight ~ cod.df$Area, xlab = "Area", ylab = "Fish Weight", col = mako(n = length(unique(cod.begin = 0.3, end = 0.7))

par(mfrow = c(1, 2))
plot(cod.df$Area, cod.df$Weight, xlab = "Area", ylab = "Fish Weight", pch = 19, col = rgb(0,0, 0, alpha = 0.5), main = "Without Jitter")
plot(jitter(cod.df$Area), cod.df$Weight, xlab = "Area", ylab = "Fish Weight", pch = 19,col = rgb(0, 0, 0, alpha = 0.05), main = "With Jitter")

head(cod.df$Area)
head(jitter(cod.df$Area))
plot(cod.df$Length, cod.df$Weight, pch = 19, xlab = "Length", ylab = "Weight", col = rgb(0, 0, 0, alpha = 0.15))

my.colors <- viridis(3, begin = 0.3, end = 0.7, alpha = 0.25)
# Separate male and female data
male.df <- subset(cod.df, subset = (Sex == "Male"))
female.df <- subset(cod.df, subset = (Sex == "Female"))
# Plot females first
plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length', ylab = 'Weight',
     col = my.colors[1])
# Add males to the plot using the function "points"
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft', #position of legend
       legend = c("Females","Males"), # labels
       col = my.colors, # colors of labels
       pch = 19, cex = 1.5, # points to draw next to labels
       bty = 'n') # box type. 'n' for no box around legend
pdf(file = 'MyFishPlot.pdf')

#Checkpoint 3. What is parasite infection prevalence across fish sexes?
(our.colors <- mako(length(unique(cod.df$Sex)), begin = 0.25, end = 0.75))
mosaicplot(prop.table(table(cod.df$ Sex, cod.df$Prevalence)),xlab = "Sex", ylab = "Parasite Status",col = our.colors,main = "Parasite Prevalence By Sex")

#Checkpoint 4. What is parasite infection prevalence across areas?
(extra.colors <- mako(length(unique(cod.df$Area)), begin = 0.25, end = 0.75))
par(mfrow = c(1, 2))
mosaicplot(prop.table(table(cod.df$ Prevalence, cod.df$Area)),xlab = "Parasite Status", ylab = "Area", col=extra.colors, main = "Parasite Prevalence By Area")
#Checkpoint 5. What is the relationship between parasite infection and fish size? That is, are larger or smaller fish more at risk of parasite infection (jitter could be helpful here)?
my.colors <- viridis(3, begin = 0.3, end = 0.7, alpha = 0.25)
parasite.df <- subset(cod.df, subset=(Prevalence=="1"))
noparasite.df <- subset(cod.df, subset=(Prevalence=="0"))
plot(jitter(parasite.df$Length),parasite.df$Weight, 
      , pch = 19, cex = 1.5, xlab = 'Length', ylab = 'Weight',
     col = my.colors[1], main='Prevalence of Parasites by Fish Size')
points(jitter(noparasite.df$Length), noparasite.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2], )
legend('topleft',
       legend = c("Parasite","No Parasites"), 
       col = my.colors, 
       pch = 19, cex = 1.5, bty='n')

alphabet <- letters[1:26]
sample(alphabet, size = 10, replace = T) 
sample(alphabet, size = 10, replace = F)

mu <- 10
# population sd
sigma <- 2
# Sample size
n <- 30
# sampling distribution mean
xbar.mu <- mu
# sampling distributrion standard deviation (=standard error)
xbar.sd <- sigma/sqrt(n)
pot.outcomes <- seq(from = 5, to = 15, length = 10000)
sam.prob <- dnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
sam.cumul.prob <- pnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
par(mfcol = c(1, 2), cex.lab = 0.8, cex.axis = 0.8)
plot(pot.outcomes, sam.prob, typ = "l", xlab = "Sample Outcome", ylab = "Probability",
     main = "Sampling Distribution")
plot(pot.outcomes, sam.cumul.prob, typ = "l", xlab = "Sample Mean", ylab = "Cumulative Probability",
     main = "Sampling Distribution")

#Checkpoint 6: Plot the t-distribution for sample size of 5 and calculate its 2.5% and 97.5% quantiles that correspond to 95% of the most likely outcomes.
pot.outcomes <- seq(from = 5, to = 15, length = 50)
t.dis<- dt(pot.outcomes,4)
plot(pot.outcomes,t.dis,xlab = "Sample Outcome", ylab = "Probability",
     main = "Sampling Distribution", typ='l')
qt(0.025,4)
qt(0.975,4)
#Checkpoint 7: Plot the t-distribution for sample size of 50 and calculate its 2.5% and 97.5% quantiles.
pot.outcomes <- seq(from = 5, to = 15, length = 50)
t.dis<- dt(pot.outcomes,49)
plot(pot.outcomes,t.dis,xlab = "Sample Outcome", ylab = "Probability",
     main = "Sampling Distribution", typ='l')
 
 qt(0.025,49)
 qt(0.975,49)
 
 n <- length(cod.df$Prevalence)
 num.infected <- sum(cod.df$Prevalence == 1)
 alpha <- 0.05
 conf.levels <- c(alpha/2, 1 - alpha/2)
 (conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n
 
#Checkpoint 8: Construct a 99% confidence interval for prevalence of parasite infection.
 alpha <- 0.01
 conf.levels <- c(alpha/2, 1 - alpha/2)
 (conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n
 
 mean.length <- mean(cod.df$Length, na.rm = T)
 n.length <- sum(!is.na(cod.df$Length))
 sd.length <- sd(cod.df$Length, na.rm = T)
 se.length <- sd.length/sqrt(n.length)
 alpha <- 0.01
 conf.levels <- c(alpha/2, 1 - alpha/2)
 qt(conf.levels, df = n.length - 1)
 (fish.conf.int <- mean.length + qt(conf.levels, df = n.length - 1) * se.length)
 length.mdl <- lm(Length ~ 1, data = cod.df)
 confint(length.mdl, level = 0.99)
 
#Checkpoint 10: Find the 95% confidence intervals for fish lengths in each year of the study.
 unique(cod.df$Year)
 n.cod.df <- cod.df[cod.df$Year=="1999",]
 n.mean.length <- mean(n.cod.df$Length, na.rm = T)
 n.n.length <- sum(!is.na(n.cod.df$Length))
 n.sd.length <- sd(n.cod.df$Length, na.rm = T)
 n.se.length <- n.sd.length/sqrt(n.n.length)
 alpha <- 0.05
 n.conf.levels <- c(alpha/2, 1 - alpha/2)
 qt(conf.levels, df = n.n.length - 1)
 (n.fish.conf.int <- n.mean.length + qt(n.conf.levels, df = n.n.length - 1) * n.se.length)
 n.length.mdl <- lm(Length ~ 1, data = n.cod.df)
 confint(n.length.mdl, level = 0.95)
 
 t.cod.df <- cod.df[cod.df$Year=="2000",]
 t.mean.length <- mean(t.cod.df$Length, na.rm = T)
 t.n.length <- sum(!is.na(t.cod.df$Length))
 t.sd.length <- sd(t.cod.df$Length, na.rm = T)
 t.se.length <- t.sd.length/sqrt(t.n.length)
 qt(conf.levels, df = t.n.length - 1)
 t.length.mdl <- lm(Length ~ 1, data = t.cod.df)
 confint(t.length.mdl, level = 0.95)
 
 o.cod.df <- cod.df[cod.df$Year=="2001",]
 o.mean.length <- mean(o.cod.df$Length, na.rm = T)
 o.n.length <- sum(!is.na(o.cod.df$Length))
 o.sd.length <- sd(o.cod.df$Length, na.rm = T)
 o.se.length <- o.sd.length/sqrt(o.n.length)
 qt(conf.levels, df = o.n.length - 1)
 o.length.mdl <- lm(Length ~ 1, data = o.cod.df)
 confint(o.length.mdl, level = 0.95)
 
#Checkpoint 11: Estimate parasite prevalence (with uncertainty) in each location. Make a visualization to show the data that support this estimate. Finally, describe the pattern of prevalence across space in Norwegian Cod based on this data.
 par(mar=c(5,4,4,2))
 aggregate(Prevalence~ Area, cod.df, mean)
 #Area Prevalence
#1    0.4889706
 #2   0.3254902
 #3   0.3518072
 #4   0.7051282
 table(cod.df$Area)
#1   2   3   4 
#272 255 415 312 
 
 A1.df <- subset(cod.df, subset = (Area== "1"))
 A2.df <- subset(cod.df, subset = (Area == "2"))
 A3.df <- subset(cod.df, subset = (Area == "3"))
 A4.df <- subset(cod.df, subset = (Area == "4"))
 
 A1.mean.Prevalence <- mean(A1.df$Prevalence, na.rm = T)
 A1.n.Prevalence <- sum(!is.na(A1.df$Prevalence))
 A1.sd.Prevalence <- sd(A1.df$Prevalence, na.rm = T)
 A1.se.Prevalence <- A1.sd.Prevalence/sqrt(A1.n.Prevalence)
 alpha <- 0.01
 A1.conf.levels <- c(alpha/2, 1 - alpha/2)
 qt(A1.conf.levels, df = A1.n.Prevalence - 1)
 (A1.fish.conf.int <- A1.mean.Prevalence + qt(A1.conf.levels, df = A1.n.Prevalence - 1) * A1.se.Prevalence)
 
 A2.mean.Prevalence <- mean(A2.df$Prevalence, na.rm = T)
 A2.n.Prevalence <- sum(!is.na(A2.df$Prevalence))
 A2.sd.Prevalence <- sd(A2.df$Prevalence, na.rm = T)
 A2.se.Prevalence <- A2.sd.Prevalence/sqrt(A2.n.Prevalence)
 alpha <- 0.01
 A2.conf.levels <- c(alpha/2, 1 - alpha/2)
 qt(A2.conf.levels, df = A2.n.Prevalence - 1)
 (A2.fish.conf.int <- A2.mean.Prevalence + qt(A2.conf.levels, df = A2.n.Prevalence - 1) * A2.se.Prevalence)
 
 A3.mean.Prevalence <- mean(A3.df$Prevalence, na.rm = T)
 A3.n.Prevalence <- sum(!is.na(A3.df$Prevalence))
 A3.sd.Prevalence <- sd(A3.df$Prevalence, na.rm = T)
 A3.se.Prevalence <- A3.sd.Prevalence/sqrt(A3.n.Prevalence)
 alpha <- 0.01
 A3.conf.levels <- c(alpha/2, 1 - alpha/2)
 qt(A3.conf.levels, df = A3.n.Prevalence - 1)
 (A3.fish.conf.int <- A3.mean.Prevalence + qt(A3.conf.levels, df = A3.n.Prevalence - 1) * A3.se.Prevalence)
 
 A4.mean.Prevalence <- mean(A4.df$Prevalence, na.rm = T)
 A4.n.Prevalence <- sum(!is.na(A4.df$Prevalence))
 A4.sd.Prevalence <- sd(A4.df$Prevalence, na.rm = T)
 A4.se.Prevalence <- A4.sd.Prevalence/sqrt(A4.n.Prevalence)
 alpha <- 0.01
 A4.conf.levels <- c(alpha/2, 1 - alpha/2)
 qt(A4.conf.levels, df = A4.n.Prevalence - 1)
 (A4.fish.conf.int <- A4.mean.Prevalence + qt(A4.conf.levels, df = A4.n.Prevalence - 1) * A4.se.Prevalence)
 
 plot((aggregate(Prevalence~ Area, cod.df, mean)), xlab='Area Number', ylab= 'Prevalence of Parasite', main= "Prevalence of Parasite Across Sample Area")
 
barplot(prop.table(table(cod.df$Prevalence, cod.df$Area)),xlab= 'Area Number', ylab='Proportion of Entire Population', col = our.colors, main="Prevalence of Parasite Across Sample Area")
legend('topleft',
       legend = c("No Parasite","Parasites"), 
       col = our.colors, 
       pch = 19, cex = 0.5)

#Based on all of the data collected, we can see there there are the most parasites in area 4. It has the highest prevalence of parasites as a proportion of the sample taken in an individual area (shown in the scatter plot), and it also has the most parasites as compared to the proportion it makes up out of the entire sample (as shown by the bar graph)
