setwd("/Users/lukemorton/Desktop/Bio stats/week 6")
cod.df <- read.csv(file = 'Cod.csv')
vec <- c("Nicks", "BIO 380", "Lab")
my.vec <- seq(from = 10, to = 300, length = 5)
another.vec <- rep(c("Success","Failure"), times = c(30,70))        
vec[2]
my.vec[3]
another.vec[100]
another.vec[50:60]
vec[-2]; vec[-c(2:3)]
a.matrix <- matrix(data = LETTERS[1:20],
                   ncol = 4,
                   nrow = 5,
                   byrow = T)
a.matrix[2,3]
a.matrix[2,]
a.matrix[,3]
a.matrix[-3,c(3:4)]
str(cod.df)
cod.df$Prevalence
cod.df[cod.df$Sex== 2,"Sex"] <- "Male"
cod.df[cod.df$Sex== 1,"Sex"] <- "Female"
cod.df[cod.df$Sex== 0,"Sex"] <- "Unknown"
head(cod.df$Sex)
Male1.df <- subset(cod.df, subset = (Sex== "Male"))
Male2.df <- cod.df[cod.df$Sex== "Male",]
identical(Male1.df, Male2.df)

# Checkpoint 1: Code Below
clean.cod.df <- cod.df[cod.df$Sex != "Unknown", ]
str(clean.cod.df)
dim(clean.cod.df)
unique(clean.cod.df$Sex)
table(clean.cod.df$Sex)
table(clean.cod.df$Sex, clean.cod.df$Stage)
aggregate(Length~ Sex, data = clean.cod.df, sd)

# Checkpoint 2: 
#A 
dim(cod.df)
#B
table(cod.df$Sex)
table(cod.df$Sex) / sum(table(cod.df$Sex)) 
#C
unique(cod.df$Area)
length(unique(cod.df$Area))
#D
aggregate(Weight ~ Sex, data = cod.df, mean, na.rm = TRUE)
#E Females have the highest Prevalence
table(cod.df$Prevalence, cod.df$Sex)
prop.table(table(cod.df$Prevalence, cod.df$Sex), margin = 2)
#F Area 4 has the highest Prevalence 
table(cod.df$Prevalence, cod.df$Area)
prop.table(table(cod.df$Prevalence, cod.df$Area), margin = 2)

barplot(table(cod.df$Sex),
        xlab = 'Sex'
        ,
        ylab ='Number of Fish')
mosaicplot(Sex~ 1, data = cod.df,
           main = 'Sex Distribution in Cod')
barplot(table(cod.df$Sex),
        xlab = 'Sex',
        ylab = 'Number of Fish',
        col = 'red')
install.packages("viridisLite")
library(viridisLite)
(our.colors <- mako(length(unique(cod.df$Sex)),
                    begin = 0.25,
                    end = 0.75))
barplot(table(cod.df$Sex),
        xlab = 'Sex'
        ,
        ylab ='Number of Fish'
        ,
        col = our.colors,
        main = 'Sex Distribution of Norwegian Cod')
barplot(table(cod.df$Sex),
        xlab = 'Sex',
        ylab = 'Number of Fish',
        col = our.colors,
        main = 'Sex Distribution of Norwegian Cod',
        cex.lab = 1.5)
par(mfrow = c(2,2))
boxplot(cod.df$Weight)
plot(density(cod.df$Weight, na.rm = TRUE))
hist(cod.df$Weight)
stripchart(cod.df$Weight,
           method = "jitter",
           pch = 19)
par(mfrow = c(2,2))
boxplot(cod.df$Weight, horizontal = TRUE)
plot(density(cod.df$Weight, na.rm = TRUE))
hist(cod.df$Weight)
stripchart(cod.df$Weight,
           method = "jitter",
           pch = 19,
           col = rgb(0,0,0, alpha = 0.05))
summary(cod.df$Weight)
boxplot(cod.df$Weight~ cod.df$Area,
        xlab = 'Area'
        , ylab ='Fish Weight'
        ,
        col = mako(n = length(unique(cod.df$Area)), begin = 0.3, end = 0.7))
par(mfrow = c(1,2))
plot(cod.df$Area, cod.df$Weight,
     xlab = 'Area'
     , ylab ='Fish Weight'
     ,
     pch = 19, col = rgb(0,0,0,alpha = 0.5),
     main = "Without Jitter")
plot(jitter(cod.df$Area), cod.df$Weight,
     xlab = 'Area'
     , ylab ='Fish Weight'
     ,
     pch = 19, col = rgb(0,0,0,alpha = 0.05),
     main = 'With Jitter')
head(cod.df$Area)
head(jitter(cod.df$Area))
plot(cod.df$Length, cod.df$Weight,
     pch = 19,
     xlab = 'Length', ylab = 'Weight',
     col = rgb(0,0,0, alpha = 0.15))
my.colors <- viridis(2, begin = 0.3, end = 0.7, alpha = 0.25)
male.df <- subset(cod.df, subset = (Sex== "Male"))
female.df <- subset(cod.df, subset = (Sex== "Female"))
plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length'
     , ylab ='Weight'
     ,
     col = my.colors[1])
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft'
       , #position of legend
       legend = c("Females","Males"), # labels
       col = my.colors, # colors of labels
       pch = 19, cex = 1.5, # points to draw next to labels
       bty ='n')
pdf(file = 'MyFishPlot.pdf')
plot(female.df$Length, female.df$Weight,
     pch = 19, cex = 1.5,
     xlab = 'Length'
     , ylab ='Weight'
     ,
     col = my.colors[1])
points(male.df$Length, male.df$Weight,
       pch = 19, cex = 1.5,
       col = my.colors[2])
legend('topleft'
       , #position of legend
       legend = c("Females","Males"), # labels
       col = my.colors, # colors of labels
       pch = 19, cex = 1.5, # points to draw next to labels
       bty ='n')
dev.off()

# Checkpoint 3
infection.by.sex <- table(cod.df$Prevalence, cod.df$Sex)
infection.proportions <- prop.table(infection.by.sex, margin = 2)
library(viridisLite)
sex.colors <- mako(2, begin = 0.3, end = 0.7) 
barplot(infection.proportions,
        beside = TRUE,  
        col = sex.colors,
        xlab = 'Sex',
        ylab = 'Proportion of Fish',
        main = 'Parasite Infection Prevalence Across Sexes')
legend('topright', 
       legend = c('Not Infected', 'Infected'), 
       fill = sex.colors,
       bty = 'n')
# Checkpoint 4
infection.by.area <- table(cod.df$Prevalence, cod.df$Area)
infection.proportions.area <- prop.table(infection.by.area, margin = 2)
library(viridisLite)
area.colors <- mako(2, begin = 0.3, end = 0.7) 
barplot(infection.proportions.area,
        beside = TRUE,  
        col = area.colors,
        xlab = 'Area',
        ylab = 'Proportion of Fish with Parasite Infection',
        main = 'Parasite Infection Prevalence Across Areas')
# Checkpoint 5: The larger fish are more at risk for parasites than the smaller fish.  
cod.df$Prevalence <- factor(cod.df$Prevalence, levels = c(0, 1), labels = c("Not Infected", "Infected"))
library(viridisLite)
infection.colors <- mako(2, begin = 0.3, end = 0.7)  
stripchart(cod.df$Length ~ cod.df$Prevalence,
           method = "jitter",
           jitter = 0.2, 
           pch = 19,
           vertical = TRUE, 
           col = infection.colors,
           xlab = "Parasite Infection Status",
           ylab = "Fish Length (cm)",
           main = "Relationship Between Fish Size and Parasite Infection")
axis(1, at = c(1, 2), labels = c("Not Infected", "Infected"))

alphabet <- letters[1:26]
sample(alphabet, size = 10, replace = T)
sample(alphabet, size = 10, replace = F) # Without replacement
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
     type = 'l')
mu <- 10  
sigma <- 2
n <- 30
xbar.mu <- mu
xbar.sd <- sigma/sqrt(n)
pot.outcomes <- seq(from = 5, to = 15, length = 10000)
sam.prob <- dnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
sam.cumul.prob <- pnorm(pot.outcomes, mean = xbar.mu, sd = xbar.sd)
par(mfcol = c(1,2), cex.lab = 0.8, cex.axis = 0.8)
plot(pot.outcomes, sam.prob, typ ='l'
     ,
     xlab = 'Sample Outcome'
     ,
     ylab ='Probability'
     ,
     main = 'Sampling Distribution')
plot(pot.outcomes, sam.cumul.prob, typ ='l'
     ,
     xlab = 'Sample Mean'
     ,
     ylab ='Cumulative Probability'
     ,
     main = 'Sampling Distribution')
# Checkpoint 6
sample.size <- 5
prob.success <- 0.6
possible.outcomes <- 0:sample.size
probabilities <- dbinom(possible.outcomes,
                        size = sample.size,
                        prob = prob.success)
lower.quantile <- qbinom(0.025, size = sample.size, prob = prob.success)
upper.quantile <- qbinom(0.975, size = sample.size, prob = prob.success)
plot(possible.outcomes, probabilities,
     xlab = '# Successes',
     ylab = 'Probability',
     main = 'Probability Distribution Function',
     pch = 19)
abline(v = lower.quantile, col = "red", lty = 2, lwd = 2)
abline(v = upper.quantile, col = "blue", lty = 2, lwd = 2)
c(lower.quantile, upper.quantile)
 # Checkpoint 7
sample.size <- 50
prob.success <- 0.6
possible.outcomes <- 0:sample.size
probabilities <- dbinom(possible.outcomes,
                        size = sample.size,
                        prob = prob.success)
lower.quantile <- qbinom(0.025, size = sample.size, prob = prob.success)
upper.quantile <- qbinom(0.975, size = sample.size, prob = prob.success)
plot(possible.outcomes, probabilities,
     xlab = '# Successes',
     ylab = 'Probability',
     main = 'Probability Distribution Function',
     pch = 19)
abline(v = lower.quantile, col = "red", lty = 2, lwd = 2)
abline(v = upper.quantile, col = "blue", lty = 2, lwd = 2)
c(lower.quantile, upper.quantile)
sample.proportion <- prob.success
standard.error <- sqrt((sample.proportion * (1 - sample.proportion)) / sample.size)
confidence.level <- 0.95
z.score <- qnorm((1 + confidence.level) / 2)
lower.bound <- sample.proportion - z.score * standard.error
upper.bound <- sample.proportion + z.score * standard.error
c(lower.quantile, upper.quantile)
c(lower.bound, upper.bound)

n <- length(cod.df$Prevalence)
num.infected <- sum(cod.df$Prevalence== 1)
alpha <- 0.05
conf.levels <- c(alpha/2, 1- alpha/2)
(conf.int <- qbinom(conf.levels, size = n, prob = num.infected/n))/n
# I keep getting 0 0 instead of 0.4362041 0.4920255 like the example in the directions. I am not sure why. 

# Checkpoint 8: 
infection.by.sex <- table(cod.df$Prevalence, cod.df$Sex)
infection.proportions <- prop.table(infection.by.sex, margin = 2)
chisq.test(infection.by.sex)
fisher.test(infection.by.sex)
infection.proportions

hist(cod.df$Length)
mean.length <- mean(cod.df$Length, na.rm = T)
n.length <- sum(!is.na(cod.df$Length))
sd.length <- sd(cod.df$Length, na.rm = T)
se.length <- sd.length/sqrt(n.length)
n.length; mean.length; sd.length; se.length;
alpha <- 0.01
conf.levels <- c(alpha/2, 1-alpha/2)
qt(conf.levels, df = n.length-1)
(fish.conf.int <- mean.length + qt(conf.levels, df = n.length- 1)*se.length)
length.mdl <- lm(Length~ 1, data = cod.df)
confint(length.mdl, level = 0.99)

# Checkpoint 9 
infection.by.area <- table(cod.df$Prevalence, cod.df$Area)
infection.proportions.area <- prop.table(infection.by.area, margin = 2)
chisq.test(infection.by.area)
fisher.test(infection.by.area)
infection.proportions.area

# Checkpoint 10 
n_area <- colSums(infection.by.area)
p_area <- infection.proportions.area["Infected", ]
se_area <- sqrt((p_area * (1 - p_area)) / n_area)
z_score <- qnorm(0.975)
ci_lower <- p_area - z_score * se_area
ci_upper <- p_area + z_score * se_area
prevalence_estimates <- cbind(p_area, ci_lower, ci_upper)
colnames(prevalence_estimates) <- c("Estimated Prevalence", "Lower 95% CI", "Upper 95% CI")
prevalence_estimates

infection.matrix <- as.matrix(infection.proportions.area)
infection.colors <- c("lightblue", "red")
barplot(infection.matrix,
        col = infection.colors,
        legend = rownames(infection.matrix),
        xlab = "Area",
        ylab = "Proportion of Fish",
        main = "Parasite Prevalence Across Areas")
# Area 4 has the highest parasite prevalence around 70%
# Areas 2 and 3 have the lowest parasite prevalence around 32% to 35%
# rea 1 has an medium infection rate around 49%














