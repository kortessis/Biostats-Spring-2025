rm(list = ls())

setwd('/Users/nicholaskortessis/Library/CloudStorage/GoogleDrive-kortessn@wfu.edu/.shortcut-targets-by-id/1pChKwu6ONXS5a12mjmSTYEPyL5pbN6eM/R_COURSE_2024/W07')

metab.df <- read.csv(file = 'data/Body_Size_Observations.csv')
unique(metab.df$class)

metab.df$class <- factor(metab.df$class, 
                            levels = c('Reptilia', 'Mammalia', 'Aves'))

# Let's look just at birds, mammals, and reptiles
metab.subset.df <- subset(metab.df, 
          subset = class == "Reptilia")

# Let's add a colormap, the viridis colormap, to distinguish the different
# classes of organisms. For now, we have three. I like to get colors in the 
# middle of the color spectrum. To do that, I get a few more colors than needed
# (here, seven) and then grab three that are in the middle.
library(viridisLite)
cols <-  viridis(7, alpha = 0.95)
cols <- cols[c(2,4,6)]

par(mfcol = c(1,2))
plot(metab.subset.df$body.mass, metab.subset.df$metabolic.rate, 
     col = cols[2], 
     pch = 19,
     xlab = 'Body Mas', ylab = 'Metabolic Rate',
     main = 'Metabolic scaling of Reptiles',
     cex.axis = 1.2,
     cex.main = 1.3,
     cex.lab = 1.5)

lin.mdl <- lm(metabolic.rate ~ body.mass, data = metab.subset.df)
abline(lin.mdl$coefficients, col = cols[2])

# Let's make an empty plot and fill in the title, labels, font sizes, and 
# figure bounds. The code 'n' tells R to plot "nothing". 
plot(log10(metab.subset.df$body.mass), log10(metab.subset.df$metabolic.rate), 
     col = cols[2], 
     pch = 19,
     xlab = 'Body Mass (log10 scale)', ylab = 'Metabolic Rate (log10 scale)',
     main = 'Power Law Metabolic Scaling',
     cex.axis = 1.2,
     cex.main = 1.3,
     cex.lab = 1.5)
pwr.mdl <- lm(log10(metabolic.rate) ~ log10(body.mass), data = metab.subset.df)
abline(pwr.mdl$coefficients, col = cols[2])
# We can also add gridlines to the plots
abline(v = 0, lwd = 1, lty = 2)
abline(h = seq(from = -9, to = 9, by = 1), lty = 3, col = 'gray')
abline(v = seq(from = -9, to = 9, by = 1), lty = 3, col = 'gray')



### Exponential growth
setwd("/Users/nicholaskortessis/Library/CloudStorage/GoogleDrive-kortessn@wfu.edu/My Drive/Import/Wake Forest/Teaching/24-25/2025 - Spring/BIO 380/Week 12 - Regression")

growth.df <- read.csv(file = 'KHKgrowthcurves_LB.csv')
indx <- growth.df$time..h. <= 7 & growth.df$time..h. >=1.5
plot(growth.df$time..h.[indx], log(growth.df$No.36.1[indx]),
     pch = 21, bg = 'gray', xlab = 'Time (hr)',
     ylab = 'Bacterial Density', typ = 'b')

mdl <- lm(log(No.36.1) ~ time..h., data = growth.df, 
          subset = (time..h. <= 7 & time..h. >= 1.5))
cis <- predict.lm(mdl, interval = 'confidence')
lines(growth.df$time..h.[indx], cis[,'fit'], lty = 2, 
      col = 'blue')
lines(growth.df$time..h.[indx], cis[,'lwr'], lty = 2, 
      col = 'blue')
lines(growth.df$time..h.[indx], cis[,'upr'], lty = 2, 
      col = 'blue')
