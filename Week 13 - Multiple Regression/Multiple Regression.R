## Multiple regression

library(palmerpenguins)

penguins.mdl <- lm(bill_depth_mm ~ bill_length_mm*species, data = penguins)
summary(penguins.mdl)

library(car)
Anova(penguins.mdl, type = 3)

# Remove interaction
new.penguins.mdl <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)
Anova(new.penguins.mdl, type = 2)
summary(new.penguins.mdl)

par(mfcol = c(1,1))
plot(penguins$bill_length_mm, penguins$bill_depth_mm,
     xlab = 'Bill Length (mm)', ylab = 'Bill Depth (mm)', 
     main = 'Penguins Data Set')

new.data.n <- 200
specs <- unique(penguins$species)
specs <-specs[order(specs)]
cols <- c('orange', 'purple', 'darkgreen')

legend('bottomleft', bty = 'n', legend = specs, 
       col = cols, lwd = 3, lty = 1)

for (i in 1:3){
  spec.data <- subset(penguins, subset = species == specs[i])
  new.data <- data.frame(species = rep(specs[i], each = new.data.n),
                         bill_length_mm = seq(from = min(spec.data$bill_length_mm, na.rm = T), to = max(spec.data$bill_length_mm, na.rm = T), length = new.data.n))
  spec.ci <- data.frame(predict(new.penguins.mdl, new.data, interval = 'confidence'))
  
  poly.col <- col2rgb(cols[i])
  lines(new.data$bill_length_mm, spec.ci$fit, lwd = 3, col = cols[i])
  polygon(c(new.data$bill_length_mm, rev(new.data$bill_length_mm)), 
          c(spec.ci$upr, rev(spec.ci$lwr)), 
          col = rgb(red = t(poly.col/255), alpha = 0.35), border = NA)
  points(spec.data$bill_length_mm, spec.data$bill_depth_mm,
         pch = 21, bg = rgb(red = t(poly.col/255), alpha = 0.85),
         col = NA)
}

par(mfcol = c(2,2))
plot(new.penguins.mdl)




par(mfcol = c(1,1))
plot(penguins$body_mass_g, penguins$flipper_length_mm,
     typ = 'n', xlab = 'Body Mass (g)', ylab = 'Flipper Length (mm)')
legend('topleft', bty = 'n', legend = specs, 
       col = cols, lwd = 3, lty = 1)
#abline(v = seq(from = 2000, to = 7000, by = 250), lty = 2, col = 'gray')
#abline(h = seq(from = 170, to = 250, by = 5), lty = 2, col = 'gray')

body.mass.mdl <- lm(flipper_length_mm ~ body_mass_g*species, data = penguins)

Anova(body.mass.mdl, type = 3)
summary(body.mass.mdl)

for (i in 1:3){
  spec.data <- subset(penguins, subset = species == specs[i])
  new.data <- data.frame(species = rep(specs[i], each = new.data.n),
                         body_mass_g = seq(from = min(spec.data$body_mass_g, na.rm = T), to = max(spec.data$body_mass_g, na.rm = T), length = new.data.n))
  
  
  spec.ci <- data.frame(predict(body.mass.mdl, new.data, interval = 'confidence'))
  poly.col <- col2rgb(cols[i])
  #polygon(c(new.data$body_mass_g, rev(new.data$body_mass_g)), 
  #        c(spec.ci$upr, rev(spec.ci$lwr)), 
  #        col = rgb(red = t(poly.col/255), alpha = 0.35), border = NA)
  #lines(new.data$body_mass_g, spec.ci$fit, lwd = 3, col = cols[i])
  points(spec.data$body_mass_g, spec.data$flipper_length_mm,
         pch = 21, bg = rgb(red = t(poly.col/255), alpha = 0.85),
         col = NA)
}



par(mfcol = c(2,2))
plot(body.mass.mdl)




#### Issues with multiple regression

# Multicollinearity
n <- 150
set.seed(5)
smoking <- rbinom(n, 1, prob = 0.5)
blood.pressure <- rep(NA,n)
n.smokes <- sum(smoking == 1)
blood.pressure[smoking == 1] <- rnorm(n.smokes, 144, 7)
blood.pressure[smoking != 1] <- rnorm(n-n.smokes, 119, 7)

beta0 <- -5
betaS <- 2
betaBP <- 0.03
X.heart.attack <- beta0 + betaS*smoking + betaBP*blood.pressure
p.heart.attack <- exp(X.heart.attack)/(1+exp(X.heart.attack))
heart.attack <- rbinom(n,1,p.heart.attack)

ha.df <- data.frame(smokes = smoking, bp = blood.pressure, heart.attack = heart.attack)

# logistic regression
# 
X <-  seq(from = -3, to = 3, length = 100)
beta0 <- 0

par(mfrow=c(2,2))
beta1 <- 2
logit.p <- beta0 + beta1*X
p <- exp(logit.p)/(1+exp(logit.p))
plot(X, p, 
     xlab = 'Predictor, X', ylab = 'Probability of "Success"',
     main = 'Logistic Regression', type = 'l', lwd = 2)
plot(X, logit.p, 
     xlab = 'Predictor, X', ylab = 'logit(Probability of "Success")',
     main = 'Logistic Regression', type = 'l', lwd = 2)

beta1 <- -2
logit.p <- beta0 + beta1*X
p <- exp(logit.p)/(1+exp(logit.p))
plot(X, p, 
     xlab = 'Predictor, X', ylab = 'Probability of "Success"',
     main = 'Logistic Regression', type = 'l', lwd = 2)
plot(X, logit.p, 
     xlab = 'Predictor, X', ylab = 'logit(Probability of "Success")',
     main = 'Logistic Regression', type = 'l', lwd = 2)

 
par(mfcol = c(1,1))
plot(ha.df$bp, ha.df$heart.attack + 0.05*(runif(n)-0.5), 
     xlab = 'Systolic Blood Pressure',
     ylab = 'Heart Attack', yaxt = 'n', 
     main = 'Logistic Regression',
     pch = 21, bg = 'gray')
axis(2, at = c(0,1), labels = c('No', "Yes"))
log.glm <- glm(heart.attack~bp, family = 'binomial', data = ha.df)
new.bp <- seq(from = 100, to = 170, length = 200)
new.data <- data.frame(bp = new.bp)
pred2 <- add_ci(new.data, log.glm)
pred <- predict.glm(log.glm, new.data, type = 'link', se.fit = T)
lines(new.bp, exp(pred$fit)/(1+exp(pred$fit)), lwd = 2)
lines(new.bp, pred2$LCB0.025, lty = 2)
lines(new.bp, pred2$UCB0.975, lty = 2)
summary(log.glm)

# Colinear Responses
glm <- glm(heart.attack ~ smokes + bp, data = ha.df, family = 'binomial')
summary(glm)
plot(ha.df$bp[smoking == 1], 
     ha.df$heart.attack[smoking == 1] +  0.05*(runif(n.smokes)-0.5), 
     pch = 21, bg = 'turquoise', 
     xlab = 'Systolic Blood Pressure', 
     ylab = 'Heart Attack',
     yaxt = 'n', xlim = c(100,165))
axis(2, at = c(0,1), labels = c('No', 'Yes'))
points(ha.df$bp[smoking != 1], 
       ha.df$heart.attack[smoking != 1] +  0.05*(runif(n-n.smokes)-0.5), 
       pch = 21, bg = 'royalblue')

new.bp <- rep(seq(from = 100, to = 170, length = 200),2)
new.smoking <- rep(c(0,1), each = 200)
new.data <- data.frame(bp = new.bp, smokes = new.smoking)
pred.link <- predict.glm(glm, new.data, type = 'link')
pred <- data.frame(new.data, pred.link)
lines(pred$bp[pred$smokes == 1], exp(pred$pred.link[pred$smokes == 1])/(1 + exp(pred$pred.link[pred$smokes == 1])), col = 'turquoise', lwd = 2)
lines(pred$bp[pred$smokes != 1], exp(pred$pred.link[pred$smokes != 1])/(1 + exp(pred$pred.link[pred$smokes != 1])), col = 'royalblue', lwd = 2)

new.bp <- seq(from = 100, to = 170, length = 200)
smoke.true.p <- beta0 + betaS + betaBP*new.bp
nosmoke.true.p <- beta0 + betaBP*new.bp
lines(new.bp, exp(smoke.true.p)/(1 + exp(smoke.true.p)), col = 'turquoise', lwd = 2, lty = 2)
lines(new.bp, exp(nosmoke.true.p)/(1 + exp(nosmoke.true.p)), col = 'royalblue', lwd = 2, lty = 2)
