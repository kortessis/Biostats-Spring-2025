rm(list = ls())

t <- seq(from = -5, to = 5, length = 1000)

n <- c(2,3,4,5,10,20,50,10000)
df <- n - 1
library(viridisLite)
cols <- viridis(length(n), direction = -1)
plot(t,t, typ = 'n', xlab = 't-Statistic', ylab = 'Probability Density',
     main = 't-Distribution', ylim = c(0,1/sqrt(2*pi)), cex.lab = 1.3,
     cex.main = 1.3)
for (i in 1:length(df)){
  pdf.t <- dt(t, df[i])
  lines(t, pdf.t, col = cols[i], lwd = 2)
}
ldg.names <- paste('Sample size = ', n, sep = '')
legend('topleft', legend = ldg.names, col = cols, lwd = 2,
       bty = 'n')


chi <- seq(from = 0, to = 20, length = 1000)
pdf.chi <- dchisq(chi, df = 5)

plot(chi,pdf.chi, xlab = '', ylab = 'Probability Density', 
     main = 'Chi-Squared Distribution',
     lwd = 3, typ = 'l')
