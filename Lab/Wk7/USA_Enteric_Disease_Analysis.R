#Clear workspace
rm(list = ls())

# Set working directory
setwd('/Users/nicholaskortessis/Library/CloudStorage/GoogleDrive-kortessn@wfu.edu/My Drive/Import/Wake Forest/Teaching/24-25/2025 - Spring/BIO 380/Lab/Wk7')

# Load data
data <- read.csv(file = 'NORS_20250128.csv')
str(data) # Take a summary look

# Make a list of the states 
states <- unique(data$State)
o <- order(states); states <- states[o]

# Number of bootstrap replicates
boot.n <- 100000
# Alpha level for confidence interval
alpha <- 0.05

# Create vecotrs to store the outbreak size, the state name, and the lower and
# upper values of the confidence intervals
median.outbreak <- rep(NA, length(states))
state.name <- rep(NA, length(states))
lwr.CI <- rep(NA, length(states))
upp.CI <- rep(NA, length(states))

# Put them together in a data frame and add column names
med.CI <- data.frame(state.name, median.outbreak, lwr.CI, upp.CI)
colnames(med.CI) <- c("State", "Med.Outbreak", "Lwr.CI", "Upp.CI")


# Now estimate median outbreak size for each state
for (i in 1:length(states)){
  
  # Make a data frame for the given state
  state.df <- subset(data, subset = (State == states[i]))
  # Record state name
  med.CI$State[i] <- unique(state.df$State)
  
  # Calculate median
  med.CI$Med.Outbreak[i] <- median(state.df$Illnesses)
  

  # Now bootstrap the median for the state
  # We will need the sample size to do the resampling
  n.state <- length(state.df$Illnesses)
  
  # Make a vector to hold the number of bootstrap estimates
  med.boot <- rep(NA, boot.n)

  # Now loop over different bootstrap replicates
  for (r in 1:boot.n){
    # Resample
    boot.sam <- sample(state.df$Illnesses, n.state, replace = T)
    # Store median from bootstrap sample
    med.boot[r] <- median(boot.sam)
  }
  
  # Record CI
  med.CI$Lwr.CI[i] <- quantile(med.boot, alpha/2)
  med.CI$Upp.CI[i] <- quantile(med.boot, 1-alpha/2)
  
  # Update on status of calculations
  print(unique(state.df$State))
  print(paste(round(i/length(states),2)*100, ' % complete'))
}

# Reorder data.frame by outbreak severity. 
med.CI <- med.CI[order(med.CI$Med.Outbreak),]





##### NOW PLOT RESULTS #####

# Load my preferred color map
library(viridisLite)
# Pick a bunch of evenly spaced colors
cols <- viridis(1000, begin = 0, end = 0.9)
# Find the color that maps to median outbreak size. 
# The Marshal Islands  is so far above everyone else it makes all the other 
# colors more or less the same. We will exclude it from the color coding for 
# now. 

cols.indx <- round(med.CI$Med.Outbreak/med.CI$Med.Outbreak[length(states)-1]*max(length(cols)))
med.CI$Color <- cols[cols.indx]

# Now make outlier terriroty close to the end of the color spectrum. 
med.CI$Color[med.CI$Med.Outbreak == max(med.CI$Med.Outbreak)] <- viridis(1, begin = 0.95, end = 0.95)


# Start to save the figure
pdf(file = 'EntericDiseaseSeverity.pdf', height = 13, width = 12)

# Make a figure with enought margin space to hold all the names
par(mfcol = c(1,1), mar = c(5,13,4,1))
plot(0,0, typ = 'n',
     yaxt = 'n', 
     ylab = '', xlab = 'Median Outbreak Size',
     xlim = c(0,110), ylim = c(1,length(states)),
     main = 'Enteric Epidemic Severity in USA')

# Add vertical lines to help see the actual median outbreak sizes. 
abline(v = seq(from = 5, to = 130, by = 10), lty = 3, col = 'gray')
abline(v = seq(from = 0, to = 130, by = 10), lty = 3, lwd = 1, col = 'black')
# Add horizontal lines to help identify states
abline(h = 1:length(states), lty = 3, col = 'gray')

# Plot each median estimate as a point and add labels.
points(med.CI$Med.Outbreak, 1:length(states), 
    pch = 19, cex = 1.2, col = med.CI$Color)

# Add in y-axis names for each state
axis(2, at = 1:length(states), labels = med.CI$State, las = 1)

# Now add confidence interval bars for each estimate. Do this by state
for (i in 1:length(states)){
  lines(c(med.CI$Lwr.CI[i],med.CI$Upp.CI[i]), rep(i,2), 
        lwd = 2, col = med.CI$Color[i])
}

dev.off()
# Finish saving