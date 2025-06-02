rm(list = ls())

# Let's cook up the example of the class sizes and the differences between the
# class unit of measurement and the student unit of measurement.

set.seed(1) # This here just ensures the random samples are the same. 

# Class sizes are usually higher heterogeneous, and also very skewed. In normal
# language, that means most classes are small and few classes are very large. 
# Let's make the average class size of our hypotheical University the same 
# as Wake's average class size, which is right around 19 students. Wake has 
# about 5500 undergraduates that each take about 5 classes per semester, so 
# that is 5500*5 = 27,500 seats. With 19 students per class, that makes about 
# 1450 classes per semester.

# Moreover, Wake boasts that 99% of it's classes have fewer than 50 students. 
# Let's see if we can create a fake university with these characteristics. It 
# turns out that a gamma distribution works really well for this purpose. You've
# likely never come across a gamma distribution before. We'll emphasize this 
# much more later in class and you will learn all about it. 

# Define properties of class size distribution
AveClassSize <- 19
SkewClassSize <- 1.08

# Define gamma distribution parameters. 
alpha <- 4/SkewClassSize^2
theta <- AveClassSize/alpha

# Now let's create our university and take a look at its properties
ClassSize <- round(rgamma(1450, alpha, 1/theta))
summary(ClassSize)
hist(ClassSize)

# Great. Many Small Classes (most are 17 or below) and there are only a few 
# large classes (75% have fewer than 25 students). This has the smae general 
# characteristics as WFU.

# Now let's figure out how many classmates each student has. For the purposes of
# this exercise, let's ignore the fact that students take multiple classes. This
# is the perfect place to make an assumption. It simplifies the analysis, but 
# has no effect on the fundamental point: Sampling students is NOT the same as 
# sampling CLASSES.

# The way to do this is simple. Let's start with a class with 10 students. We 
# need to make a vector with 10 students and each of them has 10 classmates 
# (including themselves). Turns out R can do this very simply. 

Classmates <- c()
for (i in 1:length(ClassSize)){
  Classmates <- c(Classmates,rep(ClassSize[i], ClassSize[i]))
}

# Now let's look at the distribution of classmates
summary(Classmates)
hist(Classmates)
# This shows that half of students in the university sit in a class with 23 
# students or more! The average student sits in a class with almost 25 students,
# which is 6 more than the class based perspective. That doesn't sound like 
# much of a difference, but that is 30% more than the brochure says. And what 
# about this 50 seat marker? WFU says 99% of classes have fewer than 50 
# students. What proportion of students are in classes with 50 or fewer
# classmates?
sum(Classmates <= 50)/length(Classmates)

# Turns out this is 97%. Turns out that isn't too different. 


# Where this really turns into trouble is when you get to Universities with 
# massive class sizes. I went to Florida State University as an undergrad and 
# rarely has classes with fewer than 100 students in it. My intro BIO class had
# 400 and was housed in a University auditorium! We can recreate such a 
# university as well. Lets' do the same thing assuming the average class size is
# in the neighborhood of 50 and that there are 45,000 students rather than 5500.

# Define properties of class size distribution
AveClassSize <- 50
SkewClassSize <- 3
Students <- 45000
Seats <- Students*5

# Define gamma distribution parameters. 
alpha <- 4/SkewClassSize^2
theta <- AveClassSize/alpha

# Now let's create our university and take a look at its properties
ClassSize <- round(rgamma(Seats/AveClassSize, alpha, 1/theta))
ClassSize <- ClassSize[ClassSize > 0]
summary(ClassSize)
hist(ClassSize)
quantile(ClassSize, 0.99)

# This university has some mega classes (the largest has 854 students). The 
# average class size is a respectable 53. And it can claim that 99% of classes
# have fewer than 353 students. Now let's figure out how many classmates each 
# student has. 
Classmates <- c()
for (i in 1:length(ClassSize)){
  Classmates <- c(Classmates,rep(ClassSize[i], ClassSize[i]))
}
summary(Classmates)
hist(Classmates)
quantile(Classmates, 0.99)
# The classmate picture is very different. The average student sits in a class 
# of 161 students, which is over 3 times (or 200% more) than the class-based 
# perspective. Moreover, the university can now only claim that 99% of students
# sit in a class of fewer than 673 students. Hard to get individual attention
# in that class!








# Here is a visual exmple of this phenomenon.
if (!"ggplot2" %in% installed.packages()){
  install.package('ggplot2')
}
library(ggplot2)

# Example data
# Let's create a sample matrix
ClassEx <- c(2,45,2,4,4,7,5,28,3)
ClassEx <- ClassEx[order(ClassEx)]

# Convert matrix to dataframe
data_df <- as.data.frame(as.table(matrix(ClassEx, nrow = 3)))

heatmap_plot <- ggplot(data_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") + # Create heatmap
  scale_fill_gradient(low = "white", high = "steelblue") + # Set color gradient
  theme_minimal() + # Set theme
  labs(x = "", y = "", title = "Classes") + # Labels
  geom_text(aes(label = Freq), color = "black", size = 10)
print(heatmap_plot)


# Create student sampling view
MatesEx <- c()
for (i in 1:length(ClassEx)){
  MatesEx <- c(MatesEx, rep(ClassEx[i], ClassEx[i]))
}
# Reorder from smallest to largest
MatesEx <- MatesEx[order(MatesEx)]
# Convert to data frame for plotting
data_df <- as.data.frame(as.table(matrix(MatesEx,nrow = 10)))

# Make plot
heatmap_plot <- ggplot(data_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") + # Create heatmap
  scale_fill_gradient(low = "white", high = "steelblue") + # Set color gradient
  theme_minimal() + # Set theme
  labs(x = "", y = "", title = "Classmates") + # Labels
  geom_text(aes(label = Freq), color = "black", size = 10)
print(heatmap_plot)










### Spatial Ecology Example ###
# We also talked about how this kind of sampling matters for spatial ecology. 
# Let's get a visual example and do some random sampling. First, let's create
# a landscape. We will use the "spatstat' package. 

if (!"spatstat" %in% installed.packages()){
  install.package('spatstat')
}
library(spatstat)
GridMax <- 8
# Define the study area (window)
win <- owin(GridMax*c(0, 1), GridMax*c(0, 1)) 

# Define a non-homogeneous intensity function (e.g., a quadratic function)
lambda <- function(x, y) { 
  return( 0.5 * x^3 + 1.5 * y^3 ) 
}

# Generate a point pattern using the intensity function
PointPattern <- rpoispp(lambda, win = win) 

# Plot the generated point pattern
plot(PointPattern, xlab = 'Longitude', ylab = 'Latitude', 
     main = 'Spatial Distribution of Trees', cex = 0.5, pch = 19)
abline(v = c(1,GridMax-1), lty = 2, col = 'black')
abline(h = c(1,GridMax-1), lty = 2, col = 'black')

IsInRegion <- PointPattern$x >= 1 & PointPattern$x <= GridMax-1 &
              PointPattern$y >= 1 & PointPattern$y <= GridMax-1
RegionTrees <- PointPattern[IsInRegion]
set.seed(1)
samples <- 100
TreeSample <- sample(1:RegionTrees$n, samples, replace = T)
FocalTrees <- RegionTrees[TreeSample]
points(FocalTrees$x, FocalTrees$y, col = 'red', pch = 19)
Neighbors <- rep(NA, FocalTrees$n)

l2norm <- function(x1,y1,x2,y2){
  d <- sqrt((x1-x2)^2 + (y1-y2)^2)
  return(d)
}

for (i in 1:FocalTrees$n){
  distance <- l2norm(FocalTrees$x[i],FocalTrees$y[i],
                     PointPattern$x,PointPattern$y)
  Neighbors[i] = sum(distance < 1)
}

# Now let's randomly select places within the same sample area
set.seed(1)
SampleX <- runif(100)*(GridMax - 2) + 1
SampleY <- runif(100)*(GridMax - 2) + 1
points(SampleX,SampleY, pch = 19, col = 'blue', cex = 1)

Density <- rep(NA, length(SampleX))

for (i in 1:length(SampleX)){
  distance <- l2norm(SampleX[i], SampleY[i],
                     PointPattern$x,PointPattern$y)
  Density[i] = sum(distance < 1)
}
hist(Density, col = 'blue', main = 'Sample of Tree Density')
hist(Neighbors, col = 'red', main = 'Sample of Tree Density')
mean(Density)
mean(Neighbors)
