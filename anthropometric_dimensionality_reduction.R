# PCA and CCA

library(dplyr)
library(ggplot2)
library(ggbiplot)
library(psych)
library(tidyr)
library(ggbiplot)
library(CCA)

# Import data into R
ansur_male <- read.csv('ANSUR_II_MALE_Public.csv')

# Group 8 variables / pre-processing
ansur_subset <- ansur_male[ , c('footbreadthhorizontal', 'footlength', 'handbreadth', 
                'handcircumference', 'handlength', 'wristcircumference',
                'wristheight', 'anklecircumference')]

head(ansur_subset)
summary(ansur_subset)
sapply(ansur_subset, function(x) sum(is.na(x)))


pca <- prcomp(ansur_subset, scale=TRUE) # PCA Analysis
summary(pca)

# Creating a Scree Plot of explained variance by each component
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
qplot(c(1:8), var_explained) +
  geom_line() +
  xlab('Principal Component') +
  ylab('Variance Explained')

# Creating a biplot of PCA results
ggbiplot(pca, alpha=0.05)

# Conical correlation analysis
cca <- cancor(scale(ansur_subset[,c(1,2,8)]),scale(ansur_subset[,c(4,5,6,7)]))
print(cca)
cca$cor # Correlation high, meaning matrices have strong correlation.
# First pair extremely high correlations at 83%, second 59%, third 24%
# Visualizing the canonical correlations of each dimension
barplot(cca$cor, xlab = "Dimension", ylab="Canonical Correlations", ylim=c(0,1))

# Creating a second model to visualise cross-matrix pairs
cca2 <- cc(ansur_subset[,c(1,2,8)], ansur_subset[,c(4,5,6,7,8)])
plt.cc(cca2, var.label=TRUE) 
# Foot length and Hand length close proximity
# wrist length, hand circumference, wrist circumference, foot breadth in proximity
# ankle circumference and hand breadth extremely close proximity. 

