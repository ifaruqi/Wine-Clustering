
############### install and load packages ###############

# Install and load required packages

# install.packages("factoextra")
# install.packages("cluster")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("psychTools")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("devtools")
#install_github("kbroman/broman")
library(devtools)
library(factoextra)
library(cluster)
library(tidyverse)
library(dplyr)
library(psych)
library(psychTools)
library(readxl)
library(splitstackshape)
library(ROSE)
library(plyr)
library(GPArotation)

############### Load the data ###############
# Load the dataset
df <- read_csv("wine-clustering.csv")

# Describe the statistical summary of the data
describe(df)
summary(df)
str(df)

# Data is clean and in the expected format (numeric)


############### Data Analysis ###############
# Checking Correlation between variables
dfMatrix<-cor(df)
round(dfMatrix, 2)
lowerCor(df)

# Some variables are highly correlated with each other. In Cluster Analysis we do not want multi-collinearity.
# We can try to remove the collinearity with Factor Analysis.


############### Factor Analysis ###############

# The aim of Factor Analysis is to find the best Factor that represent the original variable without cross loading.

# Cross loading: 1 variable is highly correlated with 2 or more PCs. What we want is one variable only highly correlated with one PC. 
# Reason is we cant be sure to allocate this variable to which PC.

# Check the assumption for Factor Analysis with Kaiser-Meyer-Olkin (KMO) test
KMO(df)

# The Kaiser-Meyer-Olkin (KMO) test is a standard to assess the suitability of a data set for factor analysis. 
# We are looking for a KMO value of 0.5 or more. Here it is 0.78, so it is good.

# Perform bartlett test for correlation matrix
cortest.bartlett(df, n=178)

# Bartlett test is other method to test the suitability of data for Factor Analyisis. 
# We have p-value close to 0, it means that it is significant and the data is suitable for factor analysis


############### Maximum Likelihood Extraction ###############

# One Factor Solution with Maximum Likelihood (ML) extraction
fa1<-(fa(df,1, fm="ml"))
print(fa1)
fa.diagram(fa1)

# Two Factors Solution 
fa2<-(fa(df,2, n.obs=178, rotate="none", fm="ml"))

# This command prints the factor loading matrix associated the model, but displaying only the loadings above 0.3, 
# and sorting items by the size of their loadings which is useful for interpretation purpose.
print(fa2, cut=0.3,sort="TRUE")
fa.diagram(fa2)

# Three Factors Solution
fa3<-(fa(df,3, n.obs=178, rotate="none", fm="ml"))
print.psych(fa3, cut=0.3,sort="TRUE")
fa.diagram(fa3)

#=====Factor Analysis with Oblique rotation=====#

# Two Factors Solution with Oblimin Rotation
fa2o<-(fa(df,2, n.obs=178, rotate="oblimin", fm="ml"))
print.psych(fa2o, cut=0.3,sort="TRUE")
fa.diagram(fa2o)

# Three Factors Solution with Oblimin Rotation
fa3o<-(fa(df,3, n.obs=178, rotate="oblimin", fm="ml"))
print.psych(fa3o, cut=0.3,sort="TRUE")
fa.diagram(fa3o)

#=====Factor Analysis with Orthogonal rotation=====#

# Two Factors Solution with Varimax Rotation
fa2v<-(fa(df,2, n.obs=178, rotate="varimax", fm="ml"))
print.psych(fa2v, cut=0.3,sort="TRUE")
fa.diagram(fa2v)

# Three Factors Solution with Varimax Rotation
fa3v<-(fa(df,3, n.obs=178, rotate="varimax", fm="ml"))
print.psych(fa3v, cut=0.3,sort="TRUE")
fa.diagram(fa3v)


# Two Factors Solution with Quartimax Rotation
fa2q<-(fa(df,2, n.obs=178, rotate="quartimax", fm="ml"))
print.psych(fa2v, cut=0.3,sort="TRUE")
fa.diagram(fa2v)

# Three Factors Solution with Quartimax Rotation
fa3q<-(fa(df,3, n.obs=178, rotate="quartimax", fm="ml"))
print.psych(fa3v, cut=0.3,sort="TRUE")
fa.diagram(fa3v)


############### Principal Component Extraction ###############

# Extract the principal component
pcModel<-principal(df, 13, rotate="none")
print(pcModel)

# Standardized loadings (pattern matrix) is the correlation matrix between the principal components and the original variables.
# SS loadings are the sum of square loadings of the principal components, which are the variances of the principal components, 
# and they sum up to 13 (the number of all principal components.)

# This command prints the factor loading matrix associated the model, but displaying only the loadings above 0.3 and sorting items by the size of their loadings.
print.psych(pcModel, cut=0.3, sort=TRUE)

# Values are the eigen values of all components, which are printed against the components to produce the scree plot.
plot(pcModel$values, type="b")

# Try 2 and 3 clusters

# 2  based on scree plot, the elbow
# 3  based on 60% cumulative variance and SS loading > 1

# Try two PCs solution
pcModel2<-principal(df, 2, rotate="none")
print(pcModel2)

# Three PCs solution
pcModel3<-principal(df, 3, rotate="none")
print(pcModel3)

#=====Principal Component with Oblique rotation=====#

# Two factors solution
pcModel2o<-principal(df, 2, rotate="Oblimin")
print.psych(pcModel2o, cut=0.4, sort=TRUE)

# Three factors solution
pcModel3o<-principal(df, 3, rotate="Oblimin")
print.psych(pcModel3o, cut=0.4, sort=TRUE)

#=====Principal Component with Orthogonal rotation=====#

# Two factors solution
pcModel2q<-principal(df, 2, rotate="quartimax")
print.psych(pcModel2q, cut=0.4, sort=TRUE)

# Three factors solution
pcModel3q<-principal(df, 3, rotate="quartimax")
print.psych(pcModel3q, cut=0.4, sort=TRUE)

# Two factors solution
pcModel2v<-principal(df, 2, rotate="varimax")
print.psych(pcModel2v, cut=0.4, sort=TRUE)

# Three factors solution
pcModel3v<-principal(df, 3, rotate="varimax")
print.psych(pcModel3v, cut=0.4, sort=TRUE)

# From all of those method, the best is 3 PC extraction with Varimax rotation.

# Decide best method
faModel<-principal(df, 3, rotate="varimax", scores=TRUE)
fscores <- faModel$scores


############### Cluster Analysis ###############

# check assumptions to see whether the data are suitable for Cluster Analysis:
FscoresMatrix<-cor(fscores)
round(FscoresMatrix, 2)
lowerCor(fscores)

# No correlation between variable. Its good.

# Calculate Mahalanobis distance to identify potential outliers for multivariate variables.
Maha <- mahalanobis(fscores,colMeans(fscores),cov(fscores))
print(Maha)

# Check the significancy of the distance
# df = total variables - 1
MahaPvalue <-pchisq(Maha,df=2,lower.tail = FALSE)
print(MahaPvalue)

# Here we use critical value of 0.01
print(sum(MahaPvalue<0.01))

# There are four observation considered as outliers
# Remove the outliers

# Combine Factor Analysis Sample data with Mahalanobis value
fasaMaha<-as.data.frame(cbind(fscores, Maha, MahaPvalue))

# filter
fasaMaha <- fasaMaha %>% filter(MahaPvalue > 0.01)

# Drop column Maha and MahaPValue
fasaMaha[c("Maha", "MahaPvalue")] <- NULL


# Define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute agglomerative coefficient
ac <- function(x) {
  agnes(fasaMaha, method = x)$ac
}

# Calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

# Ward is the best linkage method

# calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(fasaMaha, FUN = hcut, nstart = 1, K.max = 10, B = 500)
fviz_gap_stat(gap_stat)

# We have three optimal clusters

# Finding distance matrix
distance_mat <- dist(fasaMaha, method = 'euclidean')


# Fitting Hierarchical clustering Model to dataset
set.seed(10)
Hierar_cl <- hclust(distance_mat, method = "ward") %>% as.dendrogram()
Hierar_cl

# Plot the dendogram
# install.packages('dendextend')
library(dendextend)

Hierar_cl %>% 
  set("branches_k_color", 
      value = c("red",  "green", "blue"), k = 3) %>% 
  plot(main = "Customized colors")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )
fit

# Find number of observations in each cluster
table(fit)

# Append cluster labels to original data
final_data <-cbind(fasaMaha, cluster = fit)

# Display first six rows of final data
head(final_data)

# Find mean values for each cluster
hcentres<-aggregate(x=final_data, by=list(cluster=fit), FUN="mean")
print(hcentres)



# Kmeans clustering
set.seed(10)
k_cl <- kmeans(fasaMaha,3,nstart=1)
k_cl

# Plot K-means clustering
fviz_cluster(k_cl, data = fasaMaha,
             palette = c("red",  "green", "blue"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


# Plot 3D to understand better the cluster
library(plotly)
library(dplyr)

p <- plot_ly(final_data, x=~RC1, y=~RC2, 
             z=~RC3, color=~cluster) %>%
  add_markers(size=1.5)
print(p)
