# Clustering - collaborative and content based

movies = read.table("movielens.txt", header = F, sep = "|", quote = "\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown",
                    "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime",
                    "Documentary", "Drama","Fantasy", "filmnoir", "Horror", "Musical",
"Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

# Remove ID, IMDB and Video and release date variables from the data frame
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
# Remove duplicate entries
movies = unique(movies)

# Use hierarchical clustering to create clusters within the movies data frame on every colum other than the Title i.e. [2,20]
# First, calculate the distances between points using the euclidean method
distances = dist(movies[2:20], method = "euclidean")

# Second, create cluster by using the ward.D method which takes the euclidean method as well as the variance of the distances
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies) # Plots the dendrogram of the clustering 

# Third, from inspection of the dendrogram determine how many groups of clusters you want to use
clusterGroups = cutree(clusterMovies, k = 10) # k represents the number of cluster groups that will be 'cut' from the hclust output
# This sorts each movie based on its genre into one of 10 cluster groups

# Determine the percentage of moives in each of the 10 clusters that belong to each genre (independent variable)
tapply(movies$Action, clusterGroups, mean)
# Result show that 0.78 (78%) of the movies have the genre 'Action' in the 10 cluster groups

# E.g. Find the cluster group that the movie 'Men in Black' falls under
subset(movies, Title == "Men in Black (1997)") # Returns row 257
# Find the cluster group that corresponds to row 257 i.e. the movie Men in Black
clusterGroups[257] # Returns the cluster group 2. 
# Create subset of cluster group 2
clusterGroup2 = subset(movies, clusterGroups == 2)
clusterGroup2$Title[1:10] # Returns first 10 movies of this cluster group

# Rather than using a tapply for each movie genre (18 in total), a more advanced approach 
# for finding the mean occurence of movies in each genre. 
spl = split(movies[2:20], clusterGroups) # splits the data into subsets based on the clusters
lapply(spl, colMeans) #lapply function runs the second argument (colMeans) on each element of the first argument (each cluster subset in spl)

************************************
# Segmenting images to create data based on grayscale that is clustered from scale of 0 to 1
  
flower = read.csv("flower.csv", header = F)
# Convert data frame to matrix form first.
flowerMatrix = as.matrix(flower) # Results show a 50 x 50 resolution image i.e. 50 rows and 50 columns
# Then convert to vector form
flowerVector = as.vector(flowerMatrix) 
# Results in 2500 terms, as expected of a 50 x 50 matrix. 

# Begin clustering process, by first caculating the distance matrix using Euclidean method
distances = dist(flowerVector, method = 'euclidean')
# Apply hierarchical clustering method on the distance matrix
cluster = hclust(distances, method = 'ward.D')
# Plot cluster dendrogram
plot(cluster)
# From inspection, there appears to be an acceptable total of 3 clusters that can be used
# Use the cutree function to cut the dendrogram tree into 3 cluster groups
flowerCluster = cutree(cluster, k = 3)

# Find the mean intensity of each of our 3 clusters by grouping the values in the flowerVector by the flowerCluster
tapply(flowerVector, flowerCluster, mean)
# Results show that 0.93 of the values in the flowerVector fall under the 3rd cluster group (or the one with lighter shading i.e. 1)

# Plot picture by first converting flower cluster (a vector) into a matrix and then using combine function to build a 50 by 50 matrix
dim(flowerCluster) = c(50,50)
image(flowerCluster, axes = F) # image function takes in matrix as first argument
# Result shows that the darker bacgkround color is cluster 1, inner part of flower as cluster 2 and lighter shade of flower petals as cluster 3
# Compare with original image in grayscale
image(flowerMatrix, axes = F, col = grey(seq(0,1,length = 256))) # grayscale is produced by creating sequence between 0 (black) and 1 (white) over 256 points

*********************
# MRI image analysis
healthy = read.csv("healthy.csv", header = F) # Image is wider than it is long

# 1. Convert data frame to matrix
healthyMatrix = as.matrix(healthy) # 566 by 646 matrix
image(healthyMatrix, axes = F, col = grey(seq(0,1,length = 256)))
# 2. Convert matrix to vector
healthyVector = as.vector(healthyMatrix) # Vector has total of 365,636 elements!

# Begin hierarchical clustering
# 1. Compute the distance matrix
# distance = dist(healthyVector, method = 'euclidean')
# Error, vector size too large for R to compute. Need to find alternative clustering method e.g. k-means clustering

# K-means clustering
# 1. specify number of clusters, k equal to 5 (set based on specific user case)
k = 5
# 2. randomly assign individual points to clusters 
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
# Each value in the healthy vector will be assigned to a cluster between 1 and 5.
# centers shows the mean intensity value for each of the 5 clusters e.g. cluster 1 has a mean intensity value of 0.4818 (on the darker side)
# size shows the number of values in each cluster, which in this case is cluster 3 with total of 133162 values (with mean intensity of 0.1062 i.e. fairly dark)
healthyClusters = KMC$cluster

# 3. Convert vector to matrix and print image with each cluster assigned a color
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = F, col = rainbow(k))

# Now we can compare healthy image with tumour image, using the former as the training set and later as testing
tumour = read.csv("tumor.csv", header = F)
tumourMatrix = as.matrix(tumour)
tumourVector = as.vector(tumourMatrix)
install.packages("flexclust")
library("flexclust") # contains object class KCCA (k-centroid cluster anaaysis), which is used to convert cluster matrix for readiness to make predictions from

# Perform KCCA using healthyVector as testing set and previously defined KMC matrix
KMC.kcca = as.kcca(KMC, healthyVector)

# Create tumour clusters vector and make predictions using the tumour test image
tumourClusters = predict(KMC.kcca, newdata = tumourVector)

# Convert tumour vector to matrix in order to produce image
dim(tumourClusters) = c(nrow(tumourMatrix), ncol(tumourMatrix))
image(tumourClusters, axes = F, col = rainbow(k))

**********************************
# Document clustering using kierarchical and k-means alogrithms on a blog site
kos = read.csv("dailykos.csv") # 1545 words in 3430 blog entries

# First calculate distance matrix and build a hclust model
distances = dist(kos, method = 'euclidean')
clusters = hclust(distances, method = 'ward.D')
plot(clusters)
# From inspecting the dendrogram, a good cluster choice would be 2 or 3 groups. 
# However, 7 or 8 would be more usable given the nature of the observations e.g. blog or news entries
# Cut cluster into 7 groups
clusterGroups = cutree(clusters, k = 7)

# Create 7 new datasets that subset the main observations from each cluster
clusterGroups1 = subset(kos, clusterGroups == 1)
clusterGroups2 = subset(kos, clusterGroups == 2)
clusterGroups3 = subset(kos, clusterGroups == 3)
clusterGroups4 = subset(kos, clusterGroups == 4)
clusterGroups5 = subset(kos, clusterGroups == 5)
clusterGroups6 = subset(kos, clusterGroups == 6)
clusterGroups7 = subset(kos, clusterGroups == 7)
table(clusterGroups) # produces same result summarised for all clusters
# Cluster 1 has most observations, Cluster 4 the least.

# instead of calculating the average value of each variable individually, just explore the top 6 words in each cluster
tail(sort(colMeans(clusterGroups1)))
# This computes the mean frequency values of each of the words in cluster 1, 
#and then outputs the 6 words that occur the most frequently. 
#The colMeans function computes the column (word) means, the sort function orders 
#the words in increasing order of the mean values, and the tail function outputs the last 6 words listed, which are the ones with the largest column means
# Most frequency word in this cluster is 'bush' relating to George.W. Bush
tail(sort(colMeans(clusterGroups2)))
# November is most frequent word in cluster 2
# Iraw war related words more frequent in cluster 5, democrat related words in cluster 7

# Run k-means clustering
set.seed(1000)
KMC = kmeans(kos, centers = 7)
# subset data in 7 clusers
KMC1 = subset(kos, KMC$cluster == 1)
KMC2 = subset(kos, KMC$cluster == 2)
KMC3 = subset(kos, KMC$cluster == 3)
KMC4 = subset(kos, KMC$cluster == 4)
KMC5 = subset(kos, KMC$cluster == 5)
KMC6 = subset(kos, KMC$cluster == 6)
KMC7 = subset(kos, KMC$cluster == 7)
table(KMC$cluster) # Or use table to produce summary of all clusters 
KMC = split(kos, KMC$cluster) # splits kos into clusters, which can be accessed by KMC[[1]]

# Compute the 6 most frequent words in each cluster
tail(sort(colMeans(KMC1)))
tail(sort(colMeans(KMC2)))
tail(sort(colMeans(KMC3)))
tail(sort(colMeans(KMC4)))
tail(sort(colMeans(KMC5)))
tail(sort(colMeans(KMC6)))
tail(sort(colMeans(KMC7)))
# Iraq war most frequent words in cluster 3, Democrat related words in cluster 2

# Compare how each alogrithm (hierarchical vs. k-means) go about defining clusters
table(clusterGroups, KMC$cluster)

****************************************
# Market segmentation for airlines
airlines = read.csv("AirlinesCluster.csv")

# Some of the variables in the data set have large scales e.g. Balance and Bonus Miles and some small e.g. BonusTrans and FlightTrans
# In order to prevent large scale variables influencing the clustering, we need to normalize the data

# Use the preProcess function in the caret package to peformce pre-processing and predict function to normalize the data set
preProc = preProcess(airlines) 
airlinesNorm = predict(preProc, airlines)
# All variables in new data set should have mean equal to 0 and standard deviation of 1.

# Peform hierarchical clustering on the new normalized data frame
distances = dist(airlinesNorm, method = 'euclidean')
clusters = hclust(distances, method = 'ward.D')
plot(clusters)
# Choice of 2, 3 or 7 clusters identified as suitable. Decide on 5 cluster groups to cut tree into.

clusterGroups = cutree(clusters, k = 5)
table(clusterGroups) # Shows that there are 776 data points in cluster 1. 

# Compute the mean values for each of the variables in each cluster group (centroids of clusters)
tapply(airlines$Balance, clusterGroups, mean) # Balance variable
# Or you can use colMeans(subset(airlines, clusterGroups == 1)) to average each column/variable in each cluster group
tapply(airlines$QualMiles, clusterGroups, mean) # QualMiles variable
tapply(airlines$BonusMiles, clusterGroups, mean) # BonusMiles variable
tapply(airlines$BonusTrans, clusterGroups, mean) # BonusTrans variable
tapply(airlines$FlightMiles, clusterGroups, mean) # FlightMiles variable
tapply(airlines$FlightTrans, clusterGroups, mean) # FlightTrans variable
tapply(airlines$DaysSinceEnroll, clusterGroups, mean) # DaysSineEnroll variable
# Or even simpler, use: 
lapply(split(airlines, clusterGroups), colMeans) #to get a summary of all cluster groups and variables

# Cluster 1 has highest value for DaysSinceEnroll than all other cluster groups, which suggests this customer segment is very loyal but fly infruently
# Cluster 2 has highest value for QualMiles, FlightMiles and FlightTrans, suggesting customers with a large amount of miles, mostly accumulated through flight transactions.
# Cluster 3 has highest value for Balance, BonusMiles and BonusTrans, suggesting  customers with a lot of miles, and who have earned the miles mostly through bonus transactions.
# Cluster 4 has highest value for No value out of all cluster groups. These customers have the smallest value in DaysSinceEnroll, but they are already accumulating a reasonable number of miles
# Cluster 5 has highest value for No value out of all cluster groups. Customers here have lower values for all variables. 

# Run k-means clustering on the normalized data using 5 clusters and 1000 max iterations
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(KMC)
table(KMC$cluster) # shows number of observations in each cluster

*****************************************
# Predicting stock prices with cluster-then-predict i.e. create cluster groups and build cluster specific models

stocks = read.csv("StocksCluster.csv")
# PositiveDec is the variable that shows whether a stock (in year of the observation) had a positive return in December (1 if yes, 0 if no)
# All values represent proportional changes of stock price for that month in % e.g. 0.05 equals a 5% incrase from previous month stock price

# Largest correlation between any two variables
cor(stocks) # highest correlation is between months Oct and Nov (0.1916)

# Month with the highest and lowest mean return across all observations
colMeans(stocks) # Highest is April (0.026) and lowest is Sep (-0.0147)

# Peform logistic regression first, splitting data into train and test sets
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
train = subset(stocks, spl == T)
test = subset(stocks, spl == F)
stocksLogit = glm(PositiveDec ~., data = train, family = binomial)
logitPredict = predict(stocksLogit, type = "response")
table(train$PositiveDec, logitPredict > 0.5)
# Overall accuracy of the training set is (990 + 3640) / 8106 = 0.571 (marginal improvement over baseline of 0.546)

# The accuracy of the test set is then determined
logitPredict2 = predict(stocksLogit, newdata = test, type = 'response')
table(test$PositiveDec, logitPredict2 > 0.5)
# Overall accuracy of testing set is (1553 + 417) / 3474 = 0.567

# Perform k-means clustering to stocks data set
# 1. Remove dependent variable first before creating clusters, since our goal is to predict the outcome variable and including it will make the clustering invalid.
limitedTrain = train # make copy of training set and name it limitedTrain
limitedTrain$PositiveDec = NULL # Set dependent variable to empty
limitedTest = test
limitedTest$PositiveDec = NULL

# 2. Normalize the train and test sets, but using just the training set and preProcess function to perform normalization
preProc = preProcess(limitedTrain) 
trainNorm = predict(preProc, limitedTrain) # mean equals approx. 0
testNorm = predict(preProc, limitedTest) # mean equals -0.000419

# 3. Run k-means clustering on the normailzed training set
set.seed(144)
KMC = kmeans(trainNorm, centers = 3)
table(KMC$cluster) # Summary shows cluster 2 has the largest number of observations of 4696

# Use the flexclust package to obtain training set and testing set cluster assignments for our observations
KMC.kcca = as.kcca(KMC, trainNorm)
clusterTrain = predict(KMC.kcca)
clusterTest = predict(KMC.kcca, newdata = testNorm)
table(clusterTest) # Summary of test-set observations assigned to each cluster

# Subset the training and testing set into 3, one for each cluster
train1 = subset(train, clusterTrain == 1 )
train2 = subset(train, clusterTrain == 2)
train3 = subset(train, clusterTrain == 3)
test1 = subset(test, clusterTest == 1)
test2 = subset(test, clusterTest == 2)
test3 = subset(test, clusterTest == 3)
mean(train1$PositiveDec) # Returns the highest average value of the dependent variable

# Build predictive model (logistic regression) for each of the 3 subsets, trained on their respective training set e.g. stockModel1 trained on train1
logitModel1 = glm(PositiveDec ~., data = train1, family = binomial)
logitModel2 = glm(PositiveDec ~., data = train2, family = binomial)
logitModel3 = glm(PositiveDec ~., data = train3, family = binomial)

# Make predictions using the 3 logistic models on each of the 3 testing sets
predict1 = predict(logitModel1, newdata = test1, type = 'response')
predict2 = predict(logitModel2, newdata = test2, type = 'response')
predict3 = predict(logitModel3, newdata = test3, type = 'response')
table(test1$PositiveDec, predict1 > 0.5) # Accuracy of testing set equals (30+774)/1298 = 0.619
table(test2$PositiveDec, predict2 > 0.5) # Accuracy of testing set equals (388+757)/2080 = 0.55
table(test3$PositiveDec, predict3 > 0.5) # Accuracy of testing set equals (49+13)/96 = 0.645

# Finally, compute overall test set accuracy by combining the test set predictions into a single vector and all true outcomes in a single vector
AllPredictions = c(predict1, predict2, predict3)
AllOutcomes = c(test1$PositiveDec, test2$PositiveDec, test3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5) # Overall accuracy of test set is (1544+467)/3474 = 0.5788


