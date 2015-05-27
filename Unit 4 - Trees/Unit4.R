# US Supreme Court Stevens data from 1994 to 2001 
stevens = read.csv("stevens.csv")
# Dependent variable is Reverse, where = 1 reverse decision and 0 = affirm
# Split data into Training and Testing set
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7 )
stevensTrain = subset(stevens, split == T)
stevensTest = subset(stevens, split == F)
# Create CART (Classification and regression tree)
stevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                    data = stevensTrain, method = "class", minbucket = 25) 
# Use prp function within the rpart package to build decision tree
install.packages("rpart.plot")
prp(stevensTree)

# Make predictions of reversal probability using the test data and the created CART
stevensPredic = predict(stevensTree, newdata = stevensTest, type = "class")
table(stevensTest$Reverse, stevensPredic)
# Accuracy equals 41 + 71 / 170 or .659 (compared with baseline of 0.55)

# Create ROC chart for the CART model without the class type as before
ROCpredic = predict(stevensTree, newdata = stevensTest)
# Use the 2nd column (reversal = 1) in the prediction function
stevensPredic2 = prediction(ROCpredic[,2], stevensTest$Reverse)
stevensPerf = performance(stevensPredic2, "tpr", "fpr")
plot(stevensPerf, colorize= T, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))
# AUC equals 0.693
auc = as.numeric(performance(stevensPredic2, "auc")@y.values)

# How many branches are there when minbucket is changed from 25 to 5?
stevensTree5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                    data = stevensTrain, method = "class", minbucket = 5) 
prp(stevensTree5)

# Minbucket changed to 100
stevensTree100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                    data = stevensTrain, method = "class", minbucket = 100) 
prp(stevensTree100)

# minbucket of 5 produces 16 branches, this is too high and suggests overfit to the training data set
# minbucket of 100 produces only a single branch, the tree does not fit well to do training data

# Generate a Random Forrest model
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt+
                               Unconst, data = stevensTrain, nodesize = 25, ntree = 100)
# Warning message will ask if you are sure you want to run classification using randomForest, since there is no type "class" as there was for CART model
# In order to run classification using randomForest, the dependent variable needs to be turned into factors in both Train and Test data set
stevensTest$Reverse = as.factor(stevensTest$Reverse)
stevensTrain$Reverse = as.factor(stevensTrain$Reverse)
# Compute predictiosn on the forrest model
forestPredic = predict(stevensForest, newdata = stevensTest)
table(stevensTest$Reverse, forestPredic)
# The accuracy will vary given the random nature, but should be higher than that using CART and logistic regression e.g approx. 0.68

# Install carat and e1071 packages to perform cross-validation
install.packages("caret")
install.packages("e1071")

# Take the whole training set data and split it up into k equally sized subsets or 'folds', then make predictions
# for each k - 1 subset, continuing building models for each different subset with different fold combinations.
# This is known as k-fold Cross-Validation
# First, define number of folds
numFolds = trainControl(method = "cv", number = 10) # cv = cross validation
# Pick the possible values for the cp (complexity parameter) values to test i.e. 5 vaues from 0.01 to 0.05
cpGrid = expand.grid(.cp=seq(0.1, 0.5, 0.01))
# Perform cross validation with our CART model
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
        data = stevensTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
# Final value used for the model was cp = 0.18, this is the cp value to use in the CART model
# Therefore, create a new CART model instead of the minbucket value of 25 used earlier
stevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data = stevensTrain, method = "class", cp = .18) 
cvPredic = predict(stevensTreeCV, newdata = stevensTest, type = "class")
table(stevensTest$Reverse, cvPredic)
# THe new accuracy of the CART model is now 0.723, an increase over the previous accuracy of the CART model of 0.659.
# Plot the tree for this new cross validatino CART model and note the number of branches
prp(stevensTreeCV)
# This tree has one split and gives us the best out-of-sample accuracy. This reminds us that sometimes the simplest models are the best!

********************************
# D2Hawkeye
claims = read.csv("ClaimsData.csv")
# 5 total cost 'buckets' which denote the amount claimed by each person in that year
# Calculate the percentage of patients in each bucket to determine which bucket has largest proportion of claims
table(claims$bucket2009) / nrow(claims) * 100
# We see that bucket one has the largest proportion of claims at 67% and bucket 5 the least at 0.57%
# Goal is to predict the cost bucket a patient will fall into in 2009 (using 2008 as Training data set)

# First, split the data into Training and Testing sets with a split ration of 0.6 in Train
set.seed(88)
split = sample.split(claims$bucket2009, SplitRatio = 0.6)
claimsTrain = subset(claims, split == T)
claimsTest = subset(claims, split == F)

# Determine the accuracy of the baseline model using the actual (2009) and predicted (2008) values
classMatrix = table(claimsTest$bucket2009, claimsTest$bucket2008) #this is our classification or confusion matrix
accu = (110138 + 10721 + 2774 + 1539 + 103) / nrow(claimsTest)
# By summing the diagonal results and dividing by total claims in the testing set, we get an accuracy of 68%
# We also need to determine the penalty error, which can be done by creating a penalty matrix, multiplying this by the confusion matrix and dividing by the rows in the test set
penaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = T, nrow = 5)
# Predictions are the columns and actual outcomes the rows
# multiple classification and penalty matrices together
as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix 
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix 
) / nrow(claimsTest)
# Penalty error for the baseline error is therefore 0.73

# Objective is to product a CART model that has accuracy greater than 0.68 and penalty error less than 0.73 of the baseline
# First build the tree using rpart and prp functions. Note that we use the same method as binary (LM and Logit) for this 5 level classification matrix
claimsTree1 = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression +
                   diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
                   reimbursement2008 + bucket2008, data = claimsTrain, method = "class", cp = 0.00005)
prp(claimsTree1)
# Compute the predictions for the tree model
claimsPredic1 = predict(claimsTree1, newdata = claimsTest, type = "class")
table(claimsTest$bucket2009, claimsPredic)
accuTree1 = (114141 + 16102 + 118 + 201 + 0) / nrow(claimsTest)
# Accuracy is 0.71 (greater than 0.68 of baseline model)
sum(as.matrix(table(claimsTest$bucket2009, claimsPredic)) * penaltyMatrix 
) / nrow(claimsTest)
# Penalty error for CART model is 0.758 (greater than 0.73 of baseline model i.e. not what we want)

# By default rpart will try to maximise the accuracy and every type of error as seeing as having a penalty of 1. CART model sees very few 3, 4 and 5 values. 
# Rebuild CART model using the loss function and the penalty matrix
claimsTree2 = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression +
                     diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
                     reimbursement2008 + bucket2008, data = claimsTrain, method = "class", cp = 0.00005, parms = list(loss=penaltyMatrix))
claimsPredic2 = predict(claimsTree2, newdata = claimsTest, type = "class")
table(claimsTest$bucket2009, claimsPredic2)
accuTree2 = (94310 + 18942 + 4692 + 636 + 2) / nrow(claimsTest)
# Accuracy is equal to 0.647 (lower than baseline and first CART model)
sum(as.matrix(table(claimsTest$bucket2009, claimsPredic2)) * penaltyMatrix 
) / nrow(claimsTest)
# Penalty error is now 0.642 (less than baseline and first CART model)

*********************************
# Boston house prices, where dependent or outcome variable is medium value (in '000) or MEDV
boston = read.csv("boston.csv")
# Plot of TRACT, NOX and CHAS
ggplot(aes(x = LON, y = LAT), data = subset(boston, TRACT == 3531)) +
  geom_point(color = "red", size = 5) + 
  geom_jitter(aes(LON,LAT, color = CHAS), alpha = 0.5, data = boston) + 
  geom_point(aes(LON, LAT), color = "green",data = subset(boston,NOX >=0.55))

# Build a regression tree
bostonTree = rpart(MEDV ~ LAT + LON, data = boston)
prp(bostonTree)
# Make predictions on CART model
bostonPredic = predict(bostonTree) 
# Plot results of prediction along with subset of data with MEDV greater than or equal to 21.2
ggplot(aes(x = LON, y = LAT), data = boston) +
  geom_point(shape = 1) + 
  geom_point(aes(LON, LAT), color = "red",data = subset(boston,MEDV >=21.2)) +
  geom_point(shape = bostonPredic, color = "blue")

# Build a new tree with minbucket size equal to 50. 
bostonTree2 = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
prp(bostonTree2)
# Again plot the results and segment by following the tree branches towards the lowest MEDV
ggplot(aes(x = LON, y = LAT), data = boston) +
  geom_point(shape = 1) + 
  geom_point(aes(LON, LAT), color = "red",data = subset(boston,MEDV >=21.2)) +
  geom_hline(aes(yintercept = 42.21)) +
  geom_hline(aes(yintercept = 42.17)) + 
  geom_vline(aes(xintercept = -71.07))
# The new regression tree model has 'carved' out a rectangle that predicts closely what the MEDV is for the main data set
# Re-run prediction on CART model
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
bostonTrain = subset(boston, split == T)
bostonTest = subset(boston, split == F)
# Build linear regression model 
bostonLinear = lm(MEDV ~ LON + LAT + CRIM + ZN + INDUS +
                    CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
                  data = bostonTrain) # choose all variables except TOWN as independent variables
# Adjusted R squared is 0.65, there could be codependency 
bostonPredic2 = predict(bostonLinear, newdata = bostonTest)
# Calculated sum of square error for this Linear regression model
bostonSSE = sum((bostonPredic2 - bostonTest$MEDV)^2)
# SSE equals 3037, determine if regression tree can improve on this error vaule
# Create another regression tree, now with all the variables instead of just LAT and LON
bostonTree3 = rpart(MEDV ~  LON + LAT + CRIM + ZN + INDUS +
                      CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
                    data = bostonTrain) 
prp(bostonTree3)
# Make predictions of this tree
bostonPredic3 = predict(bostonTree3, newdata = bostonTest)
treeSSE = sum((bostonPredic3 - bostonTest$MEDV)^2)
# SSE using regression tree gives us a value of 4328, meaning linear regression is better than regression tree in this instance

# Use cross validation to build final regression tree
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp= (0:10)* 0.001) # caret function will try all values given in grid e.g. 10 values starting from 0 and incrementing by 0.001
# Now fit the predictive model over different tuning parameters
bostonTree4 = train(MEDV ~  LON + LAT + CRIM + ZN + INDUS +
                      CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
                    data = bostonTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid) 
# Running bostonTree4 gives a summary of results from sampling each parameter and provides the final value for cp = 0.001
bestTree = bostonTree4$finalModel # get the best tree
prp(bestTree) # plot the tree corresponding to this model, shows very detailed tree as per the small cp value.
# Finally compute the SSE for this best tree model
bestTreePredic = predict(bestTree, newdata = bostonTest)
bestTreeSSE = sum((bestTreePredic - bostonTest$MEDV)^2)
# SSE equals 3675. Using cross validation improved on the SSE of the regression tree, but not as low as the SSE from linear regression (3037)

