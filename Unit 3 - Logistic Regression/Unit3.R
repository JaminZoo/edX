### Unit 3

<<<<<<< HEAD
#Logistical regression is an extension of linear regression and allows
#predictions to be made for a dependent variable that is binary (categorical) e.g. 0 or 1

#Predict the outcome of qualit of care given, 1 = Poor Care, 0 = Good Care
#Inreasing the beta coefficients of the linear reg equation increases
#the probability of 1 (poor care), whilst negative/decreasing the betas will decrease
#the probability of 1 i.e. improved care. 

# exp(x) takes the exponent of x

****************************************
# Care quality for patients with diabeties 
quality = read.csv("quality.csv")
# Install package to randomnise data frame into Train and Test groups
install.packages("caTools")
# Set random seed to 88 in accordance with video tutorial
set.seed(88)
# Use sample.split function to spit PoorCare variable into 75/25 ratio
# It ensures that both Train and Test sets are representative of the initial
# quality data frames ratio of 0.75 split between good and poor quality care
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
# Create Train and Test subsets based on the true/false vaues of split
qualityTrain = subset(quality, split == T)
qualityTest = subset(quality, split == F)

# Use glm function to build logistic regression model using the quality Train data set
qualityLog = glm(PoorCare ~ Narcotics + OfficeVisits, 
                 data = qualityTrain, family = binomial)
# Create prediction using the qualityLog logistic regression model
predictTrain = predict(qualityLog, type = "response")
predictTest = predict(qualityLog, type = "response", newdata = qualityTest)
# Compute the mean of the prediction for each of the two variables in qualityTrain$PoorCare
tapply(predictTrain, qualityTrain$PoorCare, mean)

# Create confusion matrix to compare actual and predicted outcomes using
# a threshold value of 0.5 on the predicted outcome of good or poor care
table(qualityTrain$PoorCare, predictTrain > 0.5)
# Results: Predict 72 good care where actual is 72, predict 2 poor where actual is good
# predict 15 good care where actual is poor, predict 10 poor care where actual is poor

# install package ROCR (Reciver Operator Charactersitic)
install.packages("ROCR")
# Create a prediction and chart using ROCR functions prediction and performance
ROCpredic = prediction(predictTrain, qualityTrain$PoorCare)
ROCpredicTest = prediction(predictTest, qualityTest$PoorCare)
ROCperf = performance(ROCpredic, 'tpr', 'fpr') # tpr = true positive, fpr = false positive
plot(ROCperf, colorize= T, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))
# colors plot and adds 0.1 threshold data points
# Determine AUC (area under chart) for predictTest 
auc = as.numeric(performance(ROCpredicTest, "auc")@y.values)

***********************************
# Framingham hear study: evaluating risk factors to predict Cornary Heart Disease (CHD) in patients 
#TenYearCHD is the dependent variable of interest, and the risk factors of CHD are assigned as ind. var
  
fram = read.csv("framingham.csv")
# Set random seed in accordance with video tutorial
set.seed(1000)

# split fram data fram into train and test data frames. Typically want to put 50 - 80% of your 
# data in the train set vs. test set (a larger main data set, the lower the split ratio can be)
split = sample.split(fram$TenYearCHD, SplitRatio = 0.65)

# Create Train and Test subsets based on the true/false vaues of split
framTrain = subset(fram, split == T)
framTest = subset(fram, split == F)

# Create logistic regression model using the train data set ( "." means use all variables in data set as independent)
framLog = glm(TenYearCHD ~ ., data = framTrain, family = binomial)
# Create a prediction based on the test data frame and the logistic model just created
predicTest = predict(framLog, type = 'response', newdata = framTest)

# Create a confusion (classification) matrix to compare actual (Test) ad predicted (framLog) outcomes
table(framTest$TenYearCHD, predicTest > 0.5)
# results show that our model rarely predicts a ten year CHD risk above 50%
# Prediction accuracy i.e. of predicting a true outcome can be calculated by taking the 
# total true predictions (for both T & F) and dividing by total observations
predicAccur = (1069 + 11)/(1069 + 11 + 6 + 187)
# Use RORC to make predictions based on predicTest and test data set
ROCRpredic = prediction(predicTest, framTest$TenYearCHD)
# Calculate the Area under chart 
auc = as.numeric(performance(ROCRpredic, "auc")@y.values)

# External validation with other sets of data (not Framingham) is critical to validate the train model
#accuracy when predicting CHD risk for other ethnicities e.g. Black, Asian, Hispanic etc.


