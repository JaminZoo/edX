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

**********************************
# US Election results polling data
USpoll = read.csv("PollingData.csv")
# We want to use multiple imputation to fill in the missing i.e. "NA" values in the two polling data variables
# Install the MICE (multiple imputation by chained equations) package
install.packages("mice") 
# Create new data frame that includes only four key variables
USpoll.mice = USpoll[, c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]
# Set random seed so that multiple imputation can be mirrored from video tutorial
set.seed(144)
# Run imputation using compelte function that calls the mice package on the newly created USpoll.mice data frame
imputed = complete(mice(USpoll.mice))
# results show that a total of 5 rounds of imputation was been run and produce no more missing values in either polling survey variable
summary(imputed)
# Now using the "filled in" values from our imputation, we can replace the variables back into the original data frame
USpoll$Rasumssen = imputed$Rasmussen
USpoll$SurveyUSA = imputed$SurveyUSA
# Results should show no missing values, however some still exist in the Rasumussen variable after multiple imputation
# Load in correctly imputed data set as provided
USpoll2 = read.csv("PollingData_Imputed.csv")

# Split this new data frame into Train and Test, where the 2004 and 2008 election years are Train and Test will be 2012
usTrain = subset(USpoll2, Year == 2004 | Year == 2008)
usTest = subset(USpoll2, Year == 2012)

# Develop a baseline to compare our logistic regression model
# A 'smart' baseline assumes that if the Democrat or Republican candidate is leading the polls,
# then they are likely to win that seat. Using the sign function, we can assign a +1 to any republican vote
# a -1 to any democratic vote and a 0 for any tied vote counts. 
table(sign(usTrain$Rasmussen))
# results show that the smart baseline predicts 56 instanaces of republican, 42 of demoncrate and 2 tie
# Comparing this to the actual outcome of the election
table(usTrain$Republican, sign(usTrain$Rasmussen))
# shows fairly good alignment, with only 4 occurences where the actual outcome (demoncrate win) was predicted as a republican win

# Determine correlation of the dependent and independent varaibles from the training data set
cor(usTrain[c("Rasmussen","SurveyUSA", "DiffCount","PropR","Republican")])

# Develop logistic regression model using the variable with the highest correlation to dependent variable i.e. PropR
pollModel1 = glm(Republican ~ PropR, data = usTrain, family = "binomial")

# Compute predictions of probablity that republican will win using the training set only
pollPredict1 = predict(pollModel1, type = "response", newdata = usTrain)
# Create confusion matrix to see how well republican win is predicted at 50% probability
table(usTrain$Republican, pollPredict1 > 0.5)
# results show that in comparison to the 'smart' baseline, the model predictions are wrong 4 times
# 2 predicted republican wins where actual democrat is winner, and vice versa

# Develop another model using two ind. var that have lower correlations e.g. SurveyUSA and DiffCount
pollModel2 = glm(Republican ~ SurveyUSA + DiffCount, data = usTrain, family = "binomial")
pollPredict2 = predict(pollModel2, type = "response", newdata = usTrain)
table(usTrain$Republican, pollPredict2 > 0.5)
# results show that there is only 1 less mistake made, in predicting a democrat win with actual outcome of republican win

# Compare the baseline and the logistic model pollModel2 with the test data set
table(usTest$Republican, sign(usTest$Rasmussen))
# results show 4 mistakes and 2 inconclusive predictions on the testing data set
# create predictions from the logit model using the test data set (previously only used Train)
testPredict = predict(pollModel2, type = "response", newdata = usTest)
table(usTest$Republican, testPredict >= 0.5)
# results show that only 1 mistake is made with no inconclusive predictions. 
# subset the test data to pin point where the mistake occurs i.e. when the prediction was for republican win but actual outcome was democrat
subset(usTest, testPredict >= 0.5 & Republican == 0)
*********************************
# Popularity of music records
songs = read.csv("songs.csv")

# First, subset the data into Train (all years up to and incl. 2009) and Test (all of 2010)
songsTrain = subset(songs, songs$year <= 2009)
songsTest = subset(songs, songs$year == 2010)
# The dependent (or outcome) variabe is Top10
# Build the first logistic regression model by treating all song attributes as independent variables
songsLog1 = glm(Top10 ~ ., data = songsTrain, family = binomial)
# To exclude the non song attributes e.g. year, artistname etc. create a vector of variables equal to these interested attributes
nonvars = c("year","songtitle","artistname","songID", "artistID")
songsTrain = songsTrain[, !(names(songsTrain) %in% nonvars)]
songsTest = songsTest[, !(names(songsTest) %in% nonvars)]
#Akaike Information Criterion (AIC) for this first logit model is 4827.2
# Higher confidence leads to higher predicted probability of song being in the top 10, where higher confidence equates to less complex songs
# Since there is high correlation (0.739906708) between loudness and energy, this model
# exhibits multicollinearity 
# Build two more logit models, one omitting loundess and the other energy each time
songsLog2 = glm(Top10 ~ . - loudness, data = songsTrain, family = binomial)
# subtracting the loundess variable works as it is a numeric type, whereas songtitle can't be omitted due to it being non-numeric
# results show that energy now leads to high probability of song being in the top 10, but is not signficiance 
songsLog3 = glm(Top10 ~ . -energy, data = songsTrain, family = binomial)
# this again shows that the greater the loudness of a song the more liklihood for entry into the top 10
# use logit model to make prediction at 45% probability using the test data set
songsPredic = predict(songsLog3, newdata = songsTest, type = "response")
table(songsTest$Top10, songsPredic >= 0.45)
# accuracy of results show taht 87.8% prediction of song being in the top 10 charts
# compared this to the baseline, we see that the accuracy is 84% (so a slight improvement)
table(songsTest$Top10)
# Sensitivity (true positive) = 0.32 i.e. 32% of the actual popsitive (in top 10) outcomes were accurately predicted
# Specifity (true negative) = 0.98 i.e. 98% of the actual negative (no top 10) outcomes were accurately predicted
