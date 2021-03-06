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
# accuracy of results show that 87.8% prediction of song being in the top 10 charts
# compared this to the baseline, we see that the accuracy is 84% (so a slight improvement)
table(songsTest$Top10)
# Sensitivity (true positive) = 0.32 i.e. 32% of the actual popsitive (in top 10) outcomes were accurately predicted
# Specifity (true negative) = 0.98 i.e. 98% of the actual negative (no top 10) outcomes were accurately predicted

*********************************
# Parole violators
parole = read.csv("parole.csv")

# We observe that there are unordered factors (or categories) in the data set e.g. male, race, state, violator and crime
# only state and crime have more than 3 levels
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# Split parole data into Train and Test sets, set seed to 144 and higher splitratio given smaller data set size
set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
paroleTrain = subset(parole, split == T )
paroleTest = subset(parole, split == F)

# Build first logit model using violator as dependent variable and all others as independent
paroleLog1 = glm(violator ~ ., data = paroleTrain, family = binomial)
# results show that the variable multiple.offenses has a positive and larger coeff. than other significant variables
# this indicates that the probability of an offender breaking parole increases by log coeff. for every unit increase in the variable

# Make a prediction using this logit model on the test data set
predicParole = predict(paroleLog1, newdata = paroleTest, type = "response")
table(paroleTest$violator, predicParole >= 0.5)
# Sensitivity equals 12 / (12 + 11) or 0.521
# Specificity equals 167 / (167 + 12) or 0.932
# Accuracy of the models' predictions equals 167 + 12 / 202 = 0.88
table(paroleTest$violator)
# accuracy of the baseline in predicting parole violation is equal to 0.886
# it has 0 false positives and 23 false negatives

# Calculate the area under the chart
ROCRpredic = prediction(predicParole, paroleTest$violator)
auc = as.numeric(performance(ROCRpredic, "auc")@y.values)
ROCperf = performance(ROCRpredic, 'tpr', 'fpr') 
plot(ROCperf, colorize= T, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))
# AuC = 0.89 which means there is a 89% probability the model can correctly differentiate between a
# randomly selected parole violator and a randomly selected parole non-violator.

******************************************
# Predicting loan repayments
loans = read.csv("loans.csv")
# Check to see how many of the loans have missing values 
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
# Total of 62 loans have at least one variable with a missing value
# Instead of removing these variables, we need to fill in the missing values before proceeding with creating a logistic regression model

# Create subset of the variables that have missing values
loans.missing = loans[, "log.annual.inc", ]
set.seed(144)
# Use multiple imputation to impute missing values for all independent variables except not.fully.paid
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
# Re-add the variables that have been imputed into a new data set
loans[vars.for.imputation] = imputed

# Split data into Train and Test sets using sample.split function on the dep. var. (not.fully.paid)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loansTrain = subset(loans, split == T )
loansTest = subset(loans, split == F)

# Build logit model using all independent variables and the dependent variable as per split
loansLog1 = glm(not.fully.paid ~ ., data = loansTrain, family = binomial)
# results show that loan purpose coeff. that are negative lead to lower probability of loan default e.g. education and home improvement
# whilst positive coeff. lead to greater risk of defaulting on the loan

# Predict the probability of loan default using the logit model on the test data
loansTest$predicLoans = predict(loansLog1, newdata = loansTest, type = "response")
table(loansTest$not.fully.paid, loansTest$predicLoans > 0.5)

# Calculate AUC 
ROCRpredic = prediction(loansTest$predicLoans, loansTest$not.fully.paid)
auc = as.numeric(performance(ROCRpredic, "auc")@y.values)
ROCperf = performance(ROCRpredic, 'tpr', 'fpr') 
plot(ROCperf, colorize= T, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))
# AUC equals 0.672 i.e. there is a 67.2% probability that the logit model can differentiate between a randomlly
# selected loan defaulter and randomly selected non-loan defaulter

# Develop a bivariate logistic regression model using just the int.rate variable as the independent 
bivariateLog = glm(not.fully.paid ~ int.rate, data = loansTrain, family = "binomial")

# Conduct prediction using this bivariate logit model on the test data
bivariatePredic = predict(bivariateLog, newdata = loansTest, type = "response")
summary(bivariatePredic)
# results using bivariate logit model shows that the highest predicted probability of a 
# loan not being paid back is 0.426. This is less than the 0.5 threshold value. 

# Calculate AUC on this new bivariate model
ROCRpredic = prediction(bivariatePredic, loansTest$not.fully.paid)
auc = as.numeric(performance(ROCRpredic, "auc")@y.values)
# results show AUC is equal to 0.6239081 or 62.4%. 

# Compute the probability of a profitable investment in a loan
# use the formula c * exp(r * t) where c is initial investoment (assume $1), r is interst rate and t is term of loan (3yrs for all loans in this data set)
# Create a new variable from the test data that shows the profit (when load is repayed) and loss (default of loan)
loansTest$Profit = 1*exp(loansTest$int.rate * 3) - 1
loansTest$Profit[loansTest$not.fully.paid == 1] = -1 # value of 1 means loan was not paid

# Apply an investment strategy that seeks to maximise returns whilst minimising risk of loan default
# Required return is set at a minimum of r = 15%, and the aim is to minimise risk for any 100 loans chosen
# First, subset the test data frame to include only those loans with the required interest rate
highInterest = subset(loansTest, loansTest$int.rate >= 0.15)
# Average return for setting a minimum hurdle rate of 15% int.rate equals 0.2251 dollars per $1 invested
# this is an improvemnt over the total test set which was 0.2094 dollars per $1 invested
# However, this is offest by the fact there is a high proportion of loans not paid back of 25.17% vs 16% from all loans in test set

# Find the 100th smallest predicted probability of loan default in this highinterest set, using predicLoans created earlier
cutoff = sort(highInterest$predicLoans, decreasing = F)[100]

# Now create a new subset that includes only the 100 loans that are equal to or belowo this predicted default probability
selectedLoans = subset(highInterest, highInterest$predicLoans <= cutoff)
# results show that average profit is now 0.318 dollars per $1 invested, with a 19% predicted probability of a loan default (slighlty higher than all loans but lower than just focusing on r>15%)
