#########################
#Kaggle Competition - NYT Article Analysis, predict which will be the most popular.
#########################

# Objective: predict the popularity of a set of New York Times blog articles from the time period September 2014-December 2014.
# In doing so, help the NYTs understand the features of a blog post that make it popular.
# Qns: What are these 'features', how to identify them and build a predictive model to evaluate liklihood of other articles with same feature becoming popular

# The data: consists of two data sets, all coming from NYTs website. 
# 1. Train - 6532 articles
# 2. Test - 1870 articles

# Variables:
# Newsdesk - NYT desk that produced the story e.g. Business, Culture etc.
# SectionName - section the article appeared in e.g. Arts, Health, etc.
# SubsectionName - subsection the article appeared in e.g. Education, small business etc.
# Headline - title of article
# Snippet - small portion of article text
# Abstract - summary of blog article, written by the NYT
# WordCount - number of words in article
# PubDate - the publication date, in the format "Year-Month-Day Hour:Minute:Second"
# UniqueID - identifier for each article

# Type of problem: binary classification. Submission takes form of data frame that has Unique ID and Prediction column for each ID
submission = data.frame(UniqueID = testNYT$UniqueID, Probability1 = testPredict)
write.csv(submission, "submission.csv", row.names=FALSE)
# Solution file i.e. submission.csv shoulc include 1870 predictions (equal to obs. in test set)

#################
# Load in train and test data sets
#################
setwd("~/Documents/edX/Unit 7 - Kaggle")
trainNYT = read.csv("NYTimesBlogTrain.csv", stringsAsFactors = F)
testNYT = read.csv("NYTimesBlogTest.csv", stringsAsFactors = F)

################
# edX Helper Guide
################
SimpleMod = glm(Popular ~ WordCount, data=trainNYT, family=binomial)
summary(SimpleMod)
simplePredict = predict(SimpleMod, newdata = testNYT, type = 'response')
# Can't determine accuracy of the test model since we do not know what the dependent variable is. 
# Test a submission using the above simple logistical regression model 
submission01 = data.frame(UniqueID = testNYT$UniqueID, Probability1 = simplePredict)
write.csv(submission01, "submission01.csv", row.names=FALSE)
# Accuracy or AUC of this model based on kaggles review is equal to 0.76527. 

# To convert the date/time to something R will understand, you can use the following commands:
trainNYT$PubDate = strptime(trainNYT$PubDate, "%Y-%m-%d %H:%M:%S")
testNYT$PubDate = strptime(testNYT$PubDate, "%Y-%m-%d %H:%M:%S")

# Now this provides many more attributes of the PubDate variable to be extracted e.g. A Weekday variable be created by doing the following:
trainNYT$Weekday = trainNYT$PubDate$wday
testNYT$Weekday = testNYT$PubDate$wday
# Values range from 0 (Sunday) to 6 (Saturday)

