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
dependent = as.factor(trainNYT$Popular)
trainNYT$Popular = NULL
combinedDF = rbind(trainNYT, testNYT)

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
# Accuracy or AUC of this model based on kaggles review is equal to 0.76527 (private) and 0.696 (public). 

# To convert the date/time to something R will understand, you can use the following commands:
trainNYT$PubDate = strptime(trainNYT$PubDate, "%Y-%m-%d %H:%M:%S")
testNYT$PubDate = strptime(testNYT$PubDate, "%Y-%m-%d %H:%M:%S")

# Now this provides many more attributes of the PubDate variable to be extracted e.g. A Weekday variable be created by doing the following:
trainNYT$Weekday = trainNYT$PubDate$wday
testNYT$Weekday = testNYT$PubDate$wday
# Values range from 0 (Sunday) to 6 (Saturday)

****************************
# Try using tm package to create corpus on the SNIPPET variable out of both the train and test data sets

corpus = Corpus(VectorSource(c(trainNYT$Snippet, testNYT$Snippet)))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]]) # returns first row of corpus for the snippet variable/column
dtm = DocumentTermMatrix(corpus) # Total of 13820 words in 8402 article entries (train + test)
findFreqTerms(dtm, lowfreq = 50)
sparse = removeSparseTerms(dtm, 0.995)
sparseSnippet = as.data.frame(as.matrix(sparse)) # New sparse data frame that contains 221 columns/words 
colnames(sparseSnippet) = make.names(colnames(sparseSnippet))
snippetTrain = head(sparseSnippet, nrow(trainNYT)) # split sparseSnippet into train and test using head and tail functions
snippetTest = tail(sparseSnippet, nrow(testNYT)) # This resplitting is done due to how both original train and test sets were combined when creating the corpus

snippetTrain$Popular = trainNYT$Popular # Add back dependent variable from original trainNYT data frame
snippetTrain$WordCount = trainNYT$WordCount
snippetTest$WordCount = testNYT$WordCount

# First, try to build a logistic regression model with the sparsed train and test data sets:
modelLogit1 = glm(Popular ~ ., data = snippetTrain, family = binomial)
logitPredict1 = predict(modelLogit1, newdata = snippetTest, type = 'response')
submission02 = data.frame(UniqueID = testNYT$UniqueID, Probability1 = logitPredict1)
write.csv(submission02, "submission02.csv", row.names=FALSE)
# Accuracy of this submission is 0.7365 (Private) and 0.79517 (Public)

# Second, try same approach but with remove sparse threshold at 0.995 instead of 0.99
submission03 = data.frame(UniqueID = testNYT$UniqueID, Probability1 = logitPredict1)
write.csv(submission03, "submission03.csv", row.names=FALSE)
# Accuracy of this submission is 0.7282 (Private) and 0.79739 (Public)

********************************************
# Try building corpus using Headline variable
  
corpus = Corpus(VectorSource(c(trainNYT$Headline, testNYT$Headline)))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]]) # returns first row of corpus for the snippet variable/column
dtm = DocumentTermMatrix(corpus) # Total of 13820 words in 8402 article entries (train + test)
findFreqTerms(dtm, lowfreq = 50)
sparse = removeSparseTerms(dtm, 0.995)
sparseHeadline = as.data.frame(as.matrix(sparse)) # New sparse data frame that contains 221 columns/words 
colnames(sparseHeadline) = make.names(colnames(sparseHeadline))
headlineTrain = head(sparseHeadline, nrow(trainNYT)) # split sparseHeadline into train and test using head and tail functions
headlineTest = tail(sparseHeadline, nrow(testNYT))
# Add back dependent variable from original trainNYT data frame
headlineTrain$Popular = trainNYT$Popular 
headlineTrain$WordCount = trainNYT$WordCount
headlineTest$WordCount = testNYT$WordCount
headlineTrain$Weekday = trainNYT$Weekday
headlineTest$Weekday = testNYT$Weekday

# Logistic regression model
modelLogit2 = glm(Popular ~ ., data = headlineTrain, family = binomial)
logitPredict2 = predict(modelLogit2, newdata = headlineTest, type = 'response')
submission04 = data.frame(UniqueID = testNYT$UniqueID, Probability1 = logitPredict2)
write.csv(submission04, "submission04.csv", row.names=FALSE)
# Accuracy of this submission is 0.79078 (Private) and 0.74706 (Public)

# CART model
headlineCART = rpart(Popular ~ WordCount + Weekday, data = headlineTrain, method = "class")
cartPredict1 = predict(headlineCART, newdata = headlineTest, type = 'class')

# Random Forest 
modelRF = randomForest(Popular ~ ., data = headlineTrain)
rfPredict1 = predict(modelRF, newdata = headlineTest)
submission05 = data.frame(UniqueID = testNYT$UniqueID, Probability1 = rfPredict1)
write.csv(submission05, "submission05.csv", row.names=FALSE)

#################
# Apply imputation to address missing values in variables
#################

# Create new data frame to store imputed variables (using combinedDF as reference)
imputedDF = combinedDF
imputedDependent = dependent
imputedDF$NewsDesk[combinedDF$SectionName == "Arts"] = "Culture"
imputedDF$NewsDesk[combinedDF$SectionName == "Business Day"] = "Business"
imputedDF$NewsDesk[combinedDF$SectionName == "Health"] = "Science"
imputedDF$NewsDesk[combinedDF$SectionName == "N.Y. / Region"] = "Metro"
imputedDF$NewsDesk[combinedDF$SectionName == "Crosswords/Games"] = "Games"
imputedDF$NewsDesk[combinedDF$SectionName == "Opinion"] = "OpEd"
imputedDF$NewsDesk[combinedDF$SectionName == "Open"] = "OpEd"
imputedDF$NewsDesk[combinedDF$SectionName == "Technology"] = "Tech"
imputedDF$NewsDesk[combinedDF$SectionName == "Travel"] = "Travel"
imputedDF$NewsDesk[combinedDF$SubsectionName == "Education"] = "Education"

# Remove observations that don't have a NewsDesk entry in the testNYT data frame e.g. National and Sports
imputedDependent <- imputedDependent[(imputedDF$NewsDesk[1:length(imputedDependent)] != "National")]
imputedDF <- imputedDF[(imputedDF$NewsDesk != "National"),]
imputedDependent <- imputedDependent[(imputedDF$NewsDesk[1:length(imputedDependent)] != "Sports")]
imputedDF <- imputedDF[(imputedDF$NewsDesk != "Sports"),]

# Convert WordCount variable in imputed data frame to natural logarithms
imputedDF$WordCount = log(imputedDF$WordCount + 1)

#################
# Feature Extraction - aims to reduce the amount of data you have to process, by drawing signal out of noise.
#################

# 1. Does Headline have a question mark? 
imputedDF$Question = rep(0, nrow(imputedDF)) # Adds Question column to store value of 0 (no) and 1 (yes) 
imputedDF$Question = ifelse(grepl("?",imputedDF$Headline,fixed=TRUE), 1, 0) # Use grepl to find ? in Headline and assign 1 if yes, 0 if no
# 510 Headlines with question mark, 7886 without. 

# 2. Convert Weekday variable to factor 
imputedDF$Weekday = as.factor(imputedDF$PubDate$wday)

# 3. Create corpus using Headline as variable
corpus = Corpus(VectorSource(c(trainNYT$Headline, testNYT$Headline)))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]]) # returns first row of corpus for the snippet variable/column
dtm = DocumentTermMatrix(corpus) # Total of 13820 words in 8402 article entries (train + test)
findFreqTerms(dtm, lowfreq = 50)
sparse = removeSparseTerms(dtm, 0.995)
sparseHeadline = as.data.frame(as.matrix(sparse)) # New sparse data frame that contains 221 columns/words 
colnames(sparseHeadline) = make.names(colnames(sparseHeadline))
headlineTrain = head(sparseHeadline, nrow(trainNYT)) # split sparseHeadline into train and test using head and tail functions
headlineTest = tail(sparseHeadline, nrow(testNYT))
