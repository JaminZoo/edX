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
testNYT$Popular = NA
combinedDF = rbind(trainNYT, testNYT)
dependent = as.factor(trainNYT$Popular)

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
rowTrain = nrow(trainNYT) - sum(imputedDF$NewsDesk == "National")
rowTest = nrow(testNYT) - sum(imputedDF$NewsDesk == "National")


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
imputedDF$PubDate <- strptime(imputedDF$PubDate, "%Y-%m-%d %H:%M:%S")
imputedDF$Weekday = as.factor(imputedDF$PubDate$wday)

# 3. Create corpus using Headline and Abstract as variables
text = vector()
for (i in 1:nrow(imputedDF)) {
  text = rbind(text, paste(imputedDF$Headline[i], " ", imputedDF$Abstract[i]))
}
corpus <- Corpus(VectorSource(text))

corpus = Corpus(VectorSource(text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]]) # returns first row of corpus for the snippet variable/column
dtm = DocumentTermMatrix(corpus) # Total of 16272 words in 8396 article entries
findFreqTerms(dtm, lowfreq = 150)
sparse = removeSparseTerms(dtm, 0.99)
sparseText = as.data.frame(as.matrix(sparse)) # New sparse data frame that contains 221 columns/words 
colnames(sparseText) = make.names(colnames(sparseText))
textTrain = head(sparseText, rowTrain) # split sparseHeadline into train and test using head and tail functions
textTest = tail(sparseText, rowTest)

textTrain$Popular = dependent
textTrain$WordCount = trainNYT$WordCount
textTest$WordCount = testNYT$WordCount
textTrain$Weekday = trainNYT$Weekday
textTest$Weekday = testNYT$Weekday

modelLogit3 = glm(dependent ~ ., data = textTrain, family = binomial)
logitPredict3 = predict(modelLogit3, newdata = textTest, type = 'response')
submission06 = data.frame(UniqueID = testNYT$UniqueID, Probability1 = logitPredict3)
write.csv(submission05, "submission05.csv", row.names=FALSE)

#################
#  Attempt 3
#################
# Output length of headline for each observation (no. of words)
for (i in 1:nrow(trainNYT)) { trainNYT$Length[i] = length(strsplit(trainNYT[i,4], " ")[[1]])}
for (i in 1:nrow(trainNYT)) { trainNYT$Char[i] = nchar(trainNYT[i,4])}

# Use this to split the combined data frame into training and testing sets
combinedTrain = head(combinedDF, nrow(trainNYT))
combinedTest = tail(combinedDF, nrow(testNYT))

# When running the logistic classifier for previous attempts i.e. edX helper, R outputs a warning
# stating "fitted probabilities numerically 0 or 1 occurred", which is usually a good indicator of overfitting

# Length of Snippet and Abstract (no. of words) is effectively the same 
length(which(combinedDF$Snippet!=combinedDF$Abstract)) # 8274 out of 8402 the same word length

# Remove html tags from several of the observations' snippets
trim = function(str) {
  return(gsub("^s+|s+$", "", str))
}
# Remove diacritics from letters 
replaceHTML = function(str) {
  return(gsub("&([[:alpha:]])(acute|cedil|circ|grave|uml);", "1", str))
}
# Remove special characters, such as dashes, quotes, and ellipses
ignoreHTML = function(str) {
  return(gsub("&((m|n)dash|hellip|(l|r)squo;)", "", str))
}
# Extract the text between HTML tags
extractText = function(str) {
  return(gsub("<.*?>", "", str))
}
# Clean up: extract, remove, replace, and trim  
cleanupText = function(str) {
  return(trim(replaceHTML(ignoreHTML(extractText(str)))))
}
# Combine the snippet and abstract terms into a new variable called Summary
combinedDF$Summary = ifelse(nchar(cleanupText(combinedDF$Snippet)) > nchar(cleanupText(combinedDF$Abstract)),
                      cleanupText(combinedDF$Snippet), cleanupText(combinedDF$Abstract))

# Replace commonly observed words such as New York City and Presdient Obama as some of these may fall under the stopwords used in text mapping and therefore removed
originalText = c("new york times", "new york city", "new york", "silicon valley", "times insider",
                    "fashion week", "white house", "international herald tribune archive", "president obama", "hong kong",
                    "big data", "golden globe")
replacementText = c("NYT", "NYC", "NewYork", "SiliconValley", "TimesInsider",
                    "FashionWeek", "WhiteHouse", "IHT", "Obama", "HongKong",
                    "BigData", "GoldenGlobe")

# Write a function to enable replacement of original text with replacement words using grep package for pattern matching and replacement
mgsub = function(x, pattern, replacement, ...) {
  if (length(pattern) != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  } # function accepts 3 arguments or more (indicated by ellipses), 1st being the data frame, 2nd the original text and 3rd the replacement text
  result = x
  for (i in 1:length(pattern)) {
    result = gsub(pattern[i], replacement[i], result, ...) # use gsub function to replace original with replacemet and return to variable result 
  }
  return(result)
}
# Use above function on the Headline and Summary variables 
combinedDF$Headline = mgsub(combinedDF$Headline, originalText, replacementText, ignore.case=TRUE)
combinedDF$Summary  = mgsub(combinedDF$Summary,  originalText, replacementText, ignore.case=TRUE)

# Combine the headline and summary into one variable called Text for easier access
combinedDF$Text = paste(combinedDF$Headline, combinedDF$Summary)

# There are many observations where the independent variables e.g. NewsDesk, SectionName etc. are empty
NoCat = subset(combinedDF, combinedDF$NewsDesk=="" & combinedDF$SectionName=="" & combinedDF$SubsectionName=="")
dim(NoCat) # shows that there are 1626 observations with no entry in all of the three variables, 55 entries with 2 or less entries empty and 6721 that have at least one column empty

# Data Cleansing: since there are so many observations with empty variable entries, multiple imputation will not yield great results
# First, build a category map showing the relationship of each three main variables to identify patterns 
CatMap = as.data.frame(table(combinedDF$NewsDesk, combinedDF$SectionName, combinedDF$SubsectionName)) # Create data frame based on table output of these three variables
names(CatMap) = c("NewsDesk", "SectionName", "SubsectionName", "Freq") # Name the data frame
CatMap = subset(CatMap, Freq > 0) # Only interested in those relationships which occur i. with a frequency > 1
CatMap[order(CatMap$SectionName), ] # Order sorted data frame

# Replace empty NewsDeck sections with best fit based on CatMap findings
combinedDF$NewsDesk[combinedDF$SectionName == "Arts"] = "Culture"
combinedDF$NewsDesk[combinedDF$SectionName == "Business Day"] = "Business"
combinedDF$NewsDesk[combinedDF$SectionName == "Health"] = "Science"
combinedDF$NewsDesk[combinedDF$SectionName == "N.Y. / Region"] = "Metro"
combinedDF$NewsDesk[combinedDF$SectionName == "Opinion"] = "OpEd"
combinedDF$NewsDesk[combinedDF$SectionName == "Open"] = "Technology"
combinedDF$NewsDesk[combinedDF$SectionName == "Technology"] = "Business" # Try both Technology
combinedDF$NewsDesk[combinedDF$SectionName == "Travel"] = "Travel"
combinedDF$NewsDesk[combinedDF$SubsectionName == "World"] = "Foreign"
combinedDF$NewsDesk[combinedDF$SubsectionName == "Multimedia"] = ""

# Change the Crosswords/Games value in the SectionName variable to something more appropriate than current "Business" NewsDeck allocation
# Assign any Crosswords/Games SectionName with a NewsDeck equal to styles, and SectionName equal to Puzzles and blank in SubsectionName variable
indx = which(combinedDF$SectionName == "Crosswords/Games")
combinedDF$NewsDesk[indx] = "Styles"
combinedDF$SectionName[indx] = "Puzzles"
combinedDF$SubsectionName[indx] = ""

# Similarly, apply to the U.S SectionName the value Styles for all observations in NewsDesk and SectionName
indx = which(combinedDF$NewsDesk == "Styles" & combinedDF$SectionName == "U.S.")
combinedDF$NewsDesk[indx] = "Styles"
combinedDF$SectionName[indx] = "Style"
combinedDF$SubsectionName[indx] = ""

# Replace empty SectionName sections with best fit based on CatMap findings for NewsDesk
combinedDF$SectionName[combinedDF$NewsDesk == "Culture"] = "Arts"
combinedDF$SectionName[combinedDF$NewsDesk == "Foreign"] = "World"
combinedDF$SectionName[combinedDF$NewsDesk == "Science"] = "Science"
combinedDF$SectionName[combinedDF$NewsDesk == "National"] = "U.S."
combinedDF$SectionName[combinedDF$NewsDesk == "OpEd"] = "Opinion"
combinedDF$SectionName[combinedDF$NewsDesk == "Sports"] = "Sports"
combinedDF$SectionName[combinedDF$NewsDesk == "Styles"] = "Style" # Try both Technology
combinedDF$SectionName[combinedDF$NewsDesk == "TStyle"] = "Magazine"

# Filling in the SubsectionNames
# Identify variables that have empty values and then use pattern matching to identify set phrases within Headline of each article
indx = which(combinedDF$NewsDesk=="" & combinedDF$SectionName=="" & combinedDF$SubsectionName=="" &
              grepl("^(first draft|lunchtime laughs|politics helpline|today in politics|verbatim)",
                    combinedDF$Headline, ignore.case=TRUE))
combinedDF$NewsDesk[indx] = "National"
combinedDF$SectionName[indx] = "U.S."
combinedDF$SubsectionName[indx] = "Politics"

# Continue filling in emtpy subsection name variable by searching for more terms in the Text variable (Headline + Summary) and update NewsDesk, SectionName and SubsectioName with relevant fields
indx = which(combinedDF$SubsectionName == "" & grepl(paste0("white house|democrat|republican|tea party|",
                                                           "obama|biden|boehner|kerry|capitol|senat|",
                                                           "sen.|congress|president|washington|politic|",
                                                           "rubio|palin|clinton|bush|limbaugh|rand paul|",
                                                           "christie|mccain|election|poll|cruz|constitution|",
                                                           "amendment|federal|partisan|yellen|govern|",
                                                           "gov.|legislat|supreme court|campaign|",
                                                           "primary|primaries|justice|jury"),
                                                     combinedDF$Text, ignore.case=TRUE))
combinedDF$NewsDesk[indx] = "National"
combinedDF$SectionName[indx] = "U.S."
combinedDF$SubsectionName[indx] = "Politics"

idx = which(combinedDF$SectionName=="" &
              grepl(paste0("PAC|GOP|G.O.P.|NRA|N.R.A."),
                    combinedDF$Text, ignore.case=FALSE))
combinedDF$NewsDesk[idx] = "National"
combinedDF$SectionName[idx] = "U.S."
combinedDF$SubsectionName[idx] = "Politics"

# Fill in all remaining emtpy variable values with "Missing" so that randomForest can be applied (can not do so with missing values in place)
combinedDF$NewsDesk[which(combinedDF$NewsDesk == "")] = "Missing"
combinedDF$SectionName[which(combinedDF$SectionName == "")] = "Missing"
combinedDF$SubsectionName[which(combinedDF$SubsectionName == "")] = "Missing"

# Publication date data transformation
combinedDF$PubDate <- strptime(combinedDF$PubDate, "%Y-%m-%d %H:%M:%S")
combinedDF$PubDay = as.Date(combinedDF$PubDate) # Create new variable PubDay that is the PubDate variable convereted to Date data type
combinedDF$Weekday = combinedDF$PubDate$wday
combinedDF$Hour = combinedDF$PubDate$hour

# Word count - convert newsdesk, section name, subsection name and popular into factor data frames
combinedDF$NewsDesk = as.factor(combinedDF$NewsDesk)
combinedDF$SectionName = as.factor(combinedDF$SectionName)
combinedDF$SubsectionName = as.factor(combinedDF$SubsectionName)
combinedDF$Popular = as.factor(combinedDF$Popular)
combinedDF$LogWord = log(combinedDF$WordCount + 1)

# PLot distribution of word count with popular variable
ggplot(trainNYT, aes(x=LogWord, fill=Popular)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of LogWord") +
  xlab("log(1 + WordCount)") +
  theme(axis.title.y = element_blank())
# Graph shows a definite difference between the distributions of popular and unpopular articles based on log word count
# Articles classified as popular i.e. = 1 have a higher log word mean value than those deemed unpopular i.e = 0

# Day of week - popularity of article given the day of the week it was published
combinedDF$DayOfWeek = as.factor(weekdays(combinedDF$PubDate)) # convert Weekday variable from num to factor data type using weekdays function to extract weekday from PubDate
combinedDF$DayOfWeek = factor(combinedDF$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(combinedDF, aes(x=LogWord, fill=Popular)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of LogWC") +
  xlab("log(1 + WordCount)") +
  theme(axis.title.y = element_blank()) + 
  facet_wrap(~DayOfWeek, ncol = 1)
# Graph shows no real definitive indicator of which day of week produces more popular article. However, still indicates higher word count values tend to be more popular than lower values

# Timing of article posts, both the day of the week and the hour of the day
table(combinedDF$Weekday, combinedDF$Popular) # Shows that Sunday, then Saturday have higher probability of a popular article (33% and 27% respectively)
table(combinedDF$Hour, combinedDF$Popular) # Articles start to become more popular early morning and peak around midday, and remains steady (dropping slightly) well into the later hours of the day dropping off only around 11pm.

hourMatrix = as.matrix(table(combinedDF$Hour, combinedDF$Popular))
hourMatrix = cbind(hourMatrix, hourMatrix[, 2]/(hourMatrix[, 1] + hourMatrix[, 2]))
colnames(hourMatrix) = c("Unpopular", "Popular", "PopularDensity")

# We can conclude that the hour and day of the week an article is posted does lend itself to be dependent variables to investigate in our prediction models

# Frequency of articles per published date an relationship with popularity i.e. does more articles equal higher popularity?
ArticlesPerDay = as.data.frame(table(combinedDF$PubDay))
colnames(ArticlesPerDay) = c("Date", "NumDailyArticles")
combinedDF = merge(combinedDF, ArticlesPerDay, all.x = T)

# Summary of popular and unpopular articles for each count of articles per day
table(combinedDF$Popular, combinedDF$NumDailyArticles)

combinedDF$Pop = as.numeric(as.character(combinedDF$Popular)) # Converts Popular from factor into character and then lastly into numeric
combinedDF$Pop[which(is.na(combinedDF$Popular))] = "N/A" # Excludes the test set N/A values 
combinedDF$Pop = as.factor(combinedDF$Popular) # Convert back into factor

ggplot(trainNYT, aes(x=NumDailyArticles, fill=Pop)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of NumDailyArticles") +
  xlab("# Daily Articles Published") +
  scale_fill_discrete(name="Popular") +
  theme(axis.title.y = element_blank())
