# Text Analytics

tweets = read.csv("tweets.csv", stringsAsFactors = F) # always need the 2nd argument when dealing with text based data

# Create a new variable called Negative that groups the tweets that have a score of -1 or less i.e. negative and strongly negative sentiment 
tweets$Negative = as.factor(tweets$Avg <= -1)

# Install text mining and snowball package. SnowballC helps run the tm package. 
install.packages("tm") # this creates a corpus
library(tm)
install.packages("SnowballC") # stemmers
library(SnowballC)
#In linguistics, a corpus (plural corpora) or text corpus is a large and structured set
#of texts (nowadays usually electronically stored and processed). They are used to do 
#statistical analysis and hypothesis testing, checking occurrences or validating linguistic rules within a specific language territory.

# Therefore, create a corpus from the tweets data set
corpus = Corpus(VectorSource(tweets$Tweet)) 

# Use the tm_map function to pass in the corpus and a function to appy to all words in the corpus
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]] # outputs the first tweet of the corpus, with all words converted to lower case

# Remove punctuation in each tweet
corpus = tm_map(corpus, removePunctuation) # removes all punctuation from corpus

# Remove stop words (commonly defined words like I, me, myself etc.) AND the world apple
Sys.setlocale("LC_ALL", "C") # Changes location to USA
stopwords("english")[1:10 ] # shows the first 10 words that tm package uses as 'stop' words
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english"))) # this does not remove the word 'apple' from combined words e.g. thanxapple

# Remove the stem or ending section of each word basedon Porter's stemming algo
corpus = tm_map(corpus, stemDocument)

# View the matrix of this corpus
frequencies = DocumentTermMatrix(corpus)
# shows that there are 1181 individual tweets with 3289 words in total
# Inspect the matrix between the 1000th and 1005th tweet and for the 505th to 515th words
inspect(frequencies[1000:1005,505:515])

# Determine how often a word appears in this freqency matrix of the corpus, use a threshold value of 20 i.e. display words that occur at least 20 times
findFreqTerms(frequencies, lowfreq = 20) # results show that there are 53 individual words that occur a minimum of 20 or more times in a total of 3289 words
# There appears to be many more insignificant words that occur less frequently (or not at all) than there are frequent words. We do not want to assgin an indep. var to each of these
# Therfore, we can exclude these non sigfnicant words by using the removeSparseTerms function and a max allowed sparsity of 0.995 i.e. only keep 0.5% of total tweets in our corpus
sparse = removeSparseTerms(frequencies, 0.995) # now there are only 309 (or 9%) terms out of the original 3289 words

# Create new data frame to incorporate this reduced corpus, with each column representing the 309 different words and the same number of rows as tweets (1181)
tweetsSparse = as.data.frame(as.matrix(sparse))

# Remove any words that may have a number at the start in each column of the tweetsSparce data frame
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Lastly, add in the negative variable from the original tweets data frame
tweetsSparse$Negative = tweets$Negative

# Now begin to build the predictive model by splitting the sparse data frame into Training and Testing sets
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == T)
testSparse = subset(tweetsSparse, split == F)

# Build a CART model using the training set of the sparse data frae
tweetCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)
# Tree shows that if the word freak is in the tweet (freak < 0.5) then predict true or a negative tweet with sentitment <= -1
# If the word freak is not in the tweet but the word hate is, then also predict true. 

# Make predictions using the CART model on the test set
tweetPredic = predict(tweetCART, newdata = testSparse, type = 'class')
table(testSparse$Negative, tweetPredic)
# Accuracy of the CART model at predicting negative tweet sentitment is 18 + 294 / (18 + 294 + 37 + 6) = 0.88
# Accuracy of baseline model that always predicts negative (since this is the most common outcome) is equal to 300/355 or 0.85

# Now try to improve on the CART model by running a random forest using default settings  
set.seed(123)
tweetRF = randomForest(Negative ~ ., data = trainSparse)
predicRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predicRF)
# Accuracy of random forest prediction is 21 + 293 / (21 + 34 + 293 + 7) = 0.88

tweetLogit = glm(Negative ~ ., data = trainSparse, family = "binomial")
predicLogit = predict(tweetLogit, newdata = testSparse, type = "response")
table(testSparse$Negative, predicLogit > 0.5)
# Accuracy is equal to 32 + 253 / (23 + 32 + 47 +253) = 0.80, which is worse than both the CART and random tree models as well as the baseline

**************************************
# Enron text analytics court case
  emails = read.csv("energy_bids.csv", stringsAsFactors = F)

# First, create a corpus from the emails data set
corpus = Corpus(VectorSource(emails$email)) 

# Perform 4 pre-processing steps to prepare text for analysis. 
# 1. Use the tm_map function to pass in the corpus and a function to appy to all words in the corpus
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

# 2. Remove punctuation
corpus = tm_map(corpus, removePunctuation)

# 3. Remove stop words
corpus = tm_map(corpus, removeWords, stopwords("english"))

# 4. Remove the stem or ending section of each word basedon Porter's stemming algo
corpus = tm_map(corpus, stemDocument)

strwrap(corpus[[1]])

# View the matrix of this corpus
dtm = DocumentTermMatrix(corpus)
# Total of 22167 words in the 855 individual emails, with very high sparsity (99%)

# Therfore, we can exclude these the non sigfnicant words by using the removeSparseTerms function and a max allowed sparsity of 0.995 i.e. only keep 0.5% of total tweets in our corpus
sparse = removeSparseTerms(dtm, 0.97) 

# Create new data frame to incorporate this reduced corpus, with each column representing the 788 different words and the same number of rows as emails (855)
emailsSparse = as.data.frame(as.matrix(sparse))

# Lastly, add in the responsive variable from the original emails data frame that tells us whether the email contents had sensitive words
emailsSparse$responsive = emails$responsive

# Now begin to build the predictive model by splitting the sparse data frame into Training and Testing sets
set.seed(144)
split = sample.split(emailsSparse$responsive, SplitRatio = 0.7)
trainSparse = subset(emailsSparse, split == T)
testSparse = subset(emailsSparse, split == F)

# First, build CART model 
emailsCART = rpart(responsive ~ ., data = trainSparse, method = "class")
prp(emailsCART)
# Make predictions based on the CART model
predic = predict(emailsCART, newdata = testSparse)
predic[1:10,] # display first 10 rows of the prediction, first col shows words that are not responsive and the 2nd row those that are responsive
predic.prob = predic[,2]
table(testSparse$responsive, predic.prob >= 0.5)
# Accuracy of CART model at predicting email responsive or not equal to 25 +195 / (195 + 25 + 17 + 20) = 0.856
table(testSparse$responsive)
# Accuracy of baseline is 215 / 267 = 0.837 showing a small improvment in using CART model for the accuracy

# Plot ROC curve
predicROCR = prediction(predic.prob, testSparse$responsive)
perfROCR = performance(predicROCR, 'tpr', 'fpr')
auc = as.numeric(performance(predicROCR, "auc")@y.values)
plot(perfROCR, colorize = T)
# Area under curve is equal to 0.793. which means this model can predict between a non-responsive and responsive email approx. 79% of the time.
# Best cut off depends on the cost of choosing true positive vs. false positive. 

********************************************
# Detecting Vandalism on Wikipedia - predict whether a revision is genuine edit or vandalism
# vandal is the dependent variable and come be either 1 (edit vandalism) or 0 (not vandalism)
  
wiki = read.csv("wiki.csv", stringsAsFactors = F)
# Build corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
# Remove english-language stop words
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
# Stem the words
corpusAdded = tm_map(corpusAdded, stemDocument)
# Build Document Term Matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
# Matrix shows total of 6675 terms in a total of 3876 entries in Added

# Filter out sparse terms by keeping only the terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)

# Convert this filtered terms set into a new data frame and prepend all the words of this new data frame with the letter A
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
# Words in new data frame equals 166

# Repeat previous 4 steps of creating corpus, remove stopwords, stem words, dtm) for new data frame called called wordsRemoved and prepend all the words with the letter R
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = past("R", colnames(wordsRemoved))
# Words in removed data frame equals 162

# Combine these two data frames into a single data frame
wikiWords = cbind(wordsAdded, wordsRemoved)
# Add Vandal column
wikiWords$Vandal = wiki$Vandal

# Spit data set into training and testing set
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split == T)
test = subset(wikiWords, split == F)
# Accuracy of baseline test set is equal to 618 / 11163 = 0.53

# Build a CART model using the train set (without using cp or minbucket values)
wikiCART = rpart(Vandal ~., data = train, method = "class")
predicCART = predict(wikiCART, newdata= test, type = 'class')
table(test$Vandal, predicCART)
# Accuracy of CART model of predicting vandalism in entry is equal to 618 +12 / 1163 = 0.542
# This shows a slight improvement over the baseline
prp(wikiCART)

# Given the small improvement observed using the CART model, we should look to two other techniques (other than words) to predict if tweets are vandalism
# Look for occurence of http in Added column to detect web addresses that may be the cause of valiasm

wikiWords2 = wikiWords
# Use the grepl function to search for occurence of a string in one string and returns TRUE or FALSE
# ifelse statement returns 1 if http is found in Added column and 0 if absent
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
# Occurence of http (or a link) in revisions equals 217. 

# Split the copied data frame wikiWords2 and split into train and test sets
wikiTrain2 = subset(wikiWords2, split == T)
wikiTest2 = subset(wikiWords2, split == F)

# Build CART model using the split variable
wikiCART2 = rpart(Vandal ~ ., data = wikiTrain2, method = 'class')
predicCART2 = predict(wikiCART2, newdata= wikiTest2, type = 'class')
table(wikiTest2$Vandal, predicCART2)
# New accuracy for this training and testing set with HTTP is equal to 609+57 / 1163 = 0.573
# Compared with the original CART model, there is an increase in accuracy of approx. 3% 

# Find the occurence of words themselves rather than exact occurence of specific words like http using the DTM
# Count words added and removed (from respective DTMs), then create a column for each and add to wikiWords2 data frame
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded)) # Avg. words added is 4.05
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved)) # Avg. words removed is 3.51

# Split the data frame again
wikiTrain3 = subset(wikiWords2, split == T)
wikiTest3 = subset(wikiWords2, split == F)
# Build CART model and prediction accuracy
wikiCART3 = rpart(Vandal ~ ., data = wikiTrain3, method = 'class')
predictCART3 = predict(wikiCART3, newdata = wikiTest3, type = 'class')
table(wikiTest3$Vandal, predictCART3)
# Accuracy is now 514 + 248 / 1163 = 0.655, an improvement over the CART model focused on occurence of words rather than total occurence 

# Finish off by investigating the effect of the two remaining independent variables in the original data frame i.e. Minor and Loggedin
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

# Build CART model and make predictions of accuracy when introducing these two new variables
wikiTrain4 = subset(wikiWords3, split == T)
wikiTest4 = subset(wikiWords3, split == F)
wikiCART4 = rpart(Vandal ~., data = wikiTrain4, method = 'class')
predicCART4 = predict(wikiCART4, newdata = wikiTest4, type = 'class')
table(wikiTest4$Vandal, predicCART4)
# Accuracy is equal to 595 + 241 / 1163 = 0.719, an improvement over the CART3 model by approx. 6.5%
prp(wikiCART4)
# The tree shows that with only an addition of a single branch i.e. minimal increase in complexity, we were able to increase our accuracy from the baseline and other CART models

 *******************************************
# Automating the review of medical journals

trials = read.csv("clinical_trial.csv", stringsAsFactors = F)

# Max length of journal abstract
summary(nchar(trials$abstract)) # 3708 characters
# Number of jounrals with no abstract
sum(nchar(trials$abstract) == 0) # Total of 112
# Journal with shortest title 
which.min(nchar(trials$title)) # Which returns row 1258
trials[1258,1] # Which produces the journal with title "A decade of letrozole: FACE."

# Create a corpus for each of the Title and Abstract variables
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

# Convert corpuses to lowercase
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# Remove stop words
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem words
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# Build Document Term Matrix 
dtmTitle = DocumentTermMatrix(corpusTitle) # 2833 terms in 1860 title entries
dtmAbstract = DocumentTermMatrix(corpusAbstract) # 12224 terms in 1860 entries

# Remove sparse terms with only 5% of those appearing most frequently left
dtmTitle = removeSparseTerms(dtmTitle, 0.95) # Terms reduced to 31 from 2833
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95) # Terms reduced to 335 from 12224

# Convert corpus to data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# Combine dtmTitle and dtmAbstract into a single data frame to create model and make predictions
# Use paste0 function that concatentate strings
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# Combine these two data frames into a single data frame
dtm = cbind(dtmTitle, dtmAbstract)
# Add trials variable as column in new data frame
dtm$trial = trials$trial

# Split data frame into train and test, using split ratio of 0.7
set.seed(144)
split = sample.split(dtm$trial, 0.7)
train = subset(dtm, split == T)
test = subset(dtm, split == F)
table(train$trial)
# Accuracy of baseline of predicting most frequenct outcome in training set (not a trial) is 730 / 1302 = 0.561

# Build CART model and make predictions only on the train set
trialCART = rpart(trial ~., data = train, method = 'class')
trialPredic = predict(trialCART)[,2] # omitt type = 'class' and keep only the second column of the predict output
# Maximum predicted probability for any result is equal to 0.8719
prp(trialCART)
# Tree splits on the title term 'phase' first
table(train$trial, trialPredic >= 0.5)
# Accuracy of the CART model on the training set is 441 + 631 / 1302 = 0.823
# Sensitivity = 441 / 441 + 131 = 0.77 
# Specificity = 631 / 99 + 631 = 0.864

# Now evaluate the CART model on the testing set
predTest = predict(trialCART, newdata = test)[,2]
table(test$trial, predTest >= 0.5)
# Accuracy of testing set for predicting that a result is a trial is (261 + 162)/558 = 0.758
# AUC of the testing set is
testROCR = prediction(predTest, test$trial)
auc = as.numeric(performance(testROCR, "auc")@y.values)
ROCRPperf = performance(testROCR, 'tpr', 'fpr')
# Area under chart is equal to 0.837

**********************************************
# Spam email filtering (spam vs ham)

email = read.csv("emails.csv", stringsAsFactors = F)  
# Total of 5278 emails of which 1368 are flagged as spam
# Emails with single (rather than multiple) starting word of 'Subject' may suggest a spam email

# Email with the longest number of characters
max(nchar(email$text)) # max length = 43952
# Row that contains shortest number of characters
which.min(nchar(email$text)) # Row 1992, with only 13 characters

# Build corpus, convert to lowercase, remove punctuation, remove english stopwords, stem document and create DTM
corpus = Corpus(VectorSource(email$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = TermDocumentMatrix(corpus)
# Total of 28687 terms in 5728 emails

# Limit dtm to contain terms appearing at least 5% of the document. 
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
# Removed terms dtm now shows 330 terms in the 5278 emails.

# Build a data frame from spdtm and use make.names function to make the variable names of the data frame valid
emailSparse = as.data.frame(as.matrix(spdtm))
colnames(emailSparse) = make.names(colnames(emailSparse))
#Word stem that occurs most frequently accross all emails in emailSparse data frame

emailSparse$spam = email$spam
