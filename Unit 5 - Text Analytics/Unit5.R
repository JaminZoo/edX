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
