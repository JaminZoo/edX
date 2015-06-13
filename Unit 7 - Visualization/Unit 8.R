# Data Visualization 

# 1. Data
# 2. Aesthetic mapping - color, shape, size of variables mapped to graphical attributes
# 3. Geometric objects - type of plot e.g. boxplote, scatter, histogram


# World Health Organisation

who = read.csv("WHO.csv")
FertilityGNIPlot = ggplot(who, aes(x = GNI, y = FertilityRate ))

FertilityGNIPlot + geom_point(color = "salmon2", size = 2, shape = 19) +
  ggtitle("Fertility Rate vs Gross National Income")

# Create plot file and save to working directory folder
pdf("Myplot.pdf") # Name of plot file
print(FertilityGNIPlot)
dev.off()

# Output all available color options for plot
colors()

ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) +
  geom_point(size = 3)

ggplot(who, aes(x = log(FertilityRate), y = Under15)) +
  geom_point(size = 3) + stat_smooth(method = "lm", se=FALSE) # stat_smooth plots linear model
# level = 0.99 returns a 99% confidence interval range either side of linear regression line
# se = FALSE returns just the linear regression line
model = lm(Under15 ~ log(FertilityRate), data = who)

ggplot(who, aes(x = FertilityRate, y = Under15, color = Region)) +
  geom_point(size = 3) + scale_color_brewer(palette="Dark2") 

# The analytical policeman - Law and Order

mvt = read.csv("mvt.csv", stringsAsFactors = F)

# Convert date variable into appropriate data type
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M" )
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Create data frame of crimes on each day of the week
table(mvt$Weekday)
crimeWeekday = as.data.frame(table(mvt$Weekday)) # Allows us to produce plots using ggplot
names(crimeWeekday) = c("Day", "Freq") # Rename data frame column headers with appropriate names
# Convert Day variable into ORDERED factor so that it can be plotted in correct order i.e. Sunday to Saturday
crimeWeekday$Day = factor(crimeWeekday$Day, ordered = T, levels = c("Sunday", "Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday")) 

ggplot(crimeWeekday, aes(x = Day, y = Freq)) + 
  geom_line(aes(group = 1), alpha = 0.3, linetype = 2) + # groups data in one single line that is dashed and shaded in appearance
  xlab("Day of the Week") + 
  ylab("Total Motor Vehicle Thefts")

# Build heatmap that shows the time and day of motor vehicle thefts
crimeDayHour = as.data.frame(table(mvt$Weekday, mvt$Hour))
names(crimeDayHour) = c("Day", "Hour", "Freq")
# Convert Hour into numerical data type (from a factor variable)
crimeDayHour$Hour = as.numeric(as.character(crimeDayHour$Hour))
# Plot each day and the corresponding thefts over each hour of the day
ggplot(crimeDayHour, aes(x=Hour, y = Freq)) + 
  geom_line(aes(group = Day, color = Day), size = 2)

# Convert days of week to ordered/chronological order using factor function
crimeDayHour$Day = factor(crimeDayHour$Day, ordered = T, levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))

# Create heat map using geom_tile function on the Hour and Day variables, with fill equal to frequency of crimes
ggplot(crimeDayHour, aes(x= Hour, y = Day)) + 
  geom_tile(aes(fill = Freq)) + # fill plot with frequency observations
  scale_fill_gradient(name = "Total mv thefts", low = "white", high = "tomato3") + # choose color for low/high occurence of variable being investigated
  theme(axis.title.y = element_blank()) # gets rid of one of the axis labels (y-axis in this case)
# For each hour and each day, there is a rectangle associated with this point. 

# Separate days into type weekend and weekday
crimeDayHour$Type = ifelse((crimeDayHour$Day == "Sunday") | (crimeDayHour$Day == "Saturday"), "Weekend", "Weekday")

# Plot crime on an actual map by installing maps and ggmap packages
install.packages("maps")
install.packages("ggmap")

# use get_map function to download from Google Maps desired map
chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago) # outputs map of the city in plot window

# Apply the first 100 points of the mvt data frame to the map (ggmap is equivalent to ggplot but for map data)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

# Identify which areas have highest occurence of crimes based on Long and Lat from mvt data frame
# Create data frame that rounds to 2 sig. figures the Longtitude and Latitude of each crime obsevaion
crimeLatLong = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))

# Convert Lat and Long to numeric
names(crimeLatLong) = c("Longtitude", "Latitude", "Freq")
crimeLatLong$Longtitude = as.numeric(as.character(crimeLatLong$Longtitude))
crimeLatLong$Latitude = as.numeric(as.character(crimeLatLong$Latitude))

# Plot crimes based on size and color of occurence
ggmap(chicago) + geom_point(data = crimeLatLong, aes(x = Longtitude, y = Latitude,
                                                      color = Freq, size = Freq)) +
  scale_color_gradient(low = "yellow", high = "tomato3")

# Produce heat map using geom_tile using alpha = frequency
ggmap(chicago) + geom_tile(data = crimeLatLong, aes(x = Longtitude, y = Latitude, alpha = Freq),
                           fill = "tomato3") 

# Consider only those Long and Lat locations that have frequency > 0 i.e. create subset of crimeLatLong that excludes Freq = 0
crimeLatLong2 = subset(crimeLatLong, Freq > 0)
ggmap(chicago) + geom_tile(data = crimeLatLong2, aes(x = Longtitude, y = Latitude, alpha = Freq),
                           fill = "tomato3") 
# Total of 952 observations now removed that had Freq = 0, leaving 686 observations in new data set

# Murders in the U.S by state
murders = read.csv("murders.csv")

# Load U.S map (preloaded in R)
statesMap = map_data("state") # data frame that includes the long and lat of state lines
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# Create new variable named Region that stores the lower case of all the states in order to compare with map values also being lower case
murders$region = tolower(murders$State)

# Join murder and statesMap data frames using merge function on the region variable
murdersMap = merge(statesMap, murders, by = "region")

# Plot murders on U.S map based on color. 
ggplot(murdersMap, aes(x = long, y = lat, group = group, fill = Murders)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "gray", high = "tomato", guide = "legend")
# Plot appears similar to a population density map, need to investigate murder rate

# Create new variable MurderRate in the murdersMap data frame that equals murders divided by 100,000 head of population
murdersMap$MurderRate = murdersMap$Murders / murdersMap$Population * 100000

ggplot(murdersMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "gray", high = "salmon2", guide = "legend")
# No real contrast between states, this is due to outlier state of Washington D.C having a very high murder rate (21) vs. mean of 4.67

# Limit the murder rate to maximum of 10 murders per 100,000 population (remove Washington DC murder rate)
ggplot(murdersMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "gray", high = "salmon2", guide = "legend", limits = c(0,10))

# Heat map of gun ownership
ggplot(murdersMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

************************************************************
# Good and Bad Visualization
  
intl = read.csv("intl.csv")

ggplot(intl, aes(x = Region, y = PercentOfIntl)) +
  geom_bar(stat = "identity") + # statistical transformation to used on the data e.g. identity maps height based on y-values
  geom_text(aes(label = PercentOfIntl)) # Adds label on top of each bar corresponding to y-value (PercentOfIntl)

# Change Region to ordered factor to plot Region by decreasing PercentOfIntl value
intl = transform(intl, Region = reorder(Region, -PercentOfIntl)) # reorder takes its first argument as categorical variable, and reorders its levels based on values of second variable

# Convert to 100% for all PercentOfIntl values
intl$PercentOfIntl = intl$PercentOfIntl * 100

ggplot(intl, aes(x = Region, y = PercentOfIntl)) +
  geom_bar(stat = "identity", fill = "salmon2") + 
  geom_text(aes(label = PercentOfIntl), vjust = -0.5) + 
  ylab("Percent of Int. Students") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1))
  
# Student data for all countries in the World

intall = read.csv("intlall.csv", stringsAsFactors = F)

# Convert all N/A terms to 0 
intall[is.na(intall)] = 0

# Load world map
worldMap = map_data("world")

# Merge world map and intall data frames using the Region and Citizenship variables respectively
worldMap = merge(worldMap, intall, by.x = "region", by.y = "Citizenship")

# Plot world map
ggplot(worldMap, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_map("mercator")

# reorder groups of worldMap so it can be plotted based on border of each region
worldMap = worldMap[order(worldMap$group, worldMap$order),]
# Now worldMap data frame sorts Order column ascending 
# replot world map. 

# Convert name of China in intall to match that of worldMap region i.e. PRC to China
intall$Citizenship[intall$Citizenship == "China (People's Republic Of)"] = "China"

# Perform merge again on intall and map_data data frames with updated China region
worldMap = merge(map_data('world'), intall, by.x = "region", by.y = "Citizenship")
# Perform reordering of order column in worldMap
worldMap = worldMap[order(worldMap$group, worldMap$order),]


# Plot worldMap


# Plot worldMap using orthograthic projection with custom 3D orientation to view certain angle of world region
ggplot(worldMap, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Total), color = "black") +
  coord_map("ortho", orientation = c(-5,130,0))

# Households data 
house = read.csv("households.csv")

# Melt coverts a 2D data frame into a form acceptable to ggplot
# Doing this converts every value in the data frame into its own row, rather than segmented in its own column
house = melt(house, id = "Year")

# Now we can use ggplot to assign x, y variables according to melt output
ggplot(house, aes(x = Year, y = value, color = variable)) +
  geom_line(size = 2) +
  geom_point(size = 5)

***********************************************
# Election forecasting - plotting results of prediction model conducted in Unit 3 on map 
  
statesMap = map_data("state")
# Variable region defines the boundary of each state, and the variable order defines the order each of the long/lat points should be connected

# plot map of U.S
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# Load in Polling data set and split into train and test sets
polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year >= 2004 & Year <= 2008)
Test = subset(polling, Year == 2012)

# Create logistic regression model using the train set
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial")

# Create prediction model using the test set (2012)
TestPrediction = predict(mod2, newdata = Test, type = "response")
table(Test$Republican, TestPrediction >= 0.5)

# Create a vector for the Republican/Democrate predictions
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# Place prediction output into new data frame along with this binary vector and the States
predictionDF = data.frame(TestPrediction, TestPredictionBinary, Test$State)
# There are a total of 22 Republican and 23 Democrate predicted outomes

# Merge prediction and statesMap data frames
predictionDF$region = tolower(predictionDF$Test.State) # First, convert State values to lower case to match statesMap Region column
predictMap = merge(statesMap, predictionDF, by="region") # Merge by common variable 'region', default setting is to exclude observations not in second data frame but are foudn in the first
predictMap = predictMap[order(predictMap$order),] # Re order map 'Order' variable to be sorted in ascending order
# Observations of predictMap will be lower than statesMap due to dropping a number of states

# Plot state map showing location of prediction results
ggplot(predictMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black") 
# This gives us gradient output of predicted outcomes i.e. between 0 and 1, we would like to observe a discrete outcome

# Plot map that shows discrete outcomes showing one of two colors (Red and Blue) rather than gradient of colors
ggplot(predictMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Plot of predicted outcomes on map
ggplot(predictMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", linetype = 3, alpha = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# The two maps (TesetPredictionBinary and TestPrediction) appear very similar due to the closesness of both variables to each other
# we don't have a single predicted probability between 0.065 and 0.93.

****************************************
# Social Network vizualisation
# Verticies (nodes) represent facebook user, Edges represent link between other users/friends

# Contain row observations that include endpoints V1 and V2 representing connection bewteen two users
edges = read.csv("edges.csv") 

# Contains the verticies or facebook users in the network
users = read.csv("users.csv")

# Average number of friends determined by considering each edge or connection in the edge data frame (146 observations)
# Double this value since we need to consider both friend A and friend B together
# Divide by total number of users: 292/59 = 4.949

install.packages("igraph") # creats igraph from data frames or vice-versa
# igraph takes data frame containing an edge list in first argument, then whether graph is directed (edges only point in one direction) and final argument containing verticies

# Use igraph package to create graph using data frames edges and users, where directed argument is FALSE as we have bi-directional edge to edge
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
# There are a number of groups of nodes where all the nodes in each group are connected 
# but the groups are disjoint from one another, forming "islands" in the graph. Such groups are called "connected components," or "components" for short. 
# There are a total of 4 components in this network - one large connected component, and three smaller components

# The degree of each node/vertex is the number of friend the user has with others
# use degree(g) to produce output of all degrees(friends) for each user
degree(g)

# Highlight the nodes(facebook users) that have higher a degree on the plot by changing the size of the verticies to be a function of their degrees i.e. larger verticies correspond to larger degrees
V(g)$size = degree(g)/2+2 # V() is a function that allows you to perform operationon a subset of verticies of edges in  a graph
plot(g, vertex.label = NA) # Exclude the vertex.size argument since we have just defined it for our particular application
# plot shows that the users with more friend connections (degrees) as having larger verticies and those with few or none displaying very small vertex sizes
# largest vertex was assigned a value of 18/2 + 2 = 11 and smallest was 0/2 + 2 = 2

# Change the color of verticies based on geneder of user using the same V(g) function, but using color instead of size
# We can access the attribuets of the users data frame within the graph as this was created when graph.data.frame was used
V(g)$color = "black" # color all verticies black (shows gender that is undefined)
V(g)$color[V(g)$gender == "A"] = "red" # color all gender = A red
V(g)$color[V(g)$gender == "B"] = "gray" # color all gender = B gray

# Color each vertice based on school attended
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
# Plot shows that the two users who went to schools AB are also friends (share an edge) and that users with large degrees (Friends) attended school A and no school


# Color each vertex based on locale
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
# 3D plot using rgl package and rplplot function
install.packages("rgl")
rglplot(g, vertex.label=NA)

**********************************************
# Creating world clouds of text data from tweets data set (used in early problem set)
# A word cloud arranges the most common words in some text, using size to indicate the frequency of a word.  

tweets = read.csv("tweets.csv", stringsAsFactors = F)
  
# Create corpus and perform text mining
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus)
tweetsDF = as.data.frame(as.matrix(dtm))
# There are 3780 unique words in the corpus of 1181 tweets

install.packages("worldcloud")

wordcloud(colnames(tweetsDF), colSums(tweetsDF)) # takes the number of words in each tweet as first argument and frequency of these words as second
# Shows that the word apple is the most frequent word in the tweets (identified as largest word in wordcloud)

# Rebuild corpus but with the apple word removed
corpus2 = Corpus(VectorSource(tweets$Tweet))
corpus2 = tm_map(corpus, tolower)
corpus2 = tm_map(corpus, PlainTextDocument)
corpus2 = tm_map(corpus, removePunctuation)
corpus2 = tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm2 = DocumentTermMatrix(corpus2)
tweetsDF = as.data.frame(as.matrix(dtm2))
wordcloud(colnames(tweetsDF), colSums(tweetsDF), c(2,0.25), max.words = 150, random.color = TRUE, colors = brewer.pal(9,"YlOrRd")[3:9])
# iPhone is now the most frequent word after applie is removed from corpus

