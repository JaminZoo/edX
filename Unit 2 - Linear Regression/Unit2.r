#### Wine Linear Regression Model

wine = read.csv("wine.csv")
wineTest = read.csv("wine_test.csv")

#use linear model functin and memisc package to run multi-variable LR
m1 <- lm(I(Price) ~ I(AGST), data = wine)
m2 <- update(m1, ~ . + WinterRain)
m3 <- update(m2, ~ . + FrancePop)
m4 <- update(m3, ~ . + HarvestRain)
m5 <- update(m4, ~ . + Age)
mtable(m1, m2, m3, m4, m5)

# Alternative approach for multiple independent variables
model1 <- lm(Price ~ AGST + HarvestRain + Age + WinterRain,
             data = wine)
#Produce a summary of the LM, showing coefficients, standard error and Pr
#Signfic. codes i.e. star rating indicate how important each coeff. of the variable is 
summary(model1)

# Determine the sum of the standard errors (residuals)
sum(model1$residuals)
#Use the predict function with the linear model to predict the price of wine with the two new variables from wineTest data frame
winePredict = predict(model1, newdata = wineTest)
# Calculate the sum of square errors for both the obsereved (wineTest) and expected (linear model) prices
SSE = sum((wineTest$Price - winePredict)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
#R squared is then calculated:
1 - SSE/SST
******************************************************
#### Baseball data
baseball = read.csv("baseball.csv")
#Create new data frame that includes only the years relevant to moneyball story
moneyball = subset(baseball, baseball$Year <2002)
#Create new variable in moneyball data frame that calculates difference between RS and RA
moneyball$RD = moneyball$RS - moneyball$RA
ggplot(aes(x = RD, y = W), data = moneyball) + 
  geom_point(type = 2)
#Create linear model of Wins with independent variable of RD to predict number of wins
WinsModel = lm(W ~ RD, data = moneyball)
summary(WinsModel)

#Create linear model of RS with independent variables of OBP, SLG and BA to predict number of runs scored
RunsModel = lm(RS ~ OBP + SLG +BA, data = moneyball)
summary(RunsModel)

#Create a rank vector corresponding to how each team will finish in the MLB playoffs
teamRank = c(1,2,3,3,4,4,4,4,5,5)
#Create a win vector corresponding to the wins each team in the playoffs had in regular reason
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
********************************************************
#### NBA stats
nba = read.csv("NBA_train.csv")
nbaTest =read.csv("NBA_test.csv")
ggplot(aes(x = W, y = PD), data = nba) + 
  geom_point()
WinsModel = lm(W ~ PD, data = nba)
PointsModel = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL ,data = nba)
OppModel = lm(oppPTS ~ DRB + ORB + AST + STL + TOV + FGA + FT, data = nba)

#Find residuals of the linear model 
PointsModel$residuals
#Find SSE (sum of squared error)
SSE = sum(PointsModel$residuals^2)
#Root mean square error is more interpurtable result
RMSE = sqrt(SSE/nrow(nba)) # 184.4 point error given an average of 8370 points per season
#Make predictions for number of points a team will score based on test data
PointsPrediction = predict(PointsModel, newdata = nbaTest)
OppPrediction = predict(OppModel, newdata = nbaTest)
# Calculated out of sample R squared value using sample population of nbaTest
SSE = sum((PointsPrediction - nbaTest$PTS)^2)
SST = sum((nbaTest$PTS - mean(nba$PTS))^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(nbaTest))
*********************************************
### Climate Change
cc = read.csv("climate_change.csv")
# Split the data into two sets, one to calcualte the LM and the other as a test
ccModel = subset(cc, cc$Year <= 2006)
ccTest = subset(cc, cc$Year >2006)

# Create the linear model with all variables
ccModel1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
              data = ccModel)
#Many of the independent variables are correlated with each other, a reason why the N2O variable has negative coeff.
# Create new model focused on N2O variable and exclude the other independent varaibles that are highly correlated with it
ccModel2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = ccModel)
# Use the step function to try to determine the combinations of variables to find a good model by AIC
ccModel1.Step = step(ccModel1)
# Use the stepped model and the predict function to determine test R squared
ccPredict = predict(ccModel1.Step, newdata = ccTest)
SSE = sum((ccPredict - ccTest$Temp)^2)
SST = sum((ccTest$Temp - mean(ccModel$Temp))^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(ccModel))

ggplot(aes(x = Year, y = Temp), data = ccModel) + 
  stat_smooth(method = "lm")
**************************************************
### Reading test scores
pisaModel = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")\

# Mean reading score for each gender
tapply(pisaModel$readingScore, pisaModel$male, mean)
# Omit all variables that contain N/A from each data frame
pisaModel = na.omit(pisaModel)
pisaTest = na.omit(pisaTest)
#Include unordered factor (raceeth) in a linear regression model by defining
# a reference level and creating a binary variable for each of the remaining levels
#Choose most frequently occuring variable as the reference level - White in this case
# Set reference level
pisaModel$raceeth = relevel(pisaModel$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
# Create linear model and determine SSE, SST and RMSE
readingModel1 = lm(readingScore ~ ., data = pisaModel)
SSE = sum((readingModel1 - pisaModel$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaModel))
# Predict scores using test data frame
scorePredict = predict(readingModel1, newdata = pisaTest)
# SSE and RMSE for test data frame
SSE = sum((scorePredict- pisaTest$readingScore)^2)
SST = sum((mean(pisaModel$readingScore) - pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))
**********************************************
### Flu epidemics via search engine query data, using simple time series w/lag 
fluTrain = read.csv("FluTrain.csv")
fluTest = read.csv("FluTest.csv")
ggplot(aes(y = log(ILI), x = Queries), data = fluTrain) + 
  geom_point() + 
  stat_smooth(method = "lm")

# Create linear model with log of ILI variable
fluModel1 = lm(log(ILI) ~ Queries, data = fluTrain)
# Correlation of ILI and Queries
cor(fluTrain$Queries, log(fluTrain$ILI))
# Preict ILI using test flu data 
fluPredict = exp(predict(fluModel1, newdata = fluTest))
# RMSE between estimate ILI and observed from test data
SSE = sum((fluPredict - fluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(fluTest))

# Use Zoo package to create time series data for ILI variable that uses past ILI data to predict future value
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad = T)
# Create new variable ILILag2 in fluTrain data frame
fluTrain$ILILag2 = coredata(ILILag2)
# Plot log ILILag2 vs log of ILI
ggplot(aes(x = log(ILI), y = log(ILILag2)), data = fluTrain) + 
  geom_point()
fluModel2 = lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
# Add lag to ILI for test data set
ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad = T)
fluTest$ILILag2 = coredata(ILILag2)
# Copy ILI value from fluTrain to ILILag2 value in fluTest
fluTest$ILILag2[1] = fluTrain$ILI[416]
fluTest$ILILag2[2] = fluTrain$ILI[417]
# Get new predictions from fluModel2 
fluPredict2 = exp(predict(fluModel2, newdata = fluTest))
SSE = sum((fluPredict2 - fluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(fluTest))
*******************************************
### Hyudnai Elantra Car Sales
elantra = read.csv("elantra.csv")
elantraTrain = subset(elantra, elantra$Year <= "2012")
elantraTest = subset(elantra, elantra$Year > "2012")

salesModel1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy 
                 + Queries, data = elantraTrain)
# Create another linear model that incorporates the Month variable to analyse seasonality on sales
salesModel2= lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy 
                 + Queries + Month, data = elantraTrain)
# Model the month variable as a factor so that the effect of each month is not restricted to be linear
elantra$Month = factor(elantra$Month)
# Remove highest p-value variable from model2 until there are no insig variables
salesModel3= lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy 
                + Month, data = elantraTrain)
# Predictions, SSE, SST and R^2 calculation
salesPredict = predict(salesModel3, newdata = elantraTest)
SSE = sum((salesPredict - elantraTest$ElantraSales)^2)
SST = sum((elantraTest$ElantraSales - mean(elantraTrain$ElantraSales))^2)
R = 1 - SSE/SST
# Baseline model predict
mean(elantraTrain$ElantraSales)

ggplot(aes(x = Month, y = ElantraSales), data = elantra) + 
  geom_point(aes(color = Year))