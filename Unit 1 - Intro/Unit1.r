who <- read.csv("WHO.csv")

library(ggplot2)
ggplot(aes(x = GNI, y = FertilityRate, color = Country),
       data = subset(who, who$FertilityRate > 2.5 & who$GNI > 10000)) + 
         geom_point(size = 5)

####USA Food Health Information####
usda <- read.csv("USDA.csv")
testxml <- readHTMLTable("http://www.w3schools.com/css/css_table.asp", which = 1)

####Chicago Motor Vehicle Offences####
mvt<- read.csv("mvtWeek1.csv")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert) #extracts months from DateConvert variable
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert #Data frame Date variable now has class type "Date"

Top5 = subset(mvt, LocationDescription=="STREET" | 
                LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" |
                LocationDescription=="ALLEY" |
                LocationDescription=="GAS STATION" |
                LocationDescription=="DRIVEWAY - RESIDENTIAL")

ggplot(aes(x = Var2, y = ), data = mvt_ratio) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(2000, 2014), breaks = c(2000:2014,2))

####Historical Stock Prices####
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcGam = read.csv("ProcterGambleStock.csv")
Coke = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
Coke$Date = as.Date(Coke$Date, "%m/%d/%y")
ProcGam$Date = as.Date(ProcGam$Date, "%m/%d/%y")

ggplot() + 
  geom_line(aes(x = Date, y = StockPrice, color = "Coke"), data = Coke) +
  ylab("Price") +
  geom_vline(aes(xintercept = as.numeric(Coke$Date[c(408,420)])), 
                 linetype = 2) +
  geom_line(aes(x = Date, y = StockPrice, color = "Boeing"), data = Boeing) +
  geom_line(aes(x = Date, y = StockPrice, color = "IBM"), data = IBM) +
  geom_line(aes(x = Date, y = StockPrice, color = "GE"), data = GE) +
  geom_line(aes(x = Date, y = StockPrice, color = "Proctor & Gamble"), data = ProcGam) +
  scale_x_date(limits = as.Date(c('1995-01-01','2005-01-01')))
    
 newData = melt(list(Boeing, Coke, GE, IBM, ProcGam, id =c("Date"),as.Date(origin = "1970-01-01")))
#Determine the average monthly stock price for IBM over the historical data
tapply(GE$StockPrice, months(GE$Date), mean) - mean(GE$StockPrice)

####Demongraphics and Employment Dataset####

cps = read.csv("CPSData.csv")
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

#Finds the country code for a given country name
countryCodes$Code[match("India", countryCodes$Country)]
#Find the country given a country code
countryCodes$Country[match("460", countryCodes$Code)]
#Lists the country codes in decending order based on country code
sort(table(cps$CountryOfBirthCode), decreasing = T)
#Given a country code, outputs the number of people born in that country
table(cps$CountryOfBirthCode == "210")
#Determine the states which had all interviewees living in non-metro area i.e. "NA" value 
table(cps$State, is.na(cps$MetroAreaCode))
#Calculate using tapply and mean function the proportion of interviewees
#that were living in non-metro area i.e. "NA" for each region
tapply(is.na(cps$MetroAreaCode), cps$Region, mean)
#Proportion of Asians in each metro area
sort(tapply(cps$Race == "Asian", cps$MetroArea, mean), decreasing = T)

#merges the cps and MetroAreaMap dataframes by a common area code variable
cps = merge(cps, MetroAreaMap, by.x = "MetroAreaCode", by.y = "Code", all.x = T)
#merges the cps and CountryMap dataframes by a common country code variable
cps = merge(cps, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x =T)


####Interenet Privacy Poll####
Poll = read.csv("AnonymityPoll.csv")
#Number of interviewees from each South Region by individual state
sort(table(subset(Poll, Region == "South")$State))