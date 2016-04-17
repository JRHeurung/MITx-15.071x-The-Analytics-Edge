#Import Motor Vehicle Theft Data Set
mvtWeek1 <- read.csv("~/Documents/Analytics Edge/Unit 1/mvtWeek1.csv")
#Question 1 of Hw - How Many Rows Are in the Dataset?
#Question 2 of HW - How many variables are in the dataset?
str(mvtWeek1)
#Question 3 of HW  - Using the "max" function, 
#what is the maximum value of the variable "ID"?
max(mvtWeek1$ID)
# What is the minimum value of the variable "Beat"?
min(mvtWeek1$Beat)
# How many observations have value TRUE in the Arrest variable
# (this is the number of crimes for which an arrest was made)?
sum(mvtWeek1$Arrest=="TRUE", na.rm=TRUE)
# How many observations have a LocationDescription value of ALLEY?
sum(mvtWeek1$LocationDescription=="ALLEY", na.rm=TRUE)

##
## Problem 2.1 - Understanding Dates in R
##
## In many datasets, like this one, you have a date field. 
## Unfortunately, R does not automatically recognize entries that 
## look like dates. We need to use a function in R to extract 
## the date and time. Take a look at the first entry of Date 
## (remember to use square brackets when looking at a certain entry of a variable).
### In what format are the entries in the variable Date?
head(mvtWeek1)

#Now, let's convert these characters into a Date object in R. 
#In your R console, type

#DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
#This converts the variable "Date" into a Date object in R. 
#Take a look at the variable DateConvert using the summary function.

#What is the month and year of the median date in our dataset? 

#Enter your answer as "Month Year", without the quotes. 
#(Ex: if the answer was 2008-03-28, you would give the answer
#"March 2008", without the quotes.)

DateConvert = as.Date(strptime(mvtWeek1$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

#Now, let's extract the month and the day of the week, 
#and add these variables to our data frame mvt. 
#We can do this with two simple functions. 
#Type the following commands in R:

 mvtWeek1$Month = months(DateConvert)
 mvtWeek1$Weekday = weekdays(DateConvert)

#This creates two new variables in our data frame, Month and 
#Weekday, and sets them equal to the month and weekday 
#values that we can extract from the Date object. 
#Lastly, replace the old Date variable with DateConvert by typing:

mvtWeek1$Date = DateConvert

#### Using the table command, answer the following questions.

#### In which month did the fewest motor vehicle thefts occur?
min(table(mvtWeek1$Month))

### On which weekday did the most motor vehicle thefts occur?
table(mvtWeek1$Weekday)
max(table(mvtWeek1$Weekday))

### Each observation in the dataset represents a motor vehicle theft, 
### and the Arrest variable indicates whether an arrest was later made for this theft. 
### Which month has the largest number of motor vehicle 
### thefts for which an arrest was made?

table(mvtWeek1$Month, mvtWeek1$Arrest)

#### Problem 3.1 - Visualizing Crime Trends

#Now, let's make some plots to help us better understand how crime has 
#changed over time in Chicago. 
#Throughout this problem, and in general, you can save your plot to a file. 
#For more information, this website very clearly explains the process.

#First, let's make a histogram of the variable Date. 
#We'll add an extra argument, to specify the number of bars we want in our histogram.

#In your R console, type
hist(mvtWeek1$Date, breaks=100)

#Looking at the histogram, answer the following questions.

#In general, does it look like crime increases or decreases from 2002 - 2012?

### Create a Boxplot of the Variable
#Now, let's see how arrests have changed over time. 
#Create a boxplot of the variable "Date", 
#sorted by the variable "Arrest" 
#(if you are not familiar with boxplots and would 
#like to learn more, check out this tutorial). 

boxplot(mvtWeek1$Date ~ mvtWeek1$Arrest)

# 3.3
# For what proportion of motor vehicle thefts in 2001 was an arrest made?
#Note: in this question and many others in the course, we are asking for 
#an answer as a proportion. 
#Therefore, your answer should take a value between 0 and 1.

table(mvtWeek1$Year, mvtWeek1$Arrest, mvtWeek1$Year=="2001")
2152/(18517+2152)

table(mvtWeek1$Year, mvtWeek1$Arrest, mvtWeek1$Year=="2012")
550/(13542+550)


#Problem 4.1 - Popular Locations
# Which locations are the top five locations for motor vehicle thefts, 
#excluding the "Other" category? 
#You should select 5 of the following options.

#Decreasiong = TRUE sorts the results from greatest to smallest
sort(table(mvtWeek1$LocationDescription), decreasing=TRUE)

156564+14852+2308+2111+1675

Top5 = subset(mvtWeek1, mvtWeek1$LocationDescription=="STREET" | mvtWeek1$LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | mvtWeek1$LocationDescription=="ALLEY" |mvtWeek1$LocationDescription=="GAS STATION" |mvtWeek1$LocationDescription=="DRIVEWAY - RESIDENTIAL")

Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

#Alley
249/(2059+249)
#Driveway
132/(1543+132)
#Gas Station
439/(1672+439)
#Parking Lot
1603/(13249+1603)
#Street
11585/(144969+11595)


## On which day of the week do the most motor vehicle thefts at gas stations happen?
GasStation=subset(Top5,Top5$LocationDescription=="GAS STATION")
str(GasStation)
max(table(GasStation$Weekday))


## On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
Driveway=subset(Top5,Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL")
str(Driveway)
table(Driveway$Weekday)
min(table(Driveway$Weekday))

## Alternatively, use the formula below to create one large table rather than relying 
## on a number of subsets.
table(Top5$LocationDescription, Top5$Weekday)
