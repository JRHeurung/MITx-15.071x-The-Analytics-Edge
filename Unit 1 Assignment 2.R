####
#DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES
#In the wake of the Great Recession of 2009, there has been a good deal of focus on employment statistics, 
# one of the most important metrics policymakers use to gauge the overall strength of the economy. In the United States, 
# the government measures unemployment using the Current Population Survey (CPS), 
# which collects demographic and employment information from a wide range of Americans each month. 
# In this exercise, we will employ the topics reviewed
# in the lectures as well as a few new techniques using the September 2013 version of this rich, nationally representative dataset (available online).
#The observations in the dataset represent people surveyed in the September 2013 CPS who actually completed a survey. While the full dataset has 385 variables, 
# in this exercise we will use a more compact version of the dataset, CPSData.csv, 
####  which has the following variables:
 #PeopleInHousehold: The number of people in the interviewee's household.
 #Region: The census region where the interviewee lives.
 #State: The state where the interviewee lives.
 #MetroAreaCode: A code that identifies the metropolitan area in which the interviewee lives (missing if the interviewee does not live in a metropolitan area). The mapping from codes to names of metropolitan areas is provided in the file MetroAreaCodes.csv.
 #Age: The age, in years, of the interviewee. 80 represents people aged 80-84, and 85 represents people aged 85 and higher.
 #Married: The marriage status of the interviewee.
 #Sex: The sex of the interviewee.
 #Education: The maximum level of education obtained by the interviewee.
 #Race: The race of the interviewee.
 #Hispanic: Whether the interviewee is of Hispanic ethnicity.
 #CountryOfBirthCode: A code identifying the country of birth of the interviewee. The mapping from codes to names of countries is provided in the file CountryCodes.csv.
 #Citizenship: The United States citizenship status of the interviewee.
 #EmploymentStatus: The status of employment of the interviewee.
 #Industry: The industry of employment of the interviewee (only available if they are employed).


#Define the CPSData as data
data <-CPSData

## Problem 1.1
# How Many People were interviewed?

str(CPSData)

## Problem 1.2
# Which state has the fewest interviewees?

sort(table(CPSData$State))


#1.4 
# What proportion of interviewees are citizens of the United States?

table(CPSData$Citizenship)

# Citizen, Native Citizen, Naturalized          Non-Citizen 
# 116639                 7073                 7590

(116639+7030)/ (116639+7073+7590)

# For which races are there at least 250 interviewees in the CPS dataset 
# of Hispanic ethnicity? (Select all that apply.)

table(CPSData$Race, CPSData$Hispanic)

# Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPSData)

### Problem 2.2
# Which is most true

#compare region to null married
table(CPSData$Region,is.na(CPSData$Married))
# compare age to null married
table(CPSData$Age,is.na(CPSData$Married))
# compare sex to null married
table(CPSData$Sex,is.na(CPSData$Married))


# How many states had all interviewees living in a non-metropolitan area (aka they 
# have a missing MetroAreaCode value)? For this question, treat the District of Columbia 
# as a state (even though it is not technically a state).

table(CPSData$State, is.na(CPSData$MetroAreaCode))


# Which region of the United States has the largest proportion of 
# interviewees living in a non-metropolitan area?

table(CPSData$Region, is.na(CPSData$MetroAreaCode))

#Midwest   
10674/(20010+10674)
#Northeast 
5609/(20330+5609)
#South     
9871/(31631+9871)
#West      
8084/(25093+8084)

# USe tapply to calculate the proportions rather than manual calculations
sort(tapply(is.na(data$MetroAreaCode), data$State, mean))

# Bring in new data sets: MetroAreaCodes and CountryCodes
# 
> MetroAreaCodes <- read.csv("//dhsna01/Personal$/jheuru/My Documents/Josh's Work/R/MetroAreaCodes.csv")
>   View(MetroAreaCodes)
> CountryCodes <- read.csv("//dhsna01/Personal$/jheuru/My Documents/Josh's Work/R/CountryCodes.csv")
>   View(CountryCodes)
     
Metro <- MetroAreaCodes
Country <- CountryCodes

str(Metro)
str(Country)

# Create One Large Data Frame

CPS = merge(CPSData, MetroAreaCodes, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)

# Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea), decreasing=TRUE)

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 
# Hint: Use tapply() with mean, as in the previous subproblem. Calling sort() 
# on the output of tapply() could also be helpful here

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing=TRUE)

#Problem 3.5
#Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether 
#an interviewee is Asian, determine the number of metropolitan areas 
#in the United States from which at least 20% of interviewees are Asian.

sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean), decreasing=TRUE)

#Remove Null Education Variables
na.rm=TRUE
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea,na.rm=TRUE, mean))

#MErge CountryMap onto the CPS data frame and create CPS 2
CPSCountry = merge(CPS, CountryCodes, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
head(CPSCountry)

table(CPSCountry$Country, is.na(CPSCountry$Country))
str(CPSCountry)
summary(CPSCountry)
sort(table(CPSCountry$Country))

#Problem 4.3
# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, 
# NY-NJ-PA" 
# metropolitan area have a country of birth that is not the United States? 
 
table(CPSCountry$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPSCountry$Country != "United States")

# Problem 4.4 - Integrating Country of Birth Data
#Only people born in INdia
sort(tapply(CPSCountry$Country =="India", CPSCountry$MetroArea, sum, na.rm=TRUE))

#ONly People Born in Brazil
sort(tapply(CPSCountry$Country=="Brazil", CPSCountry$MetroArea, sum, na.rm=TRUE))
