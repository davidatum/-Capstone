#####DATASET 1: TPS Homicide Data, Descriptive Analytics


library(readxl)
TPS_Homicide_2004_2018 <- read_excel("C:/Users/Admin/Desktop/CKME136/Final Data/TPS Homicide 2004-2018.xlsx")
#TPS_Homicide_2004_2018 <- read_excel(file.choose())
View(TPS_Homicide_2004_2018)


#View Data
TPS_Homicide_20014_2018clean <- na.omit(TPS_Homicide_2004_2018)
str(TPS_Homicide_20014_2018clean)
summary(TPS_Homicide_20014_2018clean)
dim(TPS_Homicide_20014_2018clean)
sapply(TPS_Homicide_20014_2018clean, class)
sapply(TPS_Homicide_20014_2018clean[,1:13],sd)
names(TPS_Homicide_20014_2018clean)
head(TPS_Homicide_20014_2018clean)
tail(TPS_Homicide_20014_2018clean)
rowSums(is.na(TPS_Homicide_20014_2018clean))




sapply(TPS_Homicide_20014_2018clean,function(x) length(unique(x)))  ### <---- Added by Riyad
length(unique(TPS_Homicide_20014_2018clean$Occurrence_year))
length(unique(TPS_Homicide_20014_2018clean$Homicide_Type))
length(unique(TPS_Homicide_20014_2018clean$Hood_ID))

table(TPS_Homicide_20014_2018clean$Occurrence_year)
table(TPS_Homicide_20014_2018clean$Homicide_Type)
table(TPS_Homicide_20014_2018clean$Hood_ID)
table(TPS_Homicide_20014_2018clean$Homicide_Type)

#Visualize Data by Homicide Type

barplot(table(TPS_Homicide_20014_2018clean$Homicide_Type))
### barplot(table(TPS_Homicide_20014_2018clean$Homicide_Type), title = 'Frequency of Homicide Types', col = red)  ### <---- Gives Error
barplot(table(TPS_Homicide_20014_2018clean$Homicide_Type), title = 'Frequency of Homicide Types', col = 'red')
barplot(table(TPS_Homicide_20014_2018clean$Homicide_Type),col=heat.colors(3))

#Homicide Type vs. Year


xtable_Homicide_Year <- xtabs(~ TPS_Homicide_20014_2018clean$Homicide_Type + TPS_Homicide_20014_2018clean$Occurrence_year)
xtable_Homicide_Year_Hood <- xtabs(~ TPS_Homicide_20014_2018clean$Homicide_Type + TPS_Homicide_20014_2018clean$Occurrence_year + TPS_Homicide_20014_2018clean$Hood_ID)
xtable_Homicide_Year_Hood


### HomicideCountByYear <- table(TPS_Homicide_20014_2018clean$Homicide_Type + TPS_Homicide_20014_2018clean$Occurrence_year) ### <--- Gives Error
HomicideCountByYear <- table(TPS_Homicide_20014_2018clean$Homicide_Type, TPS_Homicide_20014_2018clean$Occurrence_year) ### <--- Added by Riyad
barplot(HomicideCountByYear, main = "Homicide Types by Year",col = heat.colors(3))
barplot(HomicideCountByYear, main = "Homicide Types by Year",xlab = "Year", ylab = "Frequency of Homicide Type", col = heat.colors(3))
barplot(HomicideCountByYear, main = "Homicide Types by Year '04-'18",xlab = "Year", ylab = "Frequency of Homicide Type", col = heat.colors(3), beside = TRUE, legend = TRUE)

#Create DF to use going forward with key pieces of information
#Homicide Type, Occurrence Date (Extract Month/Year from Timestamp, as Rental price is in Month/Year format), Division, Neighbourhood
TPSdf <- data.frame(TPS_Homicide_20014_2018clean$Occurrence_year, TPS_Homicide_20014_2018clean$Occurrence_Date, TPS_Homicide_20014_2018clean$Homicide_Type, TPS_Homicide_20014_2018clean$Division,TPS_Homicide_20014_2018clean$Neighbourhood, TPS_Homicide_20014_2018clean$Hood_ID)

#Going forward use Month/Date from Date Occurrence. Clean Date to match Rental price format. 
TPSdf$TPS_Homicide_20014_2018clean.Occurrence_Date <- substr(TPSdf$TPS_Homicide_20014_2018clean.Occurrence_Date, 0, 7)
names(TPSdf) <- c("occYear", "occDate", "homType", "Division", "Neighbourhood", "hoodID")
head(TPSdf)

library(ggplot2)

qplot(Division, data=TPSdf, geom="density", fill=homType, alpha=I(.5), main="Distribution of Homicide Type by Division", xlab="Division", ylab="Density")

#Prop table for Categorical variables
prop.table(table(TPSdf$Neighbourhood))
prop.table(table(TPSdf$Division))
prop.table(table(TPSdf$occYear))
prop.table(table(TPSdf$homType))

#Comparing Multiple Categorical Tables Year and Homicide Type
YearHomType <- table(TPSdf$occYear, TPSdf$homType)
YearHomType
prop.table(YearHomType)
prop.table(YearHomType,1) #normalize by row
prop.table(YearHomType,1)*100 #convert to percentages
mosaicplot(YearHomType, col=rainbow(ncol(YearHomType))) #visualize

#Check significance
YearHomType.cs <- chisq.test(YearHomType)
YearHomType.cs
#X-squared value of 41.38. 
#The H0 is that the two variables are independent and H1 is that the two variables are related.
#p-value is almost 0.05 (0.049). p is the probability that the two variables are independent. Since p ~ 0.05, the H0 is not rejected meaning, so they are likely independent.


#Comparing Multiple Categorical Tables Year and Division
YearDivision <- table(TPSdf$occYear, TPSdf$Division)
YearDivision

prop.table(YearDivision)
prop.table(YearDivision,1) #normalize by row
prop.table(YearDivision,1)*100 #convert to percentages
mosaicplot(YearDivision, main = "Homicide Type by Division and Year", xlab= "year", ylab= "division", border ="chocolate", off= 30,col=rainbow(ncol(YearHomType)), legend=TRUE) #visualize

#Check significance
YearDivision.cs <- chisq.test(YearDivision)
YearDivision.cs
#X-squared value of 271.8, 
#p-value is 0.016. p< 0.05 the variables are likely related/dependent.


#Comparing Multiple Categorical Tables Homicide Type and Division
HomDivision <- table(TPSdf$homType, TPSdf$Division)
HomDivision

prop.table(HomDivision)
prop.table(HomDivision,1) #normalize by row
prop.table(HomDivision,1)*100 #convert to percentages
mosaicplot(HomDivision, main = "Homicide Type by Division", xlab= "type", ylab= "division", border ="chocolate", off= 30, col=(ncol(YearHomType)), legend=TRUE) #visualize

#Check significance
HomDivision.cs <- chisq.test(HomDivision)
HomDivision.cs
#X-squared is 92.25, and p<<0.05. As such the variables are likely related/dependent and the H0 is rejected. 

###########################################################################################################################################
### The file "Toronto rental price by year.csv" needs cleaning so I removed unnecessary columns and data like October and columns value 'a'
###########################################################################################################################################

#DATASET #2: RENTALS
#Import rental data
### rentals<- read.csv("C:/Users/Admin/Desktop/CKME136/Final Data/Toronto rental price by year.csv")
rentals<- read.csv(file.choose())
#Data is Clean
str(rentals)

names(rentals) <- c("year", "bachelor","1bed", "2bed", "3bed", "total")
summary(rentals)
class(rentals$bachelor)
#prices are factors(cats), to be changed to numeric
rentals$year <- as.numeric(as.character(rentals$year))
rentals$bachelor <- as.numeric(as.character(rentals$bachelor))
rentals$`1bed` <- as.numeric(as.character(rentals$`1bed`))
rentals$`2bed` <- as.numeric(as.character(rentals$`2bed`))
rentals$`3bed`<- as.numeric(as.character(rentals$`3bed`))
rentals$total<- as.numeric(as.character(rentals$total))
rentals <-as.data.frame(rentals)
str(rentals)

#view data
rentals
str(rentals)
summary(rentals)
head(rentals)

#plot # Plot a price vs year for every bedroom type in the data
library(ggplot2)

ggplot(data = rentals, aes(x = rentals$year, y = rentals$bachelor))+
  geom_line(color = "#00AFBB", size = 2)

install.packages('tidyr')
library(tidyr)


rentalplot <- ggplot(data=rentals, aes(x = year,y=value,color=variable)) + 
  geom_line(aes(y = rentals$bachelor), color ="darkred")+ 
  geom_text(aes(x = 2018), y = 1100, label = "bachelor", color = "darkred")+
  geom_line(aes(y = rentals$`1bed`), color ="steelblue")+
  geom_text(aes(x = 2018), y = 1275, label = "1bed", color = "steelblue")+
  geom_line(aes(y = rentals$`2bed`), color ="darkblue")+
  geom_text(aes(x = 2018), y = 1480, label = "2bed", color = "darkblue")+
  geom_line(aes(y = rentals$`3bed`), color ="orange")+
  geom_text(aes(x = 2018), y = 1650, label = "3bed", color = "orange") 

rentalplot + labs(caption = "(based on data from CMHC 2018)")+labs(x = "Year", y="Price CAD") + labs(title="Toronto Rental Price by Year") 
#VISUALIZE RENT PRICES BY YEAR
rentalplot

#Time series analysis. Looking at the increase of rent over time. 
bachelorts<-ts (rentals$bachelor, start=c(1990), end=c(2018), frequency=1) 
plot.ts(bachelorts)

onebedts<-ts (rentals$`1bed`, start=c(1990), end=c(2018), frequency=1) 
plot.ts(onebedts)

twobedts<-ts (rentals$`2bed`, start=c(1990), end=c(2018), frequency=1) 
plot.ts(twobedts)

threebedts<-ts (rentals$`3bed`, start=c(1990), end=c(2018), frequency=1) 
plot.ts(threebedts)

