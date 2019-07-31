
#CREATE DECISION TREE (CLASSIFICATION) FOR POLICE DATA


#new CODE USING CARET PACKAGE

install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("rlang")
library(rlang)

TPSdf<- TPSdf[!TPSdf$occDate == "2004-11",]
TPSdf<- TPSdf[!TPSdf$occDate == "2011-11",]
TPSdf$Neighbourhood <- NULL
TPSdf$occDate <- NULL
head(TPSdf)

set.seed(100)
inTrain <- createDataPartition(
  y = TPSdf$homType,
  ## the outcome is the homicide that has occurred
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

str(inTrain)


training <- TPSdf[ inTrain,]
head(training)

testing <- TPSdf [-inTrain,]

dim(training) #763 rows
dim(testing)  #252 rows

str(training)
str(testing)

#PREPROCESSING & TRAINING 
install.packages("e1071")
library(e1071)

trainingcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(400)
decisiontree_fit <- train(homType ~Year+Division+hoodID, data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trainingcontrol,
                   tuneLength = 10)

decisiontree_fit

#PLOT the TREE
install.packages("rpart.plot")
library(rpart.plot)

prp(decisiontree_fit$finalModel, box.palette = "Reds")

#PREDICTION
#Model is trained with cp = 0.0068

testpredict <- predict(decisiontree_fit, newdata = testing)
confusionMatrix(testpredict, testing$homType)

#50% accuracy. Not very accurate.

specificity()
#Logistic Regression

install.packages("caTools")
library(caTools)
set.seed(401)
split = sample.split(TPSdf$homType, SplitRatio = 0.75)
split

#Create Training And Testing
TPSTrain = subset(TPSdf, split == TRUE)
TPSTest = subset(TPSdf, split == FALSE)

nrow(TPSTrain) #760 rows
nrow(TPSTest) #253

#Logistic Regression
head(TPSTrain)
TPSlogit = glm(homType ~Year+Division+hoodID, data=TPSdf, family=binomial)
summary(TPSlogit)


#Predict:
predictlogit <- predict(TPSlogit, newdata=TPSTest, type="response")
predictlogit

install.packages("plotROC")
library(plotROC)
plotROC(TPSdf$homType, predictlogit)

###Looking at both Police Data and Rent, merge on key 'Year'
head(rentals)
colnames(rentals)[colnames(rentals)=="year"] <- "Year"

head(TPSdf)
colnames(TPSdf)[colnames(TPSdf)=="occYear"] <- "Year"

TPSRentdf <- merge(TPSdf, rentals, by='Year')

TPSRentdf 



