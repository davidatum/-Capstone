
#CREATE DECISION TREE (CLASSIFICATION) FOR POLICE DATA

#Initially used rpart but took too long. 
#library(rpart)
#TPSdf_BKP <- TPSdf
#TPSdf$occYear <- as.factor(TPSdf$occYear)
#TPSdf$occDate <- as.factor(TPSdf$occDate)
#TPSdf$hoodID <- as.factor(TPSdf$hoodID)
#fit <- rpart(homType ~ Neighbourhood + occDate + occYear+ Division, method ="class", data=TPSdf)

#printcp(fit)

#plotcp(fit)

#summary(fit)

#Plot tree
#plot(fit, uniform=TRUE, main="Classification Tree for TPS Homicide Data")
#text(fit, use.n=TRUE, all=TRUE, cex=.8)

#post(fit, file="c:/tree.ps", title="Classification Tree for TPS Homicide Data")


#pruning tree
#pfit <- prune(fit)
#plot(pfit, uniform = TRUE, main="Pruned Classification Tree for TPS Homicide Data")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#post(pfit, file="c:/ptree.ps",title="Pruned Classification Tree for TPS Homicide Data")


#new CODE USING CARET PACKAGE

install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("rlang")
library(rlang)

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
decisiontree_fit <- train(homType ~., data = training, method = "rpart",
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




#Logistic Regression
#logitTPS <- glm(homType ~ Neighbourhood + occDate + occYear+ Division, data=TPSdf, family="binomial" )
#summary(logitTPS)
