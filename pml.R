# clear workspace
rm(list = ls(all = T))

# load required libraries
require(caret)
require(rpart)
require(rattle)

# load data
training <- read.table("pml-training.csv", header = T, sep = ",")
testing <- read.table("pml-testing.csv", header = T, sep = ",")

# preprocessing
# assign all test factor variables the same levels
for(i in 1:dim(training)[2])
{
    if(class(training[, i]) == "factor")
    {
        testing[, i] <- factor(testing[, i], levels = levels(training[, i]))
    }
}
# split data into training and validation
inTrain <- createDataPartition(y = training$classe, p = 0.6, list = FALSE)
myTrain <- training[inTrain, ]
myTest <- training[-inTrain, ]
# remove near zero variance variables
myTrain <- myTrain[, -nearZeroVar(myTrain)] 
# remove ID
myTrain <- myTrain[, -1]
# remove variables with too many NAs
trainingV3 <- myTrain 
for(i in 1:length(myTrain)) { 
    if(sum(is.na( myTrain[, i]))/nrow(myTrain) >= .6 ) {
        for(j in 1:length(trainingV3)) {
            if(length(grep(names(myTrain[i]), names(trainingV3)[j])) == 1)  {
                trainingV3 <- trainingV3[, -j] 
            }   
        } 
    }
}
myTrain = trainingV3
rm(trainingV3)

# build models
modDT <- rpart(classe ~ ., data = myTrain, method = "class")
fancyRpartPlot(modDT)

# observe training errors
predDT <- predict(modDT, myTest, type = "class")
confusionMatrix(predDT, myTest$classe)

modRF <- randomForest(classe ~ ., data = myTrain)
predRF <- predict(modRF, myTest, type = "class")
# get an estimate of out of sample error
confusionMatrix(predRF, myTest$classe)

# predict on testing
predRFtest <- predict(modRF, testing, type = "class")
