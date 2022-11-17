library(ISLR2)
library(caret)
library(class)
library(gmodels)
library(ggplot2)


#### Liver Disease Data Set ####

LiverData <- read.csv("bupa.data")

#Remove the final column. The final column indicates whether each case is in the training or test set in the author's work. 
#We will make our own training/test sets, so we can ignore this column
LiverData <- LiverData[,1:6]

# Attribute Information:
#   
# 1. mcv mean corpuscular volume
# 2. alkphos alkaline phosphotase
# 3. sgpt alanine aminotransferase
# 4. sgot aspartate aminotransferase
# 5. gammagt gamma-glutamyl transpeptidase
# 6. drinks number of half-pint equivalents of alcoholic beverages drunk per day
# 7. selector field created by the BUPA researchers to split the data into train/test sets

names(LiverData) <- c("mcv",
                      "alkphos",
                      "sgpt",
                      "sgot",
                      "gammagt",
                      "drinks")


#Convert "drinks" to categorical. This is our response variable.
LiverData$drinks <- factor(LiverData$drinks)

#Normalize the predictors
LiverData[,1:5] <- scale(LiverData[,1:5])

summary(LiverData)


set.seed(2021)
trainIndex <- createDataPartition(LiverData$drinks, p = .75, 
                                  list = FALSE, 
                                  times = 1)

mytrain <- LiverData[trainIndex, ]
mytest <- LiverData[-trainIndex, ]
train.X <- LiverData[trainIndex, c(1:5)]
test.X <- LiverData[-trainIndex, c(1:5)]
train.label <- LiverData[trainIndex, 6]
test.label <- LiverData[-trainIndex, 6]


CorrectPredictions <- c()

for (ii in 1:nrow(train.X)){
  set.seed(ii)
  
  knn.pred <- knn(train.X, test.X, train.label, k = ii)

  CorrectPredictions <- c(CorrectPredictions, sum(as.integer(test.label==knn.pred)) / length(test.label==knn.pred))
}

plot(CorrectPredictions, type = "l")






#### Breast Cancer Data Set ####

BCData <- read.csv("dataR2.csv")

# Attribute Information:
#   
# Quantitative Attributes:
#  Age (years)
#  BMI (kg/m2)
#  Glucose (mg/dL)
#  Insulin (µU/mL)
#  HOMA
#  Leptin (ng/mL)
#  Adiponectin (µg/mL)
#  Resistin (ng/mL)
#  MCP-1(pg/dL)
# 
# Labels:
#   1=Healthy controls
#   2=Patients

#Convert "drinks" to categorical. This is our response variable.
BCData$Classification <- factor(BCData$Classification)

#Normalize the predictors
BCData[,1:9] <- scale(BCData[,1:9])

summary(BCData)


set.seed(2021)
trainIndex <- createDataPartition(BCData$Classification, p = .75, 
                                  list = FALSE, 
                                  times = 1)

mytrain <- BCData[trainIndex, ]
mytest <- BCData[-trainIndex, ]
train.X <- BCData[trainIndex, c(1:9)]
test.X <- BCData[-trainIndex, c(1:9)]
train.label <- BCData[trainIndex, 10]
test.label <- BCData[-trainIndex, 10]


CorrectPredictions <- c()


for (ii in 1:nrow(train.X)){
  
  set.seed(ii)
  
  knn.pred <- knn(train.X, test.X, train.label, k = ii)
  
  CorrectPredictions <- c(CorrectPredictions, sum(as.integer(test.label==knn.pred)) / length(test.label==knn.pred))
}


max(CorrectPredictions)
BestK <- which(CorrectPredictions == max(CorrectPredictions))
BestK
plot(CorrectPredictions, type = "l")




# Calculate average run time
# May want to test on a larger data set - this one is very quick

TimeMatrix <- matrix(NA, nrow = 100, ncol = nrow(train.X))

for (k in 1:nrow(train.X)) {
  
  TimeList <- c()
  
  for (ii in 1:100) {
    start_time <- Sys.time()
    knn(train.X, test.X, train.label, k = 40)
    end_time <- Sys.time()
    NewTime <- end_time - start_time
    
    TimeMatrix[ii,k] <- NewTime
  }
}

MeanForEachK <- colMeans(TimeMatrix)

barplot(MeanForEachK)

mean(MeanForEachK)
MeanForEachK[BestK]

#Plot with only two predictors

knn.pred.best <- knn(train.X, test.X, train.label, k = BestK)

BCTwoPredictorPlotData <- cbind(test.X, knn.pred.best)

ggplot(BCTwoPredictorPlotData, aes(x=Age, y=BMI, color = knn.pred.best, size = 3)) + 
  geom_point() +
  theme_minimal()


#### Plot with only two predictors used in the underlying function ####
# 
# set.seed(2021)
# trainIndex <- createDataPartition(BCData$Classification, p = .75, 
#                                   list = FALSE, 
#                                   times = 1)
# 
# mytrain <- BCData[trainIndex, ]
# mytest <- BCData[-trainIndex, ]
# train.X <- BCData[trainIndex, c(1:2)]
# test.X <- BCData[-trainIndex, c(1:2)]
# train.label <- BCData[trainIndex, 10]
# test.label <- BCData[-trainIndex, 10]
# 
# CorrectPredictions <- c()
# 
# 
# for (ii in 1:nrow(train.X)){
#   
#   set.seed(ii)
#   
#   knn.pred <- knn(train.X, test.X, train.label, k = ii)
#   
#   CorrectPredictions <- c(CorrectPredictions, sum(as.integer(test.label==knn.pred)) / length(test.label==knn.pred))
# }
# 
# 
# max(CorrectPredictions)
# BestK <- which(CorrectPredictions == max(CorrectPredictions))
# BestK
# plot(CorrectPredictions, type = "l")
# 
# knn.pred.best <- knn(train.X, test.X, train.label, k = BestK)
# 
# BCTwoPredictorPlotData <- cbind(test.X, knn.pred.best)
# 
# ggplot(BCTwoPredictorPlotData, aes(x=Age, y=BMI, color = knn.pred.best, size = 3)) + 
#   geom_point() +
#   theme_minimal()
# 


















#### HIV Testing ####
library(MTPS)
data(HIV)

View(XX)
View(YY)

HIVData <- XX

yBin <- as.matrix(YY)
cutoffs <- c(2,3,3,1.5,1.5) # cutoff value to be used to define drug resistance
for(ii in 1:5) yBin[,ii] <- (10^yBin[,ii] < cutoffs[ii])*1
View(yBin)

#Pick the first outcome

HIVData$outcome <- factor(yBin[,1])


#Convert "drinks" to categorical. This is our response variable.
BCData$Classification <- factor(BCData$Classification)

#Normalize the predictors
BCData[,1:9] <- scale(BCData[,1:9])

summary(BCData)


set.seed(2021)
trainIndex <- createDataPartition(BCData$Classification, p = .75, 
                                  list = FALSE, 
                                  times = 1)

mytrain <- BCData[trainIndex, ]
mytest <- BCData[-trainIndex, ]
train.X <- BCData[trainIndex, c(1:9)]
test.X <- BCData[-trainIndex, c(1:9)]
train.label <- BCData[trainIndex, 10]
test.label <- BCData[-trainIndex, 10]


CorrectPredictions <- c()


for (ii in 1:nrow(train.X)){
  
  set.seed(ii)
  
  knn.pred <- knn(train.X, test.X, train.label, k = ii)
  
  CorrectPredictions <- c(CorrectPredictions, sum(as.integer(test.label==knn.pred)) / length(test.label==knn.pred))
}


max(CorrectPredictions)
BestK <- which(CorrectPredictions == max(CorrectPredictions))
BestK
plot(CorrectPredictions, type = "l")




# Calculate average run time
# May want to test on a larger data set - this one is very quick

TimeMatrix <- matrix(NA, nrow = 100, ncol = nrow(train.X))

for (k in 1:nrow(train.X)) {
  
  TimeList <- c()
  
  for (ii in 1:100) {
    start_time <- Sys.time()
    knn(train.X, test.X, train.label, k = 40)
    end_time <- Sys.time()
    NewTime <- end_time - start_time
    
    TimeMatrix[ii,k] <- NewTime
  }
}

MeanForEachK <- colMeans(TimeMatrix)

barplot(MeanForEachK)

mean(MeanForEachK)
MeanForEachK[BestK]

#Plot with only two predictors

knn.pred.best <- knn(train.X, test.X, train.label, k = BestK)

BCTwoPredictorPlotData <- cbind(test.X, knn.pred.best)

ggplot(BCTwoPredictorPlotData, aes(x=Age, y=BMI, color = knn.pred.best, size = 3)) + 
  geom_point() +
  theme_minimal()


