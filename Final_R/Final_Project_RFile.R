#library
library(MTPS)
#loading data
data(HIV)
View(XX)
View(YY)
yBin <- as.matrix(YY)
cutoffs <- c(2,3,3,1.5,1.5) # cutoff value to be used to define drug resistance
for(ii in 1:5) yBin[,ii] <- (10^yBin[,ii] < cutoffs[ii])*1
View(yBin)
yy = yBin[,1]
#split data into training and test
set.seed(0)
train.id = sample(1:length(yy), .8* length(yy))
data.train.X = data.frame(XX[train.id,])
data.train.Y = as.factor(yy[train.id])
data.test.X = data.frame(XX[-train.id,])
data.test.Y = as.factor(yy[-train.id])

##### performance Function
accuracy = function(m) return((m[2,2]+m[1,1])/sum(m))
misscrate = function(m) return((m[1,2]+m[2,1])/sum(m))
f1score = function(m)return((2*m[2,2])/(2*m[2,2]+m[1,2]+m[2,1]))
recal = function(m) return(m[2,2]/(m[2,2]+m[2,1]))
precision =function(m) return(m[2,2]/(m[2,2]+m[1,2]))

plotperformance= function(lib = libclass, FUN = accuracy, ylab = "Accuracy"){
  metric = sapply(lib$cM, FUN)
  bestk = ifelse(identical(FUN,misscrate),which.min(metric),which.max(metric))
  plot(1:100,metric, type = "b", xlab=paste0("k ( best = ",bestk,")"), ylab = ylab,
       main = paste0("Library : ", lib$lib))
  abline(v = bestk, col="red", lty=2, h = metric[bestk])
}

#library: class
library(class)
cM = list()
runTime = list()

for(kk in 1:100){
  runTime[[kk]] = system.time(
    model <- class::knn(data.train.X,data.test.X, data.train.Y, k=kk )
  )
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}

libclass = list(lib = "class", cM = cM, runTime = runTime)
save(libclass, file = "libclass.RData")

#load("libclass.RData")
par(mfrow=c(2,3))
plotperformance(libclass, accuracy, "Accuracy")
plotperformance(libclass, misscrate, "Misclassification")
plotperformance(libclass, f1score, "F1-Score")
plotperformance(libclass, recal, "Recall")
plotperformance(libclass, precision, "Precision")
plot(1:100,unlist(libclass$runTime)[names(unlist(libclass$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : class")

#library: FNN
library(FNN)
cM = list()
runTime = list()

for(kk in 1:100){
  runTime[[kk]] = system.time(
    model <- FNN::knn(data.train.X,data.test.X, data.train.Y, k=kk )
  )
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}

libFNN = list(lib = "FNN", cM = cM, runTime = runTime)
save(libFNN, file = "libFNN.RData")

#load("libFNN.RData")
par(mfrow=c(2,3))
plotperformance(libFNN, accuracy, "Accuracy")
plotperformance(libFNN, misscrate, "Misclassification")
plotperformance(libFNN, f1score, "F1-Score")
plotperformance(libFNN, recal, "Recall")
plotperformance(libFNN, precision, "Precision")
plot(1:100,unlist(libFNN$runTime)[names(unlist(libFNN$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : FNN")

#library: DMwR
library(remotes)
#remotes::install_github("cran/DMwR")
library(DMwR)
cM = list()
runTime = list()

for(kk in 1:100){
  runTime[[kk]] = system.time(
    model <- DMwR::kNN(y~.,data.frame(data.train.X, y=data.train.Y),data.frame(data.test.X, y=data.test.Y), k=kk )
  )
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}

libDMwR = list(lib = "DMwR", cM = cM, runTime = runTime)
save(libDMwR, file = "libDMwR.RData")

#load("libDMwR.RData")
par(mfrow=c(2,3))
plotperformance(libDMwR, accuracy, "Accuracy")
plotperformance(libDMwR, misscrate, "Misclassification")
plotperformance(libDMwR, f1score, "F1-Score")
plotperformance(libDMwR, recal, "Recall")
plotperformance(libDMwR, precision, "Precision")
plot(1:100,unlist(libDMwR$runTime)[names(unlist(libDMwR$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : DMwR")


#library: RANN
library(RANN)
cM = list()
runTime = list()
model = rep(NA, length(data.test.Y))
for(kk in 1:100){
  runTime[[kk]] = system.time(
    model1 <- RANN::nn2(data.train.X,data.test.X, k=kk)
  )
  for(ii in 1: length(data.test.Y))
    model[ii] = names(which.max(table(data.train.Y[model1$nn.idx[ii,]])))
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}

libRANN = list(lib = "RANN", cM = cM, runTime = runTime)
save(libRANN, file = "libRANN.RData")

#load("libRANN.RData")
par(mfrow=c(2,3))
plotperformance(libRANN, accuracy, "Accuracy")
plotperformance(libRANN, misscrate, "Misclassification")
plotperformance(libRANN, f1score, "F1-Score")
plotperformance(libRANN, recal, "Recall")
plotperformance(libRANN, precision, "Precision")
plot(1:100,unlist(libRANN$runTime)[names(unlist(libRANN$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : RANN")



#library: parsnip
#install.packages("parsnip")
library(parsnip)
library(kknn)
cM = list()
runTime = list()

for(kk in 1:100){
  model <- knn_cls_spec <-
    nearest_neighbor(mode = "classification",
                     engine = "kknn",
                     neighbors = kk)
  
  runTime[[kk]] = system.time(
    knn_cls_fit <- knn_cls_spec %>% fit(y ~ ., data = data.frame(data.train.X, y=data.train.Y))
  )

  model=predict(knn_cls_fit, as.data.frame(data.test.X))
  model = as.vector(model)

  cM[[kk]] = table(actual=data.test.Y, predicted = model$.pred_class)
}

libparsnip = list(lib = "parsnip", cM = cM, runTime = runTime)
save(libparsnip, file = "libparsnip.RData")

#load("libparsnip.RData")
par(mfrow=c(2,3))
plotperformance(libparsnip, accuracy, "Accuracy")
plotperformance(libparsnip, misscrate, "Misclassification")
plotperformance(libparsnip, f1score, "F1-Score")
plotperformance(libparsnip, recal, "Recall")
plotperformance(libparsnip, precision, "Precision")
plot(1:100,unlist(libparsnip$runTime)[names(unlist(libparsnip$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : parsnip")




##### library: caret ####
library(caret)
XX.scaled <- scale(XX)
data.combined <- cbind(XX, yy)
data.combined.scaled.df <- data.frame(data.combined)
data.combined.scaled.df$yy <- factor(data.combined.scaled.df$yy)
data.combined.scaled.df.train = data.combined.scaled.df[train.id,]
data.combined.scaled.df.test = data.combined.scaled.df[-train.id,]

#ctrl <- trainControl(method="repeatedcv",repeats = 3)
#CaretFit <- train(yy ~ ., data = data.combined.scaled.df, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
#plot(CaretFit)

#predict(CaretFit, newdata = data.combined.scaled.df.test)
#CaretPredictions <- predict(CaretFit, newdata = data.test.combined.dataframe)

cM = list()
runTime = list()

for(kk in 1:100){
  ctrl <- trainControl(method="repeatedcv",repeats = 3)
  
  runTime1 = system.time(
    model <- train(yy ~ ., data = data.combined.scaled.df.train, method = "knn",
                   tuneGrid=data.frame(k= kk), trControl = ctrl, preProcess = c("center","scale"))
  )
  
  runTime2 = system.time(
    model_predictions <- predict(model, newdata = data.combined.scaled.df.test)
  )
  
  runTime[[kk]] <- runTime1 + runTime2
  
  cM[[kk]] = table(actual=data.combined.scaled.df.test$yy, predicted = model_predictions)
}

libcaret = list(lib = "caret", cM = cM, runTime = runTime)
save(libcaret, file = "libcaret.RData")
#load("libcaret.RData")

par(mfrow=c(2,3))
plotperformance(libcaret, accuracy, "Accuracy")
plotperformance(libcaret, misscrate, "Misclassification")
plotperformance(libcaret, f1score, "F1-Score")
plotperformance(libcaret, recal, "Recall")
plotperformance(libcaret, precision, "Precision")
plot(1:100,unlist(libcaret$runTime)[names(unlist(libcaret$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : caret")


##### library: Rfast ####
library(Rfast)


cM = list()
runTime = list()

data.train.X.matrix <- matrix(unlist(data.train.X), nrow = nrow(data.train.X), byrow = FALSE)
data.test.X.matrix <- matrix(unlist(data.test.X), nrow = nrow(data.test.X), byrow = FALSE)
data.train.Y.matrix <- matrix(data.train.Y)
data.train.Y.matrix <- apply(data.train.Y.matrix, 2, as.numeric)
data.test.Y.matrix <- matrix(data.test.Y)
data.test.Y.matrix <- apply(data.test.Y.matrix, 2, as.numeric)

for(kk in 1:100){
  runTime[[kk]] = system.time(
    model <- knn(xnew = data.test.X.matrix, y = data.train.Y.matrix, x = data.train.X.matrix, k = kk, dist.type = "euclidean", type = "C", method = "average", 
                 freq.option = 0, mem.eff = FALSE)
  )
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}


libRfast = list(lib = "Rfast", cM = cM, runTime = runTime)
save(libRfast, file = "libRfast.RData")
#load("libRfast.RData")


par(mfrow=c(2,3))
plotperformance(libRfast, accuracy, "Accuracy")
plotperformance(libRfast, misscrate, "Misclassification")
plotperformance(libRfast, f1score, "F1-Score")
plotperformance(libRfast, recal, "Recall")
plotperformance(libRfast, precision, "Precision")
plot(1:100,unlist(libRfast$runTime)[names(unlist(libRfast$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b", main="Library : Rfast")


##############################################################################

plotperformance(libparsnip, accuracy, "Accuracy")
sapply(libparsnip$cM, accuracy)[38]
libparsnip$runTime[38]
Reduce("+", libparsnip$runTime) /100
plot(1:100,unlist(libparsnip$runTime)[names(unlist(libparsnip$runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b")

