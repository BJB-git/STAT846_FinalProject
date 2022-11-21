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

#library: class
library(class)
cM = list()
runTime = list()

for(kk in 1:100){
  runTime[[kk]] = system.time(
    model <- knn(data.train.X,data.test.X, data.train.Y, k=kk )
  )
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}

libclass = list(lib = "class", cM = cM, runTime = runTime)
save(libclass, file = "libclass.RData")

#load("libclass.RData")

#You can add your libraries here (you can copy and edit the code above):

##### library: caret ####
library(caret)

#Documentation is hard to find - hard to use
#One site says data needs to be pre-processed
#May have built-in processing options

data.train.X.scaled <- scale(data.train.X)
data.test.X.scaled <- scale(data.test.X)

data.train.combined.scaled <- cbind(data.train.X.scaled, data.train.Y)
#data.train.combined.scaled[,ncol(data.train.combined.scaled)] <- factor(data.train.combined.scaled[,ncol(data.train.combined.scaled)])

data.train.combined.dataframe <- data.frame(data.train.combined.scaled)
data.train.combined.dataframe$data.train.Y <- factor(data.train.combined.dataframe$data.train.Y)
names(data.train.combined.dataframe)[length(data.train.combined.dataframe)] <- "YData"

cM = list()
runTime = list()

ctrl <- trainControl(method="repeatedcv",repeats = 3)
CaretFit <- train(YData ~ ., data = data.train.combined.dataframe, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
plot(CaretFit)

data.test.combined.scaled <- cbind(data.test.X.scaled, data.test.Y)
data.test.combined.dataframe <- data.frame(data.test.combined.scaled)
#data.test.combined.dataframe$data.test.Y <- factor(data.test.combined.dataframe$data.test.Y)  
names(data.test.combined.dataframe)[length(data.test.combined.dataframe)] <- "YData"

predict(CaretFit, newdata = data.test.combined.dataframe)
CaretPredictions <- predict(CaretFit, newdata = data.test.combined.dataframe)



train.id

XX.scaled <- scale(XX)
data.combined <- cbind(XX, yy)
data.combined.scaled.df <- data.frame(data.combined)
data.combined.scaled.df$yy <- factor(data.combined.scaled.df$yy)

data.combined.scaled.df.train <- data.combined.scaled.df[train.id,]
data.combined.scaled.df.test <- data.combined.scaled.df[-train.id,]

ctrl <- trainControl(method="repeatedcv",repeats = 3)
CaretFit <- train(yy ~ ., data = data.combined.scaled.df.train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
plot(CaretFit)

predict(CaretFit, newdata = data.combined.scaled.df.test)





for(kk in 1:100){
  runTime[[kk]] = system.time(
    model <- knn(data.train.X.scaled,data.test.X.scaled, data.train.Y, k=kk )
  )
  cM[[kk]] = table(actual=data.test.Y, predicted = model)
}

libclass = list(lib = "class", cM = cM, runTime = runTime)
save(libclass, file = "libclass.RData")

##### library: Rfast ####
library(Rfast)

#Confusing documentation
#Needs inputs to be matrices, which is annoying 
#Some of the arugments are nice

# data.train.X.scaled <- scale(data.train.X)
# data.test.X.scaled <- scale(data.test.X)

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

plotperformance(libRfast, accuracy, "Accuracy")
Reduce("+", libRfast$runTime) 
plot(1:100,unlist(runTime)[names(unlist(runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b")


#performance
accuracy = function(m) return((m[2,2]+m[1,1])/sum(m))
misscrate = function(m) return((m[1,2]+m[2,1])/sum(m))
f1score = function(m)return((2*m[2,2])/(2*m[2,2]+m[1,2]+m[2,1]))
recal = function(m) return(m[2,2]/(m[2,2]+m[2,1]))
precision =function(m) return(m[2,2]/(m[2,2]+m[1,2]))

plotperformance= function(lib = libclass, FUN = accuracy, ylab = "Accuracy"){
  metric = sapply(lib$cM, FUN)
  bestk = which.max(metric)
  plot(1:100,metric, type = "b", xlab=paste0("k ( best = ",bestk,")"), ylab = ylab,
       main = paste0("Library : ", lib$lib))
  abline(v = bestk, col="red", lty=2, h = metric[bestk])
}

plotperformance(libclass, accuracy, "Accuracy")
Reduce("+", libclass$runTime) 
plot(1:100,unlist(runTime)[names(unlist(runTime))=="elapsed"], 
     ylab = "Run Time", xlab="k", type = "b")
