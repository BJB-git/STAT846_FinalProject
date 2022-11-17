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
