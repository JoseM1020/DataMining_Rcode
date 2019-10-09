#HW4-Take Home Test#1

#Q1
x=scan()
y=scan()
dfHouse = data.frame(y,x)
model=lm(y~x)
plot(y,x)

#1.000422489

prob = 1 - predict(model,data.frame(x=45000),type="resp")
#probability is 45.02112%

#Q2
DefData = Default
attach(DefData)
DefDataR = DefData[-2]
head(DefDataR)
head(DefData)
round(prop.table(table(default))*100, digits=1)

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

DefDataNorm = as.data.frame(lapply(DefDataR[2:3],normalize))
summary(DefDataNorm)
dim(DefDataNorm)

DefDataTrain = DefDataNorm[1:8000,]
DefDataTest = DefDataNorm[8001:10000,]

DefDataTrainL = DefData[1:8000,1]
DefDataTestL = DefData[8001:10000,1]

DefDataTestPred = knn(train = DefDataTrain, test=DefDataTest, cl=DefDataTrainL, k=67)
table(DefDataTestPred)
CrossTable(x=DefDataTestL, y=DefDataTestPred, prop.chisq=FALSE)

#accuracy of model if k=100 (k = sqrt(10000))
acc = (1930+13)/2000
#modify value to have best results -> k=67
accMod = (1929+21)/2000
#we increased the accuracy by .35% -> the accuracy of the model is 97.5%


#Q3
head(USArrests)
USArrests.norm = as.data.frame(lapply(USArrests[1:4],normalize))
USArrests.result = kmeans(USArrests, 4)
USArrests.result$size
USArrests.result$centers
USArrests.result$cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))

plot(USArrests[c(1,2)], col=USArrests.result$cluster)
plot(USArrests[c(1,2)])
plot(USArrests[c(3,4)], col=USArrests.result$cluster)
plot(USArrests[c(3,4)])
