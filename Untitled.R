canData = read.table("//Users//leprogramma//Desktop//Fall 19//Data Mining//wdbc.data", sep=",")
head(canData)
dim(canData)
names(canData)=c("id","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean",
              "smoothness_mean","compactness_mean","concavity_mean","concave_points_mean","symmetry_mean",
              "fractal_dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se",
              "compactness_se","concavity_se","concave_points_se","symmetry_se","fractal_dimension_se",
              "radius_worst","texture_worst","perimeter_worst","area_worst", "smoothness_worst",
              "compactness_worst", "concavity_worst","concave_points_worst","symmetry_worst",
              "fractal_dimension_worst")
attach(canData)
table(diagnosis)
round(prop.table(table(diagnosis)) * 100, digits=1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd=canData[-1]
wbcd_n=as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n)
wbcd_train=wbcd_n[1:469,]
wbcd_test=wbcd_n[470:569,]
#use so much data (80%) to train model and rest to test
wbcd_train_label=wbcd[1:469,1]
wbcd_test_label=wbcd[470:569,1]
library(class)
wbcd_test_pred=knn(train=wbcd_train,test=wbcd_test,
                   cl=wbcd_train_label, k=21)
table(wbcd_test_pred)
library(gmodels)
CrossTable(x=wbcd_test_label, y=wbcd_test_pred, prop.chisq=FALSE)
