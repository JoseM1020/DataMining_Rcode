#import data from local storage
autoMpg = read.table("//Users//leprogramma//Desktop//Fall 19//Data Mining//auto-mpg.data")
#set headings
names(autoMpg) = c("MPG", "Cylinders", "Displacement", "Horsepower", "Weight", "Acceleration", "ModelYear", "Origin", "CarName")
attach(autoMpg)
#check names
head(autoMpg)
plot(autoMpg)
#Fit multiple linear regression model
lrModel = lm(MPG~Cylinders + Displacement + Horsepower + Weight + Acceleration + ModelYear + Origin + CarName, data=autoMpg)
#Retrieve Coefficient of Determination
summary(lrModel)$r.square
#Get Residuals
res.simple = residuals(lrModel)
b=boxcox(lrModel)
y=MPG^(0.05)
lrModelI = lm(y~Cylinders + Displacement + Horsepower + Weight + Acceleration + ModelYear + Origin + CarName, data=autoMpg)
bI=boxcox(lrModelI)
summary(lrModelI)$r.square

#2 - data cpus is loaded as long as package is loaded
dim(cpus)
#209 observations listed
attach(cpus)
subsets = regsubsets(perf~syct + mmin + mmax + cach + chmin + chmax, data = cpus)
plot(subsets, scale="adjr2")
