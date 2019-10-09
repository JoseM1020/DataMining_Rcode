bodyData = read.table(url("http://jse.amstat.org/datasets/normtemp.dat.txt"))
names(bodyData) = c("bodytemp","gender","hr")
numObsevation=nrow(bodyData)
head(bodyData,5)
popMean=mean(bodyData$V1)
# The population mean after executing above statement does not return 98.6
meanBodyData = aggregate(bodyData, by=list(V2), FUN=mean)
# There is not much of a difference in heart rates between males and females but it can be argued this is enough of a difference
# to make a significance

routeData = read.table(url("http://media.pearsoncmg.com/cmg/pmmg_mml_shared/mathstatsresources/Akritas/DriveDurat.txt"))
meanRouteTime = aggregate(routeData, by=list(routeData$route), FUN=mean)
# It can be concluded from the mean determinance that the new route would save 32 (minutes) over the old route

bodyMeasurements = read.csv("//Users//josemuniz//Desktop//Fall 19//Data Mining//Lab2 Data.csv")
names(bodyMeasurements)
boxplot(bodyMeasurements$Height~bodyMeasurements$Sex)
# There is a significant difference in height among male and females, as it shows that males are at least 3 unites taller than females
boxplot(bodyMeasurements$Weight~bodyMeasurements$Sex)
# There is a significant enough difference between males and females of which can be conclude is around 10 units of weight