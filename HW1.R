data = read.csv("//Users//josemuniz//Desktop//Fall 19//Data Mining//HWK1_Q1.csv")
NumSummSpeed = summary(data$speed)
NumSummSpeed
hist(data$speed)
attach(data)
meanPer <- aggregate(data, by=list(period), FUN=mean, na.rm=TRUE)

data2 = read.table(url("http://users.stat.ufl.edu/~winner/data/alc_elim.dat"))
attach(data2)
names(data2) = c("id","gender","brac","bac")
numObservations = nrow(data2)
meanData2 = aggregate(data2, by=list(gender), FUN=mean)
boxplot(brac~gender, main="BrAC between genders")
boxplot(bac~gender, main="BAC between genders")
# Q2f: There is a signifigant gap between the upper and lower quartile (greater range) for females than males but the data portrays
# a higher mean for females

# Q2g There not much signifigant difference in range of the data between each quartile but males have a considerably less BrAC
# average content than females
