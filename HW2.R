#QN1
q1=read.csv("//Users//leprogramma//Desktop//Fall 19//Data Mining//HWK2.csv")

scn = q1$SystemCodeNumber
occ = q1$Occupancy

boxplot(occ~scn, cex=1.0, las=2)
names(q1)

#Q2
q2=read.table(url("http://users.stat.ufl.edu/~winner/data/nuclear_time.dat"))
head(q2,5)
names(q2)=c("Task", "Nationality", "Task completion time", "TACOM score")
barplot(q2$`Task completion time`, names.arg = q2$Nationality, xlab = "Nationality(0=non US, 1=US)", ylab="Completion Time", main="Completion time based on nationality")
boxplot(q2$`Task completion time`~q2$Nationality, xlab="Nationality(0=non US, 1=US)", ylab="Completion Time", main="Completion time between nationalities")
#Q3
q3=read.table(url("http://jse.amstat.org/datasets/diamond.dat.txt"))
names(q3) = c("Diamond Size(K)", "Price(SGD)")
dsize=q3$`Diamond Size(K)`
price=q3$`Price(SGD)`
plot(dsize,price, main = "Diamond size and price rate (K/SGD)", xlab="Diamond Size(K)", ylab="Price(SGD)")
#There is a significance between nationalities whereas it is shown in the boxplot that non US workers are significantly less versed than US workers
model=lm(price~dsize)
abline(model)
#Equation for linear regression model: Price = -259.6 + 3721*DiamondSize
predict(model, data.frame(dsize=0.24), interval="conf", level=0.95)
predict(model, data.frame(dsize=0.24), interval="pred", level=0.95)