score=c(63,79,53,77,89,66,64,91,71,74)
hour=c(3,11,8,10,13,5,4,14,5,9)
plot(hour,score)
cor(hour,score)
cor.test(hour,score)
# null hypothesis rejected, data correlated because p<0.05 (alpha)
cor.test(hour,score,alt="greater")

# Problem 2
q2=read.table(url("http://media.pearsoncmg.com/aw/aw_sharpe_business_3/datasets/txt/GDP_2013.txt"), sep="\t", head=TRUE)
head(q2,4)
names(q2)=c("Year", "GDP")
head(q2,4)
attach(q2)
mean(GDP)
dim(q2)
names(q2)
plot(Year,GDP,main="GDP values",pch=17, col.main="pink", ylim=c(0,14))
model=lm(GDP~Year)
# GDP = -387.84 + 0.1993 year
abline(model)
abline(model, iwd=3, col=2)
summary(model)$r.square
#model can explain 97% of the variability
GDP
round(fitted(model),2)
sum(GDP-fitted(model))
par(mfrow=c(2,2))
plot(model)
# Box-cox transformation
library(MASS)
b=boxcox(model)
#get lamda from this
b
y=GDP^(0.25)
m2=lm(y~Year)
m2
summary(m2)
par(mfrow=c(2,2))
plot(m2)
# GDP=(-22.66 + 0..122year)^4
predict(model, data.frame(Year=1957))
(predict(m2, data.frame(Year=1957)))^4 #then raise to power 4 since was raised to 1/4 based on box-cox
predict(model, data.frame(Year=1957), interval="conf")
predict(model, data.frame(Year=1957), interval="conf", level=0.9)
predict(model, data.frame(Year=1957), interval="conf", level=0.99)
predict(model, data.frame(Year=1957), interval="pred")

#Q3
age=scan()
cho=scan()
plot(age,cho)
m=lm(formula=cho~age)
#cho=151.354 +1.399age lin regression model
abline(m)
summary(m)$r.squared
predict(m, data.frame(age=60))
predict(m, data.frame(age=60), interval="conf", level=0.9)
predict(m, data.frame(age=60), interval="conf")
