#add values
y=scan()
x1=scan()
x2=scan()
x3=scan()
x4=scan()
x5=scan()
x6=scan()
df=data.frame(y,x1,x2,x3,x4,x5,x6)
library(psych)
pairs.panels(df)
model=lm(y~x1+x2+x3+x4+x5+x6)
summary(model)
#dots and starts stars reject null hypothesis 0.00398 < alpha=0.05
#fail to reject null hypothesis greater than alpha
model1=lm(y~x5)
summary(model1)
par(mfrow=c(2,2))
plot(model)
library(MASS)
b=boxcox(model)
x7=x4^2
modela=lm(y~x1+x2+x3+x4+x5+x6+x7)
summary(modela)
anova(model)
Smoke.Index <- c( 77 , 137 , 117 , 94 , 116 , 102 , 111 , 93 , 88 , 102,
                    + 91 , 104 , 107 , 112 , 113 , 110 , 125 , 133 , 115 , 105 , 87 , 91 ,
                    + 100 , 76 , 66 )
Cancer.Index <- c( 84 , 116 , 123 , 128 , 155 , 101 , 118 , 113 , 104 ,
                     + 88 , 104 , 129 , 86 , 96 , 144 , 139 , 113 , 125 , 146 , 115 , 79 , 85 ,
                     + 120 , 60 , 51 )
Occupation <- c("Outdoor" , "Outdoor" , "Factory" , "Factory" ,
                  + "Factory" , "Factory" , "Office" , "Outdoor" , "Factory" , "Factory" ,
                  + "Factory" , "Factory" , "Factory" , "Factory" , "Factory" , "Office" ,
                  + "Outdoor" , "Outdoor" , "Outdoor" , "Office" , "Office" , "Office" ,
                  + "Outdoor" , "Office" , "Office" )
#plot(Smoke.Index, Cancer.Index, col=ifelse(Occupation=="Outdoor", "black", ifelse(Occupation=="Office", "red, "green")))
m1= lm(Cancer.Index ~ Smoke.Index)
#m1y=lm(Cancer.Index ~ Smoke.Index + Occupation)
#abline(14.58,0.9577,col="pink", lwd=2)
predict(model, data.frame(x1=0.18,x2=3,x3=1,x4=1176,x5=47,x6=6))
predict(model, data.frame(x1=0.18,x2=3,x3=1,x4=1176,x5=47,x6=6), interval = "conf")
predict(model, data.frame(x1=0.18,x2=3,x3=1,x4=1176,x5=47,x6=6), interval = "conf", level = 0.9)
#based on one single location:
predict(model, data.frame(x1=0.18,x2=3,x3=1,x4=1176,x5=47,x6=6), interval = "pred", level = 0.9)
z=rep(c("z1", "z2", "z3"), c(12,10,6))
dfPrime=data.frame(y,x1,x2,x3,x4,x5,x6,z)
boxplot(y~z)
mprime=lm(y~x1+x2+x3+x4+x5+x6+z)
summary(mprime)
#rerun dataframe line