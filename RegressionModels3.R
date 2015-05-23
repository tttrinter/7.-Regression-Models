library(datasets)
data(swiss)
require(stats)
require(graphics)

pairs(swiss,panel=panel.smooth, main="Swiss data",col=3+(swiss$Catholic>50))

#The "." uses all variables in the regression
fit=lm(Fertility ~ ., data=swiss)
summary(fit)

#Marginal estimate (disregarding all other coefficients)
fitAgri=lm(Fertility ~ Agriculture, data=swiss)
summary(fitAgri)


#Hunger Example
setwd("C:/Users/Tom/Documents/Coursera/Data Science/7. Regression Models")
fileURL="https://raw.githubusercontent.com/tttrinter/courses/master/07_RegressionModels/02_02_multivariateExamples/hunger.csv"
download.file(fileURL, "hunger.csv")
hunger=read.csv("hunger.csv")
hunger=hunger[hunger$Sex!= "Both sexes",]

lm1=lm(Numeric ~ Year, data=hunger)
plot(hunger$Year, hunger$Numeric, pch=19, col="blue")
lines(hunger$Year, lm1$fitted, lwd=3, col="darkgrey")

#show male vs. female
plot(hunger$Year, hunger$Numeric, pch=19)
points(hunger$Year, hunger$Numeric, pch=19,col=((hunger$Sex=="Male")*1+1))

#add separate regression lines
lmM=lm(Numeric ~ Year, data=hunger[hunger$Sex=="Male",])
lmF=lm(Numeric ~ Year, data=hunger[hunger$Sex=="Female",])

plot(hunger$Year, hunger$Numeric, pch=19)
points(hunger$Year, hunger$Numeric, pch=19,col=((hunger$Sex=="Male")*1+1))
lines(hunger$Year[hunger$Sex=="Male"], lmM$fitted, lwd=3, col="black")
lines(hunger$Year[hunger$Sex=="Female"], lmF$fitted, lwd=3, col="red")

#Separate regression lines with the same slope
lmBoth=lm(Numeric ~ Year+Sex, data=hunger)
plot(hunger$Year, hunger$Numeric, pch=19)
points(hunger$Year, hunger$Numeric, pch=19,col=((hunger$Sex=="Male")*1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1]+lmBoth$coeff[3],lmBoth$coeff[2]),col="black",lwd=3)

#Separate lines, different slopes (non-zero intercept?)
lmBoth=lm(Numeric ~ Year+Sex+Sex*Year, data=hunger)
plot(hunger$Year, hunger$Numeric, pch=19)
points(hunger$Year, hunger$Numeric, pch=19,col=((hunger$Sex=="Male")*1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1]+lmBoth$coeff[3],lmBoth$coeff[2]+lmBoth$coeff[4]),col="black",lwd=3)

#Simulated Data
n=100
t=rep(c(0,1),c(n/2, n/2))
x=c(runif(n/2),runif(n/2))
beta0=0; beta1=2; tau=1; sigma=2
y=beta0+x*beta1+t*tau+rnorm(n, sd=sigma)
plot(x,y, type="n",frame=FALSE)
#regression disregarding the treatment
abline(lm(y~x),lwd=2)

#mean for untreated group
abline(h=mean(y[1:(n/2)]),lwd=3)

#mean of treated group
abline(h=mean(y[(n/2+1):n]),lwd=3)

fit=lm(y~x+t)
abline(coef(fit)[1],coef(fit)[2],lwd=3)
abline(coef(fit)[1]+coef(fit)[3],coef(fit)[2],lwd=3)
points(x[1:(n/2)],y[1:(n/2)],pch=21, col="black",bg="lightblue", cex=2)
points(x[(n/2+1):n],y[(n/2+1):n],pch=21, col="black",bg="salmon", cex=2)


#Residuals, Diagnostics and Variation
data(swiss)
par(mfrow=c(2,2))
fit=lm(Fertility ~., data=swiss)
plot(fit)

?influence.measures

#Variance Inflation
n=100; nosim=1000
x1=rnorm(n); x2=rnorm(n); x3=rnorm(n);
betas = sapply(1:nosim, function(i){
    y<-x1 + rnorm(n, sd=0.3)
    c(coef(lm(y~x1))[2],
      coef(lm(y~x1+x2))[2],
      coef(lm(y~x1+x2+x3))[2])
    })

round(apply(betas,1,sd),5)

x1=rnorm(n)
x2=x1/sqrt(2)+rnorm(n)/sqrt(2)
x3=x1*0.95 + rnorm(n)*sqrt(1-0.95^2)
betas = sapply(1:nosim, function(i){
    y<-x1 + rnorm(n, sd=0.3)
    c(coef(lm(y~x1))[2],
      coef(lm(y~x1+x2))[2],
      coef(lm(y~x1+x2+x3))[2])
})

round(apply(betas,1,sd),5)

#doesn't depend on which y is used - looking at Variance inflation 
y=x1+rnorm(n,sd=0.3)
a= summary(lm(y~x1))$cov.unscaled[2,2]
c(summary(lm(y~x1+x2))$cov.unscaled[2,2],
  summary(lm(y~x1+x2+x3))$cov.unscaled[2,2])/a

#with the Swiss data
data(swiss)
fit1=lm(Fertility ~ Agriculture, data=swiss)
a=summary(fit1)$cov.unscaled[2,2]
fit2=update(fit1,Fertility ~ Agriculture+Examination)
fit3=update(fit1, Fertility ~ Agriculture+Examination+Education)
fit5=update(fit1,Fertility ~Agriculture+Examination+Education+Catholic+Infant.Mortality)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2])/a
anova(fit1,fit2, fit3, fit5)

library(car)
fit=lm(Fertility ~ ., data=swiss)
vif(fit)