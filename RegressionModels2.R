#Regression Models - week 2

library(UsingR)
data(diamond)

#Data is diamond prices in $SGD and dizes in carats

##Plot the data
library(ggplot2)
g=ggplot(diamond,aes(x=carat, y=price),)
g=g+xlab("Mass (carats)")
g=g+ylab("Price ($SDGD)")
g=g+geom_point(size=5, color="black",alpha=0.2)
g=g+geom_point(size=4,color="blue",alpha=0.2)
g=g+geom_smooth(method="lm",color="black")
g

#getting the linear regression model
fit=lm(price ~ carat, data=diamond)
coef(fit)
fit
summary(fit)

#mean centering the data
fit2=lm(price ~ I(carat-mean(carat)),data=diamond)
#need to wrap calculations with the I function when inside the model

fit3=lm(price ~ I(carat*10),data=diamond) 
coef(fit3)

#applying the regression equation
newx = c(0.16, 0.27, 0.34)
coef(fit)[1]+coef(fit)[2]*newx

#OR - using the predict function
predict(fit,newdata=data.frame(carat=newx))
predict(fit)

#Residual variation is that variation that is not explained by the regression
# the "residuals" are the amounts not explained by the model. 
# The residual is just the difference between the predicted values and the observed value.

#Calculating residuals
y=diamond$price
x=diamond$carat
n=length(y)
fit=lm(y~x)
#using the resid function to get the residuals
e=resid(fit)
#using the predict function gives the predicted values for each X
yhat=predict(fit)
#subtracting the observed y values from the predicted values (yhat) also gives the residuals
max(abs(e-(y-yhat)))
max(abs(e-(y-coef(fit)[1]-coef(fit)[2]*x)))

#Using base R graphics, plotting data and residuals
plot(x, y, 
    xlab="Mass(carats)",
    ylab="Price (SGD)", 
    bg="lightblue",
    col="black", cex=1.1, pch=21, frame=FALSE)
abline(fit,lwd,2)
for(i in 1 : n)
    lines(c(x[i],x[i]),c(y[i],yhat[i]),col="red",lwd=2)

#Plotting again, but showing only the residuals
plot(x,e, 
      xlab="Mass(carats)",
      ylab="Residuals (SGD)",
      bg="lightblue",
      col="black",cex=2, pch=21, frame=FALSE)
abline(h=0,lwd=2)
for(i in 1 : n)
    lines(c(x[i],x[i]),c(e[i],0),col="red",lwd=2)

#Looking at non-linear data
x=runif(100,-3,3)
#identity line plus sin component plus some normal noise
y=x+sin(x)+rnorm(100,sd=0.2)
library(ggplot2)
g=ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
g=g+geom_smooth(method="lm",color="black")
g=g+geom_point(size=7, color="black",alpha=0.4)
g=g+geom_point(size=5,color="red",alpha=0.4)
g

#the regression model isn't the "correct" model since it
#doesn't capture the sin component of the variation.
#It does capture the linearity component

#Looking at the residuals should help describe the missing variability
g=ggplot(data.frame(x=x,y=resid(lm(y~x))),
            aes(x=x,y=y))
g=g+geom_hline(yintercept=0,size=2)
g=g+geom_point(size=7,color="black",alpha=0.4)
g=g+geom_point(size=5, color="red",alpha=0.4)
g=g+xlab("X")+ylab("Residual")
g

#Another example
x=runif(100,0,6)
y=x+rnorm(100,mean=0,sd=0.001*x)
g=ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
g=g+geom_smooth(method="lm",color="black")
g=g+geom_point(size=7,color="black",alpha=0.4)
g=g+geom_point(size=5,color="red",alpha=0.4)
g

#replotting the residuals
g=ggplot(data.frame(x=x,y=resid(lm(y~x))), 
         aes(x=x,y=y))
g=g+geom_hline(yintercept=0,size=2)
g=g+geom_point(size=7,color="black",alpha=0.4)
g=g+geom_point(size=5,color="red",alpha=0.4)
g=g+xlab("X")+ylab("Residual")

#this shows that the variability is increasing with the x variable
#this is called heteroskedaskisity

#Back to the diamond data
#adding the residuals as a variable to the data
diamond$e=resid(lm(price ~ carat, data=diamond))
g=ggplot(diamond, aes(x=carat,y=e))
g=g+xlab("Mass (carats)")
g=g+ylab("Residual Price (SGD)")
g=g+geom_hline(yintercept=0,size=2)
g=g+geom_point(size=7,color="black",alpha=0.5)
g=g+geom_point(size=5,color="blue", alpha=0.2)
g

#since there is little observable pattern to the residuals
#that indicates a relatively goood model fit

#Diamond data residual plot
#first residual is just fitting an intercept-  so differences around an average price
e=c(resid(lm(price ~ 1, data=diamond)), 
    resid(lm(price ~ carat, data=diamond)))
fit=factor(c(rep("Itc",nrow(diamond)),
             rep("Itc,slope",nrow(diamond))))

g=ggplot(data.frame(e=e,fit=fit),aes(y=e,x=fit, fill=fit))
g=g+geom_dotplot(binaxis="y",size=2,stackdir="center", binwidth=10)
g=g+xlab("fitting approach")
g=g+ylab("Residual price")
g

#Estimating residual variation
y=diamond$price; x=diamond$carat; n=length(y)
fit=lm(y~x)
summary(fit)$sigma

#Rsquared = a measure of model fit.
#Rsquared is the percentatge of total variability explained
#by the linear relationship with the predictor

#Statisitcal Inference with Regression
beta1=cor(y,x)*sd(y)/sd(x)
beta0=mean(y)-beta1*mean(x)
e=y-beta0=beta1*x
sigma=sqrt(sum(e^2)/(n-2))
ssx=sum(x-mean(x)))^2
seBeta0=(1/n +mean(x)^2/ssx)^.5*sigma
#more stuff here

#using the linear model 
fit=lm(y~x)
summary(fit)$coefficients

#getting a confidence interval
summary(fit)
#t values and p values are testing if the slope or intercept are zero
sumCoef=summary(fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(0.975,df=fit$df)*sumCoef[1,2]
sumCoef[2,1]+c(-1,1)*qt(0.975,df=fit$df)*sumCoef[2,2]


#New Example
newx=data.frame(x=seq(min(x),max(x),length=100))
p1=data.frame(predict(fit,newdata=newx,interval=("confidence")))
p2=data.frame(predict(fit,newdata=newx,interval=("prediction")))
p1$interval="confidence"
p2$interval="prediction"
p1$x=newx$x
p2$x=newx$x
dat=rbind(p1,p2)
names(dat)[1]="y"

g=ggplot(dat,aes(x=x,y=y))
g=g+geom_ribbon(aes(ymin=lwr,ymax=upr,fill=interval),alpha=0.2)
g=g+geom_line()
g=g+geom_point(data=data.frame(x=x,y=y),aes(x=x,y=y), size=4)
g

#Multivariate Regression
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3))

#Quiz 2
 #1.  
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
#using the linear model 
fit=lm(y~x)
summary(fit)$coefficients

#getting a confidence interval
summary(fit)
#t values and p values are testing if the slope or intercept are zero
sumCoef=summary(fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(0.975,df=fit$df)*sumCoef[1,2]
sumCoef[2,1]+c(-1,1)*qt(0.975,df=fit$df)*sumCoef[2,2]

#3
data(mtcars)
fit=lm(mtcars$mpg~mtcars$wt)
mtcars$wtdev=mtcars$wt-mean(mtcars$wt)
fit=lm(mtcars$mpg~mtcars$wtdev)
sumCoef=summary(fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(0.975,df=fit$df)*sumCoef[1,2]

#5
newx = 3-mean(mtcars$wt)
coef(fit)[1]+coef(fit)[2]*newx
predict(fit,newdata=data.frame(wtdev=newx), interval=("prediction"))
