data(mtcars)

pairs(mpg~.,data=mtcars,panel=panel.smooth, main="Cars data")
par(mfrow=c(3,3))
plot(mtcars$cyl, mtcars$mpg, main="Cylinders"); lines(mtcars$cyl, lm(mpg ~ cyl, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$disp, mtcars$mpg,main="Displacement"); lines(mtcars$disp, lm(mpg ~ disp, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$hp, mtcars$mpg,main="HP"); lines(mtcars$hp, lm(mpg ~ hp, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$drat, mtcars$mpg,main="Rear Axle Ratio"); lines(mtcars$drat, lm(mpg ~ drat, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$wt, mtcars$mpg,main="Weight"); lines(mtcars$wt, lm(mpg ~ wt, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$vs, mtcars$mpg,main="V/S"); lines(mtcars$vs, lm(mpg ~ vs, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$am, mtcars$mpg,main="Transmission"); lines(mtcars$am, lm(mpg ~ am, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$gear, mtcars$mpg,main="Gears"); lines(mtcars$gear, lm(mpg ~ gear, data=mtcars)$fitted, lwd=3, col="green")
plot(mtcars$carb, mtcars$mpg,main="Carbs"); lines(mtcars$carb, lm(mpg ~ carb, data=mtcars)$fitted, lwd=3, col="green")







#mean mpg for automatic
mean_auto=mean(mtcars[mtcars$am==0,]$mpg)

#mean mpg for manual
mean_manual=mean(mtcars[mtcars$am==1,]$mpg)

#Did the student interpret the coefficients correctly?


#Did the student do some exploratory data analyses?

#start by looking at MPG vs automatic and manual
library(ggplot2)
g=ggplot(mtcars,aes(x=am, y=mpg),)
g=g+xlab("Transmission")
g=g+ylab("MPG")
g=g+geom_point(size=5, color="black",alpha=0.2)
g=g+geom_point(size=4,color="blue",alpha=0.2)
g=g+geom_smooth(method="lm",color="black")
g

#add in weight and group by transmission
mtcars$trans="Automatic"
mtcars[mtcars$am==1,]$trans="Manual"
g=ggplot(mtcars,aes(x=wt, y=mpg, fill=trans),)
g=g+xlab("Weight")
g=g+ylab("MPG")
g=g+geom_point(size=5, color="black",alpha=0.2)
g=g+geom_point(size=4, color="blue", alpha=0.2)
g=g+geom_smooth(method="lm",color="black")
g

#add in weight and group by transmission
g=ggplot(mtcars,aes(x=cyl, y=mpg, fill=trans),)
g=g+xlab("Cylinders")
g=g+ylab("MPG")
g=g+geom_point(size=5, color="black",alpha=0.2)
g=g+geom_point(size=4, color="blue", alpha=0.2)
g=g+geom_smooth(method="lm",color="black")
g

#Did the student fit multiple models and detail their strategy for model selection?
#Look at linear model on transmission only
fit=lm(mtcars$mpg ~ mtcars$am)
summary(fit)

fit=lm(mtcars$mpg ~ mtcars$am+mtcars$wt)
summary(fit)


fit=lm(mtcars$mpg ~ mtcars$am+mtcars$wt+mtcars$cyl)
summary(fit)


#Did the student answer the questions of interest or detail why the question(s) is (are) not answerable?

#Did the student do a residual plot and some diagnostics?


#Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?


#Was the report brief (about 2 pages long) for the main body of the report and no longer than 5 with supporting appendix of figures?

#Did the report include an executive summary?

#Was the report done in Rmd (knitr)?