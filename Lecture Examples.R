#Regression R examples from Coursera

library(UsingR)
data(galton)
library(reshape2)
library(ggplot2)
long=melt(galton)

g=ggplot(long,aes(x=value,fill=variable))
g=g+geom_histogram(color="black", binwidth=1)
g=g+facet_grid(. ~ variable)
g

library(manipulate)
myhist=function(mu) {
    mse=mean((galton$child-mu)^2)
    g=ggplot(galton, aes(x=child))+geom_histogram(fill="salmon", color="black",binwidth=1)
    g=g+geom_vline(xintercept=mu,size=1)
    g=g+ggtitle(paste("mu = ", mu, ", MSE = ", round(mse,2),sep=""))
    g
    }
manipulate(myhist(mu),mu=slider(62,74,step=0.25))

ggplot(galton, aes(x=parent, y=child))+geom_point()

#Plot with sized and colored points
library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

#Regression through the origin: fitting a line through the origin that best fits the data
#if we subtract off the means from the data, then it moves the origin to the means and simplifies
#the regression problem

#subtract the mean from each point to center the data
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)

freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")

#convert from factors to numbers
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))

#create the plot
myPlot <- function(beta){
    g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
    g <- g  + scale_size(range = c(2, 20), guide = "none" )
    g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
    g <- g + geom_point(aes(colour=freq, size = freq))
    g <- g + scale_colour_gradient(low = "lightblue", high="white")                     

    #add the regression line
    g <- g + geom_abline(intercept = 0, slope = beta, size = 1)
    mse <- mean( (y - beta * x) ^2 )
    g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
    g
}
library(manipulate)
manipulate(myPlot(beta), beta = slider(0.5, 1.2, step = 0.01))

#Using the linear model to create the regression line
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
lm1 <- lm(galton$child ~ galton$parent)
g <- g + geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], size = 1, color="black")
g

#Linear Regression
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+10, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g <- g + geom_smooth(method="lm", formula=y~x, color="black")
g

#Regression to the mean
library(UsingR)
data(father.son)

#normalized data
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)

#Create the plot
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
#not sure why the points are defined twice; alpha -makes the points semi transparent
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)

#define the axis limits
g = g + xlim(-4, 4) + ylim(-4, 4)

#add the "identity" line
g = g + geom_abline(intercept = 0, slope = 1)

#add the axis
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)

#add the regression line - son's height = outcome; father predictor
g = g + geom_abline(intercept = 0, slope = rho, size = 1,color="blue")

#add the regression line - father's height = outcome; son predictor
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 1, color="green4")
g

g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")

g

#Week 1 Quizz

#1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
xBar=sum(x*w)/sum(w)

#2
b=0.59915
b=0.8263
b=-0.04462
b=-1.713
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
yest=b*x
leastSquare=sum((y-yest)^2)
lm(ymn~xmn)
g=ggplot(data.frame(x,y),aes(x=x,y=y))
g=g+geom_point(size=3,color="black")
g = g + geom_smooth(method="lm", formula=y~x, color="black")
g

#3
data(mtcars)
lm(mtcars$mpg~mtcars$wt)

#6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xCenter=x-mean(x)
xScale=xCenter/(sd(xCenter))

#7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
xmn=mean(x)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
ymn=mean(y)
lm((y-mean(y)) ~ (x-mean(x)))

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)