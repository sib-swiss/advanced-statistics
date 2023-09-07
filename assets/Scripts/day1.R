########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# GLM



########################################################
# class dataset
########################################################

library(ggplot2)

class <- read.table("exercises/class.txt")

summary(class[,-1])

class$Gender[class$Gender == "F"] <- 0
class$Gender[class$Gender == "M"] <- 1
class$Gender <- as.numeric(class$Gender)

pairs(class[,-1])

model <- lm(Height ~ Age, data=class)
summary(model)

plot(class$Age, class$Height, xlim=c(0,20), ylim=c(0,200))
abline(model, col="red", lwd=2)

hat <- lm.influence(model)
plot(hat$hat)

library(car)
influencePlot(model, xlab="Hat-Values", ylab="Studentized Residuals")

new_age <- seq(11, 16, by=0.25)

conf_interval <- predict.lm(model, newdata=data.frame(Age=new_age), interval="confidence", level = 0.95)
lines(new_age, conf_interval[,2], col="blue", lty=2)
lines(new_age, conf_interval[,3], col="blue", lty=2)

pred_interval <- predict(model, newdata=data.frame(Age=new_age), interval="prediction", level = 0.95)
lines(new_age, pred_interval[,2], col="orange", lty=2)
lines(new_age, pred_interval[,3], col="orange", lty=2)

model.2 <- lm(Height ~ Weight, data=class)
summary(model.2)

model.3 <- lm(Height ~ Age + Weight, data=class)
summary(model.3)

model.4 <- lm(Height ~ Age + Gender, data=class)
summary(model.4)

model.5 <- lm(Height ~ Age * Gender, data=class)
summary(model.5)



########################################################
# hellung dataset
########################################################

library(ISwR)
data(hellung)

plot(hellung$diameter, hellung$conc, 
     xlab="Diameter", ylab="Concentration")

model <- lm(conc ~ diameter, data=hellung)
summary(model)

abline(model)

qqnorm(residuals(model))
qqline(residuals(model))
ks.test(residuals(model), "pnorm")

influencePlot(model, xlab="Hat-Values", ylab="Studentized Residuals")

logconc <- log(hellung$conc)
plot(hellung$diameter, logconc, 
     xlab="Diameter", ylab="log(concentration)")

modellog <- lm(logconc ~ diameter, data=hellung)
summary(modellog)

abline(modellog)

plot(fitted(modellog), residuals(modellog))
qqnorm(residuals(modellog))
qqline(residuals(modellog))
ks.test(residuals(modellog), "pnorm")

modellog.2 <- lm(logconc ~ diameter + glucose, data=hellung)
summary(modellog.2)



########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Moving Beyond Linearity



########################################################
# Exercise 1: Janka hardness data
########################################################

library(SemiPar)
data(janka)
attach(janka)

plot(dens,log(hardness), ylab="response", xlab="X", main="Janka data")


# ------------------------------------------
# 1. linear fit

# plot raw data
plot(dens,log(hardness), ylab="response", xlab="X")

# fit a linear model
fit.linear <- lm( log(hardness) ~ dens )

# plot the estimated linear fit
abline(fit.linear, lwd=2, col="red")

# let's also look at residuals vs. fitted values
plot(fit.linear$fitted.values, fit.linear$residuals,
     ylim=c(-max(range(fit.linear$residuals)),max(range(fit.linear$residuals))),
     ylab="residuals", xlab="fitted values")

# we want the error terms to be normally distributed around zero, so let's add that to the plot
abline(a=0, b=0, col="blue", lwd=2)

# another way to assess the residual plot is to use a scatterplot smoother that captures the trend in residuals
# scatter.smooth uses loess algorithm for local weighted regression
scatter.smooth(fit.linear$fitted.values, fit.linear$residuals,
               ylim=c(-max(range(fit.linear$residuals)),max(range(fit.linear$residuals))),
               ylab="residuals", xlab="fitted values",
               lpars=list(col="blue", lwd=2, lty=2))
abline(a=0, b=0, col="blue", lwd=2)
legend(7.6,0.22,"loess fit",lty=2, col="blue", cex=0.75)


# ------------------------------------------
# 2. quadratic fit

# plot raw data
plot(dens,log(hardness), ylab="response", xlab="X")

# fit a quadratic model: y=ax2+bx+c 
fit.quad <- lm( log(hardness) ~ poly(dens, degree=2) ) 

# plot the estimated linear fit
# to do so, we need to predict the fit at desired points on the X axis (let's call it the grid)
dens.range <- range(dens)
dens.grid <- seq(from=dens.range[1], to=dens.range[2], length.out=100)

# now that we have the grid, we can use predict() to estimate the fitted model on the specified grid.
# the arguments to predict() can vary depending on the object class of the fit, but R detects the appropriate predict function automatically.
# for example, there are predict.lm(), predict.glm(), predict.poly(), etc. The list goes on and on!
# Here we're feeding an object of class "lm" to predict() so you can use ?predict.lm to get more info about its arguments.
predict_fit.quad <- predict(fit.quad, newdata=data.frame(dens=dens.grid))
lines(dens.grid, predict_fit.quad, col="red", lwd=2)

# once again, let's check residuals vs. fitted values
plot(fit.quad$fitted.values, fit.quad$residuals, 
     ylim=c(-max(range(fit.quad$residuals)),max(range(fit.quad$residuals))), 
     ylab="residuals", xlab="fitted values")
abline(a=0, b=0, col="blue", lwd=2)

# Let me say a few more things about the arguments required by predict()
# Well, you first need to specify the model you're trying to predict from. Depending on the class of the fit object (e.g. lm, glm, etc.), R will then invoke a particular predict method.
# The second optional argument is "newdata", a data frame to let R know at which values of the predictor(s) you would want to make the predictions.
# Always make sure the variable names in newdata are the same as in the model.
# If newdata is omitted, by default predictions are made based on the data used for the fit (observed data).
# The accuracy or precision of our estimates can be expressed in terms of a confidence interval (CI) or a prediction interval (PI).
# Here is a link where these intervals are nicely explained: https://www.graphpad.com/support/faq/the-distinction-between-confidence-intervals-prediction-intervals-and-tolerance-intervals/
# By setting the "interval" argument to either "confidence" or "prediction" predict() will return the requested interval along the fitted values.
# If you would rather do the calculations manually, you can get the standard errors on your predictions as well by setting the argument se.fit to TRUE.
# We will come back to intervals later. Let's move on!

# and the scatterplot smoothing to assess whether the quadratic fit improved our residual plot.
scatter.smooth(fit.quad$fitted.values, fit.quad$residuals,
               ylim=c(-max(range(fit.quad$residuals)),max(range(fit.quad$residuals))),
               ylab="residuals", xlab="fitted values",
               lpars=list(col="blue", lwd=2, lty=2))
abline(a=0, b=0, col="blue", lwd=2)
legend(6.2,-0.1,"loess fit",lty=2, col="blue", cex=0.75)
# looks much better, don't you agree?



########################################################
# Exercise 2: light detection and ranging (LIDAR) data
########################################################

library(SemiPar)
data(lidar)
attach(lidar)

# linear and quadratic fit are clearly not appropriate for the lidar data, so let's start from polynomial degree 3. 
# fitting polynomials of degree 3, 4, and 10 to lidar data


# ------------------------------------------
# 1. cubic fit

# plot raw data
plot(range,logratio, ylab="response", xlab="X")

# fit a cubic model
fit.cubic <- lm( logratio ~ poly(range,3) )

# set up the grid
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# use predict() to estimated the model at desired points on the X-axis (i.e. the grid)
predict_fit.cubic <- predict(fit.cubic, newdata=data.frame(range=range.grid))
lines(range.grid, predict_fit.cubic, col="red", lwd=2)

# let's look at the residuals of our cubic fit
plot(fit.cubic$fitted.values, fit.cubic$residuals, 
     ylim=c(-max(range(fit.cubic$residuals)),max(range(fit.cubic$residuals))), 
     ylab="residuals", xlab="fitted values")
scatter.smooth(fit.cubic$fitted.values, fit.cubic$residuals,
               ylim=c(-max(range(fit.cubic$residuals)),max(range(fit.cubic$residuals))),
               ylab="residuals", xlab="fitted values",
               lpars=list(col="blue", lwd=2, lty=2))
abline(a=0, b=0, col="blue", lwd=2)


# ------------------------------------------
# 2. polynomial degree 4

# plot raw data
plot(range,logratio, ylab="response", xlab="X")

# fit a quadratic model
fit.quad <- lm( logratio ~ poly(range,4) )

# set up the grid
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# make predictions on the grid
predict_fit.quad <- predict(fit.quad, newdata=data.frame(range=range.grid))
lines(range.grid, predict_fit.quad, col="red", lwd=2)

# residual plot
plot(fit.quad$fitted.values, fit.quad$residuals, 
     ylim=c(-max(range(fit.quad$residuals)),max(range(fit.quad$residuals))), 
     ylab="residuals", xlab="fitted values")
scatter.smooth(fit.quad$fitted.values, fit.quad$residuals,
               ylim=c(-max(range(fit.quad$residuals)),max(range(fit.quad$residuals))),
               ylab="residuals", xlab="fitted values",
               lpars=list(col="blue", lwd=2, lty=2))
abline(a=0, b=0, col="blue", lwd=2)


# ------------------------------------------
# 3. polynomial degree 10

# plot raw data
plot(range,logratio, ylab="response", xlab="X")

# fit a polynomial degree 10
fit.poly10 <- lm( logratio ~ poly(range,10) )

# set up the grid
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# make predictions on the grid
predict_fit.poly10 <- predict(fit.poly10, newdata=list(range=range.grid))
lines(range.grid, predict_fit.poly10, col="red", lwd=2)

# residual plot
plot(fit.poly10$fitted.values, fit.poly10$residuals, 
     ylim=c(-max(range(fit.poly10$residuals)),max(range(fit.poly10$residuals))), 
     ylab="residuals", xlab="fitted values")
scatter.smooth(fit.poly10$fitted.values, fit.poly10$residuals,
               ylim=c(-max(range(fit.poly10$residuals)),max(range(fit.poly10$residuals))),
               ylab="residuals", xlab="fitted values",
               lpars=list(col="blue", lwd=2, lty=2))
abline(a=0, b=0, col="blue", lwd=2)

# remove the junk
rm(fit.cubic, fit.quad, fit.poly10, predict_fit.cubic, predict_fit.quad, predict_fit.poly10, range.range, range.grid)


# ------------------------------------------
# 4. confidence and prediction intervals

# plot raw data
plot(range,logratio, ylab="response", xlab="X")

# fit the model
fit.cubic <- lm( logratio ~ poly(range,3) )

# set up the grid
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# use the interval argument to fetch confidence intervals
predict_fit.cubic <- predict(fit.cubic, newdata=data.frame(range=range.grid), interval="confidence")

# keep in mind that confidence level is by default set to 0.95, but of course can be modified via the "level" argument
lines(range.grid, predict_fit.cubic[,"fit"], col="red", lwd=2)
lines(range.grid, predict_fit.cubic[,"lwr"], col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.cubic[,"upr"], col="red", lwd=2, lty=2)

# alternatively we can write our own code to compute confidence intervals using the standard errors of the estimates
predict_fit.cubic <- predict(fit.cubic, newdata=list(range=range.grid), se.fit=TRUE)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")

# plot the estimated fit
lines(range.grid, predict_fit.cubic$fit, col="red", lwd=2)

# compute and plot confidence bands
lines(range.grid, predict_fit.cubic$fit + 2 * predict_fit.cubic$se.fit, col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.cubic$fit - 2 * predict_fit.cubic$se.fit, col="red", lwd=2, lty=2)

# use the interval argument to fetch prediction intervals
predict_fit.cubic <- predict(fit.cubic, newdata=data.frame(range=range.grid), interval="prediction")

lines(range.grid, predict_fit.cubic[,"lwr"], col="blue", lwd=2, lty=3)
lines(range.grid, predict_fit.cubic[,"upr"], col="blue", lwd=2, lty=3)


# ------------------------------------------
# 5. step functions

plot(range,logratio, ylab="response", xlab="X")

table(cut(range, breaks=3)) # note that you need to specify the number of breaking points
# (390,500] (500,610] (610,720] 
# 74        74        73

fit.pwsf <- lm( logratio ~ cut(range,breaks=3) )
summary(fit.pwsf)

plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)
predict_fit.pwsf <- predict(fit.pwsf, newdata=list(range=range.grid), se.fit=TRUE)
lines(range.grid, predict_fit.pwsf$fit, col="red", lwd=2)
lines(range.grid, predict_fit.pwsf$fit + 2 * predict_fit.pwsf$se.fit, col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.pwsf$fit - 2 * predict_fit.pwsf$se.fit, col="red", lwd=2, lty=2)


# ------------------------------------------
# 6. piecewise linear fits

fit.left.linear.1knot <- lm( logratio ~ range, subset=(range<575) )
fit.right.linear.1knot <- lm( logratio ~ range, subset=(range>=575) )

summary(fit.left.linear.1knot)
summary(fit.right.linear.1knot)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid.left <- seq(from=range.range[1], to=575, length.out=50)
range.grid.right <- seq(from=575, to=range.range[2], length.out=50)

# predict the fit on the grid
# left
predict_fit.left.linear.1knot <- predict(fit.left.linear.1knot, newdata=list(range=range.grid.left), se.fit=TRUE)
lines(range.grid.left, predict_fit.left.linear.1knot$fit, col="red", lwd=2)
lines(range.grid.left, predict_fit.left.linear.1knot$fit + 2 * predict_fit.left.linear.1knot$se.fit, col="red", lwd=2, lty=2)
lines(range.grid.left, predict_fit.left.linear.1knot$fit - 2 * predict_fit.left.linear.1knot$se.fit, col="red", lwd=2, lty=2)
# right
predict_fit.right.linear.1knot <- predict(fit.right.linear.1knot, newdata=list(range=range.grid.right), se.fit=TRUE)
lines(range.grid.right, predict_fit.right.linear.1knot$fit, col="red", lwd=2)
lines(range.grid.right, predict_fit.right.linear.1knot$fit + 2 * predict_fit.right.linear.1knot$se.fit, col="red", lwd=2, lty=2)
lines(range.grid.right, predict_fit.right.linear.1knot$fit - 2 * predict_fit.right.linear.1knot$se.fit, col="red", lwd=2, lty=2)
# indicate the breakpoint
abline(v=575, col="blue", lwd=2, lty=2)


# ------------------------------------------
# 7. piecewise cubic fits

fit.left.cubic.1knot <- lm( logratio ~ poly(range,3), subset=(range<575) )
fit.right.cubic.1knot <- lm( logratio ~ poly(range,3), subset=(range>=575) )

summary(fit.left.cubic.1knot)
summary(fit.right.cubic.1knot)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid.left <- seq(from=range.range[1], to=575, length.out=50)
range.grid.right <- seq(from=575, to=range.range[2], length.out=50)

# predict the fit on the grid
# left
predict_fit.left.cubic.1knot <- predict(fit.left.cubic.1knot, newdata=list(range=range.grid.left), se.fit=TRUE)
lines(range.grid.left, predict_fit.left.cubic.1knot$fit, col="red", lwd=2)
lines(range.grid.left, predict_fit.left.cubic.1knot$fit + 2 * predict_fit.left.cubic.1knot$se.fit, col="red", lwd=2, lty=2)
lines(range.grid.left, predict_fit.left.cubic.1knot$fit - 2 * predict_fit.left.cubic.1knot$se.fit, col="red", lwd=2, lty=2)
# right
predict_fit.right.cubic.1knot <- predict(fit.right.cubic.1knot, newdata=list(range=range.grid.right), se.fit=TRUE)
lines(range.grid.right, predict_fit.right.cubic.1knot$fit, col="red", lwd=2)
lines(range.grid.right, predict_fit.right.cubic.1knot$fit + 2 * predict_fit.right.cubic.1knot$se.fit, col="red", lwd=2, lty=2)
lines(range.grid.right, predict_fit.right.cubic.1knot$fit - 2 * predict_fit.right.cubic.1knot$se.fit, col="red", lwd=2, lty=2)
# indicate the breakpoint
abline(v=575, col="blue", lwd=2, lty=2)


# ------------------------------------------
# 8. linear splines

library(splines)

?bs

fit.ls.1knot <- lm( logratio ~ bs(range, knots=575, degree=1) )
summary(fit.ls.1knot)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# predict the fit on the grid
predict_fit.ls.1knot <- predict(fit.ls.1knot, newdata=list(range=range.grid), se.fit=TRUE)
lines(range.grid, predict_fit.ls.1knot$fit, col="red", lwd=2)
lines(range.grid, predict_fit.ls.1knot$fit + 2 * predict_fit.ls.1knot$se.fit, col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.ls.1knot$fit - 2 * predict_fit.ls.1knot$se.fit, col="red", lwd=2, lty=2)
abline(v=575, col="blue", lwd=2, lty=2)

# repeat above with 2 internal knots at desired points
fit.ls.2knots <- lm( logratio ~ bs(range, knots=c(550,600), degree=1) )
summary(fit.ls.2knots)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# predict the fit on the grid
predict_fit.ls.2knots <- predict(fit.ls.2knots, newdata=list(range=range.grid), se.fit=TRUE)
lines(range.grid, predict_fit.ls.2knots$fit, col="red", lwd=2)
lines(range.grid, predict_fit.ls.2knots$fit + 2 * predict_fit.ls.2knots$se.fit, col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.ls.2knots$fit - 2 * predict_fit.ls.2knots$se.fit, col="red", lwd=2, lty=2)
abline(v=c(550,600), col="blue", lwd=2, lty=2)


# ------------------------------------------
# 9. cubic splines

# fit cubic splines with 1 internal knot
fit.cs.1knot <- lm( logratio ~ bs(range, knots=575) )
summary(fit.cs.1knot)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# predict the fit on the grid
predict_fit.cs.1knot <- predict(fit.cs.1knot, newdata=list(range=range.grid), se.fit=TRUE)
lines(range.grid, predict_fit.cs.1knot$fit, col="red", lwd=2)
lines(range.grid, predict_fit.cs.1knot$fit + 2 * predict_fit.cs.1knot$se.fit, col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.cs.1knot$fit - 2 * predict_fit.cs.1knot$se.fit, col="red", lwd=2, lty=2)
abline(v=575, col="blue", lwd=2, lty=2)

# fit cubic splines with 2 internal knots
fit.cs.2knots <- lm( logratio ~ bs(range, knots=c(550,600)) )
summary(fit.cs.2knots)

# plot raw data
plot(range,logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=100)

# predict the fit on the grid
predict_fit.cs.2knots <- predict(fit.cs.2knots, newdata=list(range=range.grid), se.fit=TRUE)
lines(range.grid, predict_fit.cs.2knots$fit, col="red", lwd=2)
lines(range.grid, predict_fit.cs.2knots$fit + 2 * predict_fit.cs.2knots$se.fit, col="red", lwd=2, lty=2)
lines(range.grid, predict_fit.cs.2knots$fit - 2 * predict_fit.cs.2knots$se.fit, col="red", lwd=2, lty=2)
abline(v=c(550,600), col="blue", lwd=2, lty=2)


# ------------------------------------------
# 10. Compare the goodness of fit of each model

mse = function(model){
  rmse = sqrt(mean(model$residual^2))
  round(rmse, 3)
}
mse_fit.ls.1knot = mse(fit.ls.1knot)
mse_fit.ls.2knots = mse(fit.ls.2knots)
mse_fit.cs.1knot = mse(fit.cs.1knot)
mse_fit.cs.2knots = mse(fit.cs.2knots)

model_name = c("linear splines with 1 knot", "linear splines with 2 knots", 
               "cubic splines with 1 knot", "cubic splines with 2 knots")
mses = c(mse_fit.ls.1knot, mse_fit.ls.2knots,
         mse_fit.cs.1knot, mse_fit.cs.2knots)
mse_table = cbind(model_name, mses)
colnames(mse_table) = c("Models", "RMSE")
knitr::kable(mse_table, caption = "Mean squared errors for different models", digits = 3, "simple")


# ------------------------------------------
# 11. smoothing/natural splines

fit.ss <- smooth.spline(range, logratio)
fit.ss

# plot raw data
plot(range, logratio, ylab="response", xlab="X")
range.range <- range(range)
range.grid <- seq(from=range.range[1], to=range.range[2], length.out=length(range))

# predict the fit on the grid
?predict.smooth.spline
predict_fit.ss <- predict(fit.ss, newdata=list(range=range.grid))
lines(predict_fit.ss$x, predict_fit.ss$y, col="red", lwd=2)

# bootstrap standard errors
# helper functions borrowed from: 
# https://stackoverflow.com/questions/23852505/how-to-get-confidence-interval-for-smooth-spline

# Helper functions
resampler <- function(data) {
  n <- nrow(data)
  resample.rows <- sample(1:n,size=n,replace=TRUE)
  return(data[resample.rows,])
}

spline.estimator <- function(data,m=100) {
  fit <- smooth.spline(x=data[,1],y=data[,2], cv=T)
  eval.grid <- seq(from=min(data[,1]),to=max(data[,1]),length.out=m)
  return(predict(fit,x=eval.grid)$y) # We only want the predicted values
}

spline.cis <- function(data,B,alpha=0.05,m=100) {
  spline.main <- spline.estimator(data,m=m)
  spline.boots <- replicate(B,spline.estimator(resampler(data),m=m))
  cis.lower <- 2*spline.main - apply(spline.boots,1,quantile,probs=1-alpha/2)
  cis.upper <- 2*spline.main - apply(spline.boots,1,quantile,probs=alpha/2)
  return(list(main.curve=spline.main,lower.ci=cis.lower,upper.ci=cis.upper,
              x=seq(from=min(data[,1]),to=max(data[,1]),length.out=m)))
}

# sample data
data <- data.frame(x=range, y=logratio)

# run and plot
sp.cis <- spline.cis(data, B=10000, alpha=0.05)
plot(data[,1],data[,2], ylab="response", xlab="X")
lines(x=sp.cis$x,y=sp.cis$main.curve, col="red")
lines(x=sp.cis$x,y=sp.cis$lower.ci, lty=2, lwd=2, col="red")
lines(x=sp.cis$x,y=sp.cis$upper.ci, lty=2, lwd=2, col="red")
