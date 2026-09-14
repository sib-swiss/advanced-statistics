########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; Aug 2023
# GAM

library(akima)
library(gam)
library(ISLR)
library(splines)



########################################################
# Mid-Atlantic Wage Data : Wage and other data for a group of 3000 male workers 
# in the Mid-Atlantic region.
########################################################

# load the MAWage data set
attach(Wage)

# explore the data
str(Wage)


# ------------------------------------------
# The goal is to build a predictive model for wage based on year, age, marital status, etc.


# ------------------------------------------
# STEP1: Fit models ranging from linear to a degree-5 polynomial and seek to determine 
# the simplest model which is sufficient to explain the relationship between wage and age.
# Hint: Use anova() to perform an F-test to compare nested models.

fit1 <- lm(wage ~ age, data=Wage)
summary(fit1)
fit2 <- lm(wage ~ poly(age,2), data=Wage)
summary(fit2)
anova(fit1, fit2)
fit3 <- lm(wage ~ poly(age,3), data=Wage)
summary(fit3)
anova(fit2, fit3)
fit4 <- lm(wage ~ poly(age,4), data=Wage)
summary(fit4)
anova(fit3, fit4)
fit5 <- lm(wage ~ poly(age,5), data=Wage)
summary(fit5)
anova(fit4, fit5)

# The p-value comparing the linear Model 1 to the quadratic Model 2 is
# essentially zero (<10???15), indicating that a linear fit is not sufficient. Similarly
# the p-value comparing the quadratic Model 2 to the cubic Model 3
# is very low (0.0017), so the quadratic fit is also insufficient. The p-value
# comparing the cubic and degree-4 polynomials, Model 3 and Model 4, is approximately
# 5% while the degree-5 polynomial Model 5 seems unnecessary
# because its p-value is 0.37. Hence, either a cubic or a quartic polynomial
# appear to provide a reasonable fit to the data, but lower- or higher-order
# models are not justified.


# ------------------------------------------
# STEP2: Consider the task of predicting whether an individual earns more than $250,000 per year.
# Hint: First create the appropriate response vector using I(), and then apply the glm() function using family="binomial" in order to fit a polynomial logistic regression model.

fit.glm.4 <- glm(I(wage>250) ~ poly(age,4), data=Wage, family=binomial)
summary(fit.glm.4)

agelims = range(age)
age.grid = seq(from=agelims[1],to=agelims[2])
pred.fit.glm.4 = predict(fit.glm.4, newdata=list(age=age.grid), type="response", se=T)

proba.fit.glm.4 = exp(pred.fit.glm.4$fit)/(1+exp(pred.fit.glm.4$fit))
se.bands.logit = cbind(pred.fit.glm.4$fit + 2*pred.fit.glm.4$se.fit, 
                       pred.fit.glm.4$fit - 2*pred.fit.glm.4$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

plot(age, I(wage > 250), xlim=agelims, type ="n", ylim=c(0,1))
points(jitter(age), I(wage>250), cex=.5, pch="|", col ="darkgrey")
lines(age.grid, proba.fit.glm.4, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd =1, col="blue", lty =2)


# ------------------------------------------
# STEP3: Use a step function to model the relationship between wage and age.
# Hint: Use cut() to fit step functions.

table(cut(age,4))
fit.step = lm(wage ~ cut(age,4), data=Wage)
summary(fit.step)
plot(fit.step)


# ------------------------------------------
# STEP4: i) Fit wage to age using linear splines with internal knots at 25th, 50th and 75th percentiles of age.
#        ii) Repeat with a cubic spline.
#        iii) Test if using cubic splines improved the goodness of fit.
# Hint: Use bs() to fit splines

quantile(age, probs=0.25)
quantile(age, probs=0.5)
quantile(age, probs=0.75)
fit.linearsplines = lm(wage ~ bs(age, knots=c(33.75,42,51)), data=Wage)
pred.fit.linearsplines = predict(fit.linearsplines, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred.fit.linearsplines$fit, lwd=2)
lines(age.grid, pred.fit.linearsplines$fit+2*pred.fit.linearsplines$se, lty="dashed")
lines(age.grid, pred.fit.linearsplines$fit-2*pred.fit.linearsplines$se, lty="dashed")


# ------------------------------------------
# STEP5: i) Fit wage to age using smoothing splines.
#        ii) Assess the effect of varying "df" on the smooth. 

# try different df for different smoothing
# df controls the "complexity" of the model employed.
# A substantial difference can be found when going from 2 to 10, 
# but very little change will take place when going from 10 to 50 
# (the model only gets more complex, but the general shape won't change).
fit.smooth.spline.5 = smooth.spline(age, wage, df=5)
fit.smooth.spline.10 = smooth.spline(age, wage, df=10)
fit.smooth.spline.15 = smooth.spline(age, wage, df=15)
fit.smooth.spline.20 = smooth.spline(age, wage, df=20)
# cross-validation
fit.smooth.spline.cv = smooth.spline(age, wage, cv=TRUE)

plot(age, wage, col="gray")
lines(fit.smooth.spline.5, col="red",lty=1)
lines(fit.smooth.spline.10, col="red",lty=2)
lines(fit.smooth.spline.15, col="red",lty=3)
lines(fit.smooth.spline.20, col="red",lty=4)
lines(fit.smooth.spline.cv, col="blue",lty=1)
legend("topright", legend=c("5 DF","10 DF","15 DF","20 DF","6.8 DF"), 
       col=c("red","red","red","red","blue"), lty=c(1,2,3,4,1), cex=.8)


# ------------------------------------------
# STEP6: i) Fit wage to age using a local regression (loess) with default parameters
#        ii) What does the parameter "span" refer to in the loess function? How does increasing the "span" affect the fit?

# spans of 0.2 and 0.5: each neighborhood consists of 20% or 50% of the observations
fit.local = loess(wage ~ age, span =.2, data=Wage)
fit.local.2 = loess(wage ~ age, span =.5, data=Wage)

plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
lines(age.grid, predict(fit.local,data.frame(age=age.grid)), col ="red ",lwd=2)
lines(age.grid, predict(fit.local.2,data.frame(age=age.grid)), col =" blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"), col=c("red", "blue"), lty =1, lwd =2, cex =.8)


# ------------------------------------------
# STEP7: i) load the library(gam) and try ?gam
#        ii) Fit a GAM to predict wage using smoothing splines of year and age, treating education as a qualitative predictor
#        iv) Try to repeat ii) using gam() with a loess smoother
#        v) Try plot() on the fitted gam objects

gam.s = gam(wage ~ s(year,4)+s(age,5)+education, data=Wage)
plot(gam.s)
gam.lo.1 = gam(wage ~ lo(year,span=0.5)+lo(age,span=0.5)+education, data=Wage)
plot(gam.lo.1)


# ------------------------------------------
# STEP8: Perform a series of ANOVA tests in order to determine which of these three models is best:
#         i) a GAM that excludes year (M1),
#         ii) a GAM that uses a linear function of year (M2),
#         iii) or a GAM that uses a spline function of year (M3).
#         iv) Make prediction on the training set (original data) using M2 from above.

gam.m1 = gam(wage ~ s(age,5)+education, data=Wage)
gam.m2 = gam(wage ~ year+s(age,5)+education, data=Wage)
gam.m3 = gam(wage ~ s(year,4)+s(age,5)+education, data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

pred.gam.m2 = predict(gam.m2,newdata=Wage)


# ------------------------------------------
# STEP9: i) Use lo() to allow for interaction between age and year in the GAM, treating education like before.
#         ii) We can plot the resulting two-dimensional surface if we first install the akima package (akima::plot)

gam.lo.2 = gam(wage ~ lo(year,age,span=0.5)+education, data=Wage)
plot(gam.lo.2)


# ------------------------------------------
# STEP10: i) Fit a logistic regression GAM, handling year with a linear function, age with a smoothign spline, and education with dummy variables like before.
#         ii) Repeat above after removing the "<HS" category.
#         iii) Use plot() to visualize the logistic regression GAMs.

gam.lr = gam(I(wage>250) ~ year+s(age,5)+education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

gam.lr.s = gam(I(wage>250) ~ year+s(age,5)+education, family=binomial, data=Wage, 
               subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

