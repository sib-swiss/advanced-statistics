# Generalized additive models

In this section, you will find the R code that we will use during the course. We will explain the code and output during correction of the exercises.

Slides of lectures:

[Download slides](assets/pdf/GAM.pdf){: .md-button }

Data for exercises:

[Download data](assets/exercises/data.zip){: .md-button }


Exercise 1: Mid-Atlantic Wage Data

Wage and other data for a group of 3000 male workers in the Mid-Atlantic region.

First we set for this exercise the libraries that we will need.

```r
library(akima)
library(gam)
library(ISLR)
library(splines)
```

We will now load the Mid-Atlantic Wage dataset which is part of the ISLR package, which is called Wage.

First have a look at the available variables by checking the help of that package. Once the ISLR package is loaded it attached to your environment some variable that are ready to use as year, age and wage for instance which are part of the Wage data.

```r
?Wage

# explore the data
str(Wage)

# see for example the head of the year variable which is available
head(year)
```

Now the goal is to build a predictive model for wage based on year, age, marital status and other variables. We will do the exercise in 7 parts, which we separate in different exercises.

# Step1: From linear to degree-5 polynomial regression

The goal is to fit models ranging from linear to a degree-5 polynomial and seek to determine the simplest model which is sufficient to explain the relationship between wage and age.

First, we perform a linear regression, and we have a look at the summary of the fit. How do you write this ?

??? done "Answer"
    ```r
    # Hint: Use anova() to perform an F-test to compare nested       models.

    fit1 <- lm(wage ~ age, data=Wage)
    summary(fit1)
    ```

Now we use the poly function for polynomial regression. Which one looks best ? Compare the different fits!

```r
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
```

??? done "Answer"
    The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero (<10???15), indicating that a linear fit is not sufficient. Similarly the p-value comparing the quadratic Model 2 to the cubic Model 3 is very low (0.0017), so the quadratic fit is also insufficient. The p-value comparing the cubic and degree-4 polynomials, Model 3 and Model 4, is approximately 5% while the degree-5 polynomial Model 5 seems unnecessary because its p-value is 0.37. Hence, either a cubic or a quartic polynomial appear to provide a reasonable fit to the data, but lower- or higher-order models are not justified.
    
    
# Step2: Predict whether an individual earns more than $250'000 per year  

??? Hint
    First create the appropriate response vector using I(), and then apply the glm() function using family="binomial" in order to fit a polynomial logistic regression model.


```r
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
```

#Step3 : Step functions to model relationship between wage and age

??? Hint
    Use cut() to fit step functions.



```r
table(cut(age,4))
fit.step = lm(wage ~ cut(age,4), data=Wage)
summary(fit.step)
plot(fit.step)
```

#Step4: Linear and cubic splines

We proceed in 3 steps here.

i) Fit wage to age using linear splines with internal knots at 25th, 50th and 75th percentiles of age.
ii) Repeat with a cubic spline.
iii) Test if using cubic splines improved the goodness of fit.

??? Hint
    Use bs() to fit splines

```r
quantile(age, probs=0.25)
quantile(age, probs=0.5)
quantile(age, probs=0.75)
fit.linearsplines = lm(wage ~ bs(age, knots=c(33.75,42,51)), data=Wage)
pred.fit.linearsplines = predict(fit.linearsplines, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred.fit.linearsplines$fit, lwd=2)
lines(age.grid, pred.fit.linearsplines$fit+2*pred.fit.linearsplines$se, lty="dashed")
lines(age.grid, pred.fit.linearsplines$fit-2*pred.fit.linearsplines$se, lty="dashed")
```

#Step5 : Smoothing splines

Start with fitting wage to age using smoothing splines.Then assess the effect of varying "df" on the smooth. 
In this exercise, try different df for different smoothing. df controls the "complexity" of the model employed. What do you observe ?





```r

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
```

??? done "Answer""
    A substantial difference can be found when going from 2 to 10, but very little change will take place when going from 10 to 50 
    
# Step6: Local regressions
Fit wage to age using a local regression (loess function) with default parameters
What does the parameter "span" refer to in the loess function? How does increasing the "span" affect the fit?

```r
fit.local = loess(wage ~ age, span =.2, data=Wage)
fit.local.2 = loess(wage ~ age, span =.5, data=Wage)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
lines(age.grid, predict(fit.local,data.frame(age=age.grid)), col ="red ",lwd=2)
lines(age.grid, predict(fit.local.2,data.frame(age=age.grid)), col =" blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"), col=c("red", "blue"), lty =1, lwd =2, cex =.8)
```

#Step7 : General Additive Model 
Load the library "gam" and try to look at the help of the gam function. Fit a GAM to predict wage using smoothing splines of year and age, treating education as a qualitative predictor
Repeat using a GAM with a loess smoother. Try to plot the fitted gam objects.


```r
gam.s = gam(wage ~ s(year,4)+s(age,5)+education, data=Wage)
plot(gam.s)
gam.lo.1 = gam(wage ~ lo(year,span=0.5)+lo(age,span=0.5)+education, data=Wage)
plot(gam.lo.1)
```

#Step8: ANOVA tests

Determine which of these three models is the best:
i) a GAM that excludes year (M1),
ii) a GAM that uses a linear function of year (M2),
iii) or a GAM that uses a spline function of year (M3).
iv) Make prediction on the training set (original data) using M2 from above.

```r
gam.m1 = gam(wage ~ s(age,5)+education, data=Wage)
gam.m2 = gam(wage ~ year+s(age,5)+education, data=Wage)
gam.m3 = gam(wage ~ s(year,4)+s(age,5)+education, data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

pred.gam.m2 = predict(gam.m2,newdata=Wage)
```

#Step9: GAMs with interaction

Use `r lo()` function to allow for interaction between age and year in the GAM, treating education like before. We can plot the resulting two-dimensional surface using the akima package (akima::plot).


```r
gam.lo.2 = gam(wage ~ lo(year,age,span=0.5)+education, data=Wage)
plot(gam.lo.2)
```

# STEP10: Logistic regression GAMs

Try to fit a logistic regression GAM, handling year with a linear function, age with a smoothing spline, and education with dummy variables like before. Try removing the "<HS" category. Use the `r plot()` function to visualize the logistic regression GAMs.

```r
gam.lr = gam(I(wage>250) ~ year+s(age,5)+education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

gam.lr.s = gam(I(wage>250) ~ year+s(age,5)+education, family=binomial, data=Wage, 
               subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
```
