########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Longitudinal Data Analysis

library(lattice)
library(nlme)
library(splines)



########################################################
# Tolerance data set
########################################################
library(lattice)
library(nlme)
library(splines)
# load the "tolerance" data set: "tolerance.RData"
load("tolerance.RData")

str(tolerance_untidy)
str(tolerance_tidy)


# ------------------------------------------
# Perform a graphical exploration of the data

# scatterplots of raw data
xyplot(tolerance ~ age | as.factor(id), ylim=c(0,4), data=tolerance_tidy, as.table=F)


# ------------------------------------------
# Analysis of individual change over time

# nonparametric smoothing spline fit
xyplot(tolerance ~ age | as.factor(id), ylim=c(0,4), data=tolerance_tidy, 
       prepanel = function(x,y) prepanel.spline(x,y), 
       xlab = "age", ylab = "tolerance", 
       panel = function(x,y){ 
         panel.xyplot(x,y) 
         panel.spline(x,y)}
)

# nonparametric loess fit (we will learn more about loess during GAM lecture)
xyplot(tolerance ~ age | as.factor(id), ylim=c(0,4), data=tolerance_tidy,
       prepanel = function(x,y) prepanel.loess(x,y, family="gaussian"),
       xlab = "age", ylab = "tolerance",
       panel = function(x,y){
         panel.xyplot(x,y)
         panel.loess(x,y, family="gaussian")}
)

# parametric/linear fit
xyplot(tolerance ~ age | as.factor(id), ylim=c(0,4), data=tolerance_tidy,
       prepanel = function(x,y) prepanel.lmline(x,y),
       xlab = "age", ylab = "tolerance",
       panel = function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)}
)

# extract summary from individual linear fits
lm.summary <- by(tolerance_tidy, tolerance_tidy$id, function(x) summary(lm(tolerance ~ time, data=x)))
lm.summary[1]
lm.summary[[1]]$coefficients

# fetch intercepts
all.intercept <- sapply(lm.summary, function(x) x$coefficients[1,1])

# fetch slopes
all.slope <- sapply(lm.summary, function(x) x$coefficients[2,1])

# fetch residual variances, i.e. (Residual standard error)^2
all.resVar <- sapply(lm.summary, function(x) (x$sigma)^2)

# fetch R-squared statistic
all.r2 <- sapply(lm.summary, function(x) x$r.squared)

# combine them in one matrix
my.summary <- rbind(all.intercept, all.slope, all.resVar, all.r2)
my.summary

# remove the junk
rm(all.intercept, all.slope, all.resVar, all.r2)


# ------------------------------------------
# Analysis of inter-individual differences


# --------------
### average of the curves (nonparametric)

# start with an empty plot
plot(1, type="n", xlab="age", ylab="tolerance", xlim=c(11,15), ylim=c(0,4), main="nonparametric")

# i) discretize time on a grid
t <- seq(11,15,length.out=100)

# ii) estimate individual trajectories
indiv <- unique(tolerance_tidy$id)

# create matrix to store estimates on the grid
est.nonpara <- matrix(NA, nrow=16, ncol=100)

# loop on individuals to plot individual curves and extract estimates on the grid
for (i in 1:length(indiv)) {
  temp.data <- subset(tolerance_tidy, subset=(id==indiv[i]))
  age <- temp.data$age
  tolerance <- temp.data$tolerance
  fit <- smooth.spline(age, tolerance)
  est <- predict(fit, data.frame(age=t))
  # plot the estimate
  lines(t(est$x),t(est$y), col="grey")
  # store the estimate
  est.nonpara[i,] <- t(est$y)
}

# average individual estimates for each point on the grid
avg.est <- apply(est.nonpara, MARGIN=2, mean)

# apply same smoothing algorithm to averages
fit <- smooth.spline(t, avg.est)

# overlay the curve of the averages
lines(t, fit$y, lwd=2)

# assign ids to rownames
rownames(est.nonpara) <- indiv
est.nonpara

# remove the junk
rm(t,indiv,i,temp.data,age,tolerance,fit,est,avg.est)


# --------------
### average of the curves (parametric)

# start with an empty plot
plot(1, type="n", xlab="age", ylab="tolerance", xlim=c(11,15), ylim=c(0,4), main="parametric")

# i) discretize time on a grid
t <- seq(11,15,length.out=100)

# centering time predictor is optional but doing so improves the interpretability of the intercept (i.e. the intercept will reflect baseline exposure at age=11)
t.cent <- seq(0,4,length.out=100) 

# ii) estimate individual trajectories
indiv <- unique(tolerance_tidy$id)

# matrix to store estimates on the grid
est.para <- matrix(NA, nrow=16, ncol=100)

# loop on individuals to plot individual curves and extract estimates on the grid
for (i in 1:length(indiv)) {
  temp.data <- subset(tolerance_tidy, subset=(id==indiv[i]))
  age <- temp.data$age
  age.cent <- temp.data$age - 11
  tolerance <- temp.data$tolerance
  fit <- lm(tolerance ~ age.cent)
  est <- predict(fit, data.frame(age.cent=t.cent), type="response")
  # plot the estimate
  abline( lm( tolerance ~ age), col="grey")
  # store the estimate
  est.para[i,] <- est
}

# average individual estimates for each point on the grid
avg.est <- apply(est.para, MARGIN=2, mean)

# apply same smoothing algorithm to averages
abline(lm( avg.est ~ t), lwd=2)

# assign ids to rownames
rownames(est.para) <- indiv
est.para

# remove the junk
rm(t,t.cent,indiv,i,temp.data,age,age.cent,tolerance,fit,est,avg.est)


# --------------
### assess intercepts and slopes from linear fit

# all.intercepts
mean(my.summary["all.intercept",]) # 1.35775
sqrt( var(my.summary["all.intercept",]) ) # 0.2977792

# all.slopes
mean(my.summary["all.slope",]) # 0.1308125
sqrt( var(my.summary["all.slope",]) ) # 0.172296

# bivariate correlation
cor(my.summary["all.intercept",], my.summary["all.slope",]) # -0.4481135


# --------------
### stratify based on gender

table(tolerance_untidy$male) # 9 x females and 7 x males

# discretize time on a grid
t <- seq(11,15,length.out=100)

# figure with 2 panels
par(mfrow=c(1,2))

# plot individual males
est.para_males <- est.para[rownames(est.para) %in% tolerance_untidy$id[tolerance_untidy$male==1], ]
plot(1, type="n", xlab="age", ylab="tolerance", xlim=c(11,15), ylim=c(0,4), main="males")
apply(est.para_males, MARGIN=1, function(x,t) {lines(t,x, col="gray")}, t=t)
avg.est <- apply(est.para_males, MARGIN=2, mean)
abline(lm( avg.est ~ t), lwd=2)

# plot individual females
est.para_females <- est.para[rownames(est.para) %in% tolerance_untidy$id[tolerance_untidy$male==0], ]
plot(1, type="n", xlab="age", ylab="tolerance", xlim=c(11,15), ylim=c(0,4), main="females")
apply(est.para_females, MARGIN=1, function(x,t) {lines(t,x, col="gray")}, t=t)
avg.est <- apply(est.para_females, MARGIN=2, mean)
abline(lm( avg.est ~ t), lwd=2)

# remove the junk
rm(est.para_males, est.para_females, t, avg.est)


# --------------
### stratify based on exposure

summary(tolerance_untidy$exposure)

# exposure is a continuous time-invariant predictor, and thus can be categorized for visualization
median.exposure <- median(tolerance_untidy$exposure)

# discretize time on a grid
t <- seq(11,15,length.out=100)

# figure with 2 panels
par(mfrow=c(1,2))

# plot individuals with high-exposure
est.para_highExposure <- est.para[rownames(est.para) %in% tolerance_untidy$id[tolerance_untidy$exposure > median.exposure], ]
plot(1, type="n", xlab="age", ylab="tolerance", xlim=c(11,15), ylim=c(0,4), main="high-exposure")
apply(est.para_highExposure, MARGIN=1, function(x,t) {lines(t,x, col="gray")}, t=t)
avg.est <- apply(est.para_highExposure, MARGIN=2, mean)
abline(lm( avg.est ~ t), lwd=2)

# plot individuals with low-exposure
est.para_lowExposure <- est.para[rownames(est.para) %in% tolerance_untidy$id[tolerance_untidy$exposure <= median.exposure], ]
plot(1, type="n", xlab="age", ylab="tolerance", xlim=c(11,15), ylim=c(0,4), main="low-exposure")
apply(est.para_lowExposure, MARGIN=1, function(x,t) {lines(t,x, col="gray")}, t=t)
avg.est <- apply(est.para_lowExposure, MARGIN=2, mean)
abline(lm( avg.est ~ t), lwd=2)

# remove the junk
rm(est.para_highExposure, est.para_lowExposure, t, avg.est)
rm(median.exposure)





########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Mixed models

# The purpose of this TP is to carry out some mixed model analyses using R. 
# As usual, make sure that you read the help for any new functions that you use.

library(DAAG)
library(lattice) 
library(lme4)
library(WWGbook)


########################################################
# Corn Data
########################################################

# dataframe: ant111b in the DAAG package.
# agricultural experiment on the Caribbean island of Antigua
# Corn yield measurements were taken on 4 parcels at 8 sites

data(ant111b)

str(ant111b)
summary(ant111b) 

dotplot(reorder(site, harvwt) ~ harvwt, ant111b, xlab = "Harvest weight of corn",
        ylab = "Site", pch = 19, aspect = 0.32, type = c("p", "a"))
# The line joins the means of the harvest weight of the individual sites
# The sites have been reordered by increasing mean harvwt

summary( lm( ant111b$harvwt ~ ant111b$site ) )
anova( lm( ant111b$harvwt ~ ant111b$site ) )

model.1 <- lm( ant111b$harvwt ~ ant111b$site )
model.null <- lm( ant111b$harvwt ~ 1 )
anova(model.null, model.1)

mean(ant111b$harvwt)
mean(ant111b[ant111b$site == "DBAN",]$harvwt)
mean(ant111b[ant111b$site == "LFAN",]$harvwt)
mean(ant111b[ant111b$site == "NSAN",]$harvwt)
mean(ant111b[ant111b$site == "ORAN",]$harvwt)
mean(ant111b[ant111b$site == "OVAN",]$harvwt)
mean(ant111b[ant111b$site == "TEAN",]$harvwt)
mean(ant111b[ant111b$site == "WEAN",]$harvwt)
mean(ant111b[ant111b$site == "WLAN",]$harvwt)


ant111b.lmer <- lmer(harvwt ~ 1 + (1| site), data=ant111b)
ant111b.lmer

# Our model has one fixed effect parameter (the first 1): the mean harvest weight
# and one random effect term (1|site): the variation across sites
# There are two sources of random variation: one for site and one for parcel within site.
# The estimated variance components are:
#   . ??2site = 1.5392^2 = 2.369
#   . ??2residual = 0.762 = 0.577

mean(ant111b$harvwt)
sqrt(var(ant111b$harvwt))
fixef(ant111b.lmer)
# The numbers provided by ranef aren't estimates of the random effects
# They are called BLUPs (Best Linear Unbiased Predictors) of the random effects
ranef(ant111b.lmer)
fitted(ant111b.lmer)

means <- with(ant111b, sapply(split(harvwt, site), mean))
siteFit <- with(ant111b, sapply(split(fitted(ant111b.lmer), site), mean))
siteFit.lm <- with(ant111b, sapply(split(fitted(model.1), site), mean))

print(data.frame(mean = means, fitted = siteFit,fitted.lm=siteFit.lm))
# The fitted values are not just the sample means.
# They are shrinkage estimates that are between the grand (overall) mean 
# and the individual sample means

# site	mean	fitted		site	ranef	
# DBAN	4.885	4.851		  DBAN	0.559	  0.061
# LFAN	4.208	4.212		  LFAN	-0.079	0.061
# NSAN	2.090	2.217		  NSAN	-2.075	0.061
# ORAN	6.915	6.764		  ORAN	2.473	  0.061
# OVAN	4.833	4.801		  OVAN	0.510	  0.061
# TEAN	3.036	3.108		  TEAN	-1.183	0.061
# WEAN	5.526	5.455		  WEAN	1.164	  0.061
# WLAN	2.841	2.925		  WLAN	-1.367	0.061
# var	  2.512	2.232		  var	  2.232	
# stdev	1.585	1.494		  stdev	1.494	

dotplot(ranef(ant111b.lmer, condVar = TRUE), strip = FALSE)[[1]] 


# ------------------------------------------
# Now, we will do the same type of modeling but this time with the ears variable as the outcome.


# ------------------------------------------
# 1. make a dotplot of the number of ears by site, sorting by mean ears as follows:

dotplot(reorder(site, ears) ~ ears, ant111b, xlab = "Number of ears of corn", 
        ylab = "Site", pch = 19, aspect = 0.32, type = c("p", "a"))

# Comment on your plot - do any effects seem to contribute to the variation in ears ? 


# ------------------------------------------
# 2. fit a random effects model

ears.lmer <- lmer(ears ~ 1 + (1 | site), data=ant111b)
summary(ears.lmer)

# There are two sources of random variation, one for site and one for parcel within site (residual), 
# each with an estimated variance (and SD).

# Find the grand mean.

mean(ant111b$ears)
fixef(ears.lmer)

# Make a table showing the sample mean and fitted value for each site.
# Note that the fitted values are not just the sample means, but 
# are between the grand mean and individual group sample means.

means <- with(ant111b, sapply(split(ears, site), mean))
siteFit <- with(ant111b, sapply(split(fitted(ears.lmer), site), mean))
print(data.frame(mean = means, fitted = siteFit))

# Give the estimated variance for each source of variation, ??2site and ??2residual.

variance.site =  6.760^2 = 45.696
variance.residual = 3.980^2 = 15.839 

# Which source of variation is larger ? 
# What proportion of variation is due to differences between sites ?
# Make a caterpillar plot for the random effects.
# Does the plot support your conclusion about the source of variation ?
# Which site(s) are most 'unusual'? 

dotplot(ranef(ears.lmer, condVar = TRUE), strip = FALSE)[[1]] 


# ------------------------------------------
# 3. It is also a good idea to check the model assumptions with a few diagnostic plots.

# There should not be any apparent pattern in the residuals.
# You can check this by making a plot of residuals versus fitted values:

plot(fitted(ears.lmer), residuals(ears.lmer), main="residual plot", pch=19)
abline(h=0, lty=2)

# The residuals should also be normally distributed.
# You can check this by making a normal quantile-quantile (QQ) plot.
# If the points fall along a straight line, the distribution is approximately normal.

qqnorm(resid(ears.lmer))
qqline(resid(ears.lmer)) 
shapiro.test(resid(ears.lmer))

# What do you conclude ? 

########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Longitudinal Data Analysis

library(lattice)
library(nlme)
library(splines)

########################################################
# Tolerance data set
########################################################

## Next steps : 
# ------------------------------------------
# multi-level / mixed-effects modeling

# let's begin by assessing the need for a multi-level model
# First, we will fit a baseline model (only including an intercept) using ML
# Next, we will fit another model that allows intercepts to vary between clusters 
# (i.e. a random intercept model)
# finally we compare the two models to see if the fit has improved as a result of allowing 
# intercepts to vary

# fit the 1st model
fit.01 <- gls(tolerance ~ 1, data=tolerance_tidy, method="ML")
summary(fit.01)

# plot fit.01
plot(tolerance_tidy$age, tolerance_tidy$tolerance, ylim=c(0,4), ylab="tolerance", xlab="age")
fit.01$coefficients
abline(h=1.619375, col="red", lwd=2)

# fit the 2nd model
fit.02 <- lme(tolerance ~ 1, random = (~ 1 | id), data=tolerance_tidy, method="ML")
summary(fit.02)
fit.02$coefficients

# plot fit.02 for patient "978" and compare with fit.01
plot(tolerance_tidy$age, tolerance_tidy$tolerance, ylim=c(0,4), ylab="tolerance", xlab="age")
points(tolerance_tidy$age[tolerance_tidy$id=="978"], tolerance_tidy$tolerance[tolerance_tidy$id=="978"], col="blue", pch=16)
fit.02$coefficients
abline(h = fit.02$coefficients$fixed, col="red", lwd=2)
fit.02$coefficients$fixed + unlist(fit.02$coefficients$random)[12]
abline(h = fit.02$coefficients$fixed + unlist(fit.02$coefficients$random)[12], col="blue", lwd=2)

# plot fit.02 for all individual
plot(tolerance_tidy$age, tolerance_tidy$tolerance, ylim=c(0,4), ylab="tolerance", xlab="age")
abline(h = fit.02$coefficients$fixed + unlist(fit.02$coefficients$random), col="blue", lwd=2)

# compare the two using AIC, BIC, and likelihood-ratio(LR)
anova(fit.01,fit.02)

# keeping in mind that the assumptions of the LR test is that:
# i) models were fit using ML
# ii) model are nested
# both assumption were met and p-val<0.05; therefore we can conclude that allowing for 
# random intercepts significantly improved the fit


# --------------
# What if instead of random intercepts, we had allowed for random slopes?
# We have no reason to believe that individuals should share a baseline value 
# (i.e. fixed intercept), but let's try it anyways for the sake of completeness

# fit the 1st model
fit.03 <- gls(tolerance ~ time, data=tolerance_tidy, method="ML") # using centered age (i.e. time) for increased interpretability
summary(fit.03)

# plot fit.03
plot(tolerance_tidy$age, tolerance_tidy$tolerance, ylim=c(0,4), ylab="tolerance", xlab="age")
fit.03$coefficients
abline( gls(tolerance ~ age, data=tolerance_tidy, method="ML"), col="red", lwd=2)

# fit the 2nd model
fit.04 <- lme(tolerance ~ time, random = (~ -1 + time | id), data=tolerance_tidy, method="ML")
summary(fit.04)
fit.04$coefficients

# plot fit.04 for patient "978" and compare with fit.03
plot(tolerance_tidy$age, tolerance_tidy$tolerance, ylim=c(0,4), ylab="tolerance", xlab="age")
points(tolerance_tidy$age[tolerance_tidy$id=="978"], tolerance_tidy$tolerance[tolerance_tidy$id=="978"], col="blue", pch=16)
abline(lm(tolerance ~ age, data=tolerance_tidy), col="red", lwd=2)
predict.fit.04 <- predict(fit.04, newdata=data.frame(time=seq(0,4,length.out=100), id="978"))
lines(seq(11,15,length.out=100), predict.fit.04, col="blue", lwd=2)

# plot fit.04 for all individuals
plot(tolerance_tidy$age, tolerance_tidy$tolerance, ylim=c(0,4), ylab="tolerance", xlab="age")
id <- as.character(unique(tolerance_untidy$id))
for (i in 1:length(id)) {
  predict.fit.04 <- predict(fit.04, newdata=data.frame(time=seq(0,4,length.out=100), id=id[i]))
  lines(seq(11,15,length.out=100), predict.fit.04, col="blue", lwd=1)
}

# compare the two using AIC, BIC, and LR
anova(fit.03,fit.04)


# --------------
# What if instead we had allowed for both random intercepts and random slopes?

# the 1st model is same as fit.03
# fit the 2nd model
fit.05 <- lme(tolerance ~ time, random = (~ time | id), data=tolerance_tidy, method="ML")
summary(fit.05)

# extract both fixed and random parameters
fit.05$coefficients
coef(fit.05) # only fixed parameters
ranef(fit.05) # only random parameters

# compare the two using likelihood-ratio(LR)
anova(fit.03,fit.05)
# Based on what we saw above, can you plot these fitted models? (i.e. fit.03 and fit.05)


# --------------
# now let's bring in additional covariates

# adding gender
tolerance_tidy$male <- factor(tolerance_tidy$male, levels=c(0,1))
fit.06 <- lme(tolerance ~ exposure + time, random = (~ time | id), data=tolerance_tidy, method="ML")
summary(fit.06)
fit.06$coefficients
anova(fit.05,fit.06)

# adding exposure
fit.07 <- lme(tolerance ~male +exposure + time, random = (~ time | id), data=tolerance_tidy, method="ML")
summary(fit.07)
anova(fit.06,fit.07)

# adding interaction between male and exposure
fit.08 <- lme(tolerance ~ male * exposure + time, random = (~ time | id), data=tolerance_tidy, method="ML")
summary(fit.08)
anova(fit.07,fit.08)

# adding interaction between male and time
fit.09 <- lme(tolerance ~ exposure + male * time, random = (~ time | id), data=tolerance_tidy, method="ML")
summary(fit.09)
anova(fit.07,fit.09) # this agrees with our observation from exploratory analysis

# adding interaction between exposure and time
fit.10 <- lme(tolerance ~ male + exposure * time, random = (~ time | id), data=tolerance_tidy, method="ML")
summary(fit.10)
anova(fit.07,fit.10)

fit.12 <- lme(tolerance ~  exposure + time, random = (~ time | id), data=tolerance_tidy, method="ML")

# this agrees with our observation from exploratory analysis

# based on above, we will choose fit.07 as our model.


########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Longitudinal Data Analysis

library(lattice)
library(nlme)
library(splines)
########################################################
# BtheB data set
########################################################

# load the "BtheB_tidy" data set: "BtheB_tidy.RData"
load("BtheB_tidy.RData")

# examine the data 
str(BtheB.tidy)

# Store "pre" bdi values separately as baseline
temp.pre <- subset( BtheB.tidy, subset=(timepoint=="pre") )
temp.pre <- temp.pre[,c(5,6)]
BtheB.tidy <- subset( BtheB.tidy, subset=(timepoint!="pre") )
temp <- BtheB.tidy$indiv
temp2 <- c()
for (i in 1:length(temp)) {
  temp2[i] <- temp.pre$bdi[temp.pre$indiv==temp[i]]
}
BtheB.tidy <- cbind.data.frame(BtheB.tidy, pre.bdi=temp2)
rm(temp.pre,temp,temp2,i)

# let's convert timepoints to numerics
temp <- as.character(BtheB.tidy$timepoint)
temp <- replace(temp, temp=="2m", 2)
temp <- replace(temp, temp=="4m", 4)
temp <- replace(temp, temp=="6m", 6)
temp <- replace(temp, temp=="8m", 8)
BtheB.tidy$timepoint <- as.numeric(temp)
str(BtheB.tidy)
rm(temp)


# ------------------------------------------
# exploratory analysis

# raw data

xyplot(bdi ~ timepoint | indiv, data=BtheB.tidy[BtheB.tidy$indiv%in%sample(BtheB.tidy$indiv,5),], as.table=F,
       xlab = "time", ylab = "bdi")


# ------------------------------------------
# model fitting/selection

fit.01 <- gls(bdi ~ timepoint, data=BtheB.tidy, method="ML", na.action=na.omit)
fit.02 <- lme(bdi ~ timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
anova(fit.01,fit.02)

# including random intercepts improves the goodness of fit

fit.03 <- lme(bdi ~ timepoint, random = (~ timepoint | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
anova(fit.02,fit.03)
# including random slopes does not improve the goodness of fit
# based on above we will choose the simpler random intercepts model 

# Note that in lme by default na.action is set to na.fail, hence you'll get an error 
# if NA values are present
# By setting na.action=na.omit we throw away rows with NAs in them.
# Keep in mind that we are not throwing away subjects (cases) who happen to have a missing value, 
# just those time points that have missing response

# let's bring in drug
fit.04 <- lme(bdi ~ drug + timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.04)
anova(fit.02,fit.04)

# How about length ?
fit.05 <- lme(bdi ~ length + timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.05)
anova(fit.02,fit.05)

# How about treatment ?
fit.06 <- lme(bdi ~ treatment + timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.06)
anova(fit.02,fit.06)

# How about baseline ?
fit.07 <- lme(bdi ~ pre.bdi + timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.07)
anova(fit.02,fit.07)

# Is there an interaction between baseline and temporal pattern ?
fit.08 <- lme(bdi ~ pre.bdi * timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.08)
anova(fit.07,fit.08)

# What if we had included all covariates from the beginning ?
fit.09 <- gls(bdi ~ pre.bdi + drug + length + treatment + timepoint, data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.09)
fit.10 <- lme(bdi ~ pre.bdi + drug + length + treatment + timepoint, random = (~ 1 | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.10)
anova(fit.09,fit.10)
fit.11 <- lme(bdi ~ pre.bdi + drug + length + treatment + timepoint, random = (~ timepoint | indiv), data=BtheB.tidy, method="ML", na.action=na.omit)
summary(fit.11)
anova(fit.10,fit.11)

# let's look at summary of fit.10 one more time
summary(fit.10)
# While baseline and time both significantly affect bdi, there is no evidence for a treatment effect  

########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Longitudinal Data Analysis

library(lattice)
library(nlme)
library(splines)

########################################################
# Bone data set
########################################################

# Relative spinal bone mineral density (spnbmd) measurements on 261 North American adolescents.
# Each value is the difference in spnbmd between two consecutive visits, divided by the average.
# The age is the average of the age over the two visits.

# load the "bone" data set: "bone.RData"
load("bone.RData")

# examine the data 
str(bone)

# convert idnum to factor
bone$idnum <- factor(bone$idnum)
str(bone)


# ------------------------------------------
# exploratory analysis

# raw data
xyplot(spnbmd ~ age | idnum, data=bone, subset=(idnum %in% sample(bone$idnum, size=20, replace=F)), 
       as.table=F, xlab = "age", ylab = "spnbmd")

# parametric/linear fit
xyplot(spnbmd ~ age | idnum, data=bone, subset=(idnum %in% sample(bone$idnum, size=50, replace=F)),
       prepanel = function(x,y) prepanel.lmline(x,y),
       xlab = "age", ylab = "spnbmd",
       panel = function(x,y) {
         panel.xyplot(x,y)
         panel.lmline(x,y) }
)

# compare the trends for male and females (ignoring the repeated measures)
plot( bone$age, bone$spnbmd, xlab="age", ylab="spnbmd")
bone.spline.male <- with(subset(bone,gender=="male"), smooth.spline(age, spnbmd))
bone.spline.female <- with(subset(bone, gender=="female"), smooth.spline(age, spnbmd))
lines(bone.spline.male, col="blue", lwd=3)
lines(bone.spline.female, col="red2", lwd=3)
legend(20,0.20, legend=c("male", "Female"), col=c("blue", "red2"), lwd=2)

# using cubic splines
0.5*(bone.spline.female$df + bone.spline.male$df) # 6.6
plot(spnbmd~age,data=bone)
fit.00 <- lm(spnbmd ~ gender * bs(age, df=6), data=bone) # df=6 corresponds to 3 internal knots
predict.male <- predict(fit.00, newdata=data.frame(age=seq(10,25,by=.5), gender="male"))
predict.female <- predict(fit.00, newdata=data.frame(age=seq(10,25,by=.5), gender="female"))
lines(seq(10,25,by=.5), predict.male, col="blue", lwd=3)
lines(seq(10,25,by=.5), predict.female, col="red", lwd=3)


# ------------------------------------------
# model fitting/selection

fit.01 <- gls(spnbmd ~ gender * bs(age, df=6), data=bone, method="ML")
summary(fit.01)

fit.02 <- lme(spnbmd ~ gender * bs(age, df=6), random=(~1 | idnum), data=bone, method="ML")
summary(fit.02)

anova(fit.01,fit.02)

# testing if males and females show different trends
fit.02 <- lme(spnbmd ~ gender * bs(age, df=6), random=(~1 | idnum), data=bone, method="ML")
fit.03 <- lme(spnbmd ~ gender + bs(age, df=6), random=(~1 | idnum), data=bone, method="ML")
anova(fit.02,fit.03)

# Therefore we conclude that change in relative spinal bone mineral density versus age is 
# different between males and females.

########################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; 2023
# Mixed models

# The purpose of this TP is to carry out some mixed model analyses using R. 
# As usual, make sure that you read the help for any new functions that you use.

library(DAAG)
library(lattice) 
library(lme4)
library(WWGbook)

########################################################
# Rat Brain Data
########################################################

# The rat brain data is called rat.brain in the WWGbook package. 
# The aim of the experiment was to examine nucleotide activation 
# (guanine nucleotide bonding) in six different brain nuclei (i.e., brain regions) 
# among five adult male rats. 
# A data frame with 30 observations on the following 4 variables.
# animal: Unique identifier for each rat
# treatment: Level of drug treatment (1 = Basal, 2 = Carbachol)
# region: Brain nucleus (1 = BST, 2 = LS, 3 = VDB)
# activate: Nucleotide activation (the dependent variable)


# ------------------------------------------
# 1. Start to explore the data:

attach(rat.brain)
str(rat.brain)
summary(rat.brain)

# In order to use treatment and region correctly in the model, they will each need 
# to be coded as a factor (what type of variables are they now ?):

region.f <- region
region.f[region == 1] <- 1
region.f[region == 2] <- 2
region.f[region == 3] <- 0
region.f <- factor(region.f)
levels(region.f) <- c("VDB", "BST", "LS")

treat <- factor(treatment)
levels(treat) <- c("Basal","Carbachol")

rat.brain <- data.frame(rat.brain, region.f, treat)

str(rat.brain)
summary(rat.brain)

# First try to get some idea what the data look like through graphical exploration.
# Here are a few different representations of the data.
# Try them all out - which do you think is most revealing ?

dotplot(reorder(animal, activate) ~ activate, rat.brain, 
        groups = region.f, ylab = "Animal", xlab = "Activate", pch=19, 
        type = c("p", "a"), auto.key=list(columns=3, lines=TRUE)) 

# Here we have plotted results for each rat (ordered by increasing mean(activate), 
# but this includes both treatment measurements for each rat.
# Let's look at each rat/treatment combination separately:

rat.brain$rt <- with(rat.brain, treat:factor(animal))

dotplot(reorder(rt, activate) ~ activate, rat.brain, groups = region.f, 
        ylab = "Animal", xlab = "Activate", pch=19, 
        type = c("p", "a"), auto.key=list(columns=3, lines=TRUE))

# Each rat separately:
xyplot(activate ~ treat | animal, rat.brain, aspect = "xy", layout = c(5,1), 
       groups=region.f, pch=19, type=c("p", "l", "g"), 
       index.cond = function(x,y) coef(lm(y~x))[1], xlab = "Treatment", 
       ylab="Activate", auto.key=list(space="top",lines=TRUE,columns=3))

# Separated by treatment group:
xyplot(activate ~ region.f|treat, rat.brain, groups = animal, pch=19, 
       ylim=c(0,800), xlab="Region", ylab="Activate", 
       type = c("p","a"), auto.key = list(space="top"))

# Does the treatment appear to have an effect ? Why do you say that ? 
# Does the effect (if any) appear to be the same in each region ?
# Do there appear to be rat-specific effects ? 


# ------------------------------------------
# 2. We will start off fitting a model including all fixed effects (main effects and 
# interactions for the treatment and region variables - make sure to use the factor versions) 
# and a random effect for animal.
# As above, you can use extractor functions to view some of the model components.

rat.brain.lmer1 <- lmer(activate ~ region.f*treat + (1|animal), REML=TRUE, data = rat.brain)
summary(rat.brain.lmer1)

# Make sure that you know how to interpret the coefficients (the interpretation will be 
# determined by the coding). 


# ------------------------------------------
# 3. From the plot above, we saw that between-animal variation was greater for the 
# carbachol treatment than for the basal treatment. To accommodate this difference in 
# variation, we can add a random animal-specific effect of treatment to the model.
# The effect of treatment is fixed in our original model, therefore constant across 
# all animals. The additional random effect associated with treatment that we include 
# in the new model allows the implied marginal variance of observations for the carbachol 
# treatment to differ from that for the basal treatment.
# We can also think of the new model as having two random intercepts per rat, one for 
# the carbachol treatment and an additional one for the basal treatment.

rat.brain.lmer2 <- lmer(activate ~ region.f*treat + (treat |animal), REML=TRUE, data = rat.brain)
summary(rat.brain.lmer2)

# What happens to the estimated fixed effects coefficients? What about their standard errors?


# ------------------------------------------
# 4. We can compare the models using a likelihood ratio (LR) test, carried out with 
# the anova function. The anova method for mer objects carries out a ML (not REML) LR test, 
# even if the model has been fit by REML. The results are not identical for the two methods, 
# but in this case the conclusions are the same.

anova(rat.brain.lmer1, rat.brain.lmer2)


# ------------------------------------------
# 5. As above, we check some diagnostics for the final model.

# Residual plot:
fit <- fitted(rat.brain.lmer2)
res <- resid(rat.brain.lmer2)
plotres.fit <- data.frame(rat.brain, fit, res)
xyplot(res ~ fit, data=plotres.fit, groups=treat, pch=19, xlab="Predicted value",
       ylab="Residual", abline=0, auto.key=list(space="top", columns=2))

# QQ normal plot:
qqnorm(resid(rat.brain.lmer2))
qqline(resid(rat.brain.lmer2))
shapiro.test(resid(rat.brain.lmer2))
# What do you conclude ?
