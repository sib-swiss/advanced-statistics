#######################################################
########################################################
########################################################

# Advanced Statistics: Statistical Modeling; Aug 2023
# GLM



########################################################
# Warm up: babies
########################################################

# Load and explore the dataset babies.
load("babies.RData")
attach(babies)

# The data records the birth weight of 1174 babies along with information on the mother and the pregnancy.


# ------------------------------------------
# Perform a graphical exploration of the data

summary(babies)
pairs(babies)


# ------------------------------------------
# Which factor can explain prematurity?
# Can we use a linear model? If so, try to make predictions.

model1 <- lm(as.numeric(as.numeric(levels(prem))[prem]) ~ bwt)
summary(model1)

plot(bwt, as.numeric(as.numeric(levels(prem))[prem]), ylab="premature", xlab="birth weight", main="Babies", 
     xlim=c(0,200), ylim=c(-0.2,1.2))
abline(model1, col="red", lwd=2)

new_bwt = 170
predict.model1 <- predict.lm(model1, newdata=data.frame(bwt=new_bwt))
points(new_bwt, predict.model1, col="blue")


# ------------------------------------------
# Fit a logistic regression to find parameters explaining the probability of prematurity ?
# What is the effect of birth weight on the probability of prematurity ?
# What about parity ?

model2 <- glm(prem ~ bwt, family=binomial)
summary(model2)

model3 <- glm(prem ~ bwt*parity, family=binomial)
summary(model3)

model4 <- glm(prem ~ bwt*smoke+parity, family=binomial)
summary(model4)


# ------------------------------------------
# Check the deviance residuals using the residualPlot function in the car library

library(car)
residualPlot(model2, type = "deviance") ## likelihood ratio test for comparing logistic regression models, where we compare our current model to a saturated model.
residualPlot(model2, type = "response") ## raw residuals yi-predict = response
residualPlot(model2, type = "pearson") ## rescaled version of the raw residuals


# ------------------------------------------
# Construct the quantile residuals using the qresiduals function in the statmod library
# Analyze the deviance
# Look for potential influencial and outlying observations

library(statmod)
model2.residuals <- qresiduals(model2)
qqnorm(model2.residuals)
qqline(model2.residuals, col="red")

model.null <- glm(prem ~ 1, family = binomial)
summary(model.null)
anova(model.null, model2, test = "Chisq")

influencePlot(model2)
acf(model2.residuals)



########################################################
# Challenge: baby food
########################################################

library(faraway)
data(babyfood)
attach(babyfood)

# ------------------------------------------
# 1. Explore the data

summary(babyfood)
boxplot(disease ~ food, babyfood)
boxplot(disease ~ sex, babyfood)
boxplot((disease/(disease + nondisease)) ~ food, babyfood)
boxplot((disease/(disease + nondisease)) ~ sex, babyfood)


# ------------------------------------------
# 2. Fit a logistic regression to explain the probability of disease by sex and food.

mdl <- glm(cbind(disease, nondisease) ~ sex + food, family = binomial, babyfood)
summary(mdl)



########################################################
# Warm up: gala
########################################################

# Explore the dataset gala in library faraway. 
# Remove the variable "endemics" which we will not use here.

library(faraway) 
data(gala)
gala <- gala[,-2]

summary(gala)


# ------------------------------------------
# Study the relationship between the number of plant species and several geographical variables of interest.

pairs(gala)


# ------------------------------------------
# Fit a poisson model to the galapagos data.
# Which variables are significant ? 
# Check the deviance of the model

poisson.glm <- glm(Species ~ ., data=gala, family=poisson)
summary(poisson.glm)
residualPlots(poisson.glm)



########################################################
# Exercise 1: Michelin food
########################################################

# Here, we will use a data set containing food ratings for a number of restaurants, 
# as well as information about whether or not they are included in the Michelin guide. 
# The data set (called MichelinFood.txt) can be downloaded from the webpage of the book 
# "A Modern Approach to Regression with R" by Simon J Sheather.
# Download the file and import it into R.

michelin <- read.delim("MichelinFood.txt", header=TRUE, sep="\t", as.is=TRUE)
michelin

# The Food column represents the ranking of the food. 
# The columns InMichelin and NotInMichelin contain the number of restaurants with 
# the given food ranking that are/are not listed in the Michelin guide, respectively. 
# The mi column contains the total number of examined restaurants with the given food ranking. 
# Finally, the proportion column contains the fraction of examined restaurants with 
# the given food ranking that were included in the Michelin guide.


# ------------------------------------------
# 1. Start by graphically exploring the data

plot(michelin$Food, michelin$proportion)


# ------------------------------------------
# 2. Fit a GLM using a binomial model for the response, using the food ranking as the predictor.

glm.mich <- glm(cbind(InMichelin, NotInMichelin) ~ Food, family = binomial(logit),
                data = michelin)


# ------------------------------------------
# 3. Predict the probabilities for a number of potential food rankings xnew, and plot a smooth function

xnew <- data.frame(Food = seq(from = 14, to = 30, length.out = 50))
pred.prop <- predict(glm.mich, newdata = xnew, type = "response")
plot(michelin$Food, michelin$proportion)
lines(xnew$Food, pred.prop, col = "blue", lwd = 2)


# ------------------------------------------
# 4. Check the model by looking at the residual deviance, other residuals and especially the quantile residuals

require(car)
residualPlot(glm.mich, type = "deviance")
residualPlot(glm.mich, type = "response")
residualPlot(glm.mich, type = "pearson")
library(statmod)
qres <- qresiduals(glm.mich)
qqnorm(qres)
qqline(qres)
acf(qres)


# ------------------------------------------
# 5. Check the model for potential influencial observations.

influencePlot(glm.mich)



########################################################
# Exercise 2: moth death
########################################################

# The next data set comes from Venables and Ripley (page 190) and encodes the number of moths 
# that died after exposure to different doses of trans-cypermethrin. 20 male and 20 female moths 
# were exposed to each dose, and the number of deaths in each group was recorded.
# We generate a data frame containing the observed values. Since the doses are powers of two, 
# we use log2(dose) as the predictor rather than the actual dose.

moth <- data.frame(sex = rep(c("male", "female"), each = 6),
                   dose = log2(rep(c(1, 2, 4, 8, 16, 32), 2)),
                   numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16))
moth$numalive <- 20 - moth$numdead


# ------------------------------------------
# 1. Fit a GLM using the sex and dose as predictors. 
# Do we need to include an interaction term in the model ?

glm.moth.1 <- glm(cbind(numalive, numdead) ~ sex + dose, data = moth, family = binomial)
summary(glm.moth.1)
glm.moth.2 <- glm(cbind(numalive, numdead) ~ sex * dose, data = moth, family = binomial)
summary(glm.moth.2)


# ------------------------------------------
# 2. Does the model fit well ? Perform an analysis of deviance

glm.null <- glm(cbind(numalive, numdead) ~ 1, data = moth, family = binomial)
summary(glm.null)

# If a pair of models is nested (i.e. the smaller model is a special case of the larger one) then we can test
# H0 : smaller model is true versus H1 : larger model is true
# by doing likelihood ratio testing, and comparing the difference in deviance
# to a chi2 distribution with degrees of freedom = df for smaller model - df for larger model
anova(glm.null, glm.moth.1, test = "Chisq")


# ------------------------------------------
# 3. Predict the probabilities for different doses and include a smooth line in the plots

xnew <- data.frame(sex = rep(c("male", "female"), each = 30), 
                   dose = rep(seq(from = 0, to = 5, length.out = 30), 2))
pred.prop <- predict(glm.moth.1, newdata = xnew, type = "response")
moth$proportion <- moth$numalive/(moth$numdead + moth$numalive)
color <- moth$sex
color[color == "male"] <- "blue"
color[color == "female"] <- "red"
plot(moth$proportion ~ moth$dose, col = as.character(color))
lines(xnew$dose[which(xnew$sex == "male")], pred.prop[which(xnew$sex == "male")],
      col = "blue", lwd = 2)
lines(xnew$dose[which(xnew$sex == "female")], pred.prop[which(xnew$sex == "female")],
      col = "red", lwd = 2)



########################################################
# Exercise 3: beetle
########################################################

# The third example of a binomial response considers an experiment where beetles were exposed 
# to carbon disulphide at various concentrations, and the number of beetles who died within five 
# hours were recorded. The data set is studied in the book by Dobson (2002), and comes originally 
# from Bliss (1935).

beetles <- data.frame(dose = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.861, 1.8839), 
                      dead = c(6, 13, 18, 28, 52, 53, 61, 60), 
                      alive = c(51, 47, 44, 28, 11, 6, 1, 0))


# ------------------------------------------
# 1. Fit a logistic regression to the data using dose as a predictor

glm.beetles <- glm(cbind(alive, dead) ~ dose, beetles, family = "binomial")
summary(glm.beetles)


# ------------------------------------------
# 2. Fit another logistic regression using the log-log link. Compare the two fits.

glm.beetles.log <- glm(cbind(alive, dead) ~ dose, beetles, family = binomial(cloglog))
summary(glm.beetles.log)


# ------------------------------------------
# 3. Compare the predictions of each model

new_beetle <- data.frame(dose = seq(from = 1.5, to = 1.9, length.out = 100))
plot(new_beetle$dose,predict(glm.beetles, new_beetle, type = "response"), type = "l", col = "blue")
lines(new_beetle$dose,predict(glm.beetles.log, new_beetle, type = "response"), col = "red")



########################################################
# Exercise 4: Pima
########################################################

# The national institute of Diabetes and digestive and kidney diseases conducted a study 
# on 768 adult female Pima Indians living near Phoenix. The purpose of the study was to 
# investigate the factors related to diabetes.
# pregnant: number of times pregnant
# glucose: plasma glucose concentration at 2 hours in an oral glucose tolerance test
# diastolic: diastolic blood pressure (mm Hg)
# triceps: triceps skin fold thickness (mm)
# insulin: 2-Hour serum insulin (mu U/ml)
# bmi: body mass index (weight in kg/(height in metres squared))
# diabetes: diabetes pedigree function (scores likelihood of diabetes based on family history)
# age: age (years)
# test: test whether the patient shows signs of diabetes (0: negative, 1: positive)

library(faraway)
data("pima")


# ------------------------------------------
# 1. Perform a simple graphical and numerical inspection of the dataset. 
# Can you find any obvious irregularities in the data ? 
# If you do, use the appropriate strategy to correct the problems

str(pima)
summary(pima)

pima$glucose[which(pima$glucose == 0)] <- NA
pima$diastolic[which(pima$diastolic == 0)] <- NA
pima$triceps[which(pima$triceps == 0)] <- NA
pima$insulin[which(pima$insulin == 0)] <- NA
pima$bmi[pima$bmi == 0] <- NA

pima$test <- factor(pima$test)
levels(pima$test) <- c("negative", "positive")
table(pima$test)

boxplot(pima$diastolic ~ pima$test)
boxplot(pima$pregnant ~ pima$test)
boxplot(pima$glucose ~ pima$test)
boxplot(pima$triceps ~ pima$test)
boxplot(pima$insulin ~ pima$test)
boxplot(pima$bmi ~ pima$test)
boxplot(pima$diabetes ~ pima$test)
boxplot(pima$age ~ pima$test)


# ------------------------------------------
# 2. Fit a model with the result of the diabetes test as the response and all the 
# other variables as predictors. Can you tell whether this model fits the data ?

glm.pima.full <- glm(test ~ ., pima, family = binomial)
summary(glm.pima.full)

library(car)
residualPlots(glm.pima.full)

library(statmod)
qqnorm(qresiduals(glm.pima.full))
qqline(qresiduals(glm.pima.full))
qres <- qresiduals(glm.pima.full)
plot(qres ~ predict(glm.pima.full, type = "link"))
acf(qres)

# The fit is quite good. The residuals are good and the uniform residuals pass all the checks.
# There are many non-significant variables so we can remove them to have a better fit.


# ------------------------------------------
# 3. On the basis of the previous result, try to fit another model

glm.pima <- glm(test ~ glucose + bmi + diabetes, pima, family = binomial)
summary(glm.pima)

residualPlots(glm.pima)
qqnorm(qresiduals(glm.pima))
qqline(qresiduals(glm.pima))
qres <- qresiduals(glm.pima)
plot(qres ~ predict(glm.pima, type = "link"))
acf(qres)

influenceIndexPlot(glm.pima, vars = c("Cook", "Studentized", "hat"))


# ------------------------------------------
# 4. Using the previous model, predict the outcome for a woman with the following predictor values:

new_pima <- data.frame(pregnant = 1, glucose = 99, diastolic = 64, triceps = 22,
                       insulin = 76, bmi = 27, diabetes = 0.25, age = 25)
pred_prob <- predict(glm.pima, newdata = new_pima, type = "response", se = TRUE)

pred_prob$fit 

pred_logit <- predict(glm.pima, newdata = new_pima, se = TRUE)

ilogit(pred_logit$fit) 
ilogit(pred_logit$fit - 1.96 * pred_logit$se.fit)
ilogit(pred_logit$fit + 1.96 * pred_logit$se.fit)



########################################################
# Exercise 5: lung cancer
########################################################

# Here we will consider a data set from the ISwR package, which contains the number of 
# lung cancer cases recorded in different age categories in four different Danish cities. 
# The cases column contains the number of lung cancer cases for each city and age category.

library(ISwR)
data(eba1977)
eba1977


# ------------------------------------------
# 1. Model this count using the city and age category as predictors.
# Fit a Poisson GLM to the data. Is the fit appropriate ?

glm.cancer <- glm(cases ~ city + age, data = eba1977, family = poisson)
summary(glm.cancer)


# ------------------------------------------
# 2. In the previous model, we are not considering the number of potential cases in each group 
# (i.e. the population size). Modify the model by using an offset which takes the population size 
# into account.

glm.cancer.off <- glm(cases ~ offset(log(pop)) + city + age, data = eba1977,
                      family = poisson)
summary(glm.cancer.off)


# ------------------------------------------
# 3. Fit a binomial model to the data by considering success as being lung cancer cases and failures 
# as being: populationsize - number of cases.

success <- eba1977$cases
failures <- eba1977$pop - eba1977$cases
glm.cancer.bin <- glm(cbind(success, failures) ~ city + age, family = "binomial",
                      data = eba1977)
summary(glm.cancer.bin)


# ------------------------------------------
# 4. Compare with the Poisson model.

# We see that the results are very close to those obtained with the Poisson model with offset.
# This is because the number of cases is generally very low compared to the population size,
# in other words, the population size is almost infinite compared to the number of cases. In
# this situation, the Poisson distribution is closely related to the binomial distribution.