## **Bonus code** :champagne_glass:

The following code was added thanks to questions from course participants of past sessions. They might be useful for you too.

[Download slides](assets/pdf/Regularization.pdf){: .md-button }

## Linear Regression using model selection

### Linear regression 

```r
library(MASS)

data(birthwt)
summary(birthwt)

help(birthwt)

colnames(birthwt)
colnames(birthwt) <- c("birthwt.below.2500", "mother.age","mother.weight", "race",
                       "smoking.status", "nb.previous.prem.labor",  "hypertension", 
                       "uterine.irrit","nb.physician.visits", "birthwt.grams")

str(birthwt)
summary(birthwt)
birthwt$race <- as.factor(birthwt$race)
str(birthwt)
summary(birthwt)
```



### Model selection

```r
library(leaps) 

best_subset <- regsubsets(birthwt.grams ~ . - birthwt.below.2500, data = birthwt, nvmax = 8)
results <- summary(best_subset)

# Adjusted R-squared
plot(results$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")
# Residual sum of squares for each model
plot(results$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
# R-squared
plot(results$rsq, xlab = "Number of Variables", ylab = "R-squared", type = "l")

which.max(results$adjr2)
```


### Model selection using the validation set approach

```r
set.seed(1)
train <- sample(c(TRUE, FALSE), size = nrow(birthwt), rep = TRUE)
test <- (!train)

best_subset_train <- regsubsets(birthwt.grams ~ . - birthwt.below.2500, data = birthwt[train ,], nvmax = 8)
test_mat <- model.matrix(birthwt.grams ~ . - birthwt.below.2500, data = birthwt[test,])

val_errors  <- rep(NA , 8)
for(i in 1:8){
  coefi = coef(best_subset_train, id = i)
  pred = test_mat[,names(coefi)]%*%coefi
  val_errors[i] = mean((birthwt$birthwt.grams[test] - pred)^2)
}
which.min(val_errors)
```