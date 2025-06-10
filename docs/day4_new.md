# Generalized additive models

In this section, you will find the R code that we will use during the course. We will explain the code and output during correction of the exercises.

**After having completed this chapter you will be able to:**

- Understand Sensitivity, Specificity, ROC curve and AUC 
- Understand different regularisation method 
- Understand K-fold cross-validation 
- Understand Leave-one-out validation
- Understand how to built a signature
- Understand KNN-method
- Understand Random Forests

Probably through either the cholera dataset (from HistData) the alzheimer dataset and all through CARET package (or maybe also show how to do it in base R, which is possible) also with the ROCR package

Slides of lectures:

[Download slides](assets/pdf/GAM.pdf){: .md-button }
[Download slides afternoon](assets/pdf/Crossvalidation.pdf){: .md-button }

Exercise 1: NEW-CARET on a LM with CV

Describe the data 

First we set for this exercise the libraries that we will need.

```r
library(caret)
library(pROC)
library(ROCR)
```

Try implementing a training and test dataset. 

```r
DATA

# explore the data
str(DATA)
```

Now the goal is to build a predictive model 
We are in the testing face so we will test several ones using CV 

We use CV and RSME MAE and R2 of the different models, we will also compute other values

test the different metrics 


??? Hint
    Use anova() to perform an F-test to compare nested models.
??? done "Answer"
    ```r
    fit1 <- lm(wage ~ age, data=Wage)
    summary(fit1)
    ```

## Leave-one-out method on DATA

Here we will use the leave-one-out method should be done as an exercice as this is the same as above just changing one parameter

## Binomial data 

### Sensitivity Specificity
Overall



Do a ROC curve and calculate AUC

### Using Caret 

Using Caret package we calculate Spec, Sens in each of the folds

Understand Accuracy Kappa and the use of summaryFunction 

### Knn-method 

## Random Forest

This we will do on Categorical and also on numeric data 

## Regularisation to create a signature

Use some gene-expression data for showing how to generate a good signature. 

Start with N genes (this would be part of the DGE genes and even maybe tested on univariate model)

Use elastic-net and LASSO to find the good signature for the model
Evaluate it using CARET

Finally do some metrics




