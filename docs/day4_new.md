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

Exercise 1: Alzheimer Data

Washington University conducted a clinical study to determine if biological measurements made from cerebrospinal fluid (CSF) can be used to diagnose or predict Alzheimer's disease (Craig-Schapiro et al. 2011). These data are a modified version of the values used for the publication.

The R factor vector diagnosis contains the outcome data for 333 of the subjects. The demographic and laboratory results are collected in the data frame predictors.

One important indicator of Alzheimer's disease is the genetic background of a subject. In particular, what versions of the Apolipoprotein E gene inherited from one's parents has an association with the disease. There are three variants of the gene: E2, E3 and E4. Since a child inherits a version of the gene from each parent, there are six possible combinations (e.g. E2/E2, E2/E3, and so on). This data is contained in the predictor column named Genotype.


First we set for this exercise the libraries that we will need and load the data which is part of the AppliedPredictiveModeling package and is called AlzheimerDisease. It is data that has been published in this article "Craig-Schapiro, R., et al. (2011). Multiplexed Immunoassay Panel Identifies Novel CSF Biomarkers for Alzheimer's Disease Diagnosis and Prognosis. PLoS ONE, 6(4), e18850."

```r
library(caret)
library(pROC)
library(ROCR)
library(AppliedPredictiveModeling)
data(AzheimerDisease)

alzheimer <- cbind(predictors,diagnosis)
```

## Data Splitting

We first start by splitting the data into a training and a test set.

The test set will be used only after having decided on the best model.

Within the training set however we will again use a process of splitting and testing using cross validation.

## Model training data set using CV 

## Performance measures

### Sensitivity Specificity
Overall

### ROC curve and AUC

### Accuracy and Kappa 



NEW-CARET on a LM with CV

Try implementing a training and test dataset. 


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

## Leave-one-out method on CHOLERA as there are few samples

Here we will use the leave-one-out method should be done as an exercice as this is the same as above just changing one parameter

## Binomial data 



### Using Caret 

Using Caret package we calculate Spec, Sens in each of the folds

Understand Accuracy Kappa and the use of summaryFunction 

### Knn-method (imputing)

## Random Forest

This we will do on Categorical and also on numeric data 

## Regularisation to create a signature

Use some gene-expression data for showing how to generate a good signature. 

Start with N genes (this would be part of the DGE genes and even maybe tested on univariate model)

Use elastic-net and LASSO to find the good signature for the model
Evaluate it using CARET

Finally do some metrics




