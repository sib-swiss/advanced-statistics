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

Washington University conducted a clinical study to determine if biological measurements made from cerebrospinal fluid (CSF) can be used to diagnose or predict Alzheimer's disease ("Craig-Schapiro, R., et al. (2011). Multiplexed Immunoassay Panel Identifies Novel CSF Biomarkers for Alzheimer's Disease Diagnosis and Prognosis. PLoS ONE, 6(4), e18850."). These data are a modified version of the values used for the publication.

The R factor vector diagnosis contains the outcome data for 333 of the subjects. The demographic and laboratory results are collected in the data frame predictors.

One important indicator of Alzheimer's disease is the genetic background of a subject. In particular, what versions of the Apolipoprotein E gene inherited from one's parents has an association with the disease. There are three variants of the gene: E2, E3 and E4. Since a child inherits a version of the gene from each parent, there are six possible combinations (e.g. E2/E2, E2/E3, and so on). This data is contained in the predictor column named Genotype.


First we set for this exercise the libraries that we will need and load the data which is part of the AppliedPredictiveModeling package and is called AlzheimerDisease.

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

It is important to set a seed in order to be reproducible.

```r
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(alzheimer$diagnosis, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dim(trainIndex)
dim(alzheimer)
alzheimerTrain <- alzheimer[ trainIndex,]
alzheimerTest  <- alzheimer[-trainIndex,]
```

Try to understand how the test and training set is distributed in terms of control and impaired patients.

```r
table(alzheimerTrain$diagnosis)

table(alzheimerTest$diagnosis)

table(alzheimer$diagnosis)
```


CHANGE THIS HERE AS IT IS COPY PASTE
In some cases there is an important qualitative factor in the data that should be considered during (re)sampling. For example:

in clinical trials, there may be hospital-to-hospital differences
with longitudinal or repeated measures data, subjects (or general independent experimental unit) may have multiple rows in the data set, etc.
There may be an interest in making sure that these groups are not contained in the training and testing set since this may bias the test set performance to be more optimistic. Also, when one or more specific groups are held out, the resampling might capture the “ruggedness” of the model. In the example where clinical data is recorded over multiple sites, the resampling performance estimates partly measure how extensible the model is across sites.



## Model training data set using Cross-Validation

We start by choosing the parameters of the trainControl function in order to do cross-Validation. For that we need to choose the number of folds of the cross-validation and the number of repeats. 
```r
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)
```

The trainControl function is the function that can do it all, from bayesian Models to GAMs, but also regularizations amongst others. Have a look at the help in the caret tutorial book! Regularization will be explained in in a later exercise. 


Now to find the best model according to the chosen method with the trainControl, we use the "train" function from the caret package.
We need to provide the predictor and outcome data objects, as well as the method, specifies the type of model. As we want to fit the binomial data we need to use the option 

```r
gbmFit1 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
```

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




