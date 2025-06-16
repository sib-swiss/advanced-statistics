# CARET-Day

In this section, you will find the R code that we will use during the course. We will explain the code and output during correction of the exercises.

**After having completed this chapter you will be able to:**

- Understand Sensitivity, Specificity, ROC curve and AUC 
- Understand different regularisation method 
- Understand K-fold cross-validation 
- Understand Leave-one-out validation
- Understand how to built a signature
- Understand KNN-method
- Understand Random Forests

Slides of lectures:

[Download slides](assets/pdf/GAM.pdf){: .md-button }
[Download slides afternoon](assets/pdf/Crossvalidation.pdf){: .md-button }

## Alzheimer Data

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
summary(alzheimer) ## realize male is not a factor
alzheimer$male <- as.factor(alzheimer$male)
```

## Data Splitting

We first start by splitting the data into a training and a test set.

The test set will be used only after having decided on the best model.

Within the training set however we will again use a process of splitting and testing using cross validation.

It is important to set a seed in order to be reproducible.

```r
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

In some cases there is an important qualitative factor in the data that should be considered during (re)sampling. For example:

in clinical trials, there may be hospital-to-hospital differences
with longitudinal or repeated measures data, subjects (or general independent experimental unit) may have multiple rows in the data set, etc.
There may be an interest in making sure that these groups are not contained in the training and testing set since this may bias the test set performance to be more optimistic. Also, when one or more specific groups are held out, the resampling might capture the “ruggedness” of the model. In the example where clinical data is recorded over multiple sites, the resampling performance estimates partly measure how extensible the model is across sites.



## Performance measures using Cross-Validation

We start by choosing the parameters of the trainControl function in order to do cross-Validation (CV). For that we need to choose the number of folds of the cross-validation.



```r
fitControl_CV_accuracy <- trainControl(## 5-fold CV
                           method = "cv",
                           number = 5
                          )
```

We can also do repeated CV (RCV) which repeats the process of folds, n number of times.

```r
fitControl_RCV_accuracy <- trainControl(
                           method = "repeatedcv",
                           number = 5,
                           repeats= 10
                          )
```
We could also choose ourselves the folds using the function 
It is important to use the set the classProbs = TRUE in order to have in each folds controls and impared samples.
```r
folds <- createFolds(y = alzheimerTrain$diagnosis, k = 5, returnTrain = TRUE)
fitControl_CV_folds <- trainControl(## 5-fold CV
                           method = "cv",
                           number = 5,
                           index= folds)
```

We can also estimate ROC curves, as well as sensitivity and specificity on our training set.

```r
fitControl_CV_ROC <- trainControl(## 5-fold CV
                           method = "cv",
                           number = 5,
                      classProbs = TRUE,    # Needed for ROC
  summaryFunction = twoClassSummary )       # Needed for ROC
```

... or with repeated CV ...

```r
fitControl_RCV_ROC <- trainControl(## 5-fold CV
                           method = "repeatedcv",
                           number = 5,
                           repeats= 10,
                      classProbs = TRUE,    # Needed for ROC
  summaryFunction = twoClassSummary )       # Needed for ROC
```

Optionally one can also save the predictions in each repeat using the "savePrediction" option. 


```r
fitControl_RCV_ROC_PRED <- trainControl(## 5-fold CV
                           method = "repeatedcv",
                           number = 5,
                           repeats= 10,
                          savePredictions = "final",
                      classProbs = TRUE,   # Needed for ROC
  summaryFunction = twoClassSummary )   # Needed for ROC
```



The trainControl function is the function that can do it all, from bayesian Models to GAMs, but also regularizations amongst others. Have a look at the help in the caret tutorial book! We will see some other trainControl functions and methods later, such as regularization for example, which will be explained in a later exercise. 


Now, to find the best model according to the chosen method with the trainControl, we use the "train" function from the caret package.
We need to provide the predictor and outcome data objects, as well as the method used. As we want to fit the binomial data and look at the performance, we need to use the option family=binomial(link="logit") and the method glm. We can use the first column as a predictor for this exercise. 

By default for classification data it will choose the Accuracy as the method for performance measure. 

We start by using the default.

```r
glmFit1 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_CV_accuracy
                 )
```

To look at the output we can first look at the summary of the glm

```r
summary(glmFit1)
```

We realise that this is the same as performing our standard glm

```r
summary(glm(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain, 
  family=binomial(link="logit")))
```

Now let us check how the accuracy was calculated and how we can now be confident about the estimation of the performance in this case

```r
glmFit1$results
```

We can also look at individual Accuracy measurements in each of the folds

```r
glmFit1$resample
```

And then we can see that the Accuracy in the results section is just the average of the Accuracy in each of the folds.

```r
mean(glmFit1$resample$Accuracy)
```

Let us do the fit now with the repeated CV

```r
glmFit2 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_RCV_accuracy
                 )
```

Check what it changed in the section "resample"!


Now we can change the default measure if instead of the accuracy we would like to know more about the variability of the sensitivity and specificity. For that we only need to change the trControl with the appropriate control function and we will know.

```r
glmFit3 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_RCV_ROC
                 )
```

You must have gotten a Warning message, why ? 

??? done "Answer"
    The default metric is Accuracy and not ROC, this is therefore just a warning saying you forgot to change the metric parameter. Here is how it would be correct
    ```r
    glmFit3 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_RCV_ROC,
                 metric="ROC"
                 )
    ```

Now check the result section 
```r
glmFit3$results
```

We can now read clearly the ROC (or average ROC on our folds), as well as Sensitivity and Specificity. What can you conclude ?

And what changes if you use the "fitControl_RCV_ROC_PRED" ?

??? done "Answer"
    ```r
    glmFit4 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_RCV_ROC_PRED,
                 metric="ROC"
                 )
                 
    glmFit4$pred             
    
    ```
    This is so useful as you can assess how many times each sample is classified wrongly or correctly and in which fold, so dependant on which samples. This helps in spotting which samples would benefit from another model and also helps creating new hypotheses. 

Bonus let us make a loop of all possible predictors in uni-variate analysis and 
check which one has the best p-value and good ROC.

??? done "Answer"
    ```r
    col_num <- dim(alzheimer)[2]-1 # we do not use the last column for prediction
    pval <- rep(0,col_num) 
    rocval <- rep(0,col_num)
    alzheimerTrain_temp <- alzheimerTrain ## we create a temp copy of the alzheimer training data
    for(i in 1:col_num){
    colnames(alzheimerTrain_temp)[i] <- "temp" ## create the column name that is temp 
    
    glmFit1 <- train(diagnosis ~ temp , data = alzheimerTrain_temp, 
                 method = "glm", 
                 family="binomial",
                 trControl = fitControl_RCV_ROC,
                 metric="ROC"
                 )
                 
       colnames(alzheimerTrain_temp)[i] <- colnames(alzheimerTrain)[i]  #restore correct name         
     pval[i] <- summary(glmFit1)$coefficients[,"Pr(>|z|)"][2] ## store pvalue from GLM  
     rocval[i] <- glmFit1$results$ROC  ## store ROC result
    }
    ## careful we need to adjust the pvalues for multiple testing
    
    pval_adj <- p.adjust(pval)
    length(pval_adj[pval_adj<0.05])
    
    data_results <- data.frame(Name= colnames(alzheimerTrain)[-ncol(alzheimerTrain)], pval_adj = pval_adj, ROC= rocval)

    data_results[data_results$pval_adj<0.05,]
    
    names_candidates <- data_results[data_results$pval_adj<0.05,"Name"]
    
    ```

We now have some candidates for multivariate analysis that look interesting, tau p_tau as well as Ab_42 where already known by the literature, which is a good indicator that the code works nicely!


## Knn-method (imputing)

Let us say we have some missing data in our dataset (we do not here so we will add some).
```
alzheimerTrain_NA <- alzheimerTrain
random_rows <- sample(1:nrow(alzheimerTrain), size = 30, replace = FALSE)
random_cols <- sample(1:(ncol(alzheimerTrain)-1), size = 30, replace = FALSE)
for(i in 1:30){
alzheimerTrain_NA[random_rows[i],random_cols[i]] <- NA
}
alzheimerTrain_NA[15,1]<-NA
summary(alzheimerTrain_NA)
```
By default glm in base R will give you an Error message if you have NAs
```r
glmFit4 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain_NA, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_RCV_ROC_PRED,
                 metric="ROC"
                 )

```

So we need to do some imputation (or tell glm what to do with the NAs, via na.action = na.omit)
```r
glmFit4 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = alzheimerTrain_NA, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_RCV_ROC_PRED,
                 metric="ROC",
                 na.action = na.omit
                 )
                 
summary(glmFit4)                 

```

What changed in the summary function ?

Now this can be a problem if you have many parameters included and many NAs in different spots. Another way is imputation. With the train function this can be done easily. However knnImpute only works for numeric data. We can use it here by only selecting the numeric subset of the data. knnImpute is a function that can also be used outside of the train function, before running train. 

We use for that the function preProcess and the method knnImpute. We use the function predict to impute the values. And then combine with the output. 
```r
pre_proc <- preProcess(alzheimerTrain_NA[,setdiff(colnames(alzheimerTrain_NA),c("male","Genotype","diagnosis"))], method = c("center", "scale", "knnImpute"))

predictors_imputed <- predict(pre_proc, newdata = alzheimerTrain_NA[,setdiff(colnames(alzheimerTrain_NA),c("male","Genotype","diagnosis"))])

# Combine with untouched outcome
data_imputed <- cbind(predictors_imputed, diagnosis = alzheimerTrain_NA$diagnosis)


glmFit4 <- train(diagnosis ~ ACE_CD143_Angiotensin_Converti, data = data_imputed, 
                 method = "glm", 
                 family=binomial(link="logit"),
                 trControl = fitControl_CV_ROC,
                 metric="ROC"
                # ,
                 # preProcess =  c("center", "scale", "knnImpute") if only numeric data this works best
                 )
                 
summary(glmFit4)                 

```

## Lasso-Regularisation

There are three types of regularisations that we will try. We will start with the lasso method.

It is always recommanded not to start with too many features in order for the method to perform better. 

Therefore we will try it but only on the candidate features we have chosen in the previous step. In gene expression data, you could start with doing a DGE between the two groups to find a candidate list of genes to select for creating a model. It is okay to do so here as we are only working on our training data. 

We start by using the glmnet package and the glmnet function and find the best model

```r
library(glmnet)
lasso_model <- glmnet(y= alzheimerTrain$diagnosis, x=alzheimerTrain[,names_candidates] , family = "binomial", alpha = 1)  # alpha = 1 is LASSO

# Plot coefficient paths
plot(lasso_model, xvar = "lambda", label = TRUE)
```
Then we can use cross validation to choose the best lambda

```r
# Cross-validated LASSO
y_numeric <- ifelse(alzheimerTrain$diagnosis == "Impaired", 1, 0)
cv_lasso <- cv.glmnet(y=y_numeric , x=as.matrix(alzheimerTrain[,names_candidates]), family = "binomial", alpha = 1, nfolds = 10)

# Plot cross-validation curve
plot(cv_lasso)

# Best lambda
best_lambda <- cv_lasso$lambda.min
```

You can now observe how the coefficients are estimated from the CV lasso method


```r
coefs <- coef(cv_lasso, s = "lambda.min")
coefs
```

We then select the variables that are not 0 and create our final model. We can do this with standard glm or again with performance validation.

```r
selected_vars <- rownames(coefs)[which(coefs != 0)][-1]
alzheimer_selected <- as.matrix(alzheimerTrain[, selected_vars])
glm_fit <- glm(alzheimerTrain$diagnosis ~ alzheimer_selected, family = "binomial")
```

And then we have a look at the results

```r
summary(glm_fit)
```

We see that the coefficients for the selected columns are not alll significant. Why ?

We will now use the caret package to have results for performance as well as model optimization. 

```r

lasso_caret <- train(diagnosis ~ Alpha_1_Antitrypsin + B_Lymphocyte_Chemoattractant_BL + FAS + Fibrinogen + GRO_alpha + Gamma_Interferon_induced_Monokin + IL_7 + MIF + MMP10 + MMP7 + NT_proBNP + PAI_1 + Pancreatic_polypeptide + TRAIL_R3 + tau + p_tau + Ab_42, data = alzheimerTrain, 
                 method = "glmnet", 
                  family="binomial",
                  trControl = fitControl_CV_ROC,
                  metric="ROC",
                  tuneGrid = expand.grid(
    alpha = 1,             # LASSO (alpha = 1)
    lambda = 10^seq(-4, 1, length = 50)  # search over lambda
  )
 )  
```

We can see that based on ROC not the same lambda is selected. 

```r
lasso_caret$bestTune$lambda
best_lambda
```

```r
lasso_model_caret_final <- glmnet(y= alzheimerTrain$diagnosis, x=alzheimerTrain[,names_candidates] , family = "binomial", alpha = 1,lambda=lasso_caret$bestTune$lambda)

coefs <- coef(lasso_model_caret_final)
selected_vars_caret <- rownames(coefs)[which(coefs != 0)][-1]
unique(selected_vars_caret == selected_vars)
```
Therefore all the same variables have been selected.

## Ridge regression
Do the same now with Ridge regression.

??? done "Answer"
    We start by using the glmnet package and the glmnet function and find the best model
    
    ```r
    ridge_model <- glmnet(y= alzheimerTrain$diagnosis, x=alzheimerTrain[,names_candidates] , family = "binomial", alpha = 0)  # alpha = 0 is Ridge
    
    # Plot coefficient paths
    plot(ridge_model, xvar = "lambda", label = TRUE)
    ```
    Then we can use cross validation to choose the best lambda
    
    ```r
    # Cross-validated Ridge
    y_numeric <- ifelse(alzheimerTrain$diagnosis == "Impaired", 1, 0)
    cv_ridge <- cv.glmnet(y=y_numeric , x=as.matrix(alzheimerTrain[,names_candidates]), family = "binomial", alpha = 0, nfolds = 10)
    
    # Plot cross-validation curve
    plot(cv_ridge)
    
    # Best lambda
    best_lambda <- cv_ridge$lambda.min
    ```
    
    You can now observe how the coefficients are estimated from the CV lasso method
    
    
    ```r
    coefs <- coef(cv_ridge, s = "lambda.min")
    coefs
    ```
    
    We then select the variables that are not 0 and create our final model. We can do this with standard glm or again with performance validation.
    
    ```r
    selected_vars <- rownames(coefs)[which(coefs != 0)][-1]
    alzheimer_selected <- as.matrix(alzheimerTrain[, selected_vars])
    glm_fit <- glm(alzheimerTrain$diagnosis ~ alzheimer_selected, family = "binomial")
    ```
    
    And then we have a look at the results
    
    ```r
    summary(glm_fit)
    ```
    
    We see that the coefficients for the selected columns are not alll significant. Why ?
    
    We will now use the caret package to have results for performance as well as model optimization. 
    
    ```r
    
    ridge_caret <- train(diagnosis ~ Alpha_1_Antitrypsin + B_Lymphocyte_Chemoattractant_BL + FAS + Fibrinogen + GRO_alpha + Gamma_Interferon_induced_Monokin + IL_7 + MIF + MMP10 + MMP7 + NT_proBNP + PAI_1 + Pancreatic_polypeptide + TRAIL_R3 + tau + p_tau + Ab_42, data = alzheimerTrain, 
    method = "glmnet", 
    family="binomial",
    trControl = fitControl_CV_ROC,
    metric="ROC",
    tuneGrid = expand.grid(
    alpha = 0,             # Ridge (alpha = 0)
    lambda = 10^seq(-4, 1, length = 50)  # search over lambda
    )
    )  
    ```
    
    We can see that based on ROC not the same lambda is selected. 
    
    ```r
    ridge_caret$bestTune$lambda
    best_lambda
    ```
    
    ```r
    ridge_model_caret_final <- glmnet(y= alzheimerTrain$diagnosis, x=alzheimerTrain[,names_candidates] , family = "binomial", alpha = 0,lambda=ridge_caret$bestTune$lambda)
    
    coefs <- coef(ridge_model_caret_final)
    selected_vars_caret <- rownames(coefs)[which(coefs != 0)][-1]
    unique(selected_vars_caret == selected_vars)
    ```
    Therefore all the same variables have been selected.
    
    
## Elastic Net    

Do the same now with Elastic net regression. You will now see a difference in the caret function.

??? done "Answer"
    We start by using the glmnet package and the glmnet function and find the best model
    
    ```r
    elastic_model <- glmnet(y= alzheimerTrain$diagnosis, x=alzheimerTrain[,names_candidates] , family = "binomial", alpha = 0.5)  # alpha between 0 and 1 is Elastic net
    
    # Plot coefficient paths
    plot(elastic_model, xvar = "lambda", label = TRUE)
    ```
    Then we can use cross validation to choose the best lambda
    
    ```r
    # Cross-validated Ridge
    y_numeric <- ifelse(alzheimerTrain$diagnosis == "Impaired", 1, 0)
    cv_elastic <- cv.glmnet(y=y_numeric , x=as.matrix(alzheimerTrain[,names_candidates]), family = "binomial", alpha = 0.5, nfolds = 10)
    
    # Plot cross-validation curve
    plot(cv_elastic)
    
    # Best lambda
    best_lambda <- cv_elastic$lambda.min
    ```
    
    You can now observe how the coefficients are estimated from the CV lasso method
    
    
    ```r
    coefs <- coef(cv_elastic, s = "lambda.min")
    coefs
    ```
    
    We then select the variables that are not 0 and create our final model. We can do this with standard glm or again with performance validation.
    
    ```r
    selected_vars <- rownames(coefs)[which(coefs != 0)][-1]
    alzheimer_selected <- as.matrix(alzheimerTrain[, selected_vars])
    glm_fit <- glm(alzheimerTrain$diagnosis ~ alzheimer_selected, family = "binomial")
    ```
    
    And then we have a look at the results
    
    ```r
    summary(glm_fit)
    ```
    
    We see that the coefficients for the selected columns are not alll significant. Why ?
    
    We will now use the caret package to have results for performance as well as model optimization. The train function can optimize alpha and lambda.
    
    ```r
    
    elastic_caret <- train(diagnosis ~ Alpha_1_Antitrypsin + B_Lymphocyte_Chemoattractant_BL + FAS + Fibrinogen + GRO_alpha + Gamma_Interferon_induced_Monokin + IL_7 + MIF + MMP10 + MMP7 + NT_proBNP + PAI_1 + Pancreatic_polypeptide + TRAIL_R3 + tau + p_tau + Ab_42, data = alzheimerTrain, 
    method = "glmnet", 
    family="binomial",
    trControl = fitControl_CV_ROC,
    metric="ROC",
    tuneGrid = expand.grid(
    alpha = seq(0,1,length=20),             # Ridge (alpha = 0)
    lambda = 10^seq(-4, 1, length = 50)  # search over lambda
    )
    )  
    ```
    
    We can see that based on ROC not the same lambda is selected and the best alpha selected is neither 1 nor 0 nor 0.5
    
    ```r
    elastic_caret$bestTune$lambda
    best_lambda
    elastic_caret$bestTune$alpha
    ```
    
    ```r
    elastic_model_caret_final <- glmnet(y= alzheimerTrain$diagnosis, x=alzheimerTrain[,names_candidates] , family = "binomial", alpha = elastic_caret$bestTune$alpha,lambda=elastic_caret$bestTune$lambda)
    
    coefs <- coef(ridge_model_caret_final)
    selected_vars_caret <- rownames(coefs)[which(coefs != 0)][-1]
    unique(selected_vars_caret == selected_vars)
    ```
    Therefore all the same variables have been selected.
    
    

## Leave-one-out method 

Try the leave-one-out method. Where do you need to adapt the code in the Control or the train function ?

??? done "Answer"
     ```r
     fitControl_LOOCV_ROC <- trainControl(## 5-fold CV
                           method = "LOOCV",
                           number = 5,
                      classProbs = TRUE,    # Needed for ROC
      summaryFunction = twoClassSummary )   
     ```    




## Random Forest

Again for random forest we can just process everything the same way but specifying that we want to use random forest or rf, but where ? changing the train or the control function?

??? done "Answer"
    ```r
    
    rf_caret <- train(diagnosis ~ Alpha_1_Antitrypsin + B_Lymphocyte_Chemoattractant_BL + FAS + Fibrinogen + GRO_alpha + Gamma_Interferon_induced_Monokin + IL_7 + MIF + MMP10 + MMP7 + NT_proBNP + PAI_1 + Pancreatic_polypeptide + TRAIL_R3 + tau + p_tau + Ab_42, data = alzheimerTrain, 
    method = "rf", 
    trControl = fitControl_CV_ROC,
    metric="ROC",
    tuneLength = 5
    )  
    
    print(rf_caret)
    ```
    Have now a look at the plot with the parameters
    
    ```r
    plot(rf_caret)
    ```
    
    One can have a look at the importance of the variables 
    
    ```r
    var_imp <- varImp(rf_caret)
    print(var_imp)
    ```

So with Random forest the method selects Ab_42, tau, p_tau, MMP10 and IL_7.

Bonus : 
One can visualise random forest trees using the reprtree package 

```r
final_rf <- rf_caret$finalModel
library(reprtree)

rf <- randomForest(diagnosis ~ Ab_42 + tau + p_tau + MMP10 + IL_7, data = alzheimerTrain, ntree = 10)


reprtree::plot.getTree(rf, k = 1) ## difficult to visulize but in other datasets could be important
```

