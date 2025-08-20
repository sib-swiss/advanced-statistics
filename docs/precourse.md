


### Previous knowledge

As is stated in the course prerequisites on the [announcement web page](https://www.sib.swiss/training/course/20250901_ASSM), this course is intended for people already familiar with basic statistics and R. Participants must be comfortable with topics such as hypothesis testing, correlation and linear models, and must have a prior knowledge of the "R" language and environment for statistical computing and graphics. Participants who have already followed the SIB course ["Introduction to statistics with R"](https://www.sib.swiss/training/course/20250127_STATR) or an equivalent course, and have used its content in practice should fit this prerequisite.

Before applying to this course, please self-assess your knowledge in stats and R to make sure this course is right for you. Here are 2 quizzes:

[https://gohighbrow.com/quiz-introduction-to-statistics/](https://gohighbrow.com/quiz-introduction-to-statistics/)

[https://docs.google.com/forms/d/e/1FAIpQLSfXCnmLha0Ks4ZZZ42G_5MyIbGi-JhPayuHZ_P2jdXZEtXdqg/viewform](https://docs.google.com/forms/d/e/1FAIpQLSfXCnmLha0Ks4ZZZ42G_5MyIbGi-JhPayuHZ_P2jdXZEtXdqg/viewform)

### Technical

To do the exercises, you are required to have your own computer with at least 4 Gb of RAM and with an internet connection, as well as the latest the version of [R](https://cran.r-project.org/)
and the free version of [RStudio](https://www.rstudio.com/products/rstudio/download/) installed.

To ensure that the course runs smoothly check if you are allowed to install packages into R. Some companies block users to download packages for security reasons and an error saying : contact your administrator might appear. Make sure to discuss with the IT service of your company if you can, and if not contact us in order to be able to follow the course easily.

It might also happen that your antivirus blocks R from downloading packages.

In order to check if all runs smoothly, try to download your first package from R Studio, you can go to the menu Tools -> Install packages?, and then choose the package you need installed (choose for example “akima”). Using the RGui under Windows, you can go to menu Packages -> Install package(s). In the console, you can use the install.packages command: install.packages(“akima”) for example. Once installed you can load the library, which should then not give you an error: library(“akima”).

Please, install the necessary packages using:

```r
install.packages("akima")
install.packages("car")
install.packages("RANN")
install.packages("DAAG")
install.packages("faraway")
install.packages("gam")
install.packages("ggplot2")
install.packages("ISwR")
install.packages("lattice")
install.packages("lme4")
install.packages("nloptr")
install.packages("nlme")
install.packages("splines")
install.packages("statmod")
install.packages("WWGbook")
install.packages("caret")
install.packages("glmnet")
install.packages("ROCR")
install.packages("pROC")
install.packages("AppliedPredictiveModeling")
#devtools::install_github('araastat/reprtree') # this is optional
```

