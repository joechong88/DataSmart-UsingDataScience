################################################################################
#
# Building AI Models on Pregnancy Data
# Build 2 classifiers using the glm() function (general linear model)
# with a logistic link function and using the randomForest() (randomForest()
# bags trees, which may be anywhere from simple stumps to full decision trees)
################################################################################

# load the training and test data sets into data frames
PregnancyData <- read.csv("Pregnancy.csv")
PregnancyData.Test <- read.csv("Pregnancy_Test.csv")

# run summary() and str() on the data to get a feel for it
str(PregnancyData)
summary(PregnancyData)
str(PregnancyData.Test)
summary(PregnancyData.Test)

# you'll notice that the response variable, Pregnant indicator has been classified
# as 0 and 1. This would better serve the randomForest() function if it can be factorize
# into 2 classes. So, to factorize:
PregnancyData$PREGNANT <- factor(PregnancyData$PREGNANT)
PregnancyData.Test$PREGNANT <- factor(PregnancyData.Test$PREGNANT)
summary(PregnancyData$PREGNANT)
summary(PregnancyData.Test$PREGNANT)

# model 1 - build logistic regression. To build this, you need the glm()
# model 2 - randomForest: need randomForest package and also ROCR package for graphing
library(randomForest)
library(ROCR)

# start with model #1 glm()
# use the family=binomial("logit") for the logistic regression
# supply the data with PregnancyData
# PREGNANT ~ . -> its a formula in R, meaning "train my model to predict the PREGNANT column using all other columns
# You can also specify a subset of columns by typing their columns names explicityly. 
Pregnancy.lm <- glm(PREGNANT ~ ., data=PregnancyData, family=binomial("logit"))

# check the results
# any coefficients without at least one * next to them are of dubious worth
summary(Pregnancy.lm)

# then try with model #2 randomForest()
# note the importance=TRUE, this allows you to graph variable importance using another function, varImpPlot()
# which will allow you to understand which variables are important and which are weak
Pregnancy.rf <- randomForest(PREGNANT ~ ., data=PregnancyData, importance=TRUE)

# plot to show significant and weak variables by using type=2. You'll notice that Folic Acid rank first, followed by
# Prenatal vitamins and birth control.
varImpPlot(Pregnancy.rf, type=2)

# Once the models are built, you can predict them using the predict() function.
# Call the function, and save the results into 2 variables for later comparison
# predict() accepts a model, a dataset to predict on, and any model-specific options

# for linear model, the type="response" sets the values returned from the prediction to be between 0 and 1
PregnancyData.Test.lm.Preds <- predict(Pregnancy.lm, PregnancyData.Test, type="response")
summary(PregnancyData.Test.lm.Preds)

# for randomForest, the type="prob" ensures that you get back class probabilities - 2 columns of data
# one probability of pregnancy and one probability of no pregnanct
PregnancyData.Test.rf.Preds <- predict(Pregnancy.rf, PregnancyData.Test, type="prob")
summary(PregnancyData.Test.rf.Preds)

# Using bracket notation, you can pull out individual records or sets of records and look at their input
# data and predictions
t(PregnancyData.Test[1,])
t(PregnancyData.Test.lm.Preds[1])
t(PregnancyData.Test.rf.Preds[1,2])

# compare the models in terms of true +ve rate and false +ve rate. You can use the ROCR to compute and plot
# the graphs. So, create 2 ROCR prediction objects (using ROCR prediction() function), which simply count up
# the +ve and -ve class predictions at various cutoff levels in the class probabilities
pred.lm <- prediction(PregnancyData.Test.lm.Preds, PregnancyData.Test$PREGNANT)
pred.rf <- prediction(PregnancyData.Test.rf.Preds[,2], PregnancyData.Test$PREGNANT)

# Using performance() function passing "tpr" and "fpr" for true +ve rate and false +ve rate
perf.lm <- performance(pred.lm, "tpr", "fpr")
perf.rf <- performance(pred.rf, "tpr", "fpr")

# plot the ROC curve
plot(perf.lm, xlim=c(0, 1), ylim=c(0, 1))
plot(perf.rf, xlim=c(0, 1), ylim=c(0, 1), lty=2, add=TRUE)

# to recap :
# 1. You trained 2 different predictive models
# 2. Used them on a test set
# 3. Compared their precision versus recall 
