setwd("~/Desktop/BDA/TERM 2/PPA(Predictive & Prescriptive Analytics)/PPA Working on R")

library(car)    
library(corrplot)
library(caret)
library(caTools)
library(psych) 
library(ggplot2) 
library(rgdal) 
library(ROCR)

##Data Read:
DH<- read.csv("DATASET_HEART ATTACK POSSIBILITY.csv", stringsAsFactors = TRUE)
View(DH)
class(DH)

## Correlations:
cr<- cor(DH)
corrplot(cr,type = "full")

## Checking the Normality of the dataset
names(DH)
pairs.panels(DH)

## Data Split:
split<- sample.split(DH$Target, SplitRatio = 0.7) 
training_data <- subset(DH,split == 'TRUE')
testing_data <- subset(DH,split == 'FALSE')

## Linear Regression:
model1 <- lm (Target~ CPT+MAX.HR+
                 Exercise+Oldpeak+Major.vessels+Thal, data = training_data)
summary(model1)

## PREDICTION: 
prediction <- predict(model1,testing_data)
head(prediction)

## Plot:
plot(testing_data$Target, type= 'l', col="green")
lines(prediction,type = 'l', col= 'blue')

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

## Logistic Regression:
model2 <- glm (Target~ CPT+MAX.HR+
               Exercise+Oldpeak+Major.vessels+Thal, data = training_data)
summary(model2)

## Prediction:
res <- predict (model2,testing_data, type='response')
head(res)
head(DH$Target)

## Confusion Matrix:
table (ActualValue = testing_data$Target, PredictValue = res > 0.6)

## Accuracy:
Accuracy = (59+163)/(59+79+2+163)
Accuracy

## ROCR Curve:
ROCRpred<- prediction(res, testing_data$Target)
ROCRpref <- performance (ROCRpred, "tpr", "fpr")
plot(ROCRpref,colorize = TRUE,print.cutoffs.at = seq(0.2, by=0.3))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------








