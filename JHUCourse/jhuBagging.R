#Bgging Example
library(caret)
library(ElemStatLearn)

data(ozone)
ozone<-ozone[order(ozone$ozone),]
head(ozone)

#Predict temperature as a function of ozone