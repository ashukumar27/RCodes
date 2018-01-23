## Neural Network in R

set.seed(500)
library(MASS)
data<-Boston

#Check if any data is empty

apply(data,2, function(x) sum(is.na(x)))

#Fitting a linear regression model on the data
index<-sample(1:nrow(data),round(0.75*nrow(data)))

train<-data[index,]
test<-data[-index,]

lm.fit<-glm(medv~.,data=train)
summary(lm.fit)

pr.lm<-predict(lm.fit,test)
MSE.lm<-sum(pr.lm - test$medv)^2/nrow(test)#9.89
RMSE.lm<-sqrt(MSE.lm)#3.14


##Fitting a neural Network to this

##Part 1: Preprocessing the data
#Scaling the parameters

maxs<-apply(data,2,max)
mins<-apply(data,2,min)

#Scale: Scale() returns a matrix that needs to be converted into a data frame
scaled<-as.data.frame(scale(data, center=mins, scale = maxs-mins))


train_<-scaled[index,]
test_<-scaled[-index,]

library(neuralnet)

n<-names(train_)
f<-as.formula(paste("medv~",paste(n[!n %in% "medv"],collapse="+")))

nn<-neuralnet(f, data=train_, hidden=c(5,3), linear.output = T)
#For some reason the formula y~. is not accepted in the neuralnet() function. 
#You need to first write the formula and then pass it as an argument in the fitting function.

#The hidden argument accepts a vector with the number of neurons for each hidden layer, 
#while the argument linear.output is used to specify whether we want to do regression 
#linear.output=TRUE or classification linear.output=FALSE

plot(nn)

#Predicting using the neural network
pr.nn<-compute(nn, test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_) #15.75
RMSE.nn<-sqrt(MSE.nn)#3.96
