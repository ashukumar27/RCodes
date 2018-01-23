data(iris)
library(ggplot2)
library(caret)
names(iris)

table(iris$Species)
head(iris)


#Predicting with Decision Trees

inTrain<-createDataPartition(y=iris$Species,p=0.7, list=FALSE)

training<-iris[inTrain,]
test<-iris[-inTrain,]

dim(training)
dim(test)

qplot(Petal.Width,Sepal.Width, colour = Species, data=training)
  
modFit<-train(Species~., data=training, method="rpart")
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE,main="Classification Tree")
text(modFit$finalModel,use.n=TRUE, all=TRUE, cex=0.8)


library(rattle)
fancyRpartPlot(modFit$finalModel)
  

predict(modFit,newdata=test)
