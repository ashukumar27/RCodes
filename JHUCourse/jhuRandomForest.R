library(caret)


data(iris)
inTrain<-createDataPartition(y=iris$Species,p=0.7, list=FALSE)

training<-iris[inTrain,]
test<-iris[-inTrain,]

modFit<-train(Species~.,method="rf", data=training, prox=TRUE)
modFit


#looking at a specific tree

getTree(modFit$finalModel,k=2)

pred<-predict(modFit, test)
test$predRight<-pred==test$Species

table(pred, test$Species)
