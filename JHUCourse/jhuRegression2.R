library(UsingR)
data("diamond")

plot(diamond$carat, diamond$price,
     xlab="Mass(carats)", ylab="Price(SIN $)",
     bg="lightblue", col="black", cex=1.1, pch=21, frame=FALSE)
abline(lm(price~carat, data=diamond),lwd=2)

fit<-lm(price~carat, data=diamond)
coef(fit)

#Cost of avg sized diamond (intercpt)
fit2<-lm(price~I(carat-mean(carat)),data=diamond)
coef(fit2)

#Changing unit of size to 1/10 of a carat
fit3<-lm(price~I(carat*10), data=diamond)
coef(fit3)

#Predicting the price of a diamond
newx<-c(0.16,0.27, 0.34)

coef(fit)[1]+coef(fit)[2]*newx

predict(fit, newdata=data.frame(carat=newx))


##Residuals
y<-diamond$price
x<-diamond$carat

n<-length(y)
fit<-lm(y~x)

e<-resid(fit)
yhat<-predict(fit)

max(abs(e-(y-yhat)))
plot(x,e)
fit$sigma


##Confidence Intervals and other parameters
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)

e<-y-beta0-beta1*x
sigma<-sqrt(sum(e^2)/(n-2))

ssx<-sum((x-mean(x))^2)

seBeta0<- (1/n+mean(x)^2/ssx)^0.5*sigma
seBeta1<-sigma/sqrt(ssx)

tBeta0<-beta0/seBeta0
tBeta1<-beta1/seBeta1

pBeta0<- 2*pt(abs(tBeta0), df=n-2, lower.tail=FALSE)
pBeta1<- 2*pt(abs(tBeta1), df=n-2, lower.tail=FALSE)

coefTable<- rbind(c(beta0, seBeta0, tBeta0, pBeta0),c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable)<-c("Estimate", "StdError", "t value", "P(>|t|)")
rownames(coefTable)<-c("(Intercept)", "x")
coefTable
summary(fit)$coefficients
library(caret)


##Getting a confidence interval
sumCoef<-summary(fit)$coefficients
sumCoef[1,1] + c(-1,1)*qt(.975, df=fit$df)*sumCoef[1,2]
sumCoef[2,1] + c(-1,1)*qt(.975, df=fit$df)*sumCoef[2,2]
