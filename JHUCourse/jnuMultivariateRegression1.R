library(datasets)
data(swiss)
require(stats)
require(graphics)

pairs(swiss, panel=panel.smooth, main="Swiss Data",col=3+(swiss$Catholic>50))

fit<-lm(Fertility~.,data=swiss)
summary(fit)


###Dummy Variables

###INsectSpray Data
data("InsectSprays")
fit<-lm(count~spray, data=InsectSprays)
fit$coefficients
summary(fit)$coef


plot(fit)
plot(hatvalues(fit))


r