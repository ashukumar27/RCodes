library(UsingR);
data(galton)
par(mfrow=c(1,2))
hist(galton$child, col="blue", breaks=100)
hist(galton$parent, col="blue", breaks=100)
 
library(manipulate)
myhist<-function(mu){
  hist(galton$child,col="blue", breaks=100)
  lines(c(mu,mu), c(0,150), col="blue", lwd=5)
  mse = mean((galton$child-mu)^2)
}

manipulate(myhist(mu), mu=slider(62,74, step=0.5))


plot(galton$child, galton$parent, col="blue")

mod = lm(galton$child~galton$parent)
plot(mod)

mod$coefficients
