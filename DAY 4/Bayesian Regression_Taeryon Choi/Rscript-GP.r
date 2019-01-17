#install.packages("tgp")
library(tgp)

x=seq(0,10,length=100)
xx=seq(0,10,length=99)
truecurve=sin(pi*x/5)+0.2*cos(4*pi*x/5)
y=truecurve+rnorm(length(truecurve),sd=0.1)

sin.bgp=bgp(X=x,Z=y,XX=xx,verb=0,corr="exp")
par(mfrow=c(1,1))
plot(sin.bgp,main="GP regression",ylab="fitted",xlab="xobs",layout="surf",
ylim=c(-2,2))
lines(x,truecurve,col=4,lty=2,lwd=2)

# Using BSAR

install.packages("bsamGP")
library(bsamGP)
set.seed(1);  n <- 100
f <- function(x){
5-10*x+8*exp(-100*(x-0.3)^2)-8*exp(-100*(x-0.7)^2)}
w <- rnorm(n, sd = 0.5);  x <- runif(n)
y <- 2 * w + f(x) + rnorm(n, sd = 1)
fout <- bsar(y ~ w + fs(x), nbasis=50, shape = 'Free', 
marginal.likelihood = TRUE, spm.adequacy = TRUE)
summary(fout)

fit <- fitted(fout, HPD = FALSE)
plot(fit, ggplot2 = FALSE) 
lines(fit$xgrid, f(fit$xgrid), lwd = 3, lty = 2)
