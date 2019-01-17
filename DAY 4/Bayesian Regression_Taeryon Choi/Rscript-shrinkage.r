#install.packages("lars")
library(lars)

data(diabetes); str(diabetes)
names(diabetes); attach(diabetes)

Y <- diabetes$y
X <- diabetes$x; X[1,]; p <- ncol(X); p
X    <- (X - mean(X))/sd(X)
n = length(Y)

library(rjags)


model_tprior <- "model{
  # Likelihood
  for(i in 1:n){
    Y[i]   ~ dnorm(mu[i],inv.var)
    mu[i] <- alpha + inprod(X[i,],beta[]) # same as alpha + X[i,1]*beta[1] + ... + X[i,p]*beta[p]
  }

  # Prior for beta
  for(j in 1:p){
    beta[j] ~ dnorm(0,0.0001)
  }

  # Prior for the inverse variance
  inv.var   ~ dgamma(0.01, 0.01)
  alpha     ~ dnorm(0, 0.01)

}"

 modeltprior <- jags.model(textConnection(model_tprior), 
                    data = list(Y=Y,n=n,p=p,X=X),quiet=TRUE)

update(modeltprior, 10000, progress.bar="none")

samp <- coda.samples(modeltprior, 
        variable.names=c("beta","alpha"), 
        n.iter=20000, progress.bar="none")

summary(samp)


model_lasso <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i]   ~ dnorm(mu[i],inv.var)
    mu[i] <- alpha + inprod(X[i,],beta[])
  }

  # Prior for beta
  for(j in 1:p){
    beta[j] ~ ddexp(0,inv.var.b)
  }

  # Prior for the inverse variance
  inv.var   ~ dgamma(0.01, 0.01)
  inv.var.b ~ dgamma(0.01, 0.01)
  alpha     ~ dnorm(0, 0.01)

}"


 modellasso <- jags.model(textConnection(model_lasso), 
                    data = list(Y=Y,n=n,p=p,X=X),quiet=TRUE)

update(modellasso, 10000, progress.bar="none")

samp <- coda.samples(modellasso, 
        variable.names=c("beta"), 
        n.iter=20000, progress.bar="none")

summary(samp)


#install.packages("monomvn") #blasso function
library(monomvn)
blasso.fit <- blasso(X, Y, RJ=FALSE, normalize=F) #already normalized

summary(blasso.fit)

