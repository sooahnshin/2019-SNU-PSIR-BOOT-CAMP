
# Using R codes (Dugong data)

x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 
      5.0, 7.0, 8.0, 8.5, 9.0, 9.5, 9.5, 
     10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 
		 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)
		 
Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15,
      2.26, 2.47, 2.19, 2.26, 2.40, 2.39, 2.41, 
   	  2.50, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
			2.47, 2.64, 2.56, 2.70, 2.72, 2.57)
n = length(Y)

plot(x,Y); plot(log(x),Y)
OLS.fit <- lm(Y~log(x))
summary(OLS.fit)

mu0 <- matrix(0,2,1)
Sigm0 <- matrix(c(1000,0,0,1000),2,2)
a0 <- 0.01
b0 <- 0.01
xmat <- t(rbind(rep(1,n),log(x)))

n.iters <- 10000

# Initial values
beta0       <- OLS.fit$coef[1]
beta1        <- OLS.fit$coef[2]
sig2          <- var(OLS.fit$residuals)

vec.beta <- matrix(0,nrow=2,ncol=n.iters)
vec.sig2 <- rep(0,n.iters)

vec.beta[1,] <- c(beta0,beta1)
vec.sig2[1] <- var(OLS.fit$residuals)


library(MASS)  # for using the function 'mvrnorm'
for (i in 2:n.iters){
  Sigm1 <- solve(solve(Sigm0)+t(xmat)%*%xmat/sig2)
  mu1 <- Sigm1%*%(solve(Sigm0)%*%mu0 + t(xmat)%*%Y/sig2)
  b <- mvrnorm(1,mu1,Sigm1)
  sig2 <- 1/rgamma(1,a0+n/2,b0+sum((Y-b[1]-log(x)*b[2])^2)/2)
  vec.beta[,i] <- b
  vec.sig2[i] <- sig2
}

par(mfrow=c(3,1))
hist(vec.beta[1,],density=-1,yaxt="n",ylab="",xlab="beta0",cex=2,nclass=50,main="")
hist(vec.beta[2,],density=-1,yaxt="n",ylab="",xlab="beta1",cex=2,nclass=50,main="")
hist(vec.sig2,density=-1,yaxt="n",ylab="",xlab="sigma2",cex=2,nclass=50,main="")

mean(vec.beta[1,]); sd(vec.beta[1,])
mean(vec.beta[2,]); sd(vec.beta[2,])
mean(vec.sig2); sd(vec.sig2)


# Using WinBUGS

model{
 for (i in 1:N) {
  mu[i]<-beta0+beta1*x[i]
  y[i]~dnorm(mu[i],tau)
  }
  beta0~dnorm(0,0.0001)
  beta1~dnorm(0,0.0001)
  tau~dgamma(0.1,0.1)
  }
list(y=c(8.4,9.5,11.8,10.4,13.3,14.8,13.2,14.7,16.4,16.5,18.9,18.5),
x=c(20,22,24,26,28,30,32,34,36,38,40,42),N=12)
#initial values
list(beta0=0, beta1=0, tau=1)


# Prediction

p <- 2; nn <- length(y); N <- 10000
X <- cbind(rep(1,nn),x)
beta.hat <- solve(t(X)%*%X)%*%t(X)%*%y
s2 <- t(y)%*%(diag(nn)-X%*%solve(t(X)%*%X)%*%t(X))%*%y/(nn-p)
vec.new <- rep(0,N)
xnew <- 35
for (m in 1:N){
  r <- rgamma(1,(nn-p)/2,(nn-p)*s2/2)
  b <- mvrnorm(1,beta.hat,solve(t(X)%*%X)/r)
  vec.new[m] <- rnorm(1, mean=b[1]+b[2]*xnew, sd=sqrt(1/r))
}

par(mfrow=c(1,1))
hist(vec.new,yaxt="n",ylab="",xlab="ynew",cex=2,nclass=50,main="",prob=T)
lines(density(vec.new))


# Dugong BUGS 
           
model{
  for( i in 1:n) {
    logage[i] <- log(x[i])
    Y[i] ~ dnorm(mu[i] , tau)
    mu[i] <- beta0+ beta1*logage[i]  # uncentered
#    mu[i] <- beta0+ beta1*(logage[i] - mean(logage[])) # centered 
    }
  beta0 ~ dnorm(0, 0.001)  
  beta1 ~ dnorm(0, 0.001)
  tau ~ dgamma(0.1, 0.1)   
  sigma <- 1/sqrt(tau)
}  #  end of BUGS code
#Data:
list(x = c( 1.0,  1.5,  1.5,  1.5, 2.5,   4.0,  5.0,  5.0,  7.0,
	            8.0,  8.5,  9.0,  9.5, 9.5,  10.0, 12.0, 12.0, 13.0,
	           13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
	     Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
	           2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
	           2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57), n = 27)
# Inits:
list( beta0 = 0, beta1 = 1, tau = 1)  


# Dugong JAGS


library(rjags)

jags_code ="model{
    for( i in 1:n) {
        logage[i] <- log(x[i])
        Y[i] ~ dnorm(mu[i] , tau)
        mu[i] <- beta0+ beta1*logage[i] # uncentered
        # mu[i] <- beta0+ beta1*(logage[i] - mean(logage[])) # centered
    }
    beta0 ~ dnorm(0, 0.001)
    beta1 ~ dnorm(0, 0.001)
    tau ~ dgamma(0.1, 0.1)
    sigma <- 1/sqrt(tau)
}"
jags_data = list(x = c( 1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
            8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
            13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
     Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
           2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
           2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57), n = 27)
jags_model = jags.model(textConnection(jags_code), data=jags_data)
update(jags_model, 10000) #progress.bar="none")
samp <- coda.samples(jags_model,variable.names=c("beta0","beta1","sigma"),
        n.iter=5000)
summary(samp)
plot(samp)


 	           
 	           
library(R2jags)

jags_code ="model{
    for( i in 1:n) {
        logage[i] <- log(x[i])
        Y[i] ~ dnorm(mu[i] , tau)
        mu[i] <- beta0+ beta1*logage[i] # uncentered
        # mu[i] <- beta0+ beta1*(logage[i] - mean(logage[])) # centered
    }
    beta0 ~ dnorm(0, 0.001)
    beta1 ~ dnorm(0, 0.001)
    tau ~ dgamma(0.1, 0.1)
    sigma <- 1/sqrt(tau)
}"

#Data:
jags_data = list(x = c( 1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
            8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
            13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
     Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
           2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
           2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57), n = 27)
# Inits:
init_values = function()list(beta0 = 0, beta1 = 1, tau = 1)

jags_model = jags(model.file = textConnection(jags_code),
                  data=jags_data,
                  parameters = c("beta0", "beta1", "sigma", "mu"),
                  inits=init_values,
                  n.iter=5000,
                  n.burnin=2500,
                  n.thin=1,
                  n.chains=1)
jags_model
 	           
 	           


# Using stan


library(rstan)
rstan_options(auto_write=TRUE)
options(cores=parallel::detectCores())

stan_code="data{
    int<lower=0> n;
    vector[n] x;
    vector[n] Y;
}
parameters{
    real beta0;
    real beta1;
    real<lower=0> sigma;
}
transformed parameters{
    vector[n] logage;
    vector[n] mu;
    for (i in 1:n){
    logage[i] = log(x[i]);
    mu[i] = beta0+beta1*logage[i]; 
}}
model{
    Y ~ normal(mu, sigma);
    beta0 ~ normal(0, 1000);
    beta1 ~ normal(0, 1000);
    sigma ~ inv_gamma(0.1, 0.1);
}"



stan_data = list(x = c( 1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 
                        5.0, 7.0, 8.0, 8.5, 9.0, 9.5, 9.5, 
											 10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 
											 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
                 Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15,
								       2.26, 2.47, 2.19, 2.26, 2.40, 2.39, 2.41, 
											 2.50, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
											2.47, 2.64, 2.56, 2.70, 2.72, 2.57), n = 27)
											
init_values = function() list(beta0=0, beta1=1, sigma=1)

stan_model = stan(model_code = stan_code,
                  data = stan_data,
                  pars = c("beta0","beta1", "sigma", "mu"),
                  init = init_values,
                  iter = 5000,
                  warmup = 2500,
                  thin = 1,
                  chains = 1)

print(stan_model,digits_summary = 3)    

# Using MCMCpack

library(MCMCpack)
# Fit
posteriors <- with(stan_data,MCMCregress(Y ~ log(x)))
# Output
summary(posteriors)
# Plot
plot(posteriors)


library(bsamGP)
posteriors <- with(stan_data,blr(Y~log(x)))                   # Fit
print(posteriors); summary(posteriors)    # Summary
fit = fitted(posteriors)                        # Fitted values
plot(posteriors)                          # Plots


# Using R codes (Gibbs sampler)

x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 
      5.0, 7.0, 8.0, 8.5, 9.0, 9.5, 9.5, 
     10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 
		 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)
		 
Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15,
      2.26, 2.47, 2.19, 2.26, 2.40, 2.39, 2.41, 
   	  2.50, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
			2.47, 2.64, 2.56, 2.70, 2.72, 2.57)
n = length(Y)

plot(x,Y); plot(log(x),Y)
OLS.fit <- lm(Y~log(x))
summary(OLS.fit)

mu0 <- 0
sig20 <- 1000
a0 <- 0.01
b0 <- 0.01

n.iters <- 10000
vec.beta <- matrix(0,n.iters,2)
colnames(vec.beta)<-c("beta0","beta1")
vec.sig2 <- numeric(n.iters)




# Initial values
beta0       <- OLS.fit$coef[1]
beta1        <- OLS.fit$coef[2]
sig2          <- var(OLS.fit$residuals)
vec.beta[1,] <- c(beta0,beta1)
vec.sig2[1] <- sig2

for(i in 2:n.iters){
# sample beta0

     V     <- n/sig2+1/sig20
     M     <- sum(Y-log(x)*beta1)/sig2+mu0/sig20
     beta0 <- rnorm(1,M/V,1/sqrt(V))

# sample beta1

     V     <- sum(log(x)^2)/sig2+1/sig20
     M     <- sum((Y-beta0)*log(x))/sig2+mu0/sig20
     beta1  <- rnorm(1,M/V,1/sqrt(V))

# sample sig2

     A  <- n/2 + a0
     B  <- sum((Y-beta0-log(x)*beta1)^2)/2 + b0
     sig2 <- 1/rgamma(1,A,B)

     vec.beta[i,] <- c(beta0,beta1)
     vec.sig2[i] <- sig2
  }
  
  

par(mfrow=c(1,1))

mean(vec.beta[,1]); sd(vec.beta[,1])
mean(vec.beta[,2]); sd(vec.beta[,2])
mean(vec.sig2); sd(vec.sig2)  

beta1 <- vec.beta[,2]
hist(beta1,main="Posterior of the slope, beta1",breaks=100)
ts.plot(beta1); acf(beta1)
summary(beta1)

beta0 <- vec.beta[,1]
hist(beta0,main="Posterior of the intercept, beta0",breaks=100)
ts.plot(beta0); acf(beta0)
summary(beta0)


sig2 <- vec.sig2
hist(sig2,main="Posterior of the noise variance, sig2",breaks=100)
ts.plot(sig2); acf(sig2)
summary(sig2)

# thinning

sig2 <- vec.sig2[1000:10000]
ind <- seq(1,length(sig2),10)
sig2 <- sig2[ind]
ts.plot(sig2); acf(sig2)
summary(sig2)

# effective sample size

library(coda)
effectiveSize(beta1)
effectiveSize(beta0)
effectiveSize(sig2)








