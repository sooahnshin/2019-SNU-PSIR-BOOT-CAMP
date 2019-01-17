# Using JAGS & Motocycle data

library(splines)
library(MASS)

data(mcycle); str(mcycle); names(mcycle); attach(mcycle)

Y <- accel
X <- times

X <- X/max(X)

n <- length(Y)
n

plot(X,Y,xlab="time",ylab="Acceleration",cex.lab=1.5,cex.axis=1.5)
library(splines)

J <- 20
Bmat <- bs(X,J)
dim(Bmat)

spline_model <- "model{

   # Likelihood
   for(i in 1:n){
      Y[i]    ~ dnorm(mean[i],tau)
      mean[i] <- mu + inprod(Bmat[i,],beta[])
   }
   
   for(i in 1:J){
    beta[i] ~ dnorm(0,taub)
   }
   
   # Priors
   mu   ~ dnorm(0,0.01)
   tau ~ dgamma(0.1,0.1)
   taub ~ dgamma(0.1,0.1)
   
  }"

library(rjags)
dat  <- list(Y=Y,n=n,Bmat=Bmat,J=J)
init  <- list(mu=mean(Y),beta=rep(0,J),tau=var(Y))
spline.fit <- jags.model(textConnection(spline_model),inits=init,data = dat, n.chains=2)

samp   <- coda.samples(spline.fit, 
             variable.names=c("mean"), 
             n.iter=20000, progress.bar="none")
             
spline.samp <- summary(samp)
names(spline.samp)

   plot(X,Y,xlab="time",ylab="Acceleration",cex.lab=1.5,cex.axis=1.5)

   lines(X,spline.samp$quantiles[,1],col="red",lty=2)
   lines(X,spline.samp$statistics[,1],col="blue",lty=1)
   lines(X,spline.samp$quantiles[,5],col="red",lty=2)

   legend("bottomright",c("Mean","95% interval"),lty=1:2,col=c("blue","red"),bg=gray(1),inset=0.05,cex=1.5)



# Using Stan & Motocycle data

library(splines); library(rstan)

library(MASS)

data(mcycle); str(mcycle); names(mcycle); attach(mcycle)

Y <- accel
X <- times

X <- X/max(X)

n <- length(Y)
n

plot(X,Y,xlab="time",ylab="Acceleration",cex.lab=1.5,cex.axis=1.5)
library(splines)

num_knots=10
num_data = length(X)

# creating the B-splines: degree = 3 (cubic), knots  = 10
knots = unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots)))
B <- bs(X,num_knots)    
num_basis=ncol(B)
          
spline_code = "data { 
  int num_data; 
  int num_basis; 
  vector[num_data] Y; 
  matrix[num_data,num_basis] B; 
} 
parameters { 
  matrix[num_basis,1] beta; 
  real<lower=0> sigma; 
}
model { 
  vector[num_data] mu;
  mu = to_vector(B*beta);
 for(i in 1:num_basis){ beta[i] ~ normal(0,100);}
  sigma    ~ inv_gamma (1, 10e-5); 
  Y ~ normal(mu, sigma);
}"


# Input settings
dataList <- list(Y = Y, B = B, num_data = num_data, num_basis= num_basis)
params = c("beta", "sigma")  

stan_model = stan(model_code=spline_code, pars=params, 
             data= dataList, chains=1,warmup=5000,iter=10000)  
summary(stan_model) 

extract = extract(stan_model)  

beta = colMeans(extract$beta)
muhat = B%*% beta

plot(X,Y,xlab="time",ylab="Acceleration",cex.lab=1.5,cex.axis=1.5)
lines(X,muhat,col="blue",lty=1, lwd=2)
   


