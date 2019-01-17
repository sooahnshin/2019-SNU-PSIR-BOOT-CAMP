# Probit regression
 
#install.packages("LearnBayes")
library(LearnBayes)
data(donner); str(donner); names(donner); attach(donner); 
Probit.fit=glm(survival~age,family=binomial(link=probit))
summary(Probit.fit)

# Using JAGS
probit_model <- "model{
   for(i in 1:n){
    Y[i]  ~ dbin(p[i],1)
    p[i] <- phi(beta[1] + beta[2]*x[i])
   }
   for(j in 1:2){
    beta[j] ~ dnorm(0,0.01)
   }
  }"
library(rjags)
dat    <- list(Y=survival,n=length(survival),x=age)
model1 <- jags.model(textConnection(probit_model),
                 data = dat,n.chains=3, quiet=TRUE)
update(model1, 10000, progress.bar="none")
samp <- coda.samples(model1, variable.names=c("beta"), 
        n.iter=20000, progress.bar="none")
summary(samp); plot(samp); effectiveSize(samp)


# Using Stan

library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)

probit_code = "data {
  int <lower=0> n;
  int Y[n];
  matrix [n,2] x;
}
parameters {
  vector [2] beta ;
}
model {
  vector [n] xb;
  xb = x* beta ;
  
  // Likelihood
  for (i in 1:n) Y[i] ~ bernoulli ( Phi (xb[i]));
  for(i in 1:2) beta[i] ~ normal(0,100);
}"

library(LearnBayes); library(rstan)
data(donner);attach(donner); 
dataList = list(Y=survival,n=length(survival),x=model.matrix(~age))


stan_model = stan(model_code=probit_code, 
             data= dataList, chains=1,warmup=5000,iter=10000)  
summary(stan_model)              



