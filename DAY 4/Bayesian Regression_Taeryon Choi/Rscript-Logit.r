# MH sampling for logistic regression 

logit<-function(x){log(x/(1-x))}

 logpost<-function(Y,x,beta){
    prob1  <- exp(beta[1] + beta[2]*x)/(1+exp(beta[1]+beta[2]*x))
    like   <- sum(dbinom(Y,1,prob1,log=TRUE))
    prior  <- sum(dnorm(beta,0,10,log=TRUE))
    lpost <- like+prior
		return(lpost)
		}
		
 MH.logisticreg <- function(Y,x,n.iters,b)
{		
 beta <- rnorm(2,0,1) #inits
 post.beta <- matrix(0,n.iters,2)
 post.beta[1,] <- beta
 accept <- rep(0,2)
 logpost_current <- logpost(Y,x,beta)
 
 for (i in 2:n.iters){
   for (j in 1:2) {
   beta_cand <- beta
	 beta_cand[j] <- rnorm(1,beta[j],b)
   logpost_cand <- logpost(Y,x,beta_cand)
   
	 ratio <- exp(logpost_cand - logpost_current)
	 U <- runif(1)
	
	 if (U < ratio) {
	   beta <- beta_cand
		 logpost_current <- logpost_cand
	   accept[j] <- accept[j] + 1
    }
   } 
	 post.beta[i,] <- beta
	  }
	list(beta=post.beta, accept_rate=accept/n.iters) 
}


library(LearnBayes); 
data(donner);attach(donner); 

donnerlogistic.fit <- MH.logisticreg(survival,age,50000,0.5)
summary(glm(survival~age,family="binomial"))
apply(donnerlogistic.fit$beta[10000:50000,],2,mean)

library(MCMCpack)
MCMCpack.logistic <- MCMClogit (survival~age, burnin=10000,mcmc=50000)
summary(MCMCpack.logistic)

library(bsamGP)
bsamGP <- gblr (survival~age,family="bernoulli",
                link="logit",
                mcmc=list(nblow=10000,nskip=1,smcmc=50000))
options(digits=3)
summary (bsamGP)


# Using JAGS


logistic_model <- "model{
   # Likelihood
   for(i in 1:n){
    Y[i] ~ dbern(q[i])
    logit(q[i]) <- beta[1] + beta[2]*X[i]
   }
   #Priors
   for(j in 1:2){
    beta[j] ~ dnorm(0,0.01)
   }
  }"

library(rjags); library(LearnBayes)
data(donner);attach(donner); 

dat   <- list(Y=survival,n=length(survival),X=age)
model <- jags.model(textConnection(logistic_model),data = dat,n.chains=3, quiet=TRUE)
update(model, 10000, progress.bar="none")
samp <- coda.samples(model, 
          variable.names=c("beta"), 
          n.iter=20000, progress.bar="none")    
                   
autocorr.plot(samp)        
gelman.plot(samp)
effectiveSize(samp)
summary(samp)
plot(samp)


# Using stan

library(rstan); options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)

logit_code = "data {
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
  for (i in 1:n) Y[i] ~ bernoulli_logit (xb[i]);
  for(i in 1:2) beta[i] ~ normal(0,100);
}"
dataList = list(Y=survival,n=length(survival),x=model.matrix(~age))
stan_model = stan(model_code=logit_code, 
             data= dataList, chains=1,warmup=5000,iter=10000)  
summary(stan_model)    	
  