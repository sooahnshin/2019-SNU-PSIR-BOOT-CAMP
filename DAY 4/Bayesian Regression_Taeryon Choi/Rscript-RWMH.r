#Random-wal Metropolis with normal proposals

metroNormal=function(N,b,init){
 x<-rep(init,N)
 for (i in 2:N){
  y<-rnorm(1,x[i-1],b)
  r<-dnorm(y,0,1)/dnorm(x[i-1],0,1)
  u<-runif(1)
  if (u<r) x[i]<-y
  else x[i]<-x[i-1] }
return(x)
}

res1=metroNormal(20000,0.5,-10)
ts.plot(res1,col=3);abline(2,0,lwd=2,col=2);abline(-2,0,lwd=2,col=2)

hist(res1[10000:20000],prob=T)
lines(density(res1[10000:20000]))


