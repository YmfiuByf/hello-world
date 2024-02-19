bootLS <- function(x,y,conf=0.95,B=1000){
  # fit a linear regression model on given samples
  m = lm(y~x)
  beta0 = m$coefficients[1]
  beta1 = m$coefficients[2]
  sebeta0 = summary(m)$coefficients[,2][1]
  sebeta1 = summary(m)$coefficients[,2][2]
  N = length(x)
  beta0_b = rep(NA,B)
  beta1_b = rep(NA,B)
  t0_b = rep(NA,B)
  t1_b = rep(NA,B)
  for (i in 1:B){
    # sample from empirical distribution
    ids = sample(1:N,N,replace=TRUE)
    xb = x[ids]
    yb = y[ids]
    # fit simple linear regression model
    mb = lm(yb~xb)
    beta0_b[i] = mb$coefficients[1]
    beta1_b[i] = mb$coefficients[2]
    sebeta0_b = summary(mb)$coefficients[,2][1]
    sebeta1_b = summary(mb)$coefficients[,2][2]
    t0_b[i]=(beta0_b[i]-beta0)/(sebeta0_b)
    t1_b[i]=(beta1_b[i]-beta1)/(sebeta1_b)
  }
  boot_int = matrix(c(beta1+quantile(t1_b,c((1-conf)/2,(1+conf)/2))*sebeta1),ncol = 2)
  colnames(boot_int) = paste(c((1-conf)/2*100,(1+conf)/2*100),'%')
  return(boot_int)
}
set.seed(0)
Ns = c(50,100,500,1000,5000)
N = 1000
x = rnorm(N)
# (1)
y1 = 2 + 0.5*x + 1*rnorm(N)
bootLS(x,y1)
one_experiment <- function(N,B){
  x = runif(N,0,1)
  y1 = 2 + 0.5*x + rnorm(N,sd=1)
  y2 = 2 + 0.5*x + rexp(N,rate=1)
  
  cover <- function(range,num=0.5){
    return((range[1]<=num)&(range[2]>=num))
  }
  
  # bootstrap on normal noise
  t0 <- Sys.time()
  mb1 = bootLS(x,y1,B=B)
  t1 <- Sys.time()
  
  # bootstrap on exp noise
  mb2 = bootLS(x,y2,B=B)
  t2 <- Sys.time()
  
  # classical on normal noise
  m1 = lm(y1~x)
  m1 = confint(m1)[2,]
  
  # classical on exp noise
  t3 <- Sys.time()
  m2 = lm(y2~x)
  m2 = confint(m2)[2,]
  t4 <- Sys.time()
  #return(c(t1-t0,t2-t1,t3-t2,t4-t3))
  return (matrix(c(cover(mb1),cover(mb2),cover(m1),cover(m2),
            mb1[2]-mb1[1],mb2[2]-mb2[1],m1[2]-m1[1],m2[2]-m2[1],
            t1-t0,t2-t1,t3-t2,t4-t3),ncol=4,nrow=3,byrow=TRUE))
}


res = c()
for (N in Ns){
  T = 1000
  B = 200
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = T, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  m = matrix(0,ncol=4,nrow=3)
  for (t in 1:T){
    m <- m+one_experiment(N,B)
    setTxtProgressBar(pb, t)
  }
  m = m/T
  res = c(res,m)
}
res = array(res,dim=c(3,4,5))
par(mfrow=c(3,1))
metrics = c('coverage','average interval length','average computational time')
for (n in 1:3){
  barplot(t(res[n,,]),beside=TRUE,name=c('bootstrap norm','bootstrap exp','classic norm','classic exp'),xlab=metrics[n])
  legend("topright", legend=paste('N=',Ns), fill=1:5,cex = 0.5)
}