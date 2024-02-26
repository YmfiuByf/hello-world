library(leaps)
library(assertr)
dat = Boston[, -c(4, 9)]

cv.lm <- function(x,y,k){
  set.seed(0)
  n = length(y)
  shuffled = sample(n)
  x = x[shuffled,,drop=FALSE]
  y = y[shuffled]
  folds = cut(seq(1,n),breaks=k,labels=FALSE)
  err = 0
  for (i in 1:k){
    m = lm(y[folds!=i]~.,data=as.data.frame(x[folds!=i,,drop=FALSE]))
    pred = predict(m,newdata=as.data.frame(x[folds==i,,drop=FALSE]))
    res = y[folds==i] - pred
    err = err + sqrt(mean(res^2))
  }
  err = err/k
  return (err)
}


SequentialSelection <- function(x, y, method){
  n = dim(x)[2]
  best_c = ifelse(method %in% c('AdjR2'), -Inf, Inf)
  for (i in 1:n){
    cols = 1:i
    x_used = x[,cols,drop=FALSE]
    x_used = as.data.frame(x_used)
    m = lm(y~.,data=x_used)
    if (method=='AdjR2'){
      c = summary(m)$adj.r.squared
      if (c>best_c){
        best_c = c
        best_m = m
        best_i = i
      }
      #cat('\n',c)
    }
    else if (method=='AIC'){
      c = AIC(m)
      if (c<best_c){
        best_c = c
        best_m = m
        best_i = i
      }
      #cat('\n',c)
    }
    else if (method=='CV5'){
      c = cv.lm(x_used,y,5)
      if (c<best_c){
        best_c = c
        best_m = m
        best_i = i
      }
      #cat('\n',c)
    }
  }
  return(list(model=best_m,criterion=best_c,degree=best_i))
}

# P2
# (a,c)
T = 100
N = 200
methods = c('AdjR2','AIC','CV5')
results = matrix(NA,T,length(methods))
colnames(results) = methods
for (t in 1:T){
  x = runif(N,0,2*pi)
  y = sin(3*x) + x + rnorm(N,0,1)
  for (method in methods){
    result = SequentialSelection(poly(x,degree=20),y,method)
    results[t,method] = result$degree
  }
}

# (b)
par(mfrow = c(3, 3))
for (t in 1:3){
  x = runif(N,0,2*pi)
  y = sin(3*x) + x + rnorm(N,0,1)
  for (method in methods){
    result = SequentialSelection(poly(x,degree=20),y,method)
    x_new = seq(0, 2*pi, length.out = 100)
    y_pred = predict(result$model, newdata=poly(x_new, degree = result$degree))
    plot(x, y,main=method)
    lines(x_new, y_pred, col = "red")
  }
}

#SequentialSelection(poly(x,degree=20),y,'CV5')