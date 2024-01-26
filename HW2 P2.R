N = 1000
ns = c(50,100,200)
ks = c(2,5,10,20)
  
par(mfrow = c(3, 2))
for (n in ns){
  results <- matrix(NA, nrow = N, ncol = 2)
  for (i in 1:N){
    x = runif(n)
    y = 3 + 0.5*x + 0.5*rnorm(n,0,1)
    model = lm(y~x)
    results[i,]=model$coefficients
  }
  hist(results[, 1], main = sprintf("Intercept at n =%s", n), xlab = "Intercept")
  hist(results[, 2], main = sprintf("Slope at n =%s", n), xlab = "Slope")
}


for (k in ks){
  for (n in ns){
    results <- matrix(NA, nrow = N, ncol = 2)
    for (i in 1:N){
      x = runif(n)
      y = 3 + 0.5*x + 0.5 * rt(n, k)
      model = lm(y~x)
      results[i,]=coef(model)
    }
    hist(results[, 1], main = sprintf("Intercept at n =%s, k=%s", n, k), xlab = "Intercept")
    hist(results[, 2], main = sprintf("Slope at n =%s, k=%s", n, k), xlab = "Slope")
  }
}

