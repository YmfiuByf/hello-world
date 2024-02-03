ps = seq(1,20)
ns = c(30,50,100,200,500,1000)
par(mfrow = c(2, 1))
# (a)
condition_num_canonical <- function(p,n){
  x = seq(1,n)/(n+1)
  X = poly(x,p,raw=TRUE)
  X = cbind(rep(1,n),X)
  svd_X = svd(X)
  k_X = max(svd_X$d)/min(svd_X$d)
  return (k_X)
}

ks_canonical = matrix(NA,length(ps),length(ns))
for (i in 1:length(ps)){
  for (j in 1:length(ns)){
    ks_canonical[i,j] = condition_num_canonical(ps[i],ns[j])
  }
}

matplot(ps, ks_canonical, type = "l", lty = 1, col = 1:length(ns),
        xlab = "Degree of Polynomial (p)", ylab = "Condition Number",
        main = "Condition Number of Design Matrix",
        legend.text = ns, legend.title = "n", col.lab = "blue")
legend("topright", legend = ns, col = 1:length(ns), lty = 1, title = "n",cex=0.5)

# (b)
condition_num_orthogonal <- function(p,n){
  x = seq(1,n)/(n+1)
  X = poly(x,p)
  X = cbind(rep(1,n)/sqrt(n),X)
  svd_X = svd(X)
  k_X = max(svd_X$d)/min(svd_X$d)
  return (k_X)
}

ks_orthogonal = matrix(NA,length(ps),length(ns))
for (i in 1:length(ps)){
  for (j in 1:length(ns)){
    ks_orthogonal[i,j] = condition_num_orthogonal(ps[i],ns[j])
    cat(ps[i],ns[j])
  }
}

matplot(ps, ks_orthogonal, type = "l", lty = 1, col = 1:length(ns),
        xlab = "Degree of Polynomial (p)", ylab = "Condition Number",
        main = "Condition Number of Orthogonal Polynomial Matrix",
        legend.text = ns, legend.title = "n", col.lab = "blue")
legend("topright", legend = ns, col = 1:length(ns), lty = 1, title = "n",cex=0.5)
