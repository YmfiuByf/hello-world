# Load necessary libraries and data
library(MASS)
library(car)  # For influence.plot function

data(Boston)
B_sel = Boston[, sapply(Boston, is.numeric)]
model = lm(medv ~ ., data=B_sel)

# Standard assumptions
# 1. Mean zero
sprintf('redisuals mean=%s',mean(model$residuals))
plot(model,which=1)
abline(h=0,col = 'darkred')

# 2. Homoscedasticity
plot(model, which = 3)
durbinWatsonTest(model)

# 3. Normality
qqPlot(model$residuals)

# 4. Outliers in predictor
hatv = hatvalues(model)
plot(hatv, type = "h")
p = dim(B_sel)[2]-1; n = dim(B_sel)[1]
abline(h = 2*(p+1)/n, lty = 2,col = 'darkred')
hatv[which.max(hatv)]
B_sel[which.max(hatv),]
apply(B_sel,2,mean)

# 5. Outliers in response
rst = abs(rstudent(model))
plot(rst, type='h', col="blue", ylab="Externally Studentized Residuals (in absolute value)", main="Externally Studentized Residuals (in absolute value)")
abline(h = qt(.95, n-p-2), lty=2)
rst[which.max(rst)]
sprintf("ground truth=%s, prediction=%s",B_sel[which.max(rst),14],model$fitted.values[which.max(rst)])

# 6. Influential observations
# Cook's distances
plot(model, which=4, col="blue", lwd=2)
# DFBETAS
par(mfrow=c(4,4))
for (j in 1:p){
  plot(abs(dfbetas(model)[,j]), col=4, type='h', ylab='DFBETAS')
  abline(h = 2/sqrt(n), lty=2) # threshold for suspects
}
# DFFITS	
dffitsvalue = abs(dffits(model))
par(mfrow=c(1,1))
plot(dffitsvalue, typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt(p/n), lty=2) # threshold for suspects
dffitsvalue[which.max(dffitsvalue)]

influential = (hatv>2*(p+1)/n) & (rst>qt(.95, n-p-2))
B_sel[influential,]
model$fitted.values[influential]
hatv[influential]
rst[influential]

# checking for multicolinearity via pairwise correlations b/w predictors
round( cor(B_sel[,1:p]) , 2) # rounded to 2 digits

library(ellipse)
plotcorr(cor(B_sel[,1:p]))

# checking for multicolinearity via variance inflation factors (VIF)
plot(vif(model), type='h', col=4, lwd=3)
abline(h = 10, lty=2) # threshold for suspects 

# checking for multicolinearity via condition indices
C = cor(B_sel[, -2]) # correlation matrix for the predictors
L = eigen(C) # eigenvalues  
K = max(L$val)/L$val # condition indices
plot(K, type='h', col=4, lwd=3)
abline(h = 1000, lty=2) # threshold for suspects 