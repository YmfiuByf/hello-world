library(car)
library(glmnet)
library(quantreg)
library(MASS)


# Problem 1
train = read.csv("C:\\Users\\DELL\\Documents\\datasets\\train_data.csv")
test = read.csv("C:\\Users\\DELL\\Documents\\datasets\\test_data.csv")
val = read.csv("C:\\Users\\DELL\\Documents\\datasets\\validation_data.csv")

train[apply(train[,c(1,2,4)]==0,1,any),]

train_new = train[apply(train[,c(1,2,4)]!=0,1,all),]
test_new = test[apply(test[,c(1,2,4)]!=0,1,all),]
val_new = val[apply(val[,c(1,2,4)]!=0,1,all),]


dim(train_new);dim(train)
#train_new = train[apply]

pairs(fare~.,data=train_new)

# Problem 2
m1 = lm(fare~trip_duration+distance_traveled+num_of_passengers+surge_applied,data=train_new)
plot(m1$fitted.values,train_new[,4],ylab='fare',xlab='fare_predicted')

plot(m1$fitted.values,m1$residuals)
abline(h=0,col = 'darkred')
mean(m1$residuals)

plot(m1,which=1)

durbinWatsonTest(m1)

qqPlot(m1$residuals)

summary(m1)

vif(m1)

rsq <- function(m, validation) {
  y_pred = predict(m,newdata=validation)
  y = validation$fare
  return (1 - sum((y - y_pred)^2) / sum((y - mean(y_pred))^2))
}
rsq(m1,val_new)


plot(hatvalues(m1), type = "h")
p = 4
abline(h = 2 * (p + 1) / n, lty = 2,col = 'darkred')

confint(m1,level=0.99)


bootLS <- function(train,conf=0.99,B=1000){
  # fit a linear regression model on given samples
  m = lm(fare~trip_duration+distance_traveled+num_of_passengers+surge_applied,data=train)
  beta0 = m$coefficients[1]
  beta1 = m$coefficients[3]
  sebeta0 = summary(m)$coefficients[,2][1]
  sebeta1 = summary(m)$coefficients[,2][3]
  N = dim(train)[1]
  beta0_b = rep(NA,B)
  beta1_b = rep(NA,B)
  t0_b = rep(NA,B)
  t1_b = rep(NA,B)
  for (i in 1:B){
    # sample from empirical distribution
    ids = sample(1:N,N,replace=TRUE)
    train_boot = train[ids,]
    # fit simple linear regression model
    mb = lm(fare~trip_duration+distance_traveled+num_of_passengers+surge_applied,data=train_boot)
    beta0_b[i] = mb$coefficients[1]
    beta1_b[i] = mb$coefficients[3]
    sebeta0_b = summary(mb)$coefficients[,2][1]
    sebeta1_b = summary(mb)$coefficients[,2][3]
    t0_b[i]=(beta0_b[i]-beta0)/(sebeta0_b)
    t1_b[i]=(beta1_b[i]-beta1)/(sebeta1_b)
  }
  boot_int = t(matrix(c(beta0+quantile(t0_b,c((1-conf)/2,(1+conf)/2))*sebeta0,beta1+quantile(t1_b,c((1-conf)/2,(1+conf)/2))*sebeta1),ncol = 2))
  colnames(boot_int) = paste(c((1-conf)/2*100,(1+conf)/2*100),'%')
  rownames(boot_int) = paste(c('intercept','distance_traveled'))
  return(boot_int)
}

#bootLS(train_new[,c(1,2,3,8)],train_new[,4],conf=0.99,B=1000)
bootLS(train_new,conf=0.99,B=1000)
#summary(m1)$coefficients[,1]
#1,2,3,8  4

# Problem 4
max_r = -Inf 
p_id = c(1,2,3,8)
for (i in 1:length(p_id)){
  comb = combn(p_id,i)
  for (j in 1:dim(comb)[2]){
    m = lm(fare~.,data = train_new[,c(comb[,j],4)])
    r = rsq(m,val_new)
    print(r)
    if (r>max_r){
      max_r = r
      m2 = m
    }
  }
}
m2;max_r
r = max_r


x = model.matrix(fare ~ trip_duration + distance_traveled + num_of_passengers + surge_applied, data = train_new)[,-1]
y = train_new$fare
cv_lasso = cv.glmnet(x, y, alpha = 1, nfolds = 10)
best_lambda = cv_lasso$lambda.min
lasso = glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso)



m2.rq = rq(fare ~ trip_duration + distance_traveled + surge_applied, data = train_new)
r.rq = rsq(m2.rq,val_new)
m2.huber = rlm(fare ~ trip_duration + distance_traveled + surge_applied, psi = psi.huber,data = train_new)
r.huber = rsq(m2.huber,val_new)
m2.lms = lmsreg(fare ~ trip_duration + distance_traveled + surge_applied,data = train_new)
r.lms = rsq(m2.lms,val_new)
m2.lts = ltsreg(fare ~ trip_duration + distance_traveled + surge_applied,data = train_new)
r.lts = rsq(m2.lts,val_new)
c(r.rq,r.huber,r.lms,r.lts)
which.max(c(r.rq,r.huber,r.lms,r.lts))
max(c(r.rq,r.huber,r.lms,r.lts))

m3 = m2.rq


cook_dis = cooks.distance(m2)
out_id5 = order(cook_dis,decreasing=TRUE)[1:5]
out_id10 = order(cook_dis,decreasing=TRUE)[1:10]
train_out_5 = train_new[-out_id5,]
train_out_10 = train_new[-out_id10,]
dim(train_out_10)

m2_5 = lm(fare ~ trip_duration + distance_traveled + surge_applied,data = train_out_5)
m2_10 = lm(fare ~ trip_duration + distance_traveled + surge_applied,data = train_out_10)

rsq(m2_5,val_new);rsq(m2_10,val_new)

c(r.rq,r.huber,r.lms,r.lts)


m4 = lm(fare~poly(trip_duration,2)+poly(distance_traveled,2)+surge_applied,data=train_new)

summary(m4)

plot(m4$fitted.values,train_new$fare)
rsq(m4,val_new)

m5 = rlm(fare~poly(trip_duration,2)+poly(distance_traveled,2)+surge_applied, psi = psi.huber,data = train_new)
rsq(m5,val_new)

r1=rsq(m1,val_new);r2=rsq(m2,val_new);r3=rsq(m3,val_new);r4=rsq(m4,val_new);r5=rsq(m5,val_new)
c(r1,r2,r3,r4,r5)
rt5 = rsq(m5,test_new)
rt5

rt1=rsq(m1,test_new);rt2=rsq(m2,test_new);rt3=rsq(m3,test_new);rt4=rsq(m4,test_new);rt5=rsq(m5,test_new)
c(rt1,rt2,rt3,rt4,rt5)

hist(train_new[(train_new$surge_applied==0),]$distance_traveled,xlab='surge_applied=0',main='distance_traveled')
hist(train_new[(train_new$surge_applied==1),]$distance_traveled,xlab='surge_applied=1',main='distance_traveled')


#hist(train_new[(train_new$surge_applied==0),]$miscellaneous_fees,xlab='surge_applied=0',main='miscellaneous fees',xlim=c(-5,190),breaks=seq(-5,190,5))
#hist(train_new[(train_new$surge_applied==1),]$miscellaneous_fees,xlab='surge_applied=1',main='miscellaneous fees',xlim=c(-5,190),breaks=seq(-5,190,5))

m = glm(surge_applied~fare+distance_traveled+trip_duration+miscellaneous_fees,family = binomial(link = logit),data=train_new)
summary(m)

plot(train_new$distance_traveled,train_new$surge_applied)
points(train_new$distance_traveled,m$fitted.values,col='yellow',xlab='distance_traveled')

plot(train_new$miscellaneous_fees,train_new$surge_applied)
points(train_new$miscellaneous_fees,m$fitted.values,col='yellow',xlab='miscellaneous_fees')

m0 = glm(surge_applied~1,family = binomial(link = logit),data=train_new)
m_total = glm(surge_applied~fare+distance_traveled+trip_duration+miscellaneous_fees,family = binomial(link = logit),data=train_new)
m_final = step(m0,scope=formula(m_total),direction='forward')
summary(m_final)


test_pred = predict(m_final,newdata=test_new)
test_accuracy = sum(ifelse(test_pred>=0.5,1,0)==test_new$surge_applied)/dim(test_new)[1]
#test_accuracy
