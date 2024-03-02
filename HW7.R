#P1
#(a)
data = read.csv("C:/Users/DELL/Desktop/DSC 241/Placekick.csv")
m1 = glm(good~distance,family = binomial(link = logit),data=data)
m1$coefficients
min(data$distance)
plot(data$distance,data$good)
curve(expr=predict(m1,newdata=data.frame(distance=x),type='response'),add=TRUE,col='red')

#(b)
m2 = glm(good~1,family = binomial(link = logit),data=data)
m0 = glm(good~.,family = binomial(link = logit),data=data)
m3 = step(m2,scope=formula(m0),direction='forward')
summary(m3)

#P2 
bootGLM <-function(x,y,B=1000){
  N = length(y)
  d = dim(x)[2]
  coef = matrix(,nrow=B,ncol=d+1)
  for (i in 1:B){
    ids = sample(1:N,N,replace=TRUE)
    m = glm(y[ids]~.,family = binomial(link = logit),data=as.data.frame(x[ids,,drop=FALSE]))
    coef[i,] = m$coefficients
  }
  return (apply(coef,2,sd))
}
x = subset(data,select=names(m3$coefficients[-1]))
y = data$good
sd = bootGLM(x,y)
sd = matrix(sd,ncol=5)
colnames(sd)=names(m3$coefficients)
sd
summary(m3)