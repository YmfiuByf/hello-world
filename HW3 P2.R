# (a)
piecewiseConstant <- function(x,y,L,color="red",plot=TRUE,add=TRUE){
  xmax = max(x); xmin = min(x)
  x = (x-xmin)/(xmax-xmin)
  knots = seq(1,2^L-1)/(2^L)
  if (length(knots)==1){
    formula = as.formula(paste("y~I(x<knots[1])+I(x>=knots[1])"))
  }
  else{
    formula = as.formula(paste("y ~", paste(paste("I(x<knots[1])+" ,
    paste("(I(x>=knots[", 1:(length(knots)-1),"])&I(x<knots[",2:(length(knots)),"]))", sep = "", collapse = " + "),
    '+I(x>=knots[length(knots)])'))))
  }
  model <- lm(formula, data = data.frame(x, y))
  pts = seq(0,1,by = 1e-4)
  ori_pts = pts*(xmax-xmin)+xmin
  val = predict(model,data.frame(x=pts))
  x = x*(xmax-xmin)+xmin
  if (plot) {
    if (!add){
        plot(x, y, col = "purple", main = "Piecewise Constant Fit", xlab = "x", ylab = "y")
        legend("topright", legend = c("Data", paste("L=",L)), col = c("purple", color), lwd = 2, cex=0.5)
      }
    lines(ori_pts,val, col = color, lwd = 2)
  }
  return (model)
}

x = runif(100)
y = x^3+x^2+3*x+0.1*rnorm(10)
L = 3
model = piecewiseConstant(x,y,L,add=FALSE)

# (b)
load("datasets/04cars.rda")
Ls = c(2,3,4)
colors = c("blue","green","red")
plot(mtcars$hp,mtcars$mpg, col = "purple", main = "Piecewise Constant Fit", xlab = "x", ylab = "y")
for (i in 1:length(Ls)){
  piecewiseConstant(mtcars$hp,mtcars$mpg,Ls[i],colors[i])
}
legend("topright",legend = c("Data", paste("L=",Ls)), col = c("purple", colors),lwd = 2, cex=0.5)

