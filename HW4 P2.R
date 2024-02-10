library(quantreg)
# Fit a linear model with a cubic polynomial term
m.lm = lm(medv ~ poly(lstat, 3), data = Boston)
# Fit a quantile regression with a cubic polynomial term
m.l1 = rq(medv ~ poly(lstat, 3), data = Boston)
# Fit a robust linear model using Huber M-estimation with a cubic polynomial term
m.huber = rlm(medv ~ poly(lstat, 3), psi = psi.huber, data = Boston)
# Fit a least median of squares regression model with a cubic polynomial term
m.lms = lmsreg(medv ~ poly(lstat, 3), data = Boston)
# Fit a least trimmed squares regression model with a cubic polynomial term
m.lts = ltsreg(medv ~ poly(lstat, 3), data = Boston)
# Scatter plot of the data points
plot(Boston$lstat, Boston$medv, pch = 16)
# Generate a sequence of lstat values for prediction
lstat_seq <- seq(min(Boston$lstat), max(Boston$lstat), length.out = 100)
# Predict the medv values using the fitted models
medv_pred_lm <- predict(m.lm, newdata = data.frame(lstat = lstat_seq))
medv_pred_l1 <- predict(m.l1, newdata = data.frame(lstat = lstat_seq))
medv_pred_huber <- predict(m.huber, newdata = data.frame(lstat = lstat_seq))
medv_pred_lms <- predict(m.lms, newdata = data.frame(lstat = lstat_seq))
medv_pred_lts <- predict(m.lts, newdata = data.frame(lstat = lstat_seq))
# Add regression lines to the plot with different colors
lines(lstat_seq, medv_pred_lm, col = "blue", lwd = 2, lty = 1) # Linear Model (blue, solid)
lines(lstat_seq, medv_pred_l1, col = "purple", lwd = 2, lty = 2) # Quantile Regression (purple, dashed)
lines(lstat_seq, medv_pred_huber, col = "green", lwd = 2, lty = 3) # Huber M-estimation (green, dashed)
lines(lstat_seq, medv_pred_lms, col = "black", lwd = 2, lty = 4) # Least Median of Squares (black, dashed)
lines(lstat_seq, medv_pred_lts, col = "orange", lwd = 2, lty = 5) # Least Trimmed Squares (orange, dashed)
legend("topright", legend = c("Linear Model","QUantile Regression","Huber M-estimation","Leat Median of Squares","Least Trimmed Squares"), 
       col = c("blue","purple","green","black","orange"), lwd = 2, cex=1)