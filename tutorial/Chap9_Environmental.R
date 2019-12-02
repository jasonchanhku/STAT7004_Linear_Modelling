#import data
dat1=matrix(
  c(3.45, 190.00, 67.00, 7.40,
    3.30, 118.00, 72.00, 8.00,
    2.29, 149.00, 74.00, 12.60,
    2.62, 313.00, 62.00, 11.50,
    2.84, 299.00, 65.00, 8.60,
    2.67, 99.00,  59.00, 13.80,
    2.00, 19.00,  61.00, 20.10,
    2.52, 256.00, 69.00, 9.70,
    2.22, 290.00, 66.00, 9.20,
    2.41, 274.00, 68.00, 10.90,
    2.62, 65.00,  58.00, 13.20,
    2.41, 334.00, 64.00, 11.50,
    3.24, 307.00, 66.00, 12.00,
    1.82, 78.00,  57.00, 18.40,
    3.11, 322.00, 68.00, 11.50,
    2.22, 44.00,  62.00, 9.70,
    1.00, 8.00,   59.00, 9.70,
    2.22, 320.00, 73.00, 16.60,
    1.59, 25.00,  61.00, 9.70,
    3.17, 92.00,  61.00, 12.00,
    2.84, 13.00,  67.00, 12.00,
    3.56, 252.00, 81.00, 14.90,
    4.86, 223.00, 79.00, 5.70,
    3.33, 279.00, 76.00, 7.40,
    3.07, 127.00, 82.00, 9.70,
    4.14, 291.00, 90.00, 13.80,
    3.39, 323.00, 87.00, 11.50,
    2.84, 148.00, 82.00, 8.00,
    2.76, 191.00, 77.00, 14.90,
    3.33, 284.00, 72.00, 20.70),
  nrow=30, ncol=4, byrow = TRUE)
dat1=as.data.frame(dat1)
colnames(dat1)=c('ozone', 'radiation', 'temp', 'wind')
time=1:30
dat1=cbind(dat1,time)

###################################################
fit=lm(ozone~radiation+temp+wind, dat1)

error=resid(fit)
standarderror=rstandard(fit)

predictvalue=predict(fit, newdata = dat1)
plot(predictvalue,standarderror,xlab="Predicted Value", ylab="Standardized Residual")
abline(0,0)

###################################################
plot(dat1$radiation,standarderror,xlab="radiation", ylab="Standardized Residual")
abline(0,0)

###################################################
plot(dat1$temp,standarderror,xlab="temperature", ylab="Standardized Residual")
abline(0,0)

###################################################
plot(dat1$wind,standarderror,xlab="wind", ylab="Standardized Residual")
abline(0,0)

###################################################
plot(dat1$time,standarderror,xlab="time", ylab="Standardized Residual")
abline(0,0)

###################################################
quantile=qqnorm(resid(fit))$x
plot(quantile,resid(fit),xlab="Normal Quantiles", ylab="Residual")
abline(lm(resid(fit)~quantile))

###################################################
shapiro.test(standarderror)

###################################################
library(MASS)
boxcox(dat1$ozone~dat1$radiation+dat1$temp+dat1$wind,seq(-2, 3, by = 0.5))

###################################################
X=cbind(1,dat1$radiation,dat1$temp,dat1$wind)
leverage=diag(X %*% solve(t(X) %*% X) %*% t(X))
plot(dat1$time,leverage,xlab="Observation Number", ylab="Leverage")
abline(2*4/30,0,lty=2)

###################################################
studentdeletederror=rstudent(fit)
plot(dat1$time,studentdeletederror,xlab="Observation Number", ylab="Studentized Residual",ylim=c(-3, 3))
abline(0,0,lty=2)
abline(2,0,lty=2)
abline(-2,0,lty=2)

###################################################
D=cooks.distance(fit)
plot(dat1$time,D,xlab="Observation Number", ylab="Cook's D Influence Statistic")
abline(4/30,0,lty=2)
