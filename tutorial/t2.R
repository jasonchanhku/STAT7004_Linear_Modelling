library(magrittr)
Q1
# Regression fitting
x <- c(34,47,25,56,29,62,41,24)
y <- c(3.3,2.5,4.7,0.25,3.0,1.03,1.75,8)
plot(x, y,xlab='time spend',ylab='score')
# Sxx
Sxx<-var(x) * (length(x) - 1); Sxx
Sxx<-sum(x^2)-length(x)*mean(x)^2; Sxx
# Syy
Syy<-var(y) * (length(y) - 1);Syy
Syy<-sum(y^2)-length(y)*mean(y)^2;Syy
# Sxy
Sxy<-cov(x, y) * (length(x) - 1);Sxy
Sxy<-sum(x*y)-length(x)*mean(x)*mean(y);Sxy

#B0
B0<-mean(y)-(Sxy/Sxx)*mean(x);B0
B1<-Sxy/Sxx;B1

#Package 
fit <- lm(y ~ x)  # fit the regression model
fit
abline(a = fit$coefficient[1], b = fit$coefficient[2])
# Draw a line onto the graph.a here means the intercept, b means the slope
# fit$coefficient[1]:
#'fit' here is a list, containing many variables. 
# The dollar sign ($) is to extract the variable in 'fit'. coefficient[1] and
# coefficient[2] are the intercept the slope of the fitted line respectively.
fitted(fit)  #Display Y hat, that is the expectation of Y given the value of X under this model

#SSE
SSE = residuals(fit)**2 %>% sum;SSE
MSE = SSE/(length(x)-2);MSE
summary(fit)  #Display the summary of the fitted model
vcov(fit)  #Display the variance-covariance matrix of the estimates 
anova(fit)  #Display the anova table of the fitted model 




