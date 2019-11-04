# Assignment 1 question 4

library(magrittr)
#Q1
# Regression fitting
x <- c(0.013, 0.021, 0.032, 0.042, 0.055, 0.062, 0.073)
y <- c(-0.098, -0.087, -0.075, -0.060, -0.043, -0.024, -0.002)
plot(x, y,xlab='NF',ylab='K')
# Sxx
Sxx<-var(x) * (length(x) - 1); Sxx
Sxx<-sum(x^2)-length(x)*mean(x)^2; Sxx
# Syy
Syy<-var(y) * (length(y) - 1);Syy
Syy<-sum(y^2)-length(y)*mean(y)^2;Syy
# Sxy
Sxy<-cov(x, y) * (length(x) - 1);Sxy
Sxy<-sum(x*y)-length(x)*mean(x)*mean(y);Sxy

# (b)
#B0
beta_0<-mean(y)-(Sxy/Sxx)*mean(x);beta_0
beta_1<-Sxy/Sxx;beta_1

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

y_hat <- beta_0 + beta_1*x

resi <- y - y_hat

#SSE
SSE = residuals(fit)**2 %>% sum;SSE
MSE = SSE/(length(x)-2);MSE

residuals(fit)

summary(fit)  #Display the summary of the fitted model
vcov(fit)  #Display the variance-covariance matrix of the estimates 
anova(fit)  #Display the anova table of the fitted model 

meanx <- mean(x)
Q1 <- function(num){
  lci <- beta_0 + beta_1 * num - (SSE/6)**0.5*(1/length(x)
                                               + (num - meanx)**2/Sxx
  )**0.5*qt(p = 0.025,df = 6,lower.tail = FALSE)
  uci <- beta_0 + beta_1 * num + (SSE/6)**0.5*(1/length(x)
                                               + (num - meanx)**2/Sxx
  )**0.5*qt(p = 0.025,df = 6,lower.tail = FALSE)
  return(paste0("95%CIs:(",round(lci,4),",",round(uci,4),")", sep = ""))
}
Q1(num = 0.025)


meanx <- mean(x)
Q2 <- function(num){
  lci <- beta_0 + beta_1 * num - (SSE/6)**0.5*(1/length(x)
                                               + (num - meanx)**2/Sxx
                                               + 1)**0.5*qt(p = 0.025,df = 6,lower.tail = FALSE)
  uci <- beta_0 + beta_1 * num + (SSE/6)**0.5*(1/length(x)
                                               + (num - meanx)**2/Sxx
                                               + 1)**0.5*qt(p = 0.025,df = 6,lower.tail = FALSE)
  return(paste0("95%CIs:(",round(lci,4),",",round(uci,4),")", sep = ""))
}
Q2(num = 0.055)


