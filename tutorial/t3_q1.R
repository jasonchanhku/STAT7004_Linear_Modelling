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

#SSE
SSE = residuals(fit)**2 %>% sum;SSE
MSE = SSE/(length(x)-2);MSE

## Q1 -> mean estimate
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
Q1(num = 50)

## Q2 -> individual estimate
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
Q2(num = 45)

## Q3 -> multiple estimate
Q3 <- function(num){
  n <- length(num)
  num = mean(num)
  lci <- beta_0 + beta_1 * num - (SSE/6)**0.5*(1/length(x)
                                               + (num - meanx)**2/Sxx
                                               + 1/n)**0.5*qt(p = 0.025,df = 6,lower.tail = FALSE)
  uci <- beta_0 + beta_1 * num + (SSE/6)**0.5*(1/length(x)
                                               + (num - meanx)**2/Sxx
                                               + 1/n)**0.5*qt(p = 0.025,df = 6,lower.tail = FALSE)
  return(paste0("95%CIs:(",round(lci,4),",",round(uci,4),")", sep = ""))
}
Q3(num = c(11,23,5,9,50,80))
