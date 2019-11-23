library(magrittr)
library(matlib)
library(glue)

r2 <- 0.2851
sst <- 3004.7
# xtx
xtx <- matrix(c(30, 1070, 1639, 1070, 40122, 58110, 1639, 58110, 90797), nrow = 3, ncol = 3)

n <- xtx[1][1] ; n
p <- dim(xtx)[1] -1 ; p 

#xty
xty <- matrix(c(1413, 49160, 77117), nrow = 3, ncol = 1)

# xtx inverse
xtx_inv <- inv(xtx) ; xtx_inv

# sample mean of y
y_bar <- xty[1]/n

# Beta
beta <- xtx_inv %*% xty; beta

# R^2 = 1 - SSE/SST
# SSE = (1 - R^2 ) * SST
sse <- (1-r2)*sst ; sse

# MSE
mse <- sse / (n-p-1) ; mse

# standard error of beta
se_beta <- sqrt(diag(mse * xtx_inv))

beta_ci <- function(a=0.05) {
  # pass in which beta and returns a C.I
  beta_vec <- as.vector(beta)
  
  lower <- beta_vec - se_beta*qt(p = a/2 ,df = n-p-1,lower.tail = FALSE)
  upper <- beta_vec + se_beta*qt(p = a/2 ,df = n-p-1,lower.tail = FALSE)
  
  print(lower)
  print(upper)
  
  for (i in c(1,2,3)){
    print(glue("CI for Beta {i-1} is [{lower[i]} , {upper[i]}]"))
  }
  
  
} ; beta_ci()

# mean of x for x1


# sxx Beta 1
sxx_b1 <- xtx[2, 2] - n*(xtx[1,2]/n)^2

# (f)
# preidction interval for difference in mean Y values of patients 
# with 5 years difference, other explanatory variables are all equal
# assume that it can be simplified into one variable.....
f_pred_interval <- function(a=0.05, difference=5){
  
  l_hat <- beta[2]*difference
  
  # using back the tutorial scratch notes for individual difference
  # modified it for the mean
  
  lower <- l_hat - qt(p = a/2 ,df = n-2,lower.tail = FALSE)*sqrt(mse * (difference^2/sxx_b1))
  upper <- l_hat + qt(p = a/2 ,df = n-2,lower.tail = FALSE)*sqrt(mse * (difference^2/sxx_b1))
  
  print(glue("CI for prediction is [{lower}, {upper}]"))
  
} ; f_pred_interval()

#(g)
g_pred_interval<- function(a=0.05){
  
  x0 <- matrix(c(1, 65, 85), ncol = 1)
  
  l_hat <- t(x0) %*% beta
  se_l <- sqrt(mse * (t(x0) %*% xtx_inv %*% x0 + 1))
  
  lower <- l_hat - qt(p = a/2 ,df = n-p-1,lower.tail = FALSE)*se_l
  upper <- l_hat + qt(p = a/2 ,df = n-p-1,lower.tail = FALSE)*se_l
  
  print(glue("Prediction CI is [{lower}, {upper}]"))
  
} ; g_pred_interval()







