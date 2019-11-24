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
  
  atx0 <- matrix(c(0, difference, 0), nrow = 1, ncol = 3)
  
  l_hat <- atx0 %*% beta 
  
  se_l <- sqrt(mse * (atx0 %*% xtx_inv %*% t(atx0)))
  
  lower <- l_hat - se_l * qt(0.05/2, n-p-1, lower.tail = FALSE) 
  upper <- l_hat + se_l * qt(a/2, n-p-1, lower.tail = FALSE) 
  
  print(glue("Prediction CI is [{lower}, {upper}]"))
  
} ; f_pred_interval()

#(g)
g_pred_interval<- function(a=0.05){
  
  x0 <- matrix(c(1, 65, 85), ncol = 1)
  
  l_hat <- t(x0) %*% beta
  se_l <- sqrt(mse * (t(x0) %*% xtx_inv %*% x0 + 1))
  
  print(glue("point estimation is {l_hat}"))
  print(glue("se is {se_l}"))
  
  lower <- l_hat - qt(p = a/2 ,df = n-p-1,lower.tail = FALSE)*se_l
  upper <- l_hat + qt(p = a/2 ,df = n-p-1,lower.tail = FALSE)*se_l
  
  print(glue("Prediction CI is [{lower}, {upper}]"))
  
} ; g_pred_interval()


#(h) general linear hypothesis F-test 

test_1 <- function(){

  C <- matrix(c(0, 1, 1), ncol = 3)
  l0 <- matrix(c(-1.5), nrow = 1)
  r <- nrow(C)
  
  diff_term <- C %*% beta - l0
  
  numerator <- (diff_term %*% (C %*% xtx_inv %*% t(C))^-1 %*% t(diff_term))/ r
  
  F_stat <- numerator / mse
  
  crit_val <- qf(0.05, df1 = r, df2 = n-p-1, lower.tail = FALSE)
  
  print(glue("F-Statistic is {F_stat} and Critical Value is {crit_val}"))
  print(glue("Since F-Statistic is less than the critical value, do not reject H0"))
} ; test_1()

# (i)

test_2 <- function(){
  
  C <- matrix(c(0, 0, 2, 1, 1, 1), nrow = 2, ncol = 3)
  l0 <- matrix(c(-2, -1.2), ncol = 1)
  r <- nrow(C)
  
  diff_term <- C %*% beta - l0
  
  numerator <- (t(diff_term) %*% inv(C %*% xtx_inv %*% t(C)) %*% (diff_term))/ r
  
  F_stat <- numerator / mse
  
  crit_val <- qf(0.05, df1 = r, df2 = n-p-1, lower.tail = FALSE)
  
  print(glue("F-Statistic is {F_stat} and Critical Value is {crit_val}"))
  print(glue("Since F-Statistic is less than the critical value, do not reject H0"))

} ; test_2()


