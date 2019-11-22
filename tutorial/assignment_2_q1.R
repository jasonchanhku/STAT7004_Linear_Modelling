library(magrittr)
library(matlib)
library(glue)

n <- 30
p <- 2
r2 <- 0.2851
sst <- 3004.7
# xtx
xtx <- matrix(c(30, 1070, 1639, 1070, 40122, 58110, 1639, 58110, 90797), nrow = 3, ncol = 3)

#xty
xty <- matrix(c(1413, 49160, 77117), nrow = 3, ncol = 1)

# xtx inverse
xtx_inv <- inv(xtx) ; xtx_inv

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
  
  lower <- beta_vec - se_beta*qt(p = a/2 ,df = n-p-2,lower.tail = FALSE)
  upper <- beta_vec + se_beta*qt(p = a/2 ,df = n-p-2,lower.tail = FALSE)
  
  print(lower)
  print(upper)
  
  for (i in c(1,2,3)){
    print(glue("CI for Beta {i-1} is [{lower[i]} , {upper[i]}]"))
  }
  
  
} ; beta_ci()










