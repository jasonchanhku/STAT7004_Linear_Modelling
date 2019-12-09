library(magrittr)
library(matlib)
library(glue)



# Question 1
# (i)
n <- 5
X <- matrix(c(1,1,1,1,1, 71, 64, 43, 67, 55), nrow = 5, ncol = 2)
x <- c(71, 64, 43, 67, 55)
y <- c(82, 88, 100, 73, 87)

Sxx <- sum(x^2) - n * mean(x)^2
Syy <- sum(y^2) - n * mean(y)^2
Sxy <- sum(x*y) - n * mean(x)*mean(y)

beta_1 <- Sxy / Sxx
beta_0 <- mean(y) - beta_1*mean(x)

# (ii)
SSE <- Syy - (Sxy^2)/Sxx
MSE <- SSE/(n-2)

var_beta_0 <- sqrt(MSE*inv(t(X)%*%X)[1,1])

scale <- var_beta_0*qt(p = 0.1/2 ,df = n-2,lower.tail = FALSE)
li <- beta_0 - scale;li
ui <- beta_0 + scale;ui

#(iii)
SSR <- Syy - SSE
MSR <- SSR / 1
F_score <- MSR/MSE
F_score > qf(0.1, df1 = 1, df2 = n-1-1, lower.tail = FALSE)

#(iv)
est <- beta_0+beta_1*45
est_scale <- qt(p = 0.1/2 ,df = n-2,
                lower.tail = FALSE)*sqrt(MSE*(1/n + (45-mean(x))^2/Sxx +1))
li <- est - est_scale; li
ui <- est + est_scale; ui

#(v)
a <- matrix(c(0.5, 0.5), nrow = 1, ncol = 2)
x0 <- matrix(c(1, 1, 60, 66), nrow = 2, ncol = 2)
beta_vec <- matrix(c(beta_0, beta_1), nrow = 2, ncol = 1)

est <- a%*%x0%*%beta_vec;est

scale <- sqrt(MSE * (a%*%x0%*%inv(t(X)%*%X)%*%t(x0)%*%t(a) + a%*%t(a)))*qt(p = 0.1/2 ,df = n-2,
                                                                           lower.tail = FALSE)

li <- est - scale; li
ui <- est + scale; ui



# Question 2
SSE <- 23.73
n <- 28
p <- 2
xtx <- matrix(c(28, 0.56, -9.07, 0.56, 18.112, 0.187, -9.070, 0.187, 36.345), nrow = 3, ncol = 3)
xtx_inv <- inv(xtx)
xty <- matrix(c(26.930, -20.501, 6.944), nrow = 3, ncol = 1)

#(a)

beta <- xtx_inv%*%xty; beta
MSE <- SSE / (n-p-1); MSE

# (b)
Syy <- 81.847 - n*(26.930/n)^2; Syy
SSR <- Syy - SSE; SSR

MSR <- SSR/p
F_Score <- MSR / MSE ; F_Score
F_Score > qf(0.05, df1 = p, df2 = n-p-1, lower.tail = FALSE)

# (c)
se_b1 <- sqrt(MSE * xtx_inv[2,2])
T_val <- abs((beta[2]-(-1))/se_b1); T_val
T_val > qt(p = 0.05/2 ,df = n-p-1,lower.tail = FALSE)

# (d)

C <- matrix(c(0,0,1,1,0,2), nrow = 2, ncol = 3) ; C
l0 <- matrix(c(-1, 1), nrow = 2, ncol = 1) ; l0
r <- 2; r

CB_l0 <- C%*%beta - l0; CB_l0
# USE INV FUNCTION, NOT ^-1!
mid <- inv(C%*% xtx_inv %*%t(C)) ; mid

numerator <- t(CB_l0) %*% mid %*% CB_l0 / r ; numerator
numerator / MSE > qf(0.05, r, n-p-1, lower.tail = FALSE)

#(f)
# only add one regressor
qf(0.05, 1, n-2, lower.tail = FALSE)

xtx_inv_1 <- matrix(c(0.03574, -0.00110, -0.00110, 0.05525),2, 2); xtx_inv_1
xty_1 <- matrix(c(26.930, -20.501),2,1); xty_1
beta_1 <- xtx_inv_1 %*% xty_1; beta_1

Sxx_1 <- 1 / xtx_inv_1[2,2]
Sxy_1 <- beta_1[2] * Sxx_1
SSE_1 <- Syy - (Sxy_1)^2/Sxx_1; SSE_1
F_1 <- (Syy - SSE_1) / (SSE_1/(n-2)) ; F_1

xtx_inv_1 <- matrix(c(0.03886, 0.00970, 0.00970, 0.02993),2, 2); xtx_inv_1
xty_1 <- matrix(c(26.930, 6.944),2,1); xty_1
beta_1 <- xtx_inv_1 %*% xty_1; beta_1

Sxx_1 <- 1 / xtx_inv_1[2,2]
Sxy_1 <- beta_1[2] * Sxx_1
SSE_1 <- Syy - (Sxy_1)^2/Sxx_1; SSE_1
F_1 <- (Syy - SSE_1) / (SSE_1/(n-2)) ; F_1
