library(matlib)
library(glue)

# Q1

x <- c(17, 10, 8, 23, 27)
y <- c(2.9, 3.4, 3.2, 1.3, 1.7)
model <- lm(y ~ x)
summary(model)
anova(model)

#Q2

xtx <- matrix(c(30, -0.0263, -1.8869, -0.0263, 
                3.1226, 0.1278, -1.8869, 0.1278, 2.7279), nrow = 3, ncol = 3)
xtx_inv <- inv(xtx); xtx_inv
xty <- matrix(c(29.4908, 7.6345, -2.3394), nrow = 3, ncol = 1); xty

beta <- xtx_inv %*% xty ; beta
sse <- 28.2
n <- 30
p <- 2
mse <- sse / (n-p-1) ; mse

# (d)

C <- matrix(c(0,0,1,1,0,2), nrow = 2, ncol = 3) ; C
l0 <- matrix(c(1.2, 0), nrow = 2, ncol = 1) ; l0
r <- 2; r

CB_l0 <- C%*%beta - l0; CB_l0
# USE INV FUNCTION, NOT ^-1!
mid <- inv(C%*% xtx_inv %*%t(C)) ; mid

numerator <- t(CB_l0) %*% mid %*% CB_l0 / r ; numerator
numerator / mse > qf(0.05, r, n-p-1, lower.tail = FALSE)
