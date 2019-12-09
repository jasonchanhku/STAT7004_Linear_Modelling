library(matlib)
# 2016 past year
x <- c(3,5,8,10,14)
y <- c(3.4, 4.6, 5.8, 6.5, 8.2)

model <- lm(y ~ x)
summary(model)
anova(model)

# (b)
X <- matrix(c(1,1,1,1,1, 3,5,8,10,14), nrow = 5, ncol = 2) ; X
Y <- matrix(c(3.4, 4.6, 5.8, 6.5, 8.2), nrow = 5, ncol = 1); Y
xtx <- t(X) %*% X ; xtx
xtx_inv <- inv(xtx) ; xtx_inv
xty <- t(X) %*% Y ; xty
beta <- xtx_inv %*% xty ; beta

sse <- 13.4 - ((31.4)^2 / 74) ; sse
mse <- sse / 3
ui <- beta[1] - qt(0.025, 3, lower.tail = FALSE)*sqrt((mse)*(xtx_inv[1,1])) ; ui
li <- beta[1] + qt(0.025, 3, lower.tail = FALSE)*sqrt((mse)*(xtx_inv[2,2])) ; li
