# Tutorial 6 walkthrough

library(readxl)
library(matlib)
library(glue)
cereal <- read_excel("data/cereal.xlsx")
descriptions <- read_excel("data/cereal.xlsx", sheet = "Sheet2")


# Q1
#(a)

# adding a bias column
cereal$bias = 1
y <- as.matrix(cereal$rating, ncol=1)
x <- matrix(c(cereal$bias, cereal$sugars), ncol=2)

xtx <- t(x) %*% x ; xtx
xtx_inv <- inv(xtx) ; xtx_inv

xty <- t(x) %*% y ; xty
beta <- xtx_inv %*% xty

print(glue("ratings = {beta[1]} {beta[2]}sugars"))

# (b)
sxx <- xtx[2, 2] - xtx[1,1]*(xtx[1, 2]/xtx[1,1])^2
syy <- sum((y - mean(y))^2)
sxy <- xty[2] - xtx[1,1]*(xtx[1, 2]/xtx[1,1])*(mean(y))

# df of 75
sse <- syy - (sxy)^2/sxx
mse <- sse / (xtx[1, 1] - 2)
# df of 1
ssr <- syy - sse
msr <- ssr / 1

# F Statistic

f_stat <- msr/mse
pf(f_stat, 1, 75, lower.tail = FALSE)

# Small p-value shows that sugar is very significant

# R^2

print(glue("R-Squared is {ssr/syy}, which is the pct of variance explained by sugars"))

model <- lm(rating ~ sugars, cereal)

summary(model)
anova(model)
# Q2

model_2 <- lm(rating ~ sugars + fat, cereal)
summary(model_2)
anova(model_2)


# Q3
NBA <- read_excel("data/NBA.xlsx")

# View the scatterplot matrix
pairs(NBA[, 2:6], pch=19, lower.panel = NULL)

# (a)

NBA$BIAS <- 1 ; head(NBA)

y <- as.matrix(NBA$PPG, ncol=1)

x <- as.matrix(NBA[c(7,3,4,5,6)], ncol = 5)
xtx <- t(x) %*% x
m <- inv(t(x) %*% x)
n <- t(x) %*% y

# (b)
beta <- m %*% n

# (c) Find R^2

y_hat <- x %*% beta

num <- xtx[1, 1]

sse <- sum((y - y_hat)^2)
mse <- sse / (num -  dim(x)[2])
sst <- sum((y - mean(y))^2)
ssr <- sst - sse

r2 <- ssr/sst

# (d)
var_beta <- mse * m


# H0: B1 is 0 , H1: B0 is not

# write a loop or use the matrix

for (i in c(2,3,4,5)){

  se_b1 <- sqrt(var_beta[i,i]) 
  
  t_b1 <- abs(beta[i] / se_b1)
  crit <- qt(0.025, 25, lower.tail = FALSE)
  
  print(glue("Beta {i} T-statistic is {t_b1} and Critical Value is {crit}"))
  
  }

msr <- ssr / 4

f_stat <- msr / mse
qf(0.05, 4, 25, lower.tail = FALSE)

# (e) using only X3
model_x3 <- lm(PPG ~ RPG, NBA)
anova(model_x3)
summary(model_x3)
