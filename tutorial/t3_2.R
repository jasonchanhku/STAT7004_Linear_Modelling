## Example 2
dat <- data.frame(x = c( -3, -2, -1, -1,  0,  1,  1,  2,  2, 3),
                  y = c(114,112,110,107,107,105,104,104,101,96))

# Sxx
S_xx <- sum((dat$x - mean(dat$x))^2);S_xx
# Syy
S_yy <- sum((dat$y - mean(dat$y))^2);S_yy
# Sxy
S_xy <- sum((dat$x - mean(dat$x))*(dat$y - mean(dat$y)));S_xy

## Q1
beta_0 <- mean(dat$y) - (S_xy/S_xx*mean(dat$x));beta_0
beta_1 <- S_xy/S_xx;beta_1


SSR <- S_xy^2 / S_xx; SSR; MSR <- SSR/1; MSR
Syy <- S_yy;Syy; 
SSE <- Syy - SSR; SSE; MSE <- SSE/(length(dat$x) - 2); MSE

## Q2 -> individual estimate
meanx <- mean(dat$x)
Q2 <- function(num){
  lci <- beta_0 + beta_1 * num - (SSE/8)**0.5*(1/length(dat$x)
                                           + (num - meanx)**2/S_xx
                                           + 1)**0.5*qt(p = 0.025,df = 8,lower.tail = FALSE)
  uci <- beta_0 + beta_1 * num + (SSE/8)**0.5*(1/length(dat$x)
                                           + (num - meanx)**2/S_xx
                                           + 1)**0.5*qt(p = 0.025,df = 8,lower.tail = FALSE)
  return(paste0("95%CIs:(",round(lci,4),",",round(uci,4),")", sep = ""))
}
Q2(num = 2.5)

## Q3 -> multiple estimate
Q3 <- function(num){
  n <- length(num)
  num = mean(num)
  lci <- beta_0 + beta_1 * num - (SSE/8)**0.5*(1/length(dat$x)
                                               + (num - meanx)**2/S_xx
                                               + 1/n)**0.5*qt(p = 0.025,df = 8,lower.tail = FALSE)
  uci <- beta_0 + beta_1 * num + (SSE/8)**0.5*(1/length(dat$x)
                                               + (num - meanx)**2/S_xx
                                               + 1/n)**0.5*qt(p = 0.025,df = 8,lower.tail = FALSE)
  return(paste0("95%CIs:(",round(lci,4),",",round(uci,4),")", sep = ""))
}
Q3(num = c(-2.5,-1.5,-0.5,0.5,1.5,2.5))

## Q4
Q4 <- function(num1,num2){
  lci <- beta_1 * (num1 - num2) - (SSE/8)**0.5*((num1 - num2)**2/S_xx
                                               + 2)**0.5*qt(p = 0.025,df = 8,lower.tail = FALSE)
  uci <- beta_1 * (num1 - num2) + (SSE/8)**0.5*((num1 - num2)**2/S_xx
                                                + 2)**0.5*qt(p = 0.025,df = 8,lower.tail = FALSE)
  return(paste0("95%CIs:(",round(lci,4),",",round(uci,4),")", sep = ""))
}
Q4(num1 = -2.5, num2 = -1.5)

