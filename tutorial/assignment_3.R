library(matlib)
library(magrittr)
library(glue)
# Question 1

low <- c(7.6, 5.8, 6.9, 6.6, 6.3, 7.7, 6.0)
med <- c(6.7, 8.1, 9.4, 8.6, 7.8, 8.9, 7.9, 8.3, 8.7, 7.1, 8.4)
high <- c(8.5, 9.7, 10.1, 7.8, 9.6, 9.5, 10.2)

n <- length(low) + length(med) + length(high)

y_.. <- sum(low) + sum(med) + sum(high)

ss_yij <- sum(low^2) + sum(med^2) + sum(high^2)

y_..^2 / n

sum(low)^2 / length(low)  + sum(med)^2 / length(med) + sum(high)^2 / length(high)

0.5*mean(low) + 0.5*mean(med) - mean(high)

(7*mean(low) + 11*mean(med) + 7*mean(high)) / 25
