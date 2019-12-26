library(matlib)
library(magrittr)
library(glue)
library(ggplot2)
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


# Question 3
T <- c(20, 40, 60, 90, 120, 180, 240, 300, 360, 420)
N <- c(47, 62, 78, 103, 220, 537, 1580, 4500, 9200, 12850)
df <- data.frame(T,N)
model <- lm(N~T)
summary(model)

#(a)
# Plot N against T
ggplot(df, aes(x=T, y=N)) + geom_point() + 
  labs(x="Time From Start, T", y="Bacteral Count, N", 
       title="Scatterplot of Bacterial Count on Time") + geom_smooth(method = 'lm', se = FALSE)

#(b)
log_N <- log(N)
df <- data.frame(T, log_N)
model <- lm(log_N~T)
summary(model)

ggplot(df, aes(x=T, y=log_N)) + geom_point() + 
  labs(x="Time From Start, T", y="Log Bacteral Count, log(N)", 
       title="Scatterplot of Bacterial Count on Time") + geom_smooth(method = 'lm', se = FALSE)
