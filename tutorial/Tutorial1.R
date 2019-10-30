## A Useful built-in fucntions

### A.1 Assignment operator
x <- 1:3
x = 1:3

### A.2 Obtaining working directory
getwd()

### A.3 Changing working diectory
#setwd("/the/path/you/want/to/switch/to")

### A.4 packages
install.packages("dplyr") # install
library(dplyr) # import packages

### A.5 get help
?getwd 
help("getwd")
library(dplyr)
?mutate

### A.6 data structures
#### A.6(i) vector
v1 <- 1:3;v1
v2 <- c(1, 2, 3);v2
#### A.6(ii) matrix 
mat <- matrix(1:10, 2, 5); mat
dim(mat)
#### A.6(iii) array
arr <- array(1:12, dim=c(2, 2, 3)); arr
dim(arr)
#### A.6(iv) Dataframe
df1 <- data.frame(x1=1:10, x=runif(10), y=rnorm(10), s=paste0(1:10, "a")); df1
df1$x1
dim(df1)
#### A.6(v) list
lst <- list(v=v1, df=df1, mat=mat, arr=arr); lst
lst$v
a <- lst[[1]];a
b <- lst[1]; b



## B Some packages
### B.1 magrittr package
library(magrittr)
#### B.1(i) Motivation example
# For example, plot the density from the samples for log-normal distribution
# basic method
x <- rnorm(10000) # generate data from normal distribution
logx <- exp(x) #transform data to log-normal distribution
logx.density <- density(logx) #estimate the density
plot(logx.density, main="Estimated log-normal density")
# we may combine all the code into one line
plot(density(exp(rnorm(10000))), main="Estimated log-normal density")
# pipe operator method
rnorm(10000) %>%
  exp() %>%
  density %>%
  plot(main="Estimated log-normal density") # more elegent and readable

#### B.1(ii) Basic usage and placeholder
x <- 1
y <- 100
fn <- function(s, z){return(s/2+z*2)}

x %>% fn(y)
fn(x, y)

x %>% fn(y, .)
x %>% fn(y, z=.)
fn(y, x)

x %>% fn(., .)
fn(x, x)

#### B.1(iii) Advanced pipes
# %T>%
x %>% exp %>% log
x %T>% exp %>% log

xs <- rnorm(100)
xs %>% exp %>% plot %>% str
xs %>% exp %T>% plot %>% str

# %$%
df1 %$% cov(x, y)
cov(df1$x, df1$y)
df1 %>% {cov(.$x, .$y)}

# $<>$
x <- 1;x 
x %<>% log; x

x <- 1; x
x <- x %>% log; x

### B.2 dplyr package
str(mtcars)
# mutate/transtate
mtcarsnew1 <- mtcars %>% mutate(kpg=1.609*mpg); str(mtcarsnew1)
mtcarsnew2 <- mtcars %>% transmute(kpg=1.609*mpg); str(mtcarsnew2)

# select
mtcars.sel1 <- mtcars %>% select(c("vs", "mpg", "hp")); str(mtcars.sel1)
mtcars.sel2 <- mtcars %>% select(-c("vs", "mpg", "hp")); str(mtcars.sel2)
mtcars.sel3 <- mtcars %>% select(starts_with("d")); str(mtcars.sel3)

# filter
mtcars.fil1 <- mtcars %>% filter(carb==4 & gear == 3); mtcars.fil1
mtcars.fil2 <- mtcars %>% filter(hp>mean(hp)); mtcars.fil2

# arrange
mtcars.arr1 <- mtcars %>% arrange(hp); mtcars.arr1
mtcars.arr2 <- mtcars %>% arrange(desc(hp)); mtcars.arr2
mtcars.arr3 <- mtcars %>% arrange(gear, hp); mtcars.arr3

