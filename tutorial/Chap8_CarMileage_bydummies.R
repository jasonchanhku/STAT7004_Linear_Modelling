dat1=matrix(
  c(30, 1, 0, 0,
    26, 1, 0, 0,
    27, 1, 0, 0,
    29, 1, 0, 0,
    28, 0, 1, 0,
    29, 0, 1, 0,
    28, 0, 1, 0,
    31, 0, 1, 0,
    29, 0, 0, 1,
    32, 0, 0, 1,
    27, 0, 0, 1,
    31, 0, 0, 1,
    30, 0, 0, 1,
    45, 0, 0, 0,
    35, 0, 0, 0,
    45, 0, 0, 0,
    37, 0, 0, 0,
    40, 0, 0, 0),
  nrow=18,ncol=4,byrow=T)

dat1=as.data.frame(dat1)
colnames(dat1)=c('y', 'v1', 'v2', 'v3')

summary(lm(y~v1+v2+v3,dat1))