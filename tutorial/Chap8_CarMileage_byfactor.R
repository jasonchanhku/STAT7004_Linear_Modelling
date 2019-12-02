dat1=matrix(
  c('A', 30,
    'A', 26,
    'A', 27,
    'A', 29,
    'B', 28,
    'B', 29,
    'B', 28,
    'B', 31,
    'C', 29,
    'C', 32,
    'C', 27,
    'C', 31,
    'C', 30,
    'D', 45,
    'D', 35,
    'D', 45,
    'D', 37,
    'D', 40),
  nrow=18,ncol=2,byrow=T)

dat1=as.data.frame(dat1)
colnames(dat1)=c('x', 'y')
dat1$y=as.numeric(as.character(dat1$y))

#options(contrasts = c("contr.treatment", "contr.poly")) #default
#options(contrasts = c("contr.sum", "contr.poly"))
options(contrasts = c("contr.SAS", "contr.poly"))

dummy.coef(aov(y~x,dat1))
summary(aov(y~x,dat1))
summary(lm(y~x,dat1))
