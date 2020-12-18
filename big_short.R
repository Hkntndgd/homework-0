library(tidyverse)
set.seed(16,sample.kind = "Rounding")

n <- 1000
losses_per_foreclosure <- -200000
p<-0.02
default <- sample(c(0,1),n,replace = TRUE,prob = c(1-p,p))
total_losses <- sum(default*losses_per_foreclosure)

B<-10000
losses <- replicate(B,{
  default <- sample(c(0,1),n,replace = TRUE,prob = c(1-p,p))
  sum(default*losses_per_foreclosure)
})

losses_av <- n*(p*losses_per_foreclosure +(1-p)*0)
losses_st_err <- sqrt(n)*abs(losses_per_foreclosure-0)*sqrt(p*(1-p))

losses_per_foreclosure*p/(1-p)
l <- losses_per_foreclosure
z <- qnorm(0.01)
x <- -l*(p*n-z*sqrt(n*p*(1-p)))/(z*sqrt(n*p*(1-p))+(1-p)*n)

B<-100000
profit <- replicate(B,{
  draw <-sample(c(x,losses_per_foreclosure),n,replace = TRUE,prob = c(1-p,p))
  sum(draw)
})
mean(profit)
mean(profit>0)

p <-0.04
r <-0.05
x <-r*180000
losses_per_foreclosure*p + x*(1-p)

z<-qnorm(0.01)
n<- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p+x*(1-p))^2)

n*(losses_per_foreclosure*p + x*(1-p))
profit <- replicate(B,{
  draws <- sample(c(x,losses_per_foreclosure),n,replace = TRUE,prob = c(1-p,p))
  sum(draws)
})
mean(profit>0)

p<-0.04
profit <- replicate(B,{
  new_p <- p + sample(seq(-0.01,0.01,length = 100),1)
  draws <- sample(c(x,losses_per_foreclosure),n,replace = TRUE,prob = c(1-new_p,new_p))
  sum(draws)
})
mean(profit)
mean(profit<0)
mean(profit < -10000000)
