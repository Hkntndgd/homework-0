library(tidyverse)
library(dslabs)
N<-10
P <- seq(0.1,0.4,length=10)
ave <- sapply(P,function(p){
  x<-sample(c(1,0),N,replace = TRUE,prob=c(p,1-p))
  mean(x)
})

d <- 0.039
Ns<-c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2
confidence_intervales <- sapply(Ns,function(N){
  X <-sample(c(1,0),replace = TRUE,N,prob = c(p,1-p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat,X_hat-qnorm(0.975)*SE_hat,X_hat+qnorm(0.975)*SE_hat)-1
})

polls <- data.frame(poll =1:ncol(confidence_intervales),
                    t(confidence_intervales),sample_size=Ns)
names(polls) <- c("poll","estimate","low","high","sample_size")
polls

d_hat <- polls %>%
  summarize(avg=sum(estimate*sample_size)/sum(sample_size)) %>%
  .$avg

d_hat <- polls %>%
  summarize(avg=sum(estimate*sample_size)/sum(sample_size)) %>%
  pull(avg)

p_hat <- (d_hat + 1)/2
moe <- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe

data("polls_us_election_2016")
names(polls_us_election_2016)
polls <- polls_us_election_2016 %>%
  filter(state=="U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 -rawpoll_trump/100)

d_hat <- polls%>%
  summarize(d_hat=sum(spread * samplesize)/sum(samplesize)) %>%
  .$d_hat

p_hat <- (d_hat + 1)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

polls %>%
  ggplot(aes(spread))+
  geom_histogram(col="blue",fill="gray",binwidth = 0.01)

polls %>%
  group_by(pollster) %>%
  summarize(n())

polls %>%
  group_by(pollster)%>%
  filter(n()>=6)%>%
  ggplot(aes(pollster,spread)) +
  geom_boxplot()+
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

polls %>%
  group_by(pollster) %>%
  filter(n()>=6) %>%
  summarize(se=2*sqrt(p_hat*(1-p_hat)/median(samplesize)))

a <- polls %>%
  group_by(pollster) %>%
  filter(n()>=6) %>%
  mutate(med=median(samplesize)) %>%
  select(pollster,med,samplesize)

one_poll_per_pollster <- polls %>%
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

sd(one_poll_per_pollster$spread)
results <- one_poll_per_pollster %>%
  summarize(avg=mean(spread),se=sd(spread/sqrt(length(spread)))) %>%
  mutate(start=avg-1.96*se,end=avg+1.96*se)
round(results*100,1)