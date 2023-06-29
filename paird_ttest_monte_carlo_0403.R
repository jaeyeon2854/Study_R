# 8장 쌍체비교 예제
initial = c(24.7,46.1, 18.5, 29.5, 26.3, 33.9, 23.1, 20.7, 18.0, 19.3, 23.0)
 
after = c(12.4, 14.1, 7.6, 9.5, 19.7, 10.6, 9.1, 11.5, 13.3, 8.3, 15.0)

#col으로 두개의 데이터를 붙임
cow = cbind(initial,after)

cow = as.data.frame(cow)

ndata <- nrow(cow)

cow$diff <- cow$initial - cow$after

hist(cow$diff)

t.test(cow$diff, mu=0,alternative = "greater")

#monte carlo method

n_sim <-100000
ndata <- nrow(cow)

x <- rnorm(ndata*n_sim, mean=0, sd=1 )
x <- matrix(data = x, nrow = ndata, ncol = n_sim)
x <- as.data.frame(x)

#행별로 apply는 2
mean_x <- apply(x,2,mean)

sd_x <- apply(x,2,sd)

t_x <- (mean_x-0)/(sd_x/sqrt(ndata))

hist(t_x)

t <- 5.6077
p_value <- 0.0001126

sum(t_x > t)

p_value_sim <- sum(t_x > t)/n_sim

