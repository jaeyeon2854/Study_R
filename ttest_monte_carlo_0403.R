bulb_data <- read.table("C:/Users/taewon lee/Downloads/1height_test/height.txt")

hr_data <- c(22,25,34,35,41,41,46,46,46,47,49,54,54,59,60)
#hr_data <- c(5.7,  7.7,  7.5,  3.8,  8.8,  5.4,  7.9,  8.1,  4.7,  6.3,
# 10.3,  4.9,  7.3,  6.7,  7.2,  8.3,  7.7,  6.9,  5.5,  6.9)


hr_data <- as.data.frame(hr_data)

ndata <- nrow(hr_data)

hist(hr_data$hr_data)

mu <- mean(hr_data$hr_data)
sigma <-sd(hr_data$hr_data)

hr_data$xbar <- sum(hr_data$hr_data)/ndata
hr_data$diff <- hr_data$hr_data - hr_data$xbar
hr_data$diff_sq <- (hr_data$diff)*(hr_data$diff)
#hr_data$diff_sq <- (hr_data$diff)^2
hr_data$var <- sum(hr_data$diff_sq)/(ndata-1)
hr_data$sigma <- sqrt(hr_data$var) 

t <- (mu - 40)/(sigma/sqrt(ndata) )

p_value = 1.0 - pt(t, ndata-1, lower.tail = TRUE, log.p = FALSE)

p_value1 <- pt(t, df=ndata-1, lower.tail=FALSE)

hist(hr_data$hr_data)
boxplot(hr_data$hr_data)

# R function t.test()
t.test(hr_data$hr_data, mu=40)


n_sim <- 1000000
x <- rnorm(ndata*n_sim, mean=40, sd=1 )
x <- matrix(data = x, nrow = ndata, ncol = n_sim)
x <- as.data.frame(x)

#행별로 apply는 2
mean_x <- apply(x,2,mean)

sd_x <- apply(x,2,sd)

t_x <- (mean_x-40)/(sd_x/sqrt(ndata))

hist(t_x)

# 참이면 1이므로 두 결과값이 같음
sum(t_x > t)
# sum((t_x > t) == TRUE)

p_value_sim <- sum(t_x > t)/n_sim
  
#monte carlo method

n_sim <-1000000

x <- rnorm(ndata*n_sim, mean=6, sd=1 )
x <- matrix(data = x, nrow = ndata, ncol = n_sim)
x <- as.data.frame(x)

mean_x <- apply(x,2,mean)

sigma_x <- apply(x,2,sd)

numer_t_sim <- mean_x - 6 

denom_t_sim <- sigma_x/sqrt(ndata)

t_sim <- numer_t_sim / denom_t_sim

hist(t_sim)

test <- (t_sim > t)
test <- as.data.frame(test)
test_n <- sum(test$test == TRUE)

p_value_est <- test_n/n_sim
