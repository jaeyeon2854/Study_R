# data set
x <- c(5.7,7.7,7.5,3.8,8.8,5.4,7.9,8.1,4.7,6.3,
10.3,4.9,7.3,6.7,7.2,8.3,7.7,6.9,5.5,6.9)
df <- as.data.frame(x)
hist(df$x)

# <- 대신 = 사용가능
df$y = df$x -6

df$check1950 <- (df$x > 6) #참이면 true이고 거짓이면 false으로 저장됨

n <- nrow(df)
mu <- 6
sigma <- 1.5
xbar <- mean(df$x)

# z값
z <- (xbar-mu) / (sigma/sqrt(n))

# p-value 양쪽 검정
p_value <- 2*(1-pnorm(z)) 

# p-value 한쪽 검정
p_value2 <- 1-pnorm(z)
