# n <- 1000000
# x <- rnorm(n)
# df <- as.data.frame(x)
# hist(df$x) 

# data set
x <- c(1939,1975,1920,1972,1950,2000,1950,1943,1975,1954,
1951,1985,1975,1993,1980,1942,1925,1977,1949,1962)
df <- as.data.frame(x)
hist(df$x)

# <- 대신 = 사용가능
df$y = df$x -1950

df$check1950 <- (df$x > 1950) #참이면 true이고 거짓이면 false으로 저장됨

n <- nrow(df)
mu <- 1950
sigma <- 20
xbar <- mean(df$x)

# z값
z <- (xbar-mu) / (sigma/sqrt(n))

# p-value 양쪽 검정
p_value <- 2*(1-pnorm(z)) 
