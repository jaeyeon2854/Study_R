# n <- 1000000
# x <- rnorm(n)
# df <- as.data.frame(x)
# hist(df$x) 

# data set
x <- c(1939,1975,1920,1972,1950,2000,1950,1943,1975,1954,
1951,1985,1975,1993,1980,1942,1925,1977,1949,1962)

#data frame으로 저장
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

# p-value 한쪽 검정
p_1sided = p_value/2

n_row = n
n_col = 100000

#귀무가설을 따르는 random sample
x_sim = rnorm(n_row*n_col, mean = 1950, sd = 20)

x_sim = matrix(data = x_sim, nrow = n_row, ncol = n_col)

x_sim = as.data.frame(x_sim)

# apply (,1,)이면 row별로, (,2,)이면 col별로 계산
mean_x = apply(x_sim,2,mean)

hist(mean_x)

test = (xbar < mean_x)
test = as.data.frame(test)

test_n = sum(test$test)

pvalue_est = test_n / n_col