# data set
x <- c(22,25,34,35,41,41,46,46,46,47,49,54,54,59,60)

#data frame으로 저장
df <- as.data.frame(x)
hist(df$x)

n <- nrow(df)
mu <- 40
df$sigma <- sd(df$x) # sd : 표준편차
df$xbar <- mean(df$x)
df$diff <- df$x - df$xbar
df$diffsq <- df$diff^2

sd_cal <- sqrt(sum(df$diffsq) / (n-1))

xbar <- mean(df$x)
# t값
t <- (xbar-mu) / (sd_cal/sqrt(n))

# p-value 양쪽 검정
p_value <- 2*(1-pt(t,n-1))

# p-value 한쪽 검정
p_1sided = p_value/2

# 실제 ttest 명령어
t.test(df$x, mu=40)

