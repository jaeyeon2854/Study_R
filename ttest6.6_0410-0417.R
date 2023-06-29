# 예6-6.

x1 = c(49 ,44 ,47, 44, 46, 40, 48, 45, 45, 42)
x2 = c(44 ,41 ,45, 44, 43, 39, 42, 40, 40, 42)

# cbind : x1,x2를 col으로 붙임
paint = cbind(x1,x2)
paint = as.data.frame(paint)

boxplot(paint$x1,paint$x2, names = c("old","new"))

var.test(paint$x1,paint$x2)

t.test(paint$x1,paint$x2,paired=FALSE,var.equal=TRUE,alternative = "greater")


# 6-6 실습1.

y1 = c(12.7, 19.3, 20.5, 10.5, 14.0, 10.8, 16.6, 14.0, 17.2)
y2 = c(18.2, 32.9, 10.0, 14.3,16.2, 27.6, 15.7)

boxplot(y1,y2,names = c("y1","y2"))

var.test(y1,y2)

# alternative "less" : m1<m2 
t.test(y1,y2,paired=FALSE,var.equal=FALSE,alternative = "less")

# ttest 검정통계량식(이분산) 

mu1 <- mean(y1)
mu2 <- mean(y2)

n1 <- length(y1)
n2 <- length(y2)

sd1 <- sd(y1)
sd2 <- sd(y2)

numer <- mu1 - mu2 

denom <- sqrt(sd1^2 / n1 +  sd2^2 / n2)

t<-numer /denom

# 7.825 : 자유도
pval <- pt(t, 7.825, lower.tail = TRUE) 
# 등분산 가정을 하지 않을 때 : (Welch's t-test) 자유도 계산이 조금 어려움.

#페인트 데이터
x1 <- c(49,  44,  47,  44,  46,  40,  48,  45,  45,  42)
x2 <- c(44,  41,  45,  44,  43,  39,  42,  40,  40,  42)

boxplot(x1,x2,names = c("old","new"))

var.test(x1,x2)

# alternative "greater" : m1>m2 
t.test(x1,x2,paired=FALSE,var.equal=TRUE,alternative = "greater")

# ttest 검정통계량식(등분산) 

mu1 <- mean(x1)
mu2 <- mean(x2)

n1 <- length(x1)
n2 <- length(x2)

sd1 <- sd(x1)
sd2 <- sd(x2)

numer <- mu1 - mu2 

denom <- sqrt( ( (n1-1) * sd1^2 + (n2-1) * sd2^2 ) / ( n1 - 1 + n2- 1 ) ) * sqrt( 1/n1 + 1/n2)

t<-numer / denom

#lower.tail은 m1<m2 : true, m1>m2 : false

pval <- pt(t, n1+n2-2, lower.tail = FALSE) 

# 등분산 가정을 할 때 : 자유도 = n1+n2-2


#start monte_carlo simulation : 랜덤 넘버를 많이 만들어서 확률값 추정
#3.14 추정 

n_sim <- 100

x <- runif(n_sim , 0 , 1)
y <- runif(n_sim , 0 , 1)

data <- cbind(x,y)
data <- data.frame(data)
data$radius <- sqrt(data$x^2 + data$y^2)
test <- (data$radius <1)

est_pi <- 4*sum(test)/n_sim

plot(data$x,data$y)
par(new = T)
curve(sqrt(1-x^2), from = 0 , to = 1, col='red')

#monte_carlo simulation to estimate pi

n_sim = 1000000

x1_sim = rnorm(n1*n_sim)
x2_sim = rnorm(n2*n_sim)

x1_sim = matrix(data = x1_sim, nrow = n1, ncol = n_sim)
x2_sim = matrix(data = x2_sim, nrow = n2, ncol = n_sim)

mu1_sim = apply(x1_sim,2,mean)
mu2_sim = apply(x2_sim,2,mean)

sd1_sim = apply(x1_sim,2,sd)
sd2_sim = apply(x2_sim,2,sd)

number_sim = mu1_sim - mu2_sim
sp_sim = sqrt( ( (n1-1) * sd1_sim^2 + (n2-1) * sd2_sim^2 ) / ( n1 - 1 + n2- 1 ) )

t_sim = number_sim / (sp_sim * sqrt(1/n1 + 1/n2))

hist(t_sim)

p_value_est = sum(t_sim > t)/n_sim
