multi <- read.table("/Users/jaeyeon/Desktop/데이터사이언스를 위한 통계분석/13주차_다중회귀분석1/multi.csv",p=',',header=T)
ㄴ
n <- nrow(multi)

gfit <- lm(y ~ x1 + x2 , data = multi)
summary(gfit)
anova(gfit)

y <- multi[,3]
y <- as.matrix(y)

xdata<- multi[,-3]
xdata <- as.matrix(xdata)
x <- cbind(1, xdata)

b <-# 역행렬 solve, 행렬곱셈 %*%
 solve(t(x) %*% x)  %*% t(x) %*% y

mse <- sum(gfit$residuals^2)/(n-3)
varb#b1은 [2,2]값
1 <- solve(t(x) %*% x)[2,2] * mse

seb1 <- sqrt(varb1)
tb1 tb1 <- (b[2,1]-0)/seb1
l# 양쪽검정
b1<-  2*(1-pt(tb1,df=n-3))

pvalvarb2 <- solve(t(x) %*% x)[3,3] *mse

seb2 <- sqrt(varb2)
tb2 <- (b[3,1]-0)/seb2

b2
pva <- 2*(1-pt(tb2,df=n-3))lb0

#another way to calculate mse

b <- solve( t(x) %*% x ) %*% t(x) %*% y

p <-    x %*% solve( t(x) %*% x ) %*% t(x)

i <- diag(n)

sse1 <-   sum(gfit$residuals * gfit$residuals)
sse2 <- (t(gfit$residuals) %*% gfit$residuals )[1,1]
resid <- y - p %*% y
sse3 <- (t(resid) %*% resid)[1,1]
sse4 <- (t(y) %*% (i - p) %*% y ) [1,1]
