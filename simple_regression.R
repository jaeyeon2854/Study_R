adv <- read.table("/Users/jaeyeon/Desktop/데이터사이언스를 위한 통계분석/9주차_단순회귀분석1/adv.txt", header = T)

adv <- as.data.frame(adv)

plot(adv$total ~ adv$adv)

#회귀분석
lm_result <- lm(adv$total ~ adv$adv)
summary(lm_result)
anova(lm_result)

abline(a=-0.6753, b= 2.5850, col="red")

# ssr,sse,sst 직접 구하기
adv$yhat <- -0.6753 + 2.5850 * adv$adv
adv$residual <- adv$total - adv$yhat

n <- nrow(adv)
ybar <- mean(adv$total)

sse <- sum(adv$residual * adv$residual)
sse1 <- sd(adv$residual)^2 * (n-1)

ssr <- sum((adv$yhat - ybar)*(adv$yhat - ybar))
ssr1 <- sd(adv$yhat - ybar)^2 * (n-1)

sst <- ssr + sse
sst1 <- sum((adv$total - ybar)*(adv$total - ybar))
sst2 <- sd(adv$total - ybar)^2 * (n-1)

#regression coefficients, b0, b1
xbar <- mean(adv$adv)
ybar <- mean(adv$total)

adv$x_cen <- adv$adv - xbar 
adv$y_cen <- adv$total - ybar
sxx <- sum(adv$x_cen * adv$x_cen)
sxy <- sum(adv$x_cen * adv$y_cen)  
syy <- sum(adv$y_cen * adv$y_cen)


b1 <- sxy/sxx
b0 <- ybar - b1 * xbar

adv$yhat <- b0 + b1*adv$adv
adv$resid <- adv$total -adv$yhat
points(adv$adv,adv$yhat, pch =2,col="blue",bg="red")

plot(adv$resid ~ adv$adv)

#ANOVA table
ssr <- sum((adv$yhat-ybar) *  (adv$yhat-ybar))
sse <- sum(adv$resid * adv$resid)
sst <- syy

ssr1 <- sst-sse

anova(lm_result)
nsample <- nrow(adv)

msr <- ssr/1
mse <- sse/(nsample-2)
F <- msr/mse

pvalue <- pf(F, 1, nsample-2, lower.tail = FALSE)



rsq <- sxy^2/(sxx*syy)
rsq1 <- ssr/sst

#ttest for b1, b0

sigmahat <- sqrt(mse)
sgimahat_b1 <- sigmahat/sqrt(sxx)
seb1 <- sgimahat_b1
tb1 <- (b1 - 0)/seb1
pb1 <- 2*(1 - pt(tb1, 28))



gfit <- lm(adv$total ~ adv$adv, data = adv)
summary(gfit)
anova(gfit)

seb0 <-sqrt(mse)*sqrt(1/n + xbar^2/sxx)
tb0 <- b0/seb0
pvalb0 <- 2*(pt(tb0,df=n-2))


