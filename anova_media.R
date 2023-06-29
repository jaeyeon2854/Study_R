media_data <- read.table("/Users/jaeyeon/Desktop/데이터사이언스를 위한 통계분석/3ANOVA/media.txt", header=T)

boxplot(media_data$value ~ media_data$media )

media_result <- aov(media_data$value ~ media_data$media ) 

summary(media_result)

##

media_data <- read.table('/Users/jaeyeon/Desktop/데이터사이언스를 위한 통계분석/3ANOVA/bug.txt',header =T )

boxplot(media_data$number ~ media_data$color)

media_result <- aov(media_data$number ~ media_data$color ) 

summary(media_result)

##
media_result2 <- aov(value ~ media, data=media_data)

summary(media_result2)

mean_total <- mean(media_data$value)
mean_group <- tapply(media_data$value, media_data$media, mean)


old <- c(49,  44,  47,  44,  46,  40,  48,  45,  45,  42)
new <- c(44,  41,  45,  44,  43,  39,  42,  40,  40,  42)

boxplot(old,new, names = c("old","new"))

data <- cbind(old,new)
mean_group1 <- apply(data,2,mean)

