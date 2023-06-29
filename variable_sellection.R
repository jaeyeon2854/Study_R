survey <- read.table("/Users/jaeyeon/Desktop/데이터사이언스를 위한 통계분석/STUDY_R/supervisor_performance.csv",sep=',',header=T)

lm.full <- lm(y ~ . , data = survey)
summary(lm.full)
anova(lm.full)

#y~1 : 상수항까지, 설명력이 없는 애들부터 없앰
backward.aic = step(lm.full, y~1, direction="backward")

summary(backward.aic)


lm.mini <-  lm(y ~ 1 , data = survey)

forward.aic = step(lm.mini, scope = list(lower ~ 1, 
               upper = ~ x1 + x2 + x3 + x4 + x5 + x6),
               direction="forward")

summary(forward.aic)




lm.mini <-  lm(y ~ 1 , data = survey)

stepwise.aic = step(lm.mini, scope = list(lower ~ 1, 
                   upper = ~ x1 + x2 + x3 + x4 + x5 + x6),
                   direction="both")

summary(stepwise.aic)




backward.aic = step(lm.full, scope = list(lower ~ 1, 
                                          upper = ~ x1 + x2 + x3 + x4 + x5 + x6),
                    direction="backward")
summary(backward.aic)

forward.aic = step(lm.mini, scope = list(lower ~ 1, 
                                          upper = ~ x1 + x2 + x3 + x4 + x5 + x6),
                    direction="forward")
summary(forward.aic)