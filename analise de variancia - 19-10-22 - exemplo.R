y2 = c(5,6,7,8,3,4,6,5,4,5,5,6,7,7,9,5,10,3,10,9,8,8,9,11,11,12,9,8,9,10)

trat2 = c(rep(c("A"),10), rep(c("B"),10), rep(c("C"),10))

dados=data.frame(trat2, y2)

by(y2, trat2, mean)
by(y2, trat2, sd)

fit2 =lm(y2~trat2,dados)
summary(aov(fit2))

fit2



res2=residuals(fit2)

res2

predict(fit2)

res_p2=rstandard(fit2)

plot(res2~fitted(fit2))

abline(h=0)

bartlett.test(y2,trat2)

boxplot(y2~trat2)

anova2=(aov(fit2))

TukeyHSD(anova2, ordered = TRUE)