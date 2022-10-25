y=scan()

2370 1282 562 173 193

1687 1527 321 127 71

2592 871 636 132 82

2283 1025 317 150 62

2910 825 485 129 96

3020 920 842 227 44



y  

trat=rep(c("A","B","C","D","E"),6)

trat

dados=data.frame(trat,y)

head(dados)

fit=lm(y~trat,dados)

summary(fit)

summary(aov(fit))



res=residuals(fit)

res

predict(fit)

res_p=rstandard(fit)

plot(res_p~fitted(fit))

abline(h=0)

bartlett.test(y,trat)
