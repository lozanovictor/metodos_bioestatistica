peso=scan(,dec=",")

24,9 27,9 23,8
20,4 28,1 25,3
24,2 28,4 23,4
22,1 25,3 27,6
20,3 29,3 25,5
24,0 28,5 23,9
23,5 27,9 22


#Enter no terminal aqui pra terminar a função scan()

dieta=c(rep(c("A","B","D"),7))

dieta

peso

fit=aov(peso~dieta)

fit

summary(fit)    

#Função by()
#Aplica uma função a um data frame e divide por fatores, 
#nesse caso é a media dos pesos, separada pela dieta
by(peso, dieta, mean)
variancia = anova(peso~dieta)


#SQ
sqt = sum(peso-mean(peso))^2
round(sum(sqt),3)
dados = data.frame(peso, dieta)
dados$peso
xbA=mean(dados$peso[dados$dieta=="A"])
xbB=mean(dados$peso[dados$dieta=="B"])
xbD=mean(dados$peso[dados$dieta=="D"])

A=dados$peso[dados$dieta=="A"]
B=dados$peso[dados$dieta=="B"]
D=dados$peso[dados$dieta=="D"]
sqdA=sum((A-xbA)^2)
sqdB=sum((B-xbB)^2)
sqdD=sum((D-xbD)^2)
sqd=sqdA+sqdB+sqdD

sqe=length(A)*((xbA-mean(peso))^2)+
  length(B)*((xbB-mean(peso))^2)+
  length(D)*((xbD-mean(peso))^2)
sqe


#Tabela

#FV      somasQ      GL    QM               F
#entre sqe           3-1   QME=sqe/2        QME/QMD
#dentro sqd          28-3  QMD=QMR=sqd/25
#TOTAL    144.6      28-1


m1=lm(peso~dieta)
summary(m1)
m1
resid(m1)
cbind(peso,resid(m1))
cbind(peso, predict(m1), resid(m1))

plot(resid(m1))
plot(predict(m1))
plot(predict(m1), resid(m1))
bartlett.test(peso, dieta)



qqnorm(A)
qqline(A)
qqnorm(B)
qqline(B)
qqnorm(D)
qqline(D)
qqnorm(peso)
qqline(peso)
shapiro.test(A)
shapiro.test(B)
shapiro.test(D)
