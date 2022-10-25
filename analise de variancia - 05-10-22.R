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

