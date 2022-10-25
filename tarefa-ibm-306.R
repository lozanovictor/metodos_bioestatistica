peso <- c(11.3, 13.3, 14.4, 14.6, 15.3, 15.4, 15.6, 15.9, 15.9, 16.1, 
        16.2, 16.4, 16.7, 16.7, 16.9, 17, 17, 17, 17, 17.2, 17.2, 17.8, 
        17.8, 17.9, 17.9, 18.1, 18.3, 18.5, 18.5, 18.5, 18.6, 19, 19, 
        19.1, 19.2, 19.4, 19.4, 19.4, 19.8, 19.9, 19.9, 20, 20.5, 20.6, 
        20.6, 21.2, 21.2, 21.9, 22.4, 23.5)

genero <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2)

t.test(genero, peso)
seppeso <- split(peso, genero)  #faz uma divisão nos grupos
seppeso$`1`                     #separa os grupos usando os indicadores do objeto "genero" 
seppeso$`2`
x <- seppeso$`1`                #atribuindo a objetos auxiliares x e y
y <- seppeso$`2`
mean(x)                         #media do primeiro genero
mean(y)                         #media do segundo grupo
var(x)                          #calcula a variancia do primeiro genero
var(y)                          #calcula a variancia do segundo genero
t.test(x,y)                     #teste t.student dos grupos separados
var.test(x,y)                   #F test para comparar as variancias
t.test(x,y, alternative = c("two.sided"), conf.level = 0.95)
by(peso, genero, summary)       #descrição dos grupos
hist(x)                         #histograma do grupo x
hist(y)                         #histograma do grupo y

library(ggplot2)
library(dplyr)
library(hrbrthemes)

genero=as.factor(genero)
dado <- data.frame(type = genero, value = peso)

p <- dado %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p

boxplot(peso~genero)
