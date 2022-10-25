dados = read.csv("vento.csv", header = TRUE, sep = " ", dec = ",")

aggregate(vento ~ area, dados, var)

bartlett.test(vento ~ area, dados)

shapiro.test(dados$vento[dados$area == "AI"])
shapiro.test(dados$vento[dados$area == "AII"])
shapiro.test(dados$vento[dados$area == "AII"])
shapiro.test(dados$vento[dados$area == "AIV"])

tab.anova <- anova(lm(dados$vento ~ dados$area))
tab.anova

tab.anova2 <- aov(dados$vento ~ dados$area)
summary(tab.anova2)

par(mfrow = c(1, 1))
tab.anova$"Sum Sq"[1]/sum(tab.anova$"Sum Sq")

post.hoc <- TukeyHSD(tab.anova2)
post.hoc

