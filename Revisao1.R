amostra = rnorm(173, mean=2.5)
hist(amostra)
amostra_limpa = ifelse(amostra>1.35, amostra, 1.333)
hist(amostra_limpa)
amostra_ale = sample(amostra_limpa, 10, replace = FALSE)
amostra_ale = sort(amostra_ale)
amostra_ale
mean(amostra_ale)
sd(amostra_ale)
t.test(amostra_ale)
