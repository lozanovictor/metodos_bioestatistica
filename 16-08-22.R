xx <- rnorm(100, 25, 1)
round(xx,1)
pop <- xx
mu <- mean(pop)
a1 <- sample(pop, 10, FALSE)
xb <- mean(a1)
a2 <- sample(pop, 10, FALSE)
xb2 <- mean(a2)
a3 <- sample(pop, 10, FALSE)
xb3 <- mean(a3)
a4 <- sample(pop, 10, FALSE)
xb4 <- mean(a4)
xb
xb2
xb3
xb4

