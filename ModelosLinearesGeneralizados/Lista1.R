install.packages("patchwork")
library(patchwork)
library(ggpubr)

Y <- c(2.38,0.62,2.86,4.53,3.74,2.03,5.53,5.72,6.00,3.53,4.25,0.03,1.15,3.04,1.35,4.74,4.80,5.90,2.79,2.51) 
X1 <- c(4.65,3.87,5.92,5.67,4.53,4.24,4.73,2.19,4.91,4.83,7.08,4.92,3.94,3.68,4.20,4.93,5.19,6.98,4.42,4.22) 
X2 <- as.factor(c("A", "B", "B", "B", "A", "B", "A", "A", "A", "A", "B", "B", "B", "A", "B", "A", "A", "A", "B", "A"))

dado <- data.frame(seq(1:20),Y,X1,X2)
dado

glimpse(dado)

mod <- lm(Y~X1+X2, dado)
x <- model.matrix(mod)
x <- matrix(c(x[,1],x[,2],x[,3]), nrow = 20, ncol = 3, byrow = F)

shapiro.test(mod$residuals)
shapiro.test(Y)

t(x)%*%x

sum(X1^2)
sum(x[,1]%*%x[,2])
solve(t(x)%*%x)

#1)d)
b <- solve(t(x)%*%x)%*%t(x)%*%Y

#1)e)
r <- Y - (x%*%b)
n <- 20 #tamanho da amostra
k <- 2 # número de betas e beta zero não conta
variança <- (1/(n-k-1))*sum(r^2)
sqrt(variança)

sum((mod$fitted.values-mean(Y))^2)/sum((Y-mean(Y))^2) #fórmula R2

#1)f)
summary(mod)

#1)i)
h <- x%*%solve(t(x)%*%x)%*%t(x)
h <- round(h,2)
h%*%h%*%h-h

#1)j)
grafico1 <- data.frame(mod$fitted.values,r)

ggplot(grafico1) +
  geom_point(mapping = aes(x = mod.fitted.values, y = r))

ggplot() +
  geom_point(mapping = aes(x = X1, y = r))

#1)k)
diag(h)

a <- ggplot(data = dado) +
  geom_point(mapping = aes(x=X1, y=Y), size = 2) +
  geom_smooth(data = dado, mapping = aes(x=X1, y=Y), method="lm")

dado2 <- dado %>% 
  mutate(chapeu = diag(h)>(2*(k+1)/n)) %>% 
  mutate(individuo = seq(1:20))

dado2 %>% 
  filter(chapeu == TRUE)

#Os indivíduos 8, 11 e 18 são pontos de alavanca

#removen esses individuos no banco de dados

dado3 <- dado[-c(8),]

b <- ggplot(dado3, aes(x=X1, y=Y, color = X2)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE)


grid.arrange(a, b, ncol = 2)

lm(Y ~X1+X2, dado3)
mod
summary(mod)

#o indivíduo 8 é ponto influente

#1)K)

SQE <- sum(((Y-mod$fitted.values)^2)/(n-k-1))
MQE <- sqrt(MQE)

ri <- r/(sqrt(MQE*(1-diag(h))))
ri

#Questão 4

