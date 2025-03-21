# Lista 3 - MLG

# Questão 1
# Letra a
dado <- read.table("dados_Q1_L3_MLG.txt")

str(dado)

dado$V3 <- as.factor(dado$V3)

ajuste <- glm(V1 ~ V2 + V3 + V4, family = poisson(link = "log"), data = dado)

summary(ajuste)

# Letra b
deviance <- 2*sum(dado$V1*log(dado$V1/fitted(ajuste))-(dado$V1 - fitted(ajuste)))
deviance

ajuste$deviance

#Letra d
n <- length(dado$V1)
n

media <- n - length(coef(ajuste))
media

mediana <- qchisq(0.5,media) #50% da distribuição encontra-se na distribuição com 46 graus de liberdade
mediana

# A deviance não encontra-se entre a media e mediana

# Letra e
rP <- residuals(ajuste, type = "pearson")
rD <- residuals(ajuste, type = "deviance")
fit <- fitted(ajuste)
desv <- data.frame(rP, rD, fit)

p <- desv %>% 
  ggplot(aes(x=fit, y=rP)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.6) +
  labs(x = "Valores Ajustados", y = "Resíduos de Pearson") +
  theme_light() 

d <- desv %>% 
  ggplot(aes(x=fit, y=rD)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.6) +
  labs(x = "Valores Ajustados", y = "Resíduos de Componente do Desvio") +
  theme_light() 

plot_grid(p, d, ncol = 1)  

ytransformado <- 2*sqrt(fit)

p <- desv %>% 
  ggplot(aes(x=ytransformado, y=rP)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, colour = "red") +
  labs(x = "Valores Ajustados", y = "Resíduos de Pearson") +
  theme_light() 

d <- desv %>% 
  ggplot(aes(x=ytransformado, y=rD)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, colour = "red") +
  labs(x = "Valores Ajustados", y = "Resíduos de Componente do Desvio") +
  theme_light() 

plot_grid(p, d, ncol = 1)  

# Questão 2
# Letra a 

dado <- read.table("dados_Q2_L3_MLG.txt")
str(dado)
dado$V3 <- as.factor(dado$V3)

dado %>% 
  ggplot(aes(x=V1)) + 
  geom_histogram() +
  labs(x = "Nível de criminalidade",
       y = "Frequencia") + 
  theme_light()

dado %>% 
  ggplot(aes(x=log(V1))) + 
  geom_histogram() +
  labs(x = "Nível de criminalidade",
       y = "Frequencia") + 
  theme_light()

shapiro.test(dado$V1)
shapiro.test(log(dado$V1))
#Não acho adequado aplicar regressao linear aos dados trasnformados, pode a interpretação para a resposta também estaria transformada.

# Letra b
ajuste <- glm(V1 ~ V2 + V3, family = Gamma(link = "log"), data = dado)
summary(ajuste)

# Letra c
percent(exp(0.98681)-1)
percent(exp(2.1275)-1)

# Letra d
n <- length(dado$V1)
n

media <- n - length(coef(ajuste))
media

mediana <- qchisq(0.5,media) #50% da distribuição encontra-se na distribuição com 46 graus de liberdade
mediana

ajuste$deviance
#Deviance não está entre a mediana e a média

# Letra e

rP <- residuals(ajuste, type = "pearson")
rD <- residuals(ajuste, type = "deviance")
fit <- fitted(ajuste)
desv <- data.frame(rP, rD, fit)

p <- desv %>% 
  ggplot(aes(x=fit, y=rP)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.6) +
  labs(x = "Valores Ajustados", y = "Resíduos de Pearson") +
  theme_light() 

d <- desv %>% 
  ggplot(aes(x=fit, y=rD)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.6) +
  labs(x = "Valores Ajustados", y = "Resíduos de Componente do Desvio") +
  theme_light() 

plot_grid(p, d, ncol = 1)

# Questão 3
# Letra a 

dado <- read.table("dados_Q3_L3_MLG.txt")
str(dado)
dado <- map_df(dado, as.factor)

dadotreino <- dado[1:300,]
  
ajuste <- glm(V1 ~ V2 + V3, family = binomial(link = "logit"), data = dadotreino)
summary(ajuste)


# Letra b

dadonew <- dado[301:400,]
  
eta <- predict(ajuste, newdata = dadonew)
odds <- exp(eta)
ychapeu <- odds/(1+odds)
previsao <- ychapeu>0.5


# Letra c

original <- dado$V1[301:400]==1

sensibilidade <- sum(previsao == TRUE & original == TRUE)/sum(original)
especificidade <- sum(previsao == FALSE & original == FALSE)/(100-sum(original))

