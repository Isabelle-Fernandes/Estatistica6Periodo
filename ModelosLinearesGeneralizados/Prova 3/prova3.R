rm(list = ls())

# Questão 1

dado <- read.table("dados_01_Prova3.txt")
str(dado)
dado$V1 <- as.factor(dado$V1)
dado$V7 <- as.factor(dado$V7)
head(dado)

ajuste <- glm(V1 ~ V2 + V3 + V4 + V5 + V6 + V7, data = dado, family = binomial(link = "logit"))
summary(ajuste)

(exp(coef(ajuste)[3]*0.1) - 1) * 100

# Questão 2

individuo <- data.frame(V2=0.1,V3=0.2,V4=0.6,V5=0.7,V6=0.3,V7="0")
eta <- predict(ajuste, newdata = individuo)

exp(eta)/(exp(eta) + 1)

# Questão 3

n <- length(ajuste$residuals)

CD <- cooks.distance(ajuste)
pontos_influente <- CD[CD > 4/n]
length(pontos_influente)
                      
# Questão 4

which(hatvalues(ajuste) == max(hatvalues(ajuste)))

# Questão 5

treino <- dado[1:232,]
ajuste <- glm(V1 ~ V2 + V3 + V4 + V5 + V6 + V7, data = treino, family = binomial(link = "logit"))

individuo <- dado[250,]
eta <- predict(ajuste, newdata = individuo)
exp(eta)/(exp(eta) + 1)

# Questão 6

dadonew <- dado[233:432,]
eta <- predict(ajuste, newdata = dadonew)
odds <- exp(eta)
ychapeu <- odds/(1+odds)
previsao <- ychapeu>0.5

original <- dado$V1[233:432]==1
sensibilidade <- sum(previsao == TRUE & original == TRUE)/sum(original)
especificidade <- sum(previsao == FALSE & original == FALSE)/(100-sum(original))

# Questão 7

dado <- read.table("dados_02_Prova3.txt")
str(dado)
head(dado)

ajuste1 <- glm(V1 ~ V2 + V3 + V4, data = dado, family = poisson(link = "log"))
summary(ajuste1)

(exp(coef(ajuste1)[2]*0.1) - 1) * 100

# Questão 8

individuo <- data.frame(V2=0.5,V3=0.5,V4=0.5)
eta <- predict(ajuste1, newdata = individuo)

exp(eta)

# Questão 9

ajuste2 <- glm.nb(V1 ~ V2 + V3 + V4, data = dado, link = "log")
summary(ajuste2)

# Questão 11

dado <- read.table("dados_03_Prova3.txt")
str(dado)
head(dado)

ajuste1 <- glm(V1 ~ V2 + V3 + V4 + V5, data = dado, family = Gamma(link = "log"))
summary(ajuste1)

1/0.2262804

# Questão 12

individuo <- data.frame(V2=0.5,V3=0.5,V4=1.2,V5=0.8)
eta <- predict(ajuste1, newdata = individuo)
exp(eta)

# Questão 13

56.521/0.2262804

# Questão 14




