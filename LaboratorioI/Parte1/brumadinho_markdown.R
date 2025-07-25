install.packages("gmodels")


library(readxl)
library(tidyverse)
library(reshape2)
library(ggridges)
library(scales)
library(gmodels)

#Lendo os dados

roberta_brumadinho <- read_excel("roberta_brumadinho.xls")

#"Vendo" os dados

summary(roberta_brumadinho)

#I - Adequando o banco de dados:

roberta_brumadinho <- data.frame(roberta_brumadinho)

#1� - Transformando as colunas IA em factors

roberta_brumadinho$IA <- as.factor(roberta_brumadinho$IA)

roberta_brumadinho$IA01 <- as.factor(roberta_brumadinho$IA01)
roberta_brumadinho$IA03 <- as.factor(roberta_brumadinho$IA03)
roberta_brumadinho$IA05 <- as.factor(roberta_brumadinho$IA05)
roberta_brumadinho$IA07 <- as.factor(roberta_brumadinho$IA07)
roberta_brumadinho$IA09 <- as.factor(roberta_brumadinho$IA09)
roberta_brumadinho$IA11 <- as.factor(roberta_brumadinho$IA11)
roberta_brumadinho$IA13 <- as.factor(roberta_brumadinho$IA13)
roberta_brumadinho$IA15 <- as.factor(roberta_brumadinho$IA15)

#2� - Atualizando a coluna IA com base no criterio solicitado

any_sim <- rowSums(roberta_brumadinho[, c("IA01", "IA03", "IA05", "IA07", "IA09", "IA11", "IA13", "IA15")] == "Sim") > 0
all_nao <- rowSums(roberta_brumadinho[, c("IA01", "IA03", "IA05", "IA07", "IA09", "IA11", "IA13", "IA15")] == "N�o") == 8

roberta_brumadinho$IA <- ifelse(any_sim, "sim", ifelse(all_nao, "n�o", NA))

#3� Para calcular a porcentagem de domicilios em IA teremos que pegar somente um individuo
#por domicilio

unique_roberta_brumadinho <- roberta_brumadinho %>%
  distinct(ID.DOMICILIAR, .keep_all = TRUE)

#4� Trocar os caracteres das colunas (12,14,16,18,21,22,24,26,27,29,31) do banco para "caracter n�mero"

coluna <- c(12,14,16,18,21,22,24,26,27,29,31) # selecionar somente as colunas do banco que referem quantos dias comem determinado alimento
valores <- unique(roberta_brumadinho[,14]) #os valores s�o os mesmos para todas as colunas, ent�o peguei os valores da coluna 14
referencia <- as.matrix(cbind(valores,c(3,6,2,7,0,4,1,5,NA)))

for (i in 1:3080) {
  for (a in 1:length(coluna)) {
    for (b in 1:length(referencia[,1])) {
      if (roberta_brumadinho[i,coluna[a]]==referencia[b,1]) {
        roberta_brumadinho[i,coluna[a]]=referencia[b,2]
      }
    }
  }
}


# Trocar coluna "RENDIMENTOS MENSAIS"

for (i in 1:3080){
  if(is.na(roberta_brumadinho[i,11])){
    FALSE   
  } else if (roberta_brumadinho[i,11]=="N�o sabe/ n�o respondeu"){
    roberta_brumadinho[i,11]=NA
  } else if (roberta_brumadinho[i,11]=="N�o teve renda"){
    roberta_brumadinho[i,11]=0
  }
}

# Transformar colunas caracter para num�rico

numerico <- c(5,7,11,12,14,16,18,21,22,24,26,27,29,31,41)

for (i in 1:length(numerico)) {
  roberta_brumadinho[,numerico[i]]=as.numeric(roberta_brumadinho[,numerico[i]])
}

# Trocar dado na vari�vel COR/RA�A

for (i in 1:3080) {
  if (roberta_brumadinho$COR.RA�A[i] == "Amarela (origem oriental, japonesa, chinesa, coreana etc.)") {
    roberta_brumadinho$COR.RA�A[i] = "Amarela"
  }
}


# Trocar nome dos estratos geogr�ficos do banco Domicilios

estrato <- unique_roberta_brumadinho

# Corrego do Feijao/Pires/Parque da Cachoeira >> Estrato 1
# Tejuco >> Estrato 2
# Setores amostrados >> Estrato 3

for (i in 1:1446) {
  if (estrato$ESTRATO.GEOGRAFICO[i] == "Corrego do Feijao/Pires/Parque da Cachoeira") {
    estrato$ESTRATO.GEOGRAFICO[i] = "Estrato 1"
  }
  if (estrato$ESTRATO.GEOGRAFICO[i] == "Tejuco") {
    estrato$ESTRATO.GEOGRAFICO[i] = "Estrato 2"
  }
  if (estrato$ESTRATO.GEOGRAFICO[i] == "Setores amostrados") {
    estrato$ESTRATO.GEOGRAFICO[i] = "Estrato 3"
  }
}

# Trocar nome dos estratos geogr�ficos do banco Individuos

estrato2 <- roberta_brumadinho

# Corrego do Feijao/Pires/Parque da Cachoeira >> Estrato 1
# Tejuco >> Estrato 2
# Setores amostrados >> Estrato 3

for (i in 1:3080) {
  if (estrato2$ESTRATO.GEOGRAFICO[i] == "Corrego do Feijao/Pires/Parque da Cachoeira") {
    estrato2$ESTRATO.GEOGRAFICO[i] = "Estrato 1"
  }
  if (estrato2$ESTRATO.GEOGRAFICO[i] == "Tejuco") {
    estrato2$ESTRATO.GEOGRAFICO[i] = "Estrato 2"
  }
  if (estrato2$ESTRATO.GEOGRAFICO[i] == "Setores amostrados") {
    estrato2$ESTRATO.GEOGRAFICO[i] = "Estrato 3"
  }
}



# II- Estat�stica Descritiva:

# ISABELLE:

#Construindo uma tabela com percentual de IA  por indiv�duo e por domic�lio agrupado por Estrato Geogr�fico

tabela1 <- roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "Propor��o de Indiv�duos com IA" = round((sum(IA=="sim")/n()),2))

tabela2 <- unique_roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "Propor��o de Domic�lios com IA"= round(sum(IA=="sim")/n(),2))

tabela3 <- roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "N�mero de Habitantes" = n())

tabela4 <- unique_roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "N�mero de Domic�lios" = n())

propor��oIAd <- left_join(tabela1,tabela2, by = "ESTRATO.GEOGRAFICO")
propor��oIAd <- left_join(propor��oIAd,tabela3, by = "ESTRATO.GEOGRAFICO")
propor��oIAd <- left_join(propor��oIAd,tabela4, by = "ESTRATO.GEOGRAFICO")

propor��oIAd #tabela com percentual de IA  por indiv�duo e por domic�lio


#Descrever o consumo alimentar dos participante

#Indiv�duos

quantitativo <- c(7,11,12,14,16,18,21,22,24,27,29,31,41)
variaveis <- names(roberta_brumadinho[quantitativo])
tabelaIND <- data.frame(Vari�veis = variaveis, 
                        M�dia = rep(0,length(variaveis)), 
                        M�nimo = rep(0,length(variaveis)),
                        M�ximo = rep(0,length(variaveis)),
                        "Percentil 25" = rep(0,length(variaveis)),
                        "Percentil 50" = rep(0,length(variaveis)),
                        "Percentil 75" = rep(0,length(variaveis)),
                        "Desvio Padr�o" = rep(0,length(variaveis))
)

for (i in 1:length(quantitativo)) {
  tabelaIND[i,2:8] = round(c(mean(roberta_brumadinho[,quantitativo[i]], na.rm = T),
                             min(roberta_brumadinho[,quantitativo[i]], na.rm = T),
                             max(roberta_brumadinho[,quantitativo[i]], na.rm = T),
                             quantile(roberta_brumadinho[,quantitativo[i]], na.rm = T, 0.25),
                             quantile(roberta_brumadinho[,quantitativo[i]], na.rm = T, 0.50),
                             quantile(roberta_brumadinho[,quantitativo[i]], na.rm = T, 0.75),
                             sd(roberta_brumadinho[,quantitativo[i]], na.rm = T)), 2)
}

tabelaIND # Tabela resumo Indiv�duo

# Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por sexo

dado <- table(roberta_brumadinho$SEXO, roberta_brumadinho$IA) 
dado <- dado %>% 
  as.data.frame(dado) %>% 
  rename(SEXO = Var1, IA = Var2) %>% 
  group_by(SEXO) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(SEXO, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") + 
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por sexo",
       x = "Sexo", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5))

# Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por Cor/Ra�a

dado <- as.data.frame(table(roberta_brumadinho$COR.RA�A, roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(COR = Var1, IA = Var2) %>% 
  group_by(COR) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(COR, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") + 
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por Cor/Ra�a",
       x = "Cor/Ra�a", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))


# Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por Escolaridade

dado <- as.data.frame(table(roberta_brumadinho$ESCOLA, roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(ESCOLA = Var1, IA = Var2) %>% 
  group_by(ESCOLA) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(ESCOLA, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") + 
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por Escolaridade",
       x = "Escolaridade", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))


# Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por Estrato Geogr�fico

dado <- as.data.frame(table(roberta_brumadinho$ESTRATO.GEOGRAFICO, roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(ESTRATO = Var1, IA = Var2) %>% 
  group_by(ESTRATO) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(ESTRATO, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") + 
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de indiv�duos com Inseguran�a Alimentar (IA) por Estrato Geogr�fico",
       x = "Estrato Geogr�fico", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#JOAQUIM 

# Quantidade de domic�lios com Inseguran�a Alimentar (IA) por Estrato Geogr�fico

dado <- as.data.frame(table(unique_roberta_brumadinho$ESTRATO.GEOGRAFICO, unique_roberta_brumadinho$IA)) 
dado <- dado %>%
  rename(ESTRATO = Var1, IA = Var2) %>%
  group_by(ESTRATO) %>%
  mutate(Relativo = round(Freq/sum(Freq), 2))

dado %>% 
  ggplot(aes(x=reorder(ESTRATO, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") + 
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de domic�lios com Inseguran�a Alimentar (IA) por Estrato Geogr�fico",
       x = "Estrato Geogr�fico", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))


##Grafico e tabela com estatisticas de pessoas por domicilio

#tabela1J <- unique_roberta_brumadinho %>%
#  group_by(PESSOAS.NO.DOMICILIO) %>%
#  summarise(
#    N�mero_de_Domic�lios = n()
#  )
#tabela1J <- unique_roberta_brumadinho %>% summarise(Media = mean(PESSOAS.NO.DOMICILIO),
#                                                    Mediana = median(PESSOAS.NO.DOMICILIO),
#                                                    DesvioPadrao = sd(PESSOAS.NO.DOMICILIO))
#tabela1J <- merge(tabela1J,tabela2J)
#tabela1J


dado <- as.data.frame(table(unique_roberta_brumadinho$PESSOAS.NO.DOMICILIO, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(PESSOAS = Var1, IA = Var2) %>% 
  group_by(PESSOAS) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(PESSOAS, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  #geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de domic�lios com Inseguran�a Alimentar (IA) por Estrato Geogr�fico",
       x = "Pessoas no domicilio", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#hist(unique_roberta_brumadinho$PESSOAS.NO.DOMICILIO)#fazer estatisticas (media, mediana, etc) e fazer grafico com IA


##Grafico e tabela da Inseguranca Alimentar por Idade

#tabela2J <- unique_roberta_brumadinho %>% summarise(Media = mean(IDADE),
#                                                Mediana = median(IDADE),
#                                                DesvioPadrao = sd(IDADE))
#tabela2J

ggplot(roberta_brumadinho, aes(x = IDADE, y = IA, fill = IA)) +
  geom_density_ridges(alpha=0.75) +
  theme_light() + 
  labs(title = "",
       x = "Idade", 
       y = "Inseguran�a Alimentar") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#hist(roberta_brumadinho$IDADE)#fazer estatisticas (media, mediana, etc) e fazer grafico com IA

#roberta_brumadinho$COR.RA�A <- as.factor(roberta_brumadinho$COR.RA�A)

#ggplot(roberta_brumadinho, aes(x = IA, y=COR.RA�A,fill=COR.RA�A)) +
#  geom_bar(stat="identity") +
#   theme(axis.ticks.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
#
#hist(unique_roberta_brumadinho$RENDA.PER.CAPITA)#fazer estatisticas (media, mediana, etc) fazer grafico com IA


##Tabela e grafico comparando a inseguranca alimentar pela renda per capita

#tabela3J <- unique_roberta_brumadinho %>% summarise(Media = mean(RENDA.PER.CAPITA,na.rm=TRUE),
#                                                    Mediana = median(RENDA.PER.CAPITA,na.rm=TRUE),
#                                                    DesvioPadrao = sd(RENDA.PER.CAPITA,na.rm=TRUE))
#tabela3J

ggplot(unique_roberta_brumadinho, aes(x = RENDA.PER.CAPITA, y = IA, fill = IA)) +
  geom_density_ridges(alpha=0.75) +
  theme_light() + 
  labs(title = "Distribui��o de Inseguran�a Alimentar por Renda",
       x = "Renda per capita", 
       y = "Inseguran�a Alimentar") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#retirando os 4 valores discrepantes
temp <- unique_roberta_brumadinho
temp$RENDA.PER.CAPITA[1298] <- mean(unique_roberta_brumadinho$RENDA.PER.CAPITA,na.rm=T)
temp$RENDA.PER.CAPITA[845] <- mean(unique_roberta_brumadinho$RENDA.PER.CAPITA,na.rm=T)
temp$RENDA.PER.CAPITA[850] <- mean(unique_roberta_brumadinho$RENDA.PER.CAPITA,na.rm=T)
temp$RENDA.PER.CAPITA[1392] <- mean(unique_roberta_brumadinho$RENDA.PER.CAPITA,na.rm=T)

ggplot(temp, aes(x = RENDA.PER.CAPITA, y = IA, fill = IA)) +
  geom_density_ridges(alpha=0.75) +
  theme_light() + 
  labs(title = "Distribui��o de Inseguran�a Alimentar por Renda",
       x = "Renda per capita", 
       y = "Inseguran�a Alimentar") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

##fazer heatmap com caracteristicas alimentares X, por IA e Sexo Y


##heatmaps

caracteristicas_alimentares <- roberta_brumadinho %>% select(SEXO, IA, luz.eletrica, CD03, fossa, mesma.casa.antes.rompimento, FLV_COZIDO, FLV_CRU, FRUTAS,
                                                             SUCO.DE.FRUTA, FEIJ�O, CARNE.VERMELHA, mudan�a.status.moradia,animais,plantio,
                                                             FRANGO.GALINHA, REFRIGERANTE.SUCO.ARTIFICIAL, status.moradia,
                                                             LEITE, DOCES, ESTRATO.GEOGRAFICO)

melt_caracteristicas <- melt(caracteristicas_alimentares)

head(melt_caracteristicas)

#melt_caracteristicas <- ddply(melt_caracteristicas, .(variable), transform, rescale = rescale(value))

ggplot(melt_caracteristicas, aes(luz.eletrica,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = 'white', high = "red")

ggplot(melt_caracteristicas, aes(ESTRATO.GEOGRAFICO,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20))

ggplot(melt_caracteristicas, aes(status.moradia,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red") + 
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20))

ggplot(melt_caracteristicas, aes(IA,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red")

ggplot(melt_caracteristicas, aes(SEXO,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red")

ggplot(melt_caracteristicas, aes(CD03,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red") + 
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -10))

ggplot(melt_caracteristicas, aes(fossa,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20))

ggplot(melt_caracteristicas, aes(mesma.casa.antes.rompimento,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red")

ggplot(melt_caracteristicas, aes(mudan�a.status.moradia,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red")

ggplot(melt_caracteristicas, aes(plantio,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red")

ggplot(melt_caracteristicas, aes(animais,variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  theme_light() +
  scale_fill_gradient2(low = "white", high = "red")

#IA por CD03

dado <- as.data.frame(table(unique_roberta_brumadinho$CD03, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(CD03 = Var1, IA = Var2) %>% 
  group_by(CD03) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(CD03, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "CD03", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por fossa

dado <- as.data.frame(table(unique_roberta_brumadinho$fossa, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(fossa = Var1, IA = Var2) %>% 
  group_by(fossa) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(fossa, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Fossa", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por lixo

dado <- as.data.frame(table(unique_roberta_brumadinho$luz.eletrica, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(luz.eletrica = Var1, IA = Var2) %>% 
  group_by(luz.eletrica) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(luz.eletrica, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Luz eletrica", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por luz agua canalisada

dado <- as.data.frame(table(unique_roberta_brumadinho$agua.canalisada, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(agua.canalisada = Var1, IA = Var2) %>% 
  group_by(agua.canalisada) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(agua.canalisada, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Agua canalisada", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por agua para beber

dado <- as.data.frame(table(unique_roberta_brumadinho$agua.para.beber, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(agua.para.beber = Var1, IA = Var2) %>% 
  group_by(agua.para.beber) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(agua.para.beber, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Agua para beber", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por tratamento agua para beber

dado <- as.data.frame(table(unique_roberta_brumadinho$tratamento.agua.beber, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(tratamento.agua.beber = Var1, IA = Var2) %>% 
  group_by(tratamento.agua.beber) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(tratamento.agua.beber, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Tratamento agua para beber", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por mesma casa antes rompimento

dado <- as.data.frame(table(unique_roberta_brumadinho$mesma.casa.antes.rompimento, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(mesma.casa.antes.rompimento = Var1, IA = Var2) %>% 
  group_by(mesma.casa.antes.rompimento) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(mesma.casa.antes.rompimento, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Mesma casa antes rompimento", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por status moradia

dado <- as.data.frame(table(unique_roberta_brumadinho$status.moradia, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(status.moradia = Var1, IA = Var2) %>% 
  group_by(status.moradia) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(status.moradia, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Status moradia", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por mudanca status moradia

dado <- as.data.frame(table(unique_roberta_brumadinho$mudan�a.status.moradia, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(mudan�a.status.moradia = Var1, IA = Var2) %>% 
  group_by(mudan�a.status.moradia) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(mudan�a.status.moradia, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "mudanca Status moradia", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por desepesa geral

dado <- as.data.frame(table(unique_roberta_brumadinho$despesa.geral, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(despesa.geral = Var1, IA = Var2) %>% 
  group_by(despesa.geral) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(despesa.geral, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Despesa geral", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por plantio

dado <- as.data.frame(table(unique_roberta_brumadinho$plantio, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(plantio = Var1, IA = Var2) %>% 
  group_by(plantio) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(plantio, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Plantio", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por animal

dado <- as.data.frame(table(unique_roberta_brumadinho$animais, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(animais = Var1, IA = Var2) %>% 
  group_by(animais) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(animais, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "Animais", 
       y = "N�mero de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))



## Analise bivariada


quantitativo <- c(12,14,16,18,21,22,24,26,27,29,31) #colunas do banco que s�o de consumo alimentar
variaveis <- names(roberta_brumadinho[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise1 <- data.frame(Vari�veis = variaveis, 
                       "M�dia com IA" = rep(0,length(variaveis)), 
                       "M�dia sem IA" = rep(0,length(variaveis)),
                       "p-valor teste t" = rep(0,length(variaveis))
)


mediaComIA <- roberta_brumadinho %>%  #filtrando o banco com indiv�duos que possuem insegura�a alimentar
  filter(IA=="sim")

mediaSemIA <- roberta_brumadinho %>%  #filtrando o banco com indiv�duos que N�O possuem insegura�a alimentar
  filter(IA=="n�o")


for (i in 1:length(quantitativo)) {
  analise1[i,2] = round(mean(mediaComIA[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise1[i,3] = round(mean(mediaSemIA[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise1[i,4] = format(t.test(mediaComIA[,quantitativo[i]], mediaSemIA[,quantitativo[i]])[3], scientific = F)
}

analise1[,4] <- as.numeric(analise1[,4]) %>% 
  round(4) 

analise1 #teste t aplicado com a hip�tese:
# H0: a diferen�a das m�dias entre os grupos com IA e sem IA para o consumo alimentar � igual a 0 
# H1: a diferen�a das m�dias entre os grupos com IA e sem IA para o consumo alimentar � diferente de 0 

quantitativo <- c(12,14,16,18,21,22,24,26,27,29,31) #colunas do banco que s�o de consumo alimentar
variaveis <- names(roberta_brumadinho[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise2 <- data.frame(Vari�veis = variaveis, 
                       "M�dia Feminino" = rep(0,length(variaveis)), 
                       "M�dia Masculino" = rep(0,length(variaveis)),
                       "p-valor teste t" = rep(0,length(variaveis))
)


mediaFem <- roberta_brumadinho %>%  #filtrando o banco com indiv�duos sexo feminino
  filter(SEXO=="Feminino")

mediaMasc <- roberta_brumadinho %>%  #filtrando o banco com indiv�duos sexo masculino
  filter(SEXO=="Masculino")


for (i in 1:length(quantitativo)) {
  analise2[i,2] = round(mean(mediaFem[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise2[i,3] = round(mean(mediaMasc[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise2[i,4] = format(t.test(mediaFem[,quantitativo[i]], mediaMasc[,quantitativo[i]])[3], scientific = F)
}

analise2[,4] <- as.numeric(analise2[,4]) %>% 
  round(4) 

#teste t aplicado com a hip�tese:
# H0: a diferen�a das m�dias entre os sexos feminino e masculino para o consumo alimentar � igual a 0 
# H1: a diferen�a das m�dias entre os sexos feminino e masculino para o consumo alimentar � diferente de 0 


quantitativo <- c(5,7,41) #colunas do banco que est�o vari�veis de individuo exceto consumo alimentar
variaveis <- names(roberta_brumadinho[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise4 <- data.frame(Vari�veis = variaveis, 
                       "M�dia com IA" = rep(0,length(variaveis)), 
                       "M�dia sem IA" = rep(0,length(variaveis)),
                       "p-valor teste t" = rep(0,length(variaveis))
)


mediaComIA <- roberta_brumadinho %>%  #filtrando o banco com indiv�duos que possuem insegura�a alimentar
  filter(IA=="sim")

mediaSemIA <- roberta_brumadinho %>%  #filtrando o banco com indiv�duos que N�O possuem insegura�a alimentar
  filter(IA=="n�o")


for (i in 1:length(quantitativo)) {
  analise4[i,2] = round(mean(mediaComIA[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise4[i,3] = round(mean(mediaSemIA[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise4[i,4] = format(t.test(mediaComIA[,quantitativo[i]], mediaSemIA[,quantitativo[i]])[3], scientific = F)
}

analise4[,4] <- as.numeric(analise4[,4]) %>% 
  round(4) 

analise4 #teste t aplicado com a hip�tese:
# H0: a diferen�a das m�dias entre os grupos com IA e sem IA para as vari�veis � igual a 0 
# H1: a diferen�a das m�dias entre os grupos com IA e sem IA para as vari�veis � diferente de 0 

### Teste ANOVA para cada var�avel de consumo alimentar aplicada para os tr�s estratos geogr�ficos.

#Hip�tese aplicada para todas as ANOVA's feitas abaixo:

#H0: m�dia Estrato 1 = m�dia Estrato 2 = m�dia Estrato 3
#H1: Caso contr�rio

quantitativo <- c(12,14,16,18,21,22,24,26,27,29,31) #colunas do banco que s�o de consumo alimentar
variaveis <- names(estrato2[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise3 <- data.frame(Vari�veis = variaveis, 
                       "M�dia Estrato 1" = rep(0,length(variaveis)), 
                       "M�dia Estrato 2" = rep(0,length(variaveis)),
                       "M�dia Estrato 3" = rep(0,length(variaveis)),
                       "p-valor" = rep(0,length(variaveis))
)


media1 <- estrato2 %>%  #filtrando o banco com indiv�duos de cada estrato
  filter(ESTRATO.GEOGRAFICO=="Estrato 1")

media2 <- estrato2 %>%  
  filter(ESTRATO.GEOGRAFICO=="Estrato 2")

media3 <- estrato2 %>%  
  filter(ESTRATO.GEOGRAFICO=="Estrato 3")


for (i in 1:length(quantitativo)) {
  analise3[i,2] = round(mean(media1[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise3[i,3] = round(mean(media2[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise3[i,4] = round(mean(media3[,quantitativo[i]], na.rm = T),2)
}

for (i in 1:length(quantitativo)) {
  analise3[i,5] = format(summary(aov(estrato2[,quantitativo[i]] ~ 
                                       estrato2[,3]))[[1]]$Pr[1],
                         scientific = F)
}

analise3[,5] <- as.numeric(analise3[,5]) %>% 
  round(4)


#for (i in 1:length(quantitativo)) {
#print(TukeyHSD(aov(estrato2[,quantitativo[i]] ~ estrato2[,3])))
#}

combinacao <- c("2 e 1, 2 e 3", "2 e 1, 2 e 3", "2 e 1, 2 e 3", "1 e 2",
                "1 e 2, 1 e 3", "3 e 1, 3 e 2", "1 e 2, 1 e 3, 2 e 3", NA,
                "1 e 2, 1 e 3, 2 e 3", "2 e 1, 2 e 2", "3 e 1, 3 e 2")

analise3 <- cbind(analise3, combinacao)
names(analise3)[5] <- c("Combina��o m�ltipla")
analise3


### An�lise Qui-quadrado

# Teste Qui-quadrado: Vari�vel domic�lio vs IA

dado <- table(unique_roberta_brumadinho$PESSOAS.NO.DOMICILIO, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("N�mero de pessoas no domic�lio", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$CD03, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Revestimento do im�vel", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$fossa, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Fossa", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$lixo, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Lixo", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$agua.canalisada, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("�gua canalizada", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$agua.para.beber, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("�gua para beber", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$tratamento.agua.beber, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Tratamento da �gua para beber", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$mesma.casa.antes.rompimento, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mesma casa antes do rompimento", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$status.moradia, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Status da moradia", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$mudan�a.status.moradia, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mudan�a do status da moradia", "Inseguran�a Alimentar"))

dado <- table(unique_roberta_brumadinho$despesa.geral, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Despesa Geral", "Inseguran�a Alimentar")) 

dado <- table(unique_roberta_brumadinho$plantio, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Plantio", "Inseguran�a Alimentar")) 

dado <- table(unique_roberta_brumadinho$animais, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Animal", "Inseguran�a Alimentar")) 

# Teste Qui-quadrado: Vari�vel domic�lio vs Estrato geogr�fico

dado <- table(estrato$PESSOAS.NO.DOMICILIO, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("N�mero de pessoas no domic�lio", "Estrato geogr�fico"))

dado <- table(estrato$CD03, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Revestimento do im�vel", "Estrato geogr�fico"))

dado <- table(estrato$fossa, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Fossa", "Estrato geogr�fico"))

dado <- table(estrato$lixo, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Lixo", "Estrato geogr�fico"))

dado <- table(estrato$agua.canalisada, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("�gua canalizada", "Estrato geogr�fico"))

dado <- table(estrato$agua.para.beber, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("�gua para beber", "Estrato geogr�fico"))

dado <- table(estrato$tratamento.agua.beber, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Tratamento da �gua para beber", "Estrato geogr�fico"))

dado <- table(estrato$mesma.casa.antes.rompimento, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mesma casa antes do rompimento", "Estrato geogr�fico"))

dado <- table(estrato$status.moradia, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Status da moradia", "Estrato geogr�fico"))

dado <- table(estrato$mudan�a.status.moradia, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mudan�a do status da moradia", "Estrato geogr�fico"))

dado <- table(estrato$despesa.geral, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Despesa Geral", "Estrato geogr�fico")) 

dado <- table(estrato$plantio, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Plantio", "Estrato geogr�fico")) 

dado <- table(estrato$animais, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Animal", "Estrato geogr�fico")) 


# Teste Qui-quadrado: Estrato geogr�fico vs IA

dado <- table(estrato$IA, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Inseguran�a Alimentar", "Estrato geogr�fico"))


# Teste Qui-quadrado: Estrato geogr�fico vs IA

dado <- table(estrato2$SEXO, estrato2$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Sexo", "Estrato geogr�fico"))