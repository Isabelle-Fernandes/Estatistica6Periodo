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

#1º - Transformando as colunas IA em factors

roberta_brumadinho$IA <- as.factor(roberta_brumadinho$IA)

roberta_brumadinho$IA01 <- as.factor(roberta_brumadinho$IA01)
roberta_brumadinho$IA03 <- as.factor(roberta_brumadinho$IA03)
roberta_brumadinho$IA05 <- as.factor(roberta_brumadinho$IA05)
roberta_brumadinho$IA07 <- as.factor(roberta_brumadinho$IA07)
roberta_brumadinho$IA09 <- as.factor(roberta_brumadinho$IA09)
roberta_brumadinho$IA11 <- as.factor(roberta_brumadinho$IA11)
roberta_brumadinho$IA13 <- as.factor(roberta_brumadinho$IA13)
roberta_brumadinho$IA15 <- as.factor(roberta_brumadinho$IA15)

#2º - Atualizando a coluna IA com base no criterio solicitado

any_sim <- rowSums(roberta_brumadinho[, c("IA01", "IA03", "IA05", "IA07", "IA09", "IA11", "IA13", "IA15")] == "Sim") > 0
all_nao <- rowSums(roberta_brumadinho[, c("IA01", "IA03", "IA05", "IA07", "IA09", "IA11", "IA13", "IA15")] == "Não") == 8

roberta_brumadinho$IA <- ifelse(any_sim, "sim", ifelse(all_nao, "não", NA))

#3º Para calcular a porcentagem de domicilios em IA teremos que pegar somente um individuo
#por domicilio

unique_roberta_brumadinho <- roberta_brumadinho %>%
  distinct(ID.DOMICILIAR, .keep_all = TRUE)

#4º Trocar os caracteres das colunas (12,14,16,18,21,22,24,26,27,29,31) do banco para "caracter número"

coluna <- c(12,14,16,18,21,22,24,26,27,29,31) # selecionar somente as colunas do banco que referem quantos dias comem determinado alimento
valores <- unique(roberta_brumadinho[,14]) #os valores são os mesmos para todas as colunas, então peguei os valores da coluna 14
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
  } else if (roberta_brumadinho[i,11]=="Não sabe/ não respondeu"){
    roberta_brumadinho[i,11]=NA
  } else if (roberta_brumadinho[i,11]=="Não teve renda"){
    roberta_brumadinho[i,11]=0
  }
}

# Transformar colunas caracter para numérico

numerico <- c(5,7,11,12,14,16,18,21,22,24,26,27,29,31,41)

for (i in 1:length(numerico)) {
  roberta_brumadinho[,numerico[i]]=as.numeric(roberta_brumadinho[,numerico[i]])
}

# Trocar dado na variável COR/RAÇA

for (i in 1:3080) {
  if (roberta_brumadinho$COR.RAÇA[i] == "Amarela (origem oriental, japonesa, chinesa, coreana etc.)") {
    roberta_brumadinho$COR.RAÇA[i] = "Amarela"
  }
}


# Trocar nome dos estratos geográficos do banco Domicilios

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

# Trocar nome dos estratos geográficos do banco Individuos

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



# II- Estatística Descritiva:

# ISABELLE:

#Construindo uma tabela com percentual de IA  por indivíduo e por domicílio agrupado por Estrato Geográfico

tabela1 <- roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "Proporção de Indivíduos com IA" = round((sum(IA=="sim")/n()),2))

tabela2 <- unique_roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "Proporção de Domicílios com IA"= round(sum(IA=="sim")/n(),2))

tabela3 <- roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "Número de Habitantes" = n())

tabela4 <- unique_roberta_brumadinho %>% 
  group_by(ESTRATO.GEOGRAFICO) %>% 
  summarise(
    "Número de Domicílios" = n())

proporçãoIAd <- left_join(tabela1,tabela2, by = "ESTRATO.GEOGRAFICO")
proporçãoIAd <- left_join(proporçãoIAd,tabela3, by = "ESTRATO.GEOGRAFICO")
proporçãoIAd <- left_join(proporçãoIAd,tabela4, by = "ESTRATO.GEOGRAFICO")

proporçãoIAd #tabela com percentual de IA  por indivíduo e por domicílio


#Descrever o consumo alimentar dos participante

#Indivíduos

quantitativo <- c(7,11,12,14,16,18,21,22,24,27,29,31,41)
variaveis <- names(roberta_brumadinho[quantitativo])
tabelaIND <- data.frame(Variáveis = variaveis, 
                        Média = rep(0,length(variaveis)), 
                        Mínimo = rep(0,length(variaveis)),
                        Máximo = rep(0,length(variaveis)),
                        "Percentil 25" = rep(0,length(variaveis)),
                        "Percentil 50" = rep(0,length(variaveis)),
                        "Percentil 75" = rep(0,length(variaveis)),
                        "Desvio Padrão" = rep(0,length(variaveis))
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

tabelaIND # Tabela resumo Indivíduo

# Quantidade de indivíduos com Insegurança Alimentar (IA) por sexo

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
  labs(title = "Quantidade de indivíduos com Insegurança Alimentar (IA) por sexo",
       x = "Sexo", 
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5))

# Quantidade de indivíduos com Insegurança Alimentar (IA) por Cor/Raça

dado <- as.data.frame(table(roberta_brumadinho$COR.RAÇA, roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(COR = Var1, IA = Var2) %>% 
  group_by(COR) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(COR, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") + 
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "Quantidade de indivíduos com Insegurança Alimentar (IA) por Cor/Raça",
       x = "Cor/Raça", 
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))


# Quantidade de indivíduos com Insegurança Alimentar (IA) por Escolaridade

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
  labs(title = "Quantidade de indivíduos com Insegurança Alimentar (IA) por Escolaridade",
       x = "Escolaridade", 
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))


# Quantidade de indivíduos com Insegurança Alimentar (IA) por Estrato Geográfico

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
  labs(title = "Quantidade de indivíduos com Insegurança Alimentar (IA) por Estrato Geográfico",
       x = "Estrato Geográfico", 
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#JOAQUIM 

# Quantidade de domicílios com Insegurança Alimentar (IA) por Estrato Geográfico

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
  labs(title = "Quantidade de domicílios com Insegurança Alimentar (IA) por Estrato Geográfico",
       x = "Estrato Geográfico", 
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))


##Grafico e tabela com estatisticas de pessoas por domicilio

#tabela1J <- unique_roberta_brumadinho %>%
#  group_by(PESSOAS.NO.DOMICILIO) %>%
#  summarise(
#    Número_de_Domicílios = n()
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
  labs(title = "Quantidade de domicílios com Insegurança Alimentar (IA) por Estrato Geográfico",
       x = "Pessoas no domicilio", 
       y = "Número de IA") +
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
       y = "Insegurança Alimentar") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#hist(roberta_brumadinho$IDADE)#fazer estatisticas (media, mediana, etc) e fazer grafico com IA

#roberta_brumadinho$COR.RAÇA <- as.factor(roberta_brumadinho$COR.RAÇA)

#ggplot(roberta_brumadinho, aes(x = IA, y=COR.RAÇA,fill=COR.RAÇA)) +
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
  labs(title = "Distribuição de Insegurança Alimentar por Renda",
       x = "Renda per capita", 
       y = "Insegurança Alimentar") +
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
  labs(title = "Distribuição de Insegurança Alimentar por Renda",
       x = "Renda per capita", 
       y = "Insegurança Alimentar") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

##fazer heatmap com caracteristicas alimentares X, por IA e Sexo Y


##heatmaps

caracteristicas_alimentares <- roberta_brumadinho %>% select(SEXO, IA, luz.eletrica, CD03, fossa, mesma.casa.antes.rompimento, FLV_COZIDO, FLV_CRU, FRUTAS,
                                                             SUCO.DE.FRUTA, FEIJÃO, CARNE.VERMELHA, mudança.status.moradia,animais,plantio,
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

ggplot(melt_caracteristicas, aes(mudança.status.moradia,variable)) +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))

#IA por mudanca status moradia

dado <- as.data.frame(table(unique_roberta_brumadinho$mudança.status.moradia, unique_roberta_brumadinho$IA)) 
dado <- dado %>% 
  rename(mudança.status.moradia = Var1, IA = Var2) %>% 
  group_by(mudança.status.moradia) %>% 
  mutate(Relativo = round(Freq/sum(Freq),2))

dado %>% 
  ggplot(aes(x=reorder(mudança.status.moradia, Freq), y = Freq, fill = IA, label = Relativo)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_label(position = position_dodge(width = 1)) +
  theme_light() +
  labs(title = "",
       x = "mudanca Status moradia", 
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
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
       y = "Número de IA") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.5))



## Analise bivariada


quantitativo <- c(12,14,16,18,21,22,24,26,27,29,31) #colunas do banco que são de consumo alimentar
variaveis <- names(roberta_brumadinho[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise1 <- data.frame(Variáveis = variaveis, 
                       "Média com IA" = rep(0,length(variaveis)), 
                       "Média sem IA" = rep(0,length(variaveis)),
                       "p-valor teste t" = rep(0,length(variaveis))
)


mediaComIA <- roberta_brumadinho %>%  #filtrando o banco com indivíduos que possuem inseguraça alimentar
  filter(IA=="sim")

mediaSemIA <- roberta_brumadinho %>%  #filtrando o banco com indivíduos que NÃO possuem inseguraça alimentar
  filter(IA=="não")


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

analise1 #teste t aplicado com a hipótese:
# H0: a diferença das médias entre os grupos com IA e sem IA para o consumo alimentar é igual a 0 
# H1: a diferença das médias entre os grupos com IA e sem IA para o consumo alimentar é diferente de 0 

quantitativo <- c(12,14,16,18,21,22,24,26,27,29,31) #colunas do banco que são de consumo alimentar
variaveis <- names(roberta_brumadinho[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise2 <- data.frame(Variáveis = variaveis, 
                       "Média Feminino" = rep(0,length(variaveis)), 
                       "Média Masculino" = rep(0,length(variaveis)),
                       "p-valor teste t" = rep(0,length(variaveis))
)


mediaFem <- roberta_brumadinho %>%  #filtrando o banco com indivíduos sexo feminino
  filter(SEXO=="Feminino")

mediaMasc <- roberta_brumadinho %>%  #filtrando o banco com indivíduos sexo masculino
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

#teste t aplicado com a hipótese:
# H0: a diferença das médias entre os sexos feminino e masculino para o consumo alimentar é igual a 0 
# H1: a diferença das médias entre os sexos feminino e masculino para o consumo alimentar é diferente de 0 


quantitativo <- c(5,7,41) #colunas do banco que estão variáveis de individuo exceto consumo alimentar
variaveis <- names(roberta_brumadinho[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise4 <- data.frame(Variáveis = variaveis, 
                       "Média com IA" = rep(0,length(variaveis)), 
                       "Média sem IA" = rep(0,length(variaveis)),
                       "p-valor teste t" = rep(0,length(variaveis))
)


mediaComIA <- roberta_brumadinho %>%  #filtrando o banco com indivíduos que possuem inseguraça alimentar
  filter(IA=="sim")

mediaSemIA <- roberta_brumadinho %>%  #filtrando o banco com indivíduos que NÃO possuem inseguraça alimentar
  filter(IA=="não")


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

analise4 #teste t aplicado com a hipótese:
# H0: a diferença das médias entre os grupos com IA e sem IA para as variáveis é igual a 0 
# H1: a diferença das médias entre os grupos com IA e sem IA para as variáveis é diferente de 0 

### Teste ANOVA para cada varíavel de consumo alimentar aplicada para os três estratos geográficos.

#Hipótese aplicada para todas as ANOVA's feitas abaixo:

#H0: média Estrato 1 = média Estrato 2 = média Estrato 3
#H1: Caso contrário

quantitativo <- c(12,14,16,18,21,22,24,26,27,29,31) #colunas do banco que são de consumo alimentar
variaveis <- names(estrato2[quantitativo]) #nomes das colunas do banco de consumo alimentar
analise3 <- data.frame(Variáveis = variaveis, 
                       "Média Estrato 1" = rep(0,length(variaveis)), 
                       "Média Estrato 2" = rep(0,length(variaveis)),
                       "Média Estrato 3" = rep(0,length(variaveis)),
                       "p-valor" = rep(0,length(variaveis))
)


media1 <- estrato2 %>%  #filtrando o banco com indivíduos de cada estrato
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
names(analise3)[5] <- c("Combinação múltipla")
analise3


### Análise Qui-quadrado

# Teste Qui-quadrado: Variável domicílio vs IA

dado <- table(unique_roberta_brumadinho$PESSOAS.NO.DOMICILIO, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Número de pessoas no domicílio", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$CD03, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Revestimento do imóvel", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$fossa, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Fossa", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$lixo, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Lixo", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$agua.canalisada, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Água canalizada", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$agua.para.beber, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Água para beber", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$tratamento.agua.beber, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Tratamento da água para beber", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$mesma.casa.antes.rompimento, unique_roberta_brumadinho$IA)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mesma casa antes do rompimento", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$status.moradia, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Status da moradia", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$mudança.status.moradia, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mudança do status da moradia", "Insegurança Alimentar"))

dado <- table(unique_roberta_brumadinho$despesa.geral, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Despesa Geral", "Insegurança Alimentar")) 

dado <- table(unique_roberta_brumadinho$plantio, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Plantio", "Insegurança Alimentar")) 

dado <- table(unique_roberta_brumadinho$animais, unique_roberta_brumadinho$IA) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Animal", "Insegurança Alimentar")) 

# Teste Qui-quadrado: Variável domicílio vs Estrato geográfico

dado <- table(estrato$PESSOAS.NO.DOMICILIO, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Número de pessoas no domicílio", "Estrato geográfico"))

dado <- table(estrato$CD03, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Revestimento do imóvel", "Estrato geográfico"))

dado <- table(estrato$fossa, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Fossa", "Estrato geográfico"))

dado <- table(estrato$lixo, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Lixo", "Estrato geográfico"))

dado <- table(estrato$agua.canalisada, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Água canalizada", "Estrato geográfico"))

dado <- table(estrato$agua.para.beber, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Água para beber", "Estrato geográfico"))

dado <- table(estrato$tratamento.agua.beber, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Tratamento da água para beber", "Estrato geográfico"))

dado <- table(estrato$mesma.casa.antes.rompimento, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mesma casa antes do rompimento", "Estrato geográfico"))

dado <- table(estrato$status.moradia, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Status da moradia", "Estrato geográfico"))

dado <- table(estrato$mudança.status.moradia, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Mudança do status da moradia", "Estrato geográfico"))

dado <- table(estrato$despesa.geral, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Despesa Geral", "Estrato geográfico")) 

dado <- table(estrato$plantio, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Plantio", "Estrato geográfico")) 

dado <- table(estrato$animais, estrato$ESTRATO.GEOGRAFICO) 
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Animal", "Estrato geográfico")) 


# Teste Qui-quadrado: Estrato geográfico vs IA

dado <- table(estrato$IA, estrato$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Insegurança Alimentar", "Estrato geográfico"))


# Teste Qui-quadrado: Estrato geográfico vs IA

dado <- table(estrato2$SEXO, estrato2$ESTRATO.GEOGRAFICO)
CrossTable(dado, digits = 2, expected = T, fisher = T, format="SAS",
           prop.chisq = F, dnn=c("Sexo", "Estrato geográfico"))