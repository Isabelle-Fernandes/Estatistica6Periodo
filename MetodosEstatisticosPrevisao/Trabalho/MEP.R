#######################################################
# Leitura da serie
#######################################################

Dados=scan('TempBH.txt') 
Temperatura=ts(Dados,start=2003,frequency=12)  


# Construindo serie reduzida para previsão
H=12	# Numero de previsoes
n=length(Temperatura)-H # Coloca em n o tamanho da serie menos 12 observacoes 
TempRed=ts(Temperatura[1:n],start=2003,frequency=12)	# Serie reduzida



#######################################################
# Identificacao de modelos
#######################################################


plot(TempRed)
#parece ser arima sem precisar fazer a diferença. Não possui crescimento ou decrescimento

par(mfrow=c(2,1))

acf(TempRed, lag.max = 60)
pacf(TempRed, lag.max = 60)

#ANALISANDO PARTE SIMPLES:
#primeiro chute começando pelo mais simples arma(1,1): tem descrescimento nos dois, 
#segundo: ma(2,0): tem dois picos ACF e descrescimento no PACF

#ANALISANDO PARTE SAZONAL:
#Olhando a ACF parece que os picos sanozonais está tendo um descrescimento lento. Mas, 
#no PACF nos picos sazonais o decrescimento não é lento, indicando que não precisa de difereça sanozal
#parece ser AR(1), 1 pico no PACF e descrecimento no ACF


#######################################################
# Estimacao de modelos
#######################################################

# Para fazer os testes para os coeficientes
#install.packages("lmtest")
require(lmtest)

M1 <- arima(TempRed, order = c(1,0,1), seasonal = list(order=c(1,0,0)))
M1
coeftest(M1)
AIC(M1)

M2 <- arima(TempRed, order = c(1,0,0), seasonal = list(order=c(1,0,0)))
M2
coeftest(M2)
AIC(M2)

M3 <- arima(TempRed, order = c(0,0,2), seasonal = list(order=c(1,0,0)))
M3
coeftest(M3)
AIC(M3) 

#sobrefixo a parte simples como AR()
M4 <- arima(TempRed, order = c(2,0,0), seasonal = list(order=c(1,0,0)))
M4
coeftest(M4)
AIC(M4) 

M5 <- arima(TempRed, order = c(1,0,0), seasonal = list(order=c(1,0,1)))
M5
coeftest(M5)
AIC(M5) 

M6 <- arima(TempRed, order = c(1,0,0), seasonal = list(order=c(2,0,0)))
M6
coeftest(M6)
AIC(M6)

M7 <- arima(TempRed, order = c(1,0,0), seasonal = list(order=c(2,0,1)))
M7
coeftest(M7)
AIC(M7)

M8 <- arima(TempRed, order = c(1,0,0), seasonal = list(order=c(1,0,2)))
M8
coeftest(M8)
AIC(M8)

#sobrefixo a parte simples como MA(2)
M9 <- arima(TempRed, order = c(0,0,1), seasonal = list(order=c(1,0,0)))
coeftest(M9)
AIC(M9) 

M10 <- arima(TempRed, order = c(0,0,3), seasonal = list(order=c(1,0,0)))
coeftest(M10)
AIC(M10)

M11 <- arima(TempRed, order = c(1,0,2), seasonal = list(order=c(1,0,0)))
coeftest(M11)
AIC(M11)

M12 <- arima(TempRed, order = c(0,0,2), seasonal = list(order=c(2,0,0)))
coeftest(M12)
AIC(M12)

M13 <- arima(TempRed, order = c(0,0,2), seasonal = list(order=c(1,0,1)))
coeftest(M13)
AIC(M13)

#M5 melhor modelos: melhor AIC e mais parcimonioso, mas nem sempre o melhor modelo no ajuste
#é o melhor para previsão. Logo, vamos guardar os três melhores modelos

#######################################################
# Sobrefixando o modelo
#######################################################

M1 <- arima(TempRed, order = c(1,0,0), seasonal = list(order=c(1,0,1)))
coeftest(M1)
AIC(M1)

M2 <- arima(TempRed, order = c(0,0,2), seasonal = list(order=c(1,0,0)))
coeftest(M2)
AIC(M2)

M3 <- arima(TempRed, order = c(0,0,2), seasonal = list(order=c(2,0,0)))
coeftest(M3)
AIC(M3)

#######################################################
# Analise de residuos
#######################################################

#verificar a normalidade, variança constante e independencia dos residuos

#M1
#olhar variaça constante
par(mfrow=c(1,2))
plot(M1$res)

#olhar normarlidade
shapiro.test(M1$res)
hist(M1$res)

#olhar independencia
par(mfrow=c(2,1))
acf(M1$res, lag.max = 60)
pacf(M1$res, lag.max = 60) 
#Não tem problema ter picos fora da parte simples e sazonal
Box.test(M1$res, lag=1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

#M2
par(mfrow=c(1,2))
plot(M2$res)

shapiro.test(M2$res)
hist(M2$res)

par(mfrow=c(2,1))
acf(M2$res, lag.max = 60)
pacf(M2$res, lag.max = 60) 


Box.test(M2$res, lag=1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

#M3
par(mfrow=c(1,2))
plot(M3$res)

shapiro.test(M3$res)
hist(M3$res)

par(mfrow=c(2,1))
acf(M3$res, lag.max = 60)
pacf(M3$res, lag.max = 60) 


Box.test(M3$res, lag=1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

#M2 e M3 furaram na independencia.


#####################################################
# PREVISÃO
#####################################################

# Calculando previsoes
install.packages("forecast")
require(forecast)

PrevM1 <- forecast(M1,H,level = c(95))

EQM1 <- ((Temperatura[n:(n+H-1)]- PrevM1$mean)^2)/H
sum(EQM1)


# Grafico da serie com previsoes e intervalos de previsao

Dados=c(TempRed,rep(NA,12))
TempPrev=ts(Dados,start=2003,frequency=12)
Previsto <- ts(rep(NA,n+12),start=2003,frequency=12)
LI <- ts(rep(NA,n+12),start=2003,frequency=12)
LS <- ts(rep(NA,n+12),start=2003,frequency=12)
for(i in 1:H){
  Previsto[n+12-H+i] <- Prev$mean[i]
  LI[n+12-H+i] <- Prev$lower[i]
  LS[n+12-H+i] <- Prev$upper[i]
}

par(mfrow=c(1,1))
plot(TempPrev,type='l',xlab='Ano',ylab='Temperatura ',ylim=c(15,30))
lines(Previsto, col='blue')
lines(LI, col='red')
lines(LS, col='red')
