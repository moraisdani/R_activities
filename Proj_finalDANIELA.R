# PROJETO FINAL - ESTATÍSTICA COMPUTACIONAL
# Carregando base de dados
getwd()
setwd("C:/Users/deaul/Desktop/CECDA POLI/7 Estatística Computacional")
projeto = read.csv2("PRSA_Data_Wanliu.csv", header = TRUE, sep = ",")
View(projeto)
str(projeto)

# Alterando tipos de variáveis
projeto$PM2.5 = as.numeric(as.character(projeto$PM2.5))
projeto$PM10 = as.numeric(as.character(projeto$PM10))
projeto$SO2 = as.numeric(as.character(projeto$SO2))
projeto$NO2 = as.numeric(as.character(projeto$NO2))
projeto$CO = as.numeric(as.character(projeto$CO))
projeto$O3 = as.numeric(as.character(projeto$O3))
projeto$TEMP = as.numeric(as.character(projeto$TEMP))
projeto$PRES = as.numeric(as.character(projeto$PRES))
projeto$DEWP = as.numeric(as.character(projeto$DEWP))
projeto$RAIN = as.numeric(as.character(projeto$RAIN))
projeto$WSPM = as.numeric(as.character(projeto$WSPM))

str(projeto)
summary(projeto)

# Retirada das colunas não numéricas
projeto$station = NULL
projeto$wd = NULL

# Retirada de linhas com missing
projeto2 = na.omit(projeto)
View(projeto2)

# Análise descritiva dos atributos
summary(projeto2$PM2.5)
boxplot(projeto2$PM2.5, main = "Atributo PM2,5", ylim = c(0, 800))
boxplot(projeto2$PM10, main = "Atributo PM10", ylim = c(0, 940))
boxplot(projeto2$SO2, main = "Atributo SO2", ylim = c(0, 205))
boxplot(projeto2$NO2, main = "Atributo NO2", ylim = c(0, 265))
boxplot(projeto2$CO, main = "Atributo CO2", ylim = c(0, 10000))
boxplot(projeto2$O3, main = "Atributo O3", ylim = c(0, 365))
boxplot(projeto2$TEMP, main = "Atributo TEMPERATURA", ylim = c(-20, 45))
boxplot(projeto2$PRES, main = "Atributo PRESSÃO", ylim = c(980, 1041))
boxplot(projeto2$DEWP, main = "Atributo Pto Orvalho", ylim = c(-35, 30))
boxplot(projeto2$RAIN, main = "Atributo Precipitação", ylim = c(0, 75))
boxplot(projeto2$WSPM, main = "Atributo Velocidade do vento - WSPM", ylim = c(0, 12))

projeto3 = projeto2
projeto3$TEMP = NULL
projeto3$DEWP = NULL

# Criando amostra de dados com 10% da base
dim(projeto2)
indice = sample(dim(projeto2)[1], 3073, replace = F)
amostra1 = projeto2[indice,]
dim(amostra1)
amostra1
summary(amostra1)

# Criando gráficos
ggcorr(amostra1, label = T)
M = cor(amostra1)
corrplot(M, method = "circle")
M
hist(amostra1$PM2.5, main = "Histograma de PM2,5", col = "lightblue", xlab = "Concentração PM2,5")
plot(amostra1$PM2.5, main = "Gráfico de dispersão de PM2,5", ylab = "Concentração de PM2,5", xlab = "")
hist(amostra1$PM10, main = "Histograma de PM10", col = "darkblue", xlab = "Concentração de PM10")
plot(amostra1$PM10, main = "Gráfico de dispersão de PM10", ylab = "Concentração de PM10", xlab = "")
hist(amostra1$CO, main = "Histograma de Monóxido de Carbono", col = "", xlab = "Concentração de CO")
plot(amostra1$CO, main = "Gráfico de dispersão de Monóxido de Carbono", ylab = "Concentração de CO", xlab = "")
hist(amostra1$DEWP, main = "Histograma de Ponto de Orvalho", xlab = "Ponto de Orvalho")
plot(amostra1$DEWP, main = "Gráfico de dispersão de Ponto de orvalho", ylab = "Concentração de DEWP", xlab = "")
hist(amostra1$SO, main = "Histograma de SO", col = "green", xlab = "Concentração de CO")
plot(amostra1$SO, main = "Gráfico de dispersão de SO", ylab = "Concentração de CO", xlab = "")
hist(amostra1$PRES, main = "Histograma da Pressão atmosférica", col = "green", xlab = "Pressão atmosférica")
plot(amostra1$PRES, main = "Gráfico de dispersão da Pressão atmosférica", ylab = "Pressão Atmosférica em hPa", xlab = "")
barplot(amostra1$PRES, main = "Gráfico de barras Pressão Atmosférica", ylab = "Pressão Atm em hPa")
hist(amostra1$TEMP, main = "Histograma da Temperatura", col = "green", xlab = "Temperatura - °C")
plot(amostra1$TEMP, main = "Gráfico de dispersão da Temperatura", ylab = "Temperatura", xlab = "")
barplot(amostra1$TEMP, main = "Gráfico de barras Temperatura", ylab = "Temperatura em °C")
plot(amostra1$NO2, main = "Gráfico de dispersão de NO2", ylab = "Concentração de NO2", xlab = "")
plot(amostra1$O3, main = "Gráfico de dispersão de Ozônio", ylab = "Concentração de O3", xlab = "")

boxplot(amostra1$PM2.5, main = "Atributo PM2,5", ylim = c(0, 800))
boxplot(amostra1$PM10, main = "Atributo PM10", ylim = c(0, 940))
boxplot(amostra1$NO2, main = "Atributo NO2", ylim = c(0, 265))
boxplot(amostra1$CO, main = "Atributo CO", ylim = c(0, 10000))
boxplot(amostra1$O3, main = "Atributo O3", ylim = c(0, 365))
boxplot(amostra1$TEMP, main = "Atributo TEMPERATURA", ylim = c(-20, 45))

# Regressão Linear simples dos atributos
regressoao1 = lm(PM2.5~PM10, data = amostra1)
cor(amostra1$PM2.5, amostra1$PM10) # 0,885
plot(amostra1$PM10, amostra1$PM2.5, xlab = "Particulado PM10", ylab = "Particulado PM2.5", main = "Regressão Linear")
abline(regressoao1, col= "red", lwd=2, lty=1)
summary(regressoao1)

regressoao1$fitted.values
mean(regressoao1$residuals)
sd(regressoao1$residuals)
hist(regressoao1$residuals, main = "Histograma de resíduos Regressão 1")
X = amostra1$PM2.5
Yestimado=regressoao1$coefficients[1] + regressoao1$coefficients[2]*X
regressoao1$coefficients[1]
regressoao1$coefficients[2]
plot(fitted(regressoao1),residuals(regressoao1),main = "Resíduos de valores ajustados - Regressão 1", xlab="Valores Ajustados",ylab="Resíduos")


regressoao2 = lm(PM2.5~CO, data = amostra1)
cor(amostra1$PM2.5, amostra1$CO)
plot(amostra1$CO, amostra1$PM2.5, xlab = "Monóxido de Carbono", ylab = "Particulado PM2.5", main = "Regressão Linear")
abline(regressoao2, col= "orange", lwd=2, lty=1)
summary(regressoao2)

regressoao2$fitted.values
mean(regressoao2$residuals)
sd(regressoao2$residuals)
hist(regressoao2$residuals, main = "Histograma de resíduos Regressão 2")
X = amostra1$PM2.5
Yestimado=regressoao2$coefficients[1] + regressoao2$coefficients[2]*X
regressoao2$coefficients[1]
regressoao2$coefficients[2]
plot(fitted(regressoao2),residuals(regressoao2),main = "Resíduos de valores ajustados - Regressão 2", xlab="Valores Ajustados",ylab="Resíduos")


regressoao3 = lm(O3~TEMP, data = amostra1)
cor(amostra1$O3, amostra1$TEMP)
plot(amostra1$TEMP, amostra1$O3, xlab = "Temperatura", ylab = "Ozônio", main = "Regressão Linear")
abline(regressoao3, col= "red", lwd=2, lty=1)
summary(regressoao3)

regressoao3$fitted.values
mean(regressoao3$residuals)
sd(regressoao3$residuals)
hist(regressoao3$residuals, main = "Histograma de resíduos Regressão 3")
Z = amostra1$O3
Yestimado=regressoao3$coefficients[1] + regressoao3$coefficients[2]*Z
regressoao3$coefficients[1]
regressoao3$coefficients[2]
plot(fitted(regressoao3),residuals(regressoao3),main = "Resíduos de valores ajustados - Regressão 3", xlab="Valores Ajustados",ylab="Resíduos")

regressoao4 = lm(PM2.5~TEMP, data = projeto2)
cor(projeto2$PM2.5, projeto2$TEMP)
plot(projeto2$TEMP, projeto2$PM2.5, xlab = "Temperatura", ylab = "Particulado PM2.5", main = "Regressão Linear")
abline(regressoao4, col= "red", lwd=2, lty=1)

# Coeficientes
a1=regressoao1$coefficients[1] 
print(a1)
b1=regressoao1$coefficients[2] 
print(b1)

a2=regressoao2$coefficients[1] 
print(a2)
b2=regressoao2$coefficients[2] 
print(b2)

a3=regressoao3$coefficients[1] 
print(a3)
b3=regressoao3$coefficients[2] 
print(b3)

a4=regressoao4$coefficients[1] 
print(a4)
b4=regressoao4$coefficients[2] 
print(b4)

# Vetor de Erros
amostra1$PM2.5 - regressoao1$fitted.values
regressoao1$residuals
hist(regressoao1$residuals, main="Histogram of Residuals",ylab="Residuals")
mean(regressoao1$residuals)
sd(regressoao1$residuals) 

amostra1$PM2.5 - regressoao2$fitted.values
regressoao2$residuals
hist(regressoao2$residuals, main="Histogram of Residuals",ylab="Residuals")
mean(regressoao2$residuals)
sd(regressoao2$residuals) 

amostra1$O3 - regressoao3$fitted.values
regressoao3$residuals
hist(regressoao3$residuals, main="Histogram of Residuals",ylab="Residuals")
mean(regressoao3$residuals)
sd(regressoao3$residuals) 

# valores ajustados
regressoao1$fitted.values
regressoao2$fitted.values
regressoao3$fitted.values

# Monte Carlo e HOLDOUT
n = dim(amostra1)[1]
MC=n
DadosTodos = NULL
ModeloAjustado1 = NULL
ErroAbsoluto1 = NULL
ErroMedioQuadratico1 = NULL
ValoresPreditos1 = NULL

# Inicio do LOOP de MONTE CARLO
for (a in 1:MC){
  
  #Obtencao do Vetor Aleatorio para escolha
  DadosTodos = c(1:n)
  ind = DadosTodos[a] 
  
  # Dividindo em Treino e Teste
  test.data = amostra1[ind,]
  Vetor2=c(0)
  Vetor2 = DadosTodos[DadosTodos != ind]
  train.data = amostra1[Vetor2,]
  
  # Construção do Modelo de Regressao
  modelo1 =lm(PM2.5 ~., data = train.data)
  modelo1
  
  # Calculo do Valor predito
  ValoresPreditos1 = predict(modelo1,newdata=data.frame(test.data))
  ValoresPreditos1
 
  # Métricas para Avaliar o modelo
  ModeloAjustado1[a] =  R2(ValoresPreditos1, test.data$PM2.5)
  ErroAbsoluto1[a] = MAE(ValoresPreditos1, test.data$PM2.5)
  ErroMedioQuadratico1 [a] = RMSE(ValoresPreditos1, test.data$PM2.5)  
  dem=0
  num=0
  num = (ValoresPreditos1 - mean(test.data$PM2.5))**2
  dem = (test.data$PM2.5 - mean(test.data$PM2.5)+1)**2  
  ModeloAjustado1[a] = (mean(num/dem))/100
}

# Média final dos erros após o MC
mean(ModeloAjustado1)
mean(ErroAbsoluto1)
mean(ErroMedioQuadratico1)

# MONTE CARLO E LEAVE-ONE-OUT
n = dim(amostra1)[1]
MC=n
DadosTodos = NULL
ValoresPreditos1 = NULL
ModeloAjustado1 = NULL
ErroAbsoluto1 = NULL
ErroMedioQuadratico1 = NULL

# Inicio do LOOP de MONTE CARLO
for (a in 1:MC){
  #Obtencao do Vetor Aleatorio para escolha
  DadosTodos = c(1:n)
  ind = DadosTodos[a] 
  
  # Dividindo em Treino e Teste
  test.data = amostra1[ind,]
  Vetor2=c(0)
  Vetor2 = DadosTodos[DadosTodos != ind]
  train.data = amostra1[Vetor2,]
  
  # Construção do Modelo de Regressao
  modelo1 =lm(PM2.5 ~., data = train.data)
  modelo1
  
  # Calculo do Valor predito
  ValoresPreditos1 = predict(modelo1,newdata=data.frame(test.data))
  
  # Métricas para Avaliar o modelo
  ModeloAjustado1[a] =  R2(ValoresPreditos1, test.data$PM2.5)
  ErroAbsoluto1[a] = MAE(ValoresPreditos1, test.data$PM2.5)
  ErroMedioQuadratico1[a] = RMSE(ValoresPreditos1, test.data$PM2.5)  
  dem=0
  num=0
  num = (ValoresPreditos1 - mean(test.data$PM2.5))**2
  dem = (test.data$Fertility - mean(test.data$PM2.5)+1)**2  
  ModeloAjustado1[a] = (mean(num/dem))/100
}

# Média final dos erros após o MC
mean(ModeloAjustado1)
mean(ErroAbsoluto1)
mean(ErroMedioQuadratico1)

# Teste de Normalidade
# Gráfico
hist(amostra1$PM2.5,col="lightblue", main="Histograma")

# Traçando a curva da normal
curve(dnorm, add=TRUE)

# Teste de Normalidade - Teste de aderência
shapiro.test(amostra1$PM2.5) # como o p-valor deu muito baixo, há fortíssimos indícios de que PM2,5 não é uma distribuição normal]
ks.test(amostra1$PM2.5,"pnorm",mean(amostra1$PM2.5),sd(amostra1$PM2.5))



