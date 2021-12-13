# ATIVIDADE 02 - Análise da base de dados "economics"

rm(list = ls())

# Carregando abase de dados do pacote ggplot2
data("economics")

# Visualização do cabeçalho da base de dados
names(economics)

# Visualização dos dados da base de dados
View(economics)

# Análise descritiva inicial da base de dados
summary(economics,na.rm = TRUE)


boxplot(economics)
# Análise da variável date
length(economics$date)
mode(economics$date)

MedPosi = summary(economics)
MedPosiDate = MedPosi[,1]
MedPosiDate

mean(economics$date,na.rm = TRUE)
median(economics$date,na.rm = TRUE)

percentis = c(.09,.25,.05,.69)
perc = quantile(economics$date,na.rm = TRUE, percentis)
print(perc)

ampl=range(economics$date,na.rm = TRUE)
print(ampl)

sd(economics$date,na.rm = TRUE)
var(economics$date,na.rm = TRUE)
CVdate=(sd(economics$date,na.rm = TRUE)/mean(economics$date,na.rm = TRUE))

# Análise da variável pce
length(economics$pce)
mode(economics$pce)

MedPosi = summary(economics)
MedPosiPce = MedPosi[,2]
MedPosiPce

mean(economics$pce,na.rm = TRUE)
median(economics$pce,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(economics$pce,na.rm = TRUE, percentis)
print(perc)

ampl=range(economics$pce,na.rm = TRUE)
print(ampl)

var(economics$pce,na.rm = TRUE)
sd(economics$pce,na.rm = TRUE)
CVpce=(sd(economics$pce,na.rm = TRUE)/mean(economics$pce,na.rm = TRUE))
print(CVpce)

# Análise da variável pop
length(economics$pop)
mode(economics$pop)

MedPosi = summary(economics)
MedPosiPop = MedPosi[,3]
MedPosiPop

mean(economics$pop,na.rm = TRUE)
median(economics$pop,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(economics$pop,na.rm = TRUE, percentis)
print(perc)

ampl=range(economics$pop,na.rm = TRUE)
print(ampl)

var(economics$pop,na.rm = TRUE)
sd(economics$pop,na.rm = TRUE)
CVpop=(sd(economics$pop,na.rm = TRUE)/mean(economics$pop,na.rm = TRUE))
print(CVpop)

# Análise da variável psavert
length(economics$psavert)
mode(economics$psavert)

MedPosi = summary(economics)
MedPosiPsavert = MedPosi[,4]
MedPosiPsavert

mean(economics$psavert,na.rm = TRUE)
median(economics$psavert,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(economics$psavert,na.rm = TRUE, percentis)
print(perc)

ampl=range(economics$psavert,na.rm = TRUE)
print(ampl)

var(economics$psavert,na.rm = TRUE)
sd(economics$psavert,na.rm = TRUE)
CVPsavert=(sd(economics$psavert,na.rm = TRUE)/mean(economics$psavert,na.rm = TRUE))
print(CVPsavert)

# Análise da variável uempmed
length(economics$uempmed)
mode(economics$uempmed)

MedPosi = summary(economics)
MedPosiuempmed = MedPosi[,5]
MedPosiuempmed

mean(economics$uempmed,na.rm = TRUE)
median(economics$uempmed,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(economics$uempmed,na.rm = TRUE, percentis)
print(perc)

ampl=range(economics$uempmed,na.rm = TRUE)
print(ampl)

var(economics$uempmed,na.rm = TRUE)
sd(economics$uempmed,na.rm = TRUE)
CVuempmed=(sd(economics$uempmed,na.rm = TRUE)/mean(economics$uempmed,na.rm = TRUE))
print(CVuempmed)

# Análise da variável unemploy
length(economics$unemploy)
mode(economics$unemploy)

MedPosi = summary(economics)
MedPosiunemploy = MedPosi[,6]
MedPosiunemploy

mean(economics$unemploy,na.rm = TRUE)
median(economics$unemploy,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(economics$unemploy,na.rm = TRUE, percentis)
print(perc)

ampl=range(economics$unemploy,na.rm = TRUE)
print(ampl)

var(economics$unemploy,na.rm = TRUE)
sd(economics$unemploy,na.rm = TRUE)
CVunemploy=(sd(economics$unemploy,na.rm = TRUE)/mean(economics$unemploy,na.rm = TRUE))
print(CVunemploy)

# Amostragem de 50 observações sem reposição
indice=sample(dim(economics)[1], 50, replace = F)
amostra1 = economics[indice,]
dim(amostra1)
View(amostra1)
summary(amostra1)

# Análise da variável datesample
length(amostra1$date)
mode(amostra1$date)

MedPosi = summary(amostra1)
MedPosidatesample = MedPosi[,1]
MedPosidatesample

mean(amostra1$date,na.rm = TRUE)
median(amostra1$date,na.rm = TRUE)

percentis = c(.09,.25,.05,.69)
perc = quantile(amostra1$date,na.rm = TRUE, percentis)
print(perc)

ampl=range(amostra1$date,na.rm = TRUE)
print(ampl)

sd(amostra1$date,na.rm = TRUE)
var(amostra1$date,na.rm = TRUE)
CVdate=(sd(amostra1$date,na.rm = TRUE)/mean(amostra1$date,na.rm = TRUE))

# Análise da variável pcesample
length(amostra1$pce)
mode(amostra1$pce)

MedPosi = summary(amostra1)
MedPosipcesample = MedPosi[,2]
MedPosipcesample

mean(amostra1$pce,na.rm = TRUE)
median(amostra1$pce,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(amostra1$pce,na.rm = TRUE, percentis)
print(perc)

ampl=range(amostra1$pce,na.rm = TRUE)
print(ampl)

var(amostra1$pce,na.rm = TRUE)
sd(amostra1$pce,na.rm = TRUE)
CVpcesample=(sd(amostra1$pce,na.rm = TRUE)/mean(amostra1$pce,na.rm = TRUE))
print(CVpcesample)

# Análise da variável popsample
length(amostra1$pop)
mode(amostra1$pop)

MedPosi = summary(amostra1)
MedPosipopsample = MedPosi[,3]
MedPosipopsample

mean(amostra1$pop,na.rm = TRUE)
median(amostra1$pop,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(amostra1$pop,na.rm = TRUE, percentis)
print(perc)

ampl=range(amostra1$pop,na.rm = TRUE)
print(ampl)

var(amostra1$pop,na.rm = TRUE)
sd(amostra1$pop,na.rm = TRUE)
CVpopsample=(sd(amostra1$pop,na.rm = TRUE)/mean(amostra1$pop,na.rm = TRUE))
print(CVpopsample)

# Análise da variável psavertsample
length(amostra1$psavert)
mode(amostra1$psavert)

MedPosi = summary(amostra1)
MedPosipsavertsample = MedPosi[,4]
MedPosipsavertsample

mean(amostra1$psavert,na.rm = TRUE)
median(amostra1$psavert,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(amostra1$psavert,na.rm = TRUE, percentis)
print(perc)

ampl=range(amostra1$psavert,na.rm = TRUE)
print(ampl)

var(amostra1$psavert,na.rm = TRUE)
sd(amostra1$psavert,na.rm = TRUE)
CVPsavert=(sd(amostra1$psavert,na.rm = TRUE)/mean(amostra1$psavert,na.rm = TRUE))
print(CVPsavert)

# Análise da variável uempmedsample
length(amostra1$uempmed)
mode(amostra1$uempmed)

MedPosi = summary(amostra1)
MedPosiuempmedsample = MedPosi[,5]
MedPosiuempmedsample

mean(amostra1$uempmed,na.rm = TRUE)
median(amostra1$uempmed,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(amostra1$uempmed,na.rm = TRUE, percentis)
print(perc)

ampl=range(amostra1$uempmed,na.rm = TRUE)
print(ampl)

var(amostra1$uempmed,na.rm = TRUE)
sd(amostra1$uempmed,na.rm = TRUE)
CVuempmedsample=(sd(amostra1$uempmed,na.rm = TRUE)/mean(amostra1$uempmed,na.rm = TRUE))
print(CVuempmedsample)


# Análise da variável unemploysample
length(amostra1$unemploy)
mode(amostra1$unemploy)

MedPosi = summary(amostra1)
MedPosiunemploysample = MedPosi[,6]
MedPosiunemploysample

mean(amostra1$unemploy,na.rm = TRUE)
median(amostra1$unemploy,na.rm = TRUE)

percentis = c(.25,.5,.75)
perc = quantile(amostra1$unemploy,na.rm = TRUE, percentis)
print(perc)

ampl=range(amostra1$unemploy,na.rm = TRUE)
print(ampl)

var(amostra1$unemploy,na.rm = TRUE)
sd(amostra1$unemploy,na.rm = TRUE)
CVunemploysample=(sd(amostra1$unemploy,na.rm = TRUE)/mean(amostra1$unemploy,na.rm = TRUE))
print(CVunemploysample)
