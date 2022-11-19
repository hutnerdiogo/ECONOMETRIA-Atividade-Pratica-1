rm(list=ls(all=TRUE))
# install.packages("AER")
library(AER)
dados <- read.table("ArquivoExercicio1.csv",header=T,sep=";", dec=".",check.names = F)
names(dados) <- c("Fundos","PL em milhoes","Retorno")


# Questão 1 - Feito
plot(dados$`PL em milhoes`, dados$Retorno,
     pch  = 8,
     col  = "green",
     lwd  = 5,
     main = "Relação entre Retorno e Patrimonio liquido de fundos de investimento",
     xlab = "Patrimonio Liquido do fundo",
     ylab = "Retorno do fundo")

# Questão 2 - Feito
media <- c(mean(dados$`PL em milhoes`),mean(dados$Retorno))
mediana <- c(median(dados$`PL em milhoes`),median(dados$Retorno))
minimo <- c(min(dados$`PL em milhoes`),min(dados$Retorno))
maximo <- c(max(dados$`PL em milhoes`),max(dados$Retorno))
desvio_padrao <- c(sd(dados$`PL em milhoes`),sd(dados$Retorno))
variancia <- c(var(dados$`PL em milhoes`),var(dados$Retorno))
enriched_data <- cbind.data.frame(media,mediana,minimo,maximo,
                                  desvio_padrao,variancia)


# Questão 3 - Feito

covariancia <- cov(dados$`PL em milhoes`,dados$Retorno)
correlacao <- cor(dados$`PL em milhoes`,dados$Retorno)

# Questão 4 - Feito 

x <- dados$`PL em milhoes`
y <- dados$Retorno
reg <- lm(y ~ x)

# Questão 5 - Feito

abline(reg, col=4)

# Questão 6 - Feito

coef(reg)

reg_slm <- summary(reg)
reg_slm[13]

#' O P valor do modelo é significativo 
#' O modelo explicado tem o R-quadrado = 53,5%

# Questão 7 - Feito

reg_slm$r.squared
reg_slm$aliased
reg_slm
require(broom)
glance(reg)$p.value
toString(reg_slm$r.squared)
#' O modelo explicado tem o R-quadrado = 53,5% ou seja,
#' as variações do Retorno são explicadas em 53,5% dos casos pelo PL, sendo então,
#' um modelo aceitavel

# Questão 8 - Feito

#Intercepto
reg_slm$coefficients[1,4]
#Coeficiente
reg_slm$coefficients[2,4]
#nivel de significancia = p valor 
#' O p valor do Alfa é mairo que 5% por isso faz a variavel ser insignificante,
#' já o p valor do Beta é menor que 5%, por isso é significante


# Questão 9

result <- reg_slm$coefficients[1,1] + reg_slm$coefficients[2,1] * 7800
result
predict(reg)
# Questão 10

x <- dados$`PL em milhoes`
y <- dados$Retorno
regSemAlfa <- lm(y ~ x - 1)

# Questão 11

abline(regSemAlfa, col="red")

# Questão 12

regSemAlfa_slm <- summary(regSemAlfa)
#Printar R2, e o p valor de x
regSemAlfa_slm
#' Grande parte dos indicadores melhoraram em relação ao modelo anterior
#' por conta disso é um modelo melhorado que o primeiro
#' R2 melhorou de 53% para 85% sendo um modelo mais adequado que o primeiro,
#' o P Valor de x dele se tornou estatisticamente igual a 0, por isso melhorou
#' O P Valor do modelo se tornou estatisticamente igual 0

# Questão 13

#' Grande parte dos indicadores melhoraram em relação ao modelo anterior
#' por conta disso é um modelo melhorado que o primeiro
#' Printe o R Quadrado e a diferença entre eles assumindo e em %
#' e o 2 é o mais adequado por conta do R

# Questão 14

regSemAlfa_slm$coefficients[1]
# Assumindo o modelo mais preciso, o 2, a melhor firma é a com o maior PL disponivel


# Questão 15
 # Idade do Fundo, Taxa de adminisitração, taxa de performance 
 # Nome do artigo fonte: https://convibra.org/congresso/res/uploads/pdf/artigo23093_20201802.pdf

