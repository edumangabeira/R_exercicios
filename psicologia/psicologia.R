# install.packages(c("litteR", "moments"))

library(litteR)
library(moments)
library(readr)

saude <- read_csv("C:/Users/Eduardo/R_exercicios/psicologia/saude.csv")
head(saude)

# especificar o elemento de uma coluna para ser avaliado
espec <- saude[saude$formacao == "Assistência social",]
espec_cor <- table(espec$cor)

# gráfico de colunas
barplot(espec_cor, main="assistentes sociais por cor", ylim = c(0,450))
# barplot(espec_cor[order(espec_cor, decreasing = FALSE)], main="assistentes sociais por cor", ylim = c(0,450))

# grafico de pizza
pie(espec_cor)

# MEDIDAS DE CENTRALIDADE
# media populacional
mean(saude$idade)
# media amostral
mean(espec$idade)

# mediana populacional
median(saude$idade)
# mediana amostral
median(espec$idade)

# MEDIDAS DE DISPERSAO
# desvio padrao
sd(saude$idade)
dsd(espec$idade)

# variancia
var(saude$idade)
var(espec$idade)

# quartis
quantile(espec$idade)
boxplot(espec$idade)

# trimedia
litteR::trimean(espec$idade)

# curtose
moments::kurtosis(espec$idade)


# teste qui-quadrado
sexo_escolaridade <- table(espec$sexo, espec$escolaridade)
barplot(sexo_escolaridade, beside=T, main="sexo e escolaridade", 
        legend= T, ylim = c(0,250))
chisq.test(sexo_escolaridade, correct=T)
