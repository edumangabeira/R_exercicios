# Questão 1
# carregar dados do gdp
data <- read.csv("gdp_csv.csv")
head(data)

# selecionar coluna com país a ser analisado
arab <- data[data$Country.Name == "Arab World",]

# plotar gráfico de barras
barplot(arab$Value)

# calculando e plotando log do pib
log_pib <- log(arab$Value)
plot(log_pib)

# Questão 2
ano_19 <- data[data$Year == "2009",]
