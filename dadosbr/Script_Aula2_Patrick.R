# Patrick Maia <patrickmaia@id.uff.br>
# AVALIAÇÃO ESTATÍSTICA E GRÁFICA COM DADOS BRASILEIROS NO R
# 23/05/2019

library(readr)
library(dplyr)

# Introdução ----

# Operador %>%

mean(exp(rnorm(3000)))

rnorm(3000) %>% exp %>% mean

# 1. Manipulação - Dados do PROUNI  ----
# https://brasil.io/dataset/cursos-prouni/cursos


# Carregar os dados
PROUNI <- read_csv("cursos-prouni-7dacc3f497194d3e85fec66e2c2ec71d.csv")

# Mutate - Adicionar uma coluna com o log da mensalidade
PROUNI <- PROUNI %>%
  mutate(LogMensalidade = log(mensalidade))

# Mutate - Adicionar uma coluna com o log da mensalidade e outra com a soma
# de bolsas integrais

PROUNI <- PROUNI %>%
  mutate(LogMensalidade = log(mensalidade),
         BolsasIntegrais = sum(bolsa_integral_cotas,bolsa_integral_ampla, na.rm = T))


# Select - Selecionar apenas as variáveis relevantes

PROUNI <- PROUNI %>%
  select(uf_busca, universidade_nome, nome, grau, mensalidade, nota_integral_ampla, nota_integral_cotas)

# Filter - Filtrar as linhas que atendam a uma condição

PROUNI_RJ <- PROUNI %>%
  filter(uf_busca == "RJ")

# Summarise - Operações para reduzir uma variável a um valor

MediaMensalidades <- PROUNI_RJ %>%
  summarise(MediaRJ = mean(mensalidade))

# Arrange - Ordenar uma variável

PROUNI <- PROUNI_RJ %>% 
  arrange(mensalidade)


# O sinal de menos indica que a ordenação é decrescente

PROUNI <- PROUNI_RJ %>% 
  arrange(-mensalidade)

# Podemos realizar as operações em sequencia

# Filtrar e Resumir

PROUNI_RJ_ADM <- PROUNI_RJ %>%
  filter(nome == "Administração") %>%
  summarise(MeanAdmRJ = mean(mensalidade))

# Group_By - Agrupar, resumir os dados e ordenar do maior para o menor

MediaMensalidadesBrasil <- PROUNI %>%
  group_by(nome) %>%
  summarise(MediaMensalidade = mean(mensalidade),
            StdDevMensalidade = sd(mensalidade)) %>%
  arrange(-MediaMensalidade)


# Perguntas a responder:
# a. Qual estado possui a nota de corte mais alta no curso de medicina? 
# Onde é mais caro estudar medicina?

PerguntaA <- PROUNI %>% 
  filter(nome == "Medicina" & !is.na(nota_integral_ampla) & !is.na(nota_integral_cotas)) %>%
  group_by(uf_busca) %>%
 summarise(MeanNotaCorte_Ampla = mean(nota_integral_ampla),
           MeanNotaCorte_Cota = mean(nota_integral_cotas),
           MeanMensalidade = mean(mensalidade))

# Qual estado possui mais universidades no PROUNI?

PerguntaB <- PROUNI %>%
 group_by(uf_busca) %>%
  summarise(Cont = n_distinct(universidade_nome))
 

# Palhinha das Aulas 3 e 4
# Gráfico do # de  Universidades por Curso
library(ggplot2)

ggplot(data = PerguntaB, aes(x = reorder(uf_busca, -Cont), y = Cont)) + 
  geom_col() + 
  labs(x = "UF", 
       y = "Universidades no PROUNI", 
       title = "Gráfico de Colunas")

# Modelo para explicar a mensalidade

ModeloLinear <- lm(data = PROUNI,
                   formula = log(mensalidade) ~ uf_busca +  nota_integral_ampla + nota_integral_cotas + nota_parcial_ampla + nota_parcial_cotas)

summary(ModeloLinear)

