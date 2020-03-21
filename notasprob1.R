library(readr)
library(dplyr)
notasprob1 <- read_csv("C:/Users/Eduardo/R_exercicios/notasprob1.csv")
View(notasprob1)

# pessoas que alcançaram média parcial superior a 3
pf_ou_aprovados <- notasprob1 %>% 
  select(Prova1,Prova2) %>%
  filter(Prova1 + Prova2 > 6) %>%
  arrange(-(Prova1 + Prova2))

# pessoas que fizeram pelo menos uma prova e reprovaram
menor_que_2 <- notasprob1 %>%
  select(Prova1,Prova2) %>%
  filter(Prova1 + Prova2 != 0) %>%
  filter(Prova1 + Prova2 < 4) %>%
  arrange(-(Prova1 + Prova2))
