library(readr)
notasprob1 <- read_csv("C:/Users/Eduardo/R_exercicios/notasprob1.csv")
View(notasprob1)

notasprob1 <- notasprob1 %>% select(Prova1,Prova2) %>% filter(Prova1 + Prova2 > 6) %>% arrange(-(Prova1 + Prova2))