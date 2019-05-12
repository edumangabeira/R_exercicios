# Eduardo Freire Mangabeira 116137191
# Trabalho 1 de AED
# 1)
# item a)
nomes = read.table("eduardobrasil.txt", col.names=c("UF","Pop","Freq","Taxa"))
UF <- nomes$UF
Freq <- nomes$Freq
barplot(Freq, main = "Frequência por estado", ylim=c(0,200000),xlab="Estados",ylab="Eduardos",font.lab=2,names.arg=UF)
# item b)
Pop <- nomes$Pop
barplot(Freq/Pop, main = "Eduardos por habitante por estado", ylim=c(0,0.006),xlab="Estados",ylab="Eduardos/habitante",font.lab=2,names.arg=UF)
# item c)
Taxa <- nomes$Taxa
Sul <- c("SC","RS","PR")
Norte <- c("PA","TO","AM","AP","RR","RO","AC")
Centro_Oeste <- c("MT","MS","GO","DF")
Nordeste <- c("MA","BA","PE","CE","SE","AL","RN","PI","PB")
Sudeste <- c("RJ","SP","MG","ES")
Regioes <- c(Sul, Sudeste, Nordeste, Centro_Oeste, Norte)
Taxa_sul = 0
for(i in UF){
  for (j in Sul){
    if(UF[i] == Sul[j]){
      Taxa_sul = Taxa_sul + Taxa[i]
    }
  }
}
