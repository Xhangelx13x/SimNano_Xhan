x <- data.frame()
Resultados <- data.frame()
for (i in 1:5){
  source('P6_P.R', encoding = 'UTF-8')
  source('P6_SP.R', encoding = 'UTF-8')
  Resultados <- cbind(TiempoO,TiempoT) 
  x=rbind(x,Resultados)  
}

colnames(x) <- c("Sin paralelamiento", "Con paralelamiento")
png("P6_Tiempo.png",width=400, height=600)
boxplot(x,col=c("Green","Purple"),ylab="Tiempo de procesamiento")
graphics.off()