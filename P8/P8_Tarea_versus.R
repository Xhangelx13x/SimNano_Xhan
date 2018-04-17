codigo <- data.frame()
tiempo <- data.frame()

for(i in 1:10){
  source('P8_Tarea_par.R', encoding = 'UTF-8')
  source('P8_Tarea_s-par.R', encoding = 'UTF-8')
  tiempo <- cbind(totalo, total)
  codigo <- rbind(codigo, tiempo)
}
png("P8_Tiempom.png", width = 600, height = 800, pointsize = 15)
colnames(codigo) <- c("Paralelo", "No Paralelo")
boxplot(codigo, col = c("blue", "red"), ylab="Tiempo")
graphics.off()
