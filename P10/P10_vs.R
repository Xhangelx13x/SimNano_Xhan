Tiempos<-data.frame()
Tnp<-numeric()

Tp<-numeric()
for(corrida in 1:5) {
for(init in seq(100, 300, 100)){


  source('P10_orig.R', encoding = 'UTF-8')
  
  Tnp <- cbind("original", tiempo, init, corrida)
  png(paste("p10s_",corrida, init,".png"), width=600, height=300)
  plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
  points(1:tmax, mejores, pch=15)
  abline(h=optimo, col="green", lwd=3)
  graphics.off()

 source('P10_par.R', encoding = 'UTF-8')
  
  Tp <- cbind("paralelo",tiempo, init, corrida)
  png(paste("P10_",corrida,init,".png"), width=600, height=300)
  plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
  points(1:tmax, mejores, pch=15)
  abline(h=optimo, col="red", lwd=3)
  graphics.off()
  
  Tiempos<-  rbind(Tiempos,Tnp, Tp)
  }  
  
}
save.image(file="datosR10_1.RData")
colnames (Tiempos)= c("Tipo","Tiempo","init", "corrida")
Tiempos$Tipo=as.factor(Tiempos$Tipo)
Tiempos$Tiempo=as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
Tiempos[Tiempos$Tiempo>10,2]<-Tiempos[Tiempos$Tiempo>10,2]/60
Tiempos$init=as.numeric(levels(Tiempos$init))[Tiempos$init]
Tiempos$init=as.factor(Tiempos$init)
library(ggplot2)
png("P10_Tarea_plot1.png")
ggplot(data=Tiempos, aes(x=init, y=Tiempo, color=Tipo)) + 
  geom_boxplot()+
  scale_y_continuous(name="Tiempo (min)") +
  scale_x_discrete(name="Población inicial")+
#ggtitle("Tiempos comparados variando población inicial")
graphics.off()
