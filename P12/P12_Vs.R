suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Resultados=data.frame()
Toriginal=numeric()
Tparalelo=numeric()

  for (a in seq(500,2500,500)){
    for(r in 1:5){
    
    source('P12_O.R')
    Toriginal=cbind(a,r,"NP",Tiempo,AciertosP)
    
    
    source('P12_P.R')
    Tparalelo=cbind(a,r,"P",Tiempo,AciertosP)
    Resultados=rbind(Resultados,Toriginal,Tparalelo)
    }
  }
stopImplicitCluster() 
names(Resultados)=c("nPrueba","replica","tipo","Tiempo","Paciertos")
Resultados$Tiempo<-as.numeric(levels(Resultados$Tiempo))[Resultados$Tiempo]
Resultados$Paciertos<-as.numeric(levels(Resultados$Paciertos))[Resultados$Paciertos]
Resultados$tipo=as.factor(Resultados$tipo)
#Resultados[Resultados$Tiempo>10,3]<-Resultados[Resultados$Tiempo>10,3]/60
#Tiempos
png("P12_time.png",width=1000, height=600,pointsize = 15)
boxplot(Tiempo~tipo*nPrueba,data=Resultados,col = c("red1","royalblue2"),border=c("red4","royalblue4"),xlab="Número de pruebas",ylab="Tiempo")
legend("topleft", inset=.02,
       c("Paralelo","No Paralelo"), fill=c("red1","royalblue2"), horiz=TRUE, cex=0.8,box.lty = 0)
graphics.off()

#Porcentajes
png("P12_porc.png",width=1000, height=600,pointsize = 15)
boxplot(Paciertos~tipo*nPrueba,data=Resultados,col = c("red1","royalblue2"),border=c("red4","royalblue4"),xlab="Número de pruebas",ylab="Porcentaje de aciertos")
legend("topright", inset=.02,
       c("No Paralelo","Paralelo"), fill=c("red1","royalblue2"), horiz=TRUE, cex=0.8,box.lty = 0)

graphics.off()

#Psecuencial<-Resultados[Resultados$tipo=="NP",]
#Pparalelo<-Resultados[Resultados$tipo=="P",]


for (a in seq(500,2500,500)){
  PruebaTO<-Resultados[Resultados$nPrueba == a & Resultados$tipo=="P",] 
  PruebaTP<-Resultados[Resultados$nPrueba == a & Resultados$tipo=="NP",]
  
  vecO<-PruebaTP$Tiempo
  vecP<-PruebaTO$Tiempo
  student<-t.test(vecO,vecP)
  print(student)

}
save.image(file = "Practica12chido.RData")