repeticiones <- 1
duracion <- 1000
dimensiones <- 10
origen <- rep(0, dimensiones)
tab <- numeric(length(repeticiones*dimensiones))
elapsed <- numeric(length(duracion))
i = 1
j = 1
anterior <- 0
tiempoTotal <- system.time(
for (dimension in dimensiones:dimensiones) {
#print ("dimension")
#print (dimension)
tiempoDim <- system.time(
 for (repeticion in 1:repeticiones)  { 
	cero <- 0              
                       pos <- rep(0, dimension)
     			#print ("repeticion")
			#print (repeticion)
                           for (t in 1:duracion) {
				    tiempoDur <- system.time(
				for (x in 1:1) {
                               cambiar <- sample(1:dimension, 1)                     
                               if (runif(1) < 0.5) {
				    cambio <- 1
                                   } else {
				    cambio <- -1
                               		}
                               pos[cambiar] <- pos[cambiar] + cambio
if (all (pos == origen)) {
	cero = cero + 1
}
#print (pos)
})[3]
tiempo <- (tiempoDur + anterior)
elapsed[j] <- tiempo
anterior <- tiempo
j = j + 1
                          			}
#print (cero)
probabilidad <- ((cero/duracion)*100)
#print (probabilidad)
tab[i] <- probabilidad
i = i + 1
		      })[3]
})
mat <- matrix (tab, ncol=dimensiones, nrow=repeticiones)
#png("p1_porcentaje.png")
#png("p1_reto1(1).png")
#boxplot (mat, xlab="Dimensi\u{F3}n", ylab="Porcentaje de retorno al origen (%)", main="Efecto de la dimensi\u{F3}n en el porcentaje de regreso al origen")
plot (elapsed, type="l", xlab="N\u{FA}mero de pasos", ylab="Tiempo (s)", main="Efecto del n\u{FA}mero de pasos en el tiempo de ejecuci\u{F3}n")
#graphics.off()