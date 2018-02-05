repeticiones <- 1
duracion <- 10000
dimensiones <- 20
origen <- rep(0, dimensiones)
tab <- numeric(length(repeticiones*dimensiones))
elapsed <- numeric(length(dimensiones))
i = 1
j = 1
anterior <- 0
tiempoTotal <- system.time(
for (dimension in 1:dimensiones) {
#print ("dimension")
#print (dimension)
tiempoDim <- system.time(
 for (repeticion in 1:repeticiones)  { 
	cero <- 0              
                       pos <- rep(0, dimension)
     			#print ("repeticion")
			#print (repeticion)
                           for (t in 1:duracion) {
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
                          			}
#print (cero)
probabilidad <- ((cero/duracion)*100)
#print (probabilidad)
tab[i] <- probabilidad
i = i + 1
		      })[3]
tiempo <- (tiempoDim + anterior)
elapsed[j] <- tiempo
anterior = tiempo
j = j + 1
})
mat <- matrix (tab, ncol=dimensiones, nrow=repeticiones)
#png("p1_porcentaje.png")
png("p1_reto1.png")
#boxplot (mat, xlab="Dimensi\u{F3}n", ylab="Porcentaje de retorno al origen (%)", main="Efecto de la dimensi\u{F3}n en el porcentaje de regreso al origen")
plot (elapsed, type="l", xlab="Dimensi\u{F3}n", ylab="Tiempo (s)", main="Efecto de la dimensi\u{F3}n en el tiempo de ejecuci\u{F3}n")
points (elapsed, pch="X", col="red")
graphics.off()

