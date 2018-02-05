repeticiones <- 15
duracion <- 1000
dimensiones <- 10
origen <- rep(0, dimensiones)
tab <- numeric(length(repeticiones*dimensiones))
i = 1
system.time(
for (dimension in 1:dimensiones) {
#print ("dimension")
#print (dimension)
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
		      }
})
mat <- matrix (tab, ncol=dimensiones, nrow=repeticiones)
#png("p1_porcentaje.png")
boxplot (mat, xlab="Dimensi\u{F3}n", ylab="Porcentaje de retorno al origen (%)", main="Efecto de la dimensi\u{F3}n en el porcentaje de regreso al origen")
graphics.off()
#mat