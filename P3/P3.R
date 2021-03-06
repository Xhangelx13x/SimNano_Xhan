primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
basedatos = read.csv("primes1.txt", sep=" ", header=FALSE) #eliminar encabezado de txt
basedatos$V1 = NULL #borrar primer columna[nombre]
basedatos$V10 = NULL #borrar ultima columna[nombre]
m = as.matrix(basedatos) #crear matriz de ...
v = as.vector(t(m)) #crear vector de una matriz

vi <- v[seq(1, 1000000, 20000)] #elegir numeros al azar
vp <- v[999951:1000000]
vr <- v[1:50]
vnp <- (1:50)
vf <- sort(rbind (vr, vi, vp, vnp))

menmay <- vf[1:200]
maymen <- vf[200:1]
aleatorio1 <- sample(maymen)
aleatorio2 <- sample(maymen)
aleatorio3 <- sample(maymen)
aleatorio4 <- sample(maymen)
aleatorio5 <- sample(maymen)

replicas <- 50
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot <-  numeric()
it <-  numeric()
at1 <-  numeric()
at2 <-  numeric()
at3 <-  numeric()
at4 <-  numeric()
at5 <-  numeric()
for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = menmay, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = maymen, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    	at1 <- c(at1, system.time(foreach(n = aleatorio1, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	at2 <- c(at2, system.time(foreach(n = aleatorio2, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	at3 <- c(at3, system.time(foreach(n = aleatorio3, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	at4 <- c(at4, system.time(foreach(n = aleatorio4, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	at5 <- c(at5, system.time(foreach(n = aleatorio5, .combine=c) %dopar% primo(n))[3]) # orden aleatorio

}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at1)
summary(at2)
summary(at3)
summary(at4)
summary(at5)
tiempo <- t(rbind(ot, it, at1, at2, at3, at4, at5))
png("p3_tiempos.png")
boxplot (tiempo)
graphics.off()