##===============================================================
## Función que genera muestras de una distribución normal
##===============================================================

muestras <- function(n = 20, num = 10000){
  matrix(rnorm(n*num), ncol = n)
}

##===============================================================
## Función que estudia el tamaño del test para el
## contraste de normalidad de Shapiro-Wilk
##===============================================================
simul.shapiro <- function(datos, alfa = 0.05){
  numsimul <- dim(datos)[1]
  rechazos <- 0
  for (i in 1:numsimul){
    if (shapiro.test(datos[i, ])$p.value < alfa){
      rechazos <- rechazos + 1
    }
  }
  rechazos/numsimul
}

##======================================
## Funcion que contabiliza el tiempo
##======================================

tiempo <- function(ntiemp){
  tamano <- NULL
  t <- NULL
  alfaest <- NULL
  for (i in 1:ntiemp){
    tamano <- c(tamano, 10*i)
    m <- muestras(n = 10*i)
    inicio <- Sys.time()
    est <- simul.shapiro(m)
    final <- Sys.time()
    t <- c(t, final - inicio)
    alfaest <- c(alfaest, est)
  }
  data.frame(tam.muestral = tamano, tiempo.comp = t, alfa.estimado = alfaest)
}

prueba <- tiempo(20)
