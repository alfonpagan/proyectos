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
