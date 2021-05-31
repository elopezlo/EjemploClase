#Emilio López
#Taller 3
#########################################################################################################
#librería
library(factoextra)
decathlon2
#########################################################################################################
#DECASTAR
p<-grep(x = decathlon2$Competition, pattern = "Decastar");p
decastar<-decathlon2[p,]
#OlympicG
q<-grep(x = decathlon2$Competition, pattern = "OlympicG");q
OlympicG<-decathlon2[q,]

#1.Prueba 100m
#intervalo para Decastar
ic.m <- function(x) t.test(x)$conf.int
ic.m(decastar$X100m)

#la velocidad media de los corredores en esta competencia de 100m se estima con un 95% de cofianza
#que se encuentra entre 11.02659 y 11.32418 segundos.

#intervalo para OlympicG
ic.m <- function(x) t.test(x)$conf.int
ic.m(OlympicG$X100m)

#la velocidad media de los corredores en esta competencia de 100m se estima con un 95% de cofianza
#que se encuentra entre 10.70956 y 10.93472 segundos.

#conclusión
##Los corredores de la competencia decastar tardaron más que los corredores de la competencia 
#OlympicG en la prueba de 100 metros

# FALTÓ EL GRÁFICO!

#2 Prueba 400m
#intervalo para decastar variación
icx2 <- function(x, conf = 0.95){
  x <- na.omit(x)
  n = length(x)
  s2 = var(x)
  alpha = 1- conf
  v1 <- qchisq(alpha/2, n-1)
  v2 <- qchisq(1- alpha/2, n-1)
  c((n-1)*s2/v2, (n-1)*s2/v1)}
icx2(decastar$X400m)
sqrt(icx2(decastar$X400m))

#Los corredores de esta competencia tuvieron una variabilidad 
#media de entre 0.6471014 y 1.4896303 segundos.


#intervalo para decastar variación
icx2 <- function(x, conf = 0.95){
  x <- na.omit(x)
  n = length(x)
  s2 = var(x)
  alpha = 1- conf
  v1 <- qchisq(alpha/2, n-1)
  v2 <- qchisq(1- alpha/2, n-1)
  c((n-1)*s2/v2, (n-1)*s2/v1)}
icx2(OlympicG$X400m)
sqrt(icx2(OlympicG$X400m))

#Los corredores de esta competencia tuvieron una variabilidad 
#media de entre 0.7099653 y 1.5777337 segundos.

#conclusión
#La variabilidad media de los tiempos de los competidores de ambas competencias es similar.


##3. diferencia entre los resultados medios
decastar1<-decastar$X1500m/60 # POR QUÉ DIVIDIR ENTRE 60??
OlympicG1<-OlympicG$X1500m/60
t.test(decastar1,OlympicG1)

# (-0.01567715 , 0.25227239)
#como el intervalo contiene al 0 la diferencia de los resultados medios de la prueba de 1500m 
#es estadísticamente igual.

# ESTADÍSTICAMENTE IGUAL NO, NO SON ESTAD. DIFERENTES!

# NOTA: 1.2 + 1.7 + 1.6 = 4.5

diamonds
table(iris)
