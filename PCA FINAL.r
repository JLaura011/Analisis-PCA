install.packages("readxl")

library(readxl)

setwd("C:/Users/jlaur/OneDrive/Documentos")

data <- read.csv("DATABM.csv", header = TRUE, sep = ";")

data

names(data)

class(data)

install.packages("reshape")
require(reshape)

data <- rename(data, c(ï..Pais="Pais",
                       Final.consumption.expenditure....of.GDP.="Consumo_final", 
                       Homicidios.intencionales..por.cada.100.000.habitantes.="Homicidios",
                       Desempleo..total....de.la.poblaciÃ³n.activa.total...estimaciÃ³n.modelado.OIT.="Desempleo",
                       Ahorro.ajustado..gasto.en.educaciÃ³n..US..actuales.="Gasto_Educacion",
                       Acceso.a.la.electricidad....de.poblaciÃ³n.="Electricidad",
                       Tiempo.necesario.para.iniciar.un.negocio..dÃ.as.="Tiempo_negocio",
                       Tasa.de.fertilidad..total..nacimientos.por.cada.mujer.="Fertilidad",
                       CrÃ.dito.interno.al.sector.privado.otorgado.por.los.bancos....del.PIB.="Credito"))

names(data)

data

dim(data)

X <- as.matrix(data[,2:9])

X

dim(X) #fila(observaciones) * columnas (variables) 

class(X) #Data frame incluye todo tipo de variables (string, numeric, float), pero matrix solo es valores numericos

F <- dim(X)[1] # Numero total de filas = (analogo) al numero de observaciones
C <- dim(X)[2] # Numero total de columnas

F>C

##Suma := i'X
unos <- matrix(1,F,1) # matriz i, crea una matriz llena de unos, con F filas y 1 columna
## [*] = multiplicacion escalar (numeros) / [%*%] = multiplicacion matricial
total <- t(unos)%*%X
total

##Promedio := (1/n)i'X
promedio <- 1/F * t(unos)%*%X 
promedio

base_promsum <- data.frame(Suma = t(total) , Promedio = t(promedio))
format(base_promsum, scientific = FALSE)

promedio_consumo <- promedio[,1]
promedio_consumo

data[data$Consumo_final>= promedio_consumo,1] ##filtros
data

promedio

promedio_fertilidad <- promedio[,7]
promedio_fertilidad

data[data$Fertilidad < promedio_fertilidad,1]

##Filtro de paises con un tiempo mayor al doble del promedio
promedio_tiempo_negocio <- promedio[,6]
promedio_tiempo_negocio
doble <- 2*promedio_tiempo_negocio
doble

data[data$Tiempo_negocio > doble,1]

##Coeficiente de variación := [SD(X) / mean(X)]*100
##SD = (Variancia(X)) ^ 1/2
## Matriz S := X' Mo X
##Donde Mo:= [I - 1/n*ii´]
Mo <- diag(F) - 1/F*unos%*%t(unos)
## Matriz de variancias:= ¿es poblacional o es muestral? [Agrega el factor de correcion muestral = (1/n-1)]
## S (poblacional) := X' Mo X
## S (muestral) := (1/n-1) X' Mo X
S <- 1/(F-1)*t(X)%*%Mo%*%X

format(S, scientific = FALSE)

##Coeficiente de variación := [SD(X) / mean(X)]*100
sd_X <- sqrt(diag(S))
coeficiente_var <- sd_X / promedio
cv <- t(coeficiente_var)
cv
##en terminos porcentuales
##cv_porcentaje <- cv*100
##cv_porcentaje

##Creamos la función de coeficiente de variación
coef_var <- function(x, na.rm = FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

coeficiente_variacion <- data.frame(
  "variable" = as.character(c("Consumo_final","Homicidios","Desempleo","Educacion",
                              "Electricidad","Tiempo_negocio","Fertildad","Credito")),
  "cv" = c(coef_var(data$Consumo_final, na.rm=T), 
           coef_var(data$Homicidios, na.rm=T),
           coef_var(data$Desempleo, na.rm=T), 
           coef_var(data$Gasto_Educacion, na.rm=T),
           coef_var(data$Electricidad, na.rm=T),
           coef_var(data$Tiempo_negocio,na.rm =T),
           coef_var(data$Fertilidad, na.rm=T),
           coef_var(data$Credito, na.rm=T))
  )
coeficiente_variacion

##Matriz R (matriz de correlaciones)
## R := D^(-1/2) * S * D^(-1/2)
D = diag(S) ##Matriz diagonal pero de variancias
for(i in 1:C) {
  D[i] = D[i] ^ (-1/2)
}
D = diag(D) ## D gual a D^(-1/2)
R = D%*%S%*%D
R

traza_r <- sum(diag(R))
traza_r

###### ANALISIS PCA : PREGUNTAS 4
###### 01. Cálculo de los autovalores con la matriz de variancias - covariancias (ignora la unidad de medida)
AV_S <- eigen(S) ## funcion eigen calcula los autovalores y autovectores de una matriz
autovalores_s <- AV_S$values
round(sum(autovalores_s),2)
## cumple la condicion del total de variables :: idea de la variancia

autovectores_s <- AV_S$vectors
autovectores_s

##¿Cuanto explica cada autovalor de la variancia total? (proporción)
autov_s <- as.matrix(autovalores_s)
autov_s

proporcion_s <- round(1/C*autov_s,2)
format(proporcion_s, scientific = FALSE)

###### 01. Cálculo de los autovalores con la matriz de correlaciones
AV_R <- eigen(R) ## funcion eigen calcula los autovalores y autovectores de una matriz
autovalores_r <- AV_R$values
round(sum(autovalores_r),2)
## cumple la condicion del total de variables :: idea de la variancia

autovectores_r <- AV_R$vectors
autovectores_r

##¿Cuanto explica cada autovalor de la variancia total? (proporción)
autov_r <- as.matrix(autovalores_r)
proporcion_r <- round(1/C*autov_r,2)
proporcion_r

##Regla :: La suma de los autovalores >= 85% 
regla <- 0.37+0.23+0.17+0.10
regla >= 0.85 ## consistencia de la regla
regla
##Conclusion: me quedo con los 04 autovalores y 04 autovectores

##Calculo de los componentes principales
## Z = (X - iXprom) / desviacion estandar
## Z = MoX [Matriz centrada] / desviacion estandar
centrada <- Mo%*%X
sd = c()
for (i in 1:8) {
 sd[i] = sd(X[,i])
}
sd

##Simulacion de la matrix X en la cual se reemplaza los valores estandarizados
MC = matrix(0,18,8)
for (i in 1:8) {
  MC[,i] = (1/sd[i])*centrada[,i]
}
MC ## matriz Z

AVF <- autovectores_r[,1:4]
AVF

##Obtención de componentes := Z*AVF
componentes <- MC%*%AVF
componentes

##Pregunta 05
com1 <- sqrt(autovalores_r[1])*AVF[,1]
com2 <- sqrt(autovalores_r[2])*AVF[,2]
com3 <- sqrt(autovalores_r[3])*AVF[,3]
com4 <- sqrt(autovalores_r[4])*AVF[,4]

matriz_varcomp <- data.frame(
"variable" = as.character(c("Consumo_final","Homicidios","Desempleo","Educacion",
                            "Electricidad","Tiempo_negocio","Fertildad","Credito")),
"c1" = com1,
"c2" = com2,
"c3" = com3,
"c4" = com4
)
matriz_varcomp

##Pregunta 06
c1 <- componentes[,1]
c2 <- componentes[,2]
c3 <- componentes[,3]
c4 <- componentes[,4]

matriz_variables <- data.frame(
"pais" = as.character(c("Argentina", "Bolivia", "Brasil","Chile","Colombia","Costa_Rica","Ecuador","El_Salvador","Guatemala",
"Haiti","Honduras","Mexico","Nicaragua","Panama","Paraguay","Peru","Republica_Dominicana","Uruguay")),
"c1" = c1,
"c2" = c2,
"c3" = c3,
"c4" = c4
)
matriz_variables

