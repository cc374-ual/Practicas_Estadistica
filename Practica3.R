#Cargamos los datos
datos <- read.csv("D:/Año2023/2Cuatrimestre/Estadistica/notasA3.csv", sep=",", dec=",", header=T);

# Reemplaza los NA
datos[datos == ""] <- '0';


#Reemplazar aprobado suspenso (Transformación variables cuantitavias en cualitativas)  [Ejercicio 1]

datos$Practica1 <- ifelse(datos$Practica1 >= 1/3, "Aprobado", "Suspenso")
datos$Practica2 <- ifelse(datos$Practica2 >= 1/3, "Aprobado", "Suspenso")
datos$Practica3 <- ifelse(datos$Practica3 >= 1/3, "Aprobado", "Suspenso")
datos$NOTAFINALJUNIO <- ifelse(datos$NOTAFINALJUNIO < 5, "Suspenso", "Aprobado")
datos$TOTALpracticas <- ifelse(datos$TOTALpracticas >= 1, "Aprobado", "Suspenso")
datos$Cuestionario1y2 <- ifelse(datos$Cuestionario1y2 >= 5, "Aprobado", "Suspenso")
datos$Cuestionario3y4 <- ifelse(datos$Cuestionario3y4 >= 5, "Aprobado", "Suspenso")
datos$Cuestionario5y6 <- ifelse(datos$Cuestionario5y6 >= 5, "Aprobado", "Suspenso")
datos$Totalcuestionarios <- ifelse(datos$Totalcuestionarios >= 0.5, "Aprobado", "Suspenso")
datos$EXAMENJUNIO <- ifelse(datos$EXAMENJUNIO >= 5, "Aprobado", "Suspenso")
datos$EXAMENSEPTIEMBRE <- ifelse(datos$EXAMENSEPTIEMBRE >= 5, "Aprobado", "Suspenso")
datos$NOTAFINALSEPTIEMBRE <- ifelse(datos$NOTAFINALSEPTIEMBRE >= 5, "Aprobado", "Suspenso")


#---------------------------------------------Ejercicio 1---------------------------------------------

#
#
#Usando nota de práctica 1
#
#

#U1 = Nota Junio
#U2 = Nota Práctica 1
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Practica1")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.4122939 > 0.05 = alpha, con los datos disponibles no se puede
# rechazar la hipótesis nula por lo que concluimos que son independientes

#
#
#Usando nota de práctica 2
#
#

#U1 = Nota Junio
#U2 = Nota Práctica 2
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Practica2")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.04347826 < 0.05 = alpha, con los datos disponibles
# podemos rechazar la hipótesis nula por lo que concluimos que son dependientes


#
#
#Usando nota de práctica 3
#
#

#U1 = Nota Junio
#U2 = Nota Práctica 3
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Practica3")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.01549225 < 0.05 = alpha, con los datos disponibles
# podemos rechazar la hipótesis nula por lo que concluimos que son dependientes

#
#
#Usando nota de TotalPrácticas
#
#

#U1 = Nota Junio
#U2 = Nota TotalPrácticas
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("TOTALpracticas")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.04797601 < 0.05 = alpha, con los datos disponibles
# podemos rechazar la hipótesis nula por lo que concluimos que son dependientes

#
#
#Usando nota de Cuestionario 1 y 2
# 
#

#U1 = Nota Junio
#U2 = Nota Cuestionario1y2
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Cuestionario1y2")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.4222889 > 0.05 = alpha, con los datos disponibles
# no podemos rechazar la hipótesis nula por lo que concluimos que son independientes

#
#
#Usando nota de Cuestionario 3 y 4
# 
#

#U1 = Nota Junio
#U2 = Nota Cuestionario3y4
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Cuestionario3y4")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.1614193 > 0.05 = alpha, con los datos disponibles
# no podemos rechazar la hipótesis nula por lo que concluimos que son independientes

#
#
#Usando nota de Cuestionario 5 y 6
# 
#

#U1 = Nota Junio
#U2 = Nota Cuestionario5y6
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Cuestionario5y6")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.0109945 < 0.05 = alpha, con los datos disponibles
# podemos rechazar la hipótesis nula por lo que concluimos que son dependientes


#
#
#Usando nota de TotalCuestionarios
# 
#

#U1 = Nota Junio
#U2 = Nota TotalCuestionarios
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("Totalcuestionarios")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.0189905 < 0.05 = alpha, con los datos disponibles
# podemos rechazar la hipótesis nula por lo que concluimos que son dependientes

#
#
#Usando nota de NOTAFINALJUNIO
# 
#

#U1 = Nota Junio
#U2 = Nota Final Junio
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("NOTAFINALJUNIO")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.000995002 < 0.05 = alpha, con los datos disponibles
# podemos rechazar la hipótesis nula por lo que concluimos que son dependientes


#
#
#Usando nota de Examen Septiembre
# 
#

#U1 = Nota Junio
#U2 = Nota de Examen Septiembre
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("EXAMENSEPTIEMBRE")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 1 > 0.05 = alpha, con los datos disponibles
# no podemos rechazar la hipótesis nula por lo que concluimos que son independientes

#
#
#Usando nota Final de Septiembre
# 
#

#U1 = Nota Junio
#U2 = Nota Final Septiembre
#
#H0: La variable U1 es independiente de U2. (hipótesis nula)
#H1: La variable U1 es dependiente de U2.
#
#nivel de significancia alpha=0.5

U1=datos[,c("EXAMENJUNIO")]
U2=datos[,c("NOTAFINALSEPTIEMBRE")]

tabla = table(U1,U2)
tabla
t1=chisq.test(tabla,simulate.p.value=TRUE)
t1$p.value


# Como el p-valor = 0.5987006 > 0.05 = alpha, con los datos disponibles
# no podemos rechazar la hipótesis nula por lo que concluimos que son independientes

#---------------------------------------------Ejercicio 2---------------------------------------------
datosNum <- read.csv("D:/Año2023/2Cuatrimestre/Estadistica/notasA3.csv", sep=",", dec=",", header=T)

# Reemplaza los NA
datosNum[datosNum == ""] <- '0'

exjunio <- as.numeric(datosNum[, "EXAMENJUNIO"])
notafinjunio <- as.numeric(datosNum[, "NOTAFINALJUNIO"])

# Realizar prueba t de muestras pareadas
prop.test(exjunio, notafinjunio)
#resultado <- t.test(exjunio, notafinjunio, paired = TRUE)
#resultado <- prop.test(sum(exjunio), sum(notafinjunio))


#---------------------------------------------Ejercicio 3---------------------------------------------
exseptiembre <- as.numeric(datosNum[, "EXAMENSEPTIEMBRE"])
notafinseptiembre <- as.numeric(datosNum[, "NOTAFINALSEPTIEMBRE"])

# Realizar prueba t de muestras pareadas
t.test(exseptiembre, notafinseptiembre)


# Realizar prueba t de muestras pareadas
resultado <- t.test(exseptiembre, notafinseptiembre, paired = TRUE)

# Mostrar los resultados
resultado

#---------------------------------------------Ejercicio 4---------------------------------------------
grupoA <- subset(datosNum, Grupo == "A");
  notaExamenA <- as.numeric(grupoA[, "EXAMENJUNIO"])
  notaFinalJunioA  <- as.numeric(grupoA[, "NOTAFINALJUNIO"])
  notaFinalSeptiembreA  <- as.numeric(grupoA[, "NOTAFINALSEPTIEMBRE"])
grupoB <- subset(datosNum, Grupo == "B");
  notaExamenB<- as.numeric(grupoB[, "EXAMENJUNIO"])
  notaFinalJunioB <- as.numeric(grupoB[, "NOTAFINALJUNIO"])
  notaFinalSeptiembreB  <- as.numeric(grupoB[, "NOTAFINALSEPTIEMBRE"])
  
  res1 <- var.test(notaExamenA, notaExamenB)
  res2 <- var.test(notaFinalJunioA, notaFinalJunioB)
  res3 <- var.test(notaFinalSeptiembreA, notaFinalSeptiembreB)

  res1
  res2
  res3
  