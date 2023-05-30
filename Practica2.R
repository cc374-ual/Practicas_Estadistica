# Carga un dataset por defecto en R
datos <- read.csv("C:/Users/Chira Ciprian/OneDrive - Universidad de Almeria/UAL/4 ING/SegundoCuatri/Estadística/Prácticas/Práctica 1/notasA3.csv", sep=",", dec=",", header=T);


#Instalar paquetes
install.packages("e1071")    # DESCOMENTA ESTA LÍNEA

# carga el paquete
library(e1071)

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


#Dividimos los grupos en A y B
grupoA <- subset(datos, Grupo == "A");
grupoB <- subset(datos, Grupo == "B");


#NaiveBayes [Ejercicio 2]

#Se aplica el clasificador naiveBayes para cada grupo por separado

mA<-naiveBayes(grupoA$NOTAFINALJUNIO~.,data=grupoA)
mB<-naiveBayes(grupoB$NOTAFINALJUNIO~.,data=grupoB)

mA$apriori
mB$apriori


# 
# Hay que sacar sólo las columnas que nos interesa de ambos grupos para la predicción
# 
dfA = grupoA[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")]
dfB = grupoB[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")]

#Aplicamos la predicción para el grupo A y el grupo B

predA = predict(mA, dfA)
predB = predict(mB, dfB)


#Predición para el grupo A
tablaPredA = table(predA,grupoA$NOTAFINALJUNIO)
tablaPredA

#Prediccion para el grupo B
tablaPredB = table(predB,grupoB$NOTAFINALJUNIO)
tablaPredB
#Prob. acertar en la predicción aprobado dado que ha aprobado (A)

#Grupo A
A=tablaPredA[1,1] / (tablaPredA[1,1] + tablaPredA[2,1])
A
#GRUPO B
A1=tablaPredB[1,1] / (tablaPredB[1,1] + tablaPredB[2,1])
A1

#Prob de que acierte en la prd de suspenso dado que ha suspendido (B)

#Grupo A
B=tablaPredA[2,2] / (tablaPredA[1,2] + tablaPredA[2,2])
B
#Grupo B
B1=tablaPredB[2,2] / (tablaPredB[1,2] + tablaPredB[2,2])
B1
#Prob de que el alumno aprueba dado que predice aprobar (C)

#Grupo A
C=tablaPredA[1,1] / (tablaPredA[1,1] + tablaPredA[1,2])
C
#Grupo B
C1=tablaPredB[1,1] / (tablaPredB[1,1] + tablaPredB[1,2])
C1
#Probabilidad de que suspenda dado que predice suspender (D)

#Grupo A
D=tablaPredA[2,2]/ (tablaPredA[2,1] + tablaPredA[2,2])
D
#Grupo B
D1=tablaPredB[2,2]/ (tablaPredB[2,1] + tablaPredB[2,2])
D1
#Probablidad de que el claisicador acierte en su prediccion (E)
#Grupo A
E=(tablaPredA[1,1]/46 *A) + (tablaPredA[2,2]/ 46 *B)
E
=======
#Grupo B
E1=(tablaPredB[1,1]/54 *A) + (tablaPredB[2,2]/54 *B)
E1


tablaPredB


##
##
##
#3 Ejercicio 3
##
##
##


#
#
#A --> Usando sólo información de grupo
#
#


mA3 <- naiveBayes(grupoA[,c("Grupo")], grupoA$NOTAFINALJUNIO)
mB3 <- naiveBayes(grupoB[,c("Grupo")], grupoB$NOTAFINALJUNIO)
mA3$apriori


dfA3 = grupoA[,c("Grupo")]
dfB3 = grupoB[,c("Grupo")]



predA3 = predict(mA3,dfA3)
predB3 = predict(mB3,dfB3)



tablaPredA3 = table(predA3,grupoA$NOTAFINALJUNIO)
tablaPredB3 = table(predB3,grupoB$NOTAFINALJUNIO)



tablaPredA3
tablaPredB3




#
#
#B --> Con el grupo, las practicas 1 y 2 si podemos obtener resultados
#
#

mA3 <- naiveBayes(grupoA[,c("Grupo","Practica1","Practica2")], grupoA$NOTAFINALJUNIO)
mB3 <- naiveBayes(grupoB[,c("Grupo","Practica1","Practica2")], grupoB$NOTAFINALJUNIO)




dfA3 = grupoA[,c("Grupo","Practica1","Practica2")]
dfB3 = grupoB[,c("Grupo","Practica1","Practica2")]



predA3 = predict(mA3, dfA3)
predB3 = predict(mB3, dfB3)


tablaPredA3 = table(predA3,grupoA$NOTAFINALJUNIO)
tablaPredB3 = table(predB3,grupoB$NOTAFINALJUNIO)

tablaPredA3
tablaPredB3



#
#
#C --> Usando datos de grupo, práctica 1, práctica 2 y examen Junio
#
#

mA3 <- naiveBayes(grupoA[,c("Grupo","Practica1","Practica2","EXAMENJUNIO")], grupoA$NOTAFINALJUNIO)
mB3 <- naiveBayes(grupoB[,c("Grupo","Practica1","Practica2","EXAMENJUNIO")], grupoB$NOTAFINALJUNIO)
mA3$apriori


dfA3 = grupoA[,c("Grupo","Practica1","Practica2","EXAMENJUNIO")]
dfB3 = grupoB[,c("Grupo","Practica1","Practica2","EXAMENJUNIO")]



predA3 = predict(mA3, dfA3)
predB3 = predict(mB3, dfB3)
predA3


tablaPredA3 = table(predA3,grupoA$NOTAFINALJUNIO)
tablaPredA3 = table(predB3,grupoB$NOTAFINALJUNIO)
tablaPredA3
tablaPredB3




#
#
#D --> Usando todos los datos disponibles de grupo, práctica 1, práctica 2, total prácticas y examen Junio
#
#


mA3 <- naiveBayes(grupoA[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")], grupoA$NOTAFINALJUNIO)
mB3 <- naiveBayes(grupoB[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")], grupoB$NOTAFINALJUNIO)
mA3$apriori


dfA3 = grupoA[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")]
dfB3 = grupoB[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")]


predA3 = predict(mA3, dfA3)
predB3 = predict(mB3, dfB3)
predA3


tablaPredA3 = table(predA3,grupoA$NOTAFINALJUNIO)
tablaPredB3 = table(predB3,grupoB$NOTAFINALJUNIO)

tablaPredA3
tablaPredB3


table(grupoA$Practica1)
table(grupoB$Practica1)

table(grupoA$Practica2)
table(grupoB$Practica2)



#
#
#
#Ejercicio 4
#
#
#

datos2 <- read.csv("C:/Users/Chira Ciprian/OneDrive - Universidad de Almeria/UAL/4 ING/SegundoCuatri/Estadística/Prácticas/Práctica 2/notasA3.csv", sep=",", dec=",", header=T); 
mAGrupo <- naiveBayes(datos[,c("Grupo","Practica1")], datos$NOTAFINALJUNIO)
mAGrupo$apriori

dfAGrupo = datos2[,c("Grupo","Practica1")]
predAGrupo= predict(mAGrupo, dfAGrupo)
table(predAGrupo)

  # Carga de datos
  datos2 <- read.csv("D:/Año2023/2Cuatrimestre/Estadistica/notasA3P.csv", sep=",", dec=",", header=T)


  mAGrupo <- naiveBayes(datos[,c("Grupo","Practica1")], datos$NOTAFINALJUNIO)
  mAGrupo$apriori
  
  dfAGrupo = datos2[,c("Grupo","Practica1")]
  predAGrupo= predict(mAGrupo, dfAGrupo)
  table(predAGrupo)
predAGrupo

  