# Carga un dataset por defecto en R
datos <- read.csv("D:/Año2023/2Cuatrimestre/Estadistica/notasA3.csv", sep=",", dec=",", header=T);


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

E=(tablaPredA[1,1]/46 *A) + (tablaPredA[2,2]/ 46 *B)
E


#3 Ejericicio

#A --> Como conslusión podemos decir que los datos son insucientes para poder predecir un resultado
  mA3 <- naiveBayes(datos[,c("Grupo")], datos$NOTAFINALJUNIO)
  mA3$apriori
  
  dfA3 = datos[,c("Grupo")]
  predA3 = predict(mA3, dfA3)
  predA3
  tablaPredA3 = table(predA3,datos$NOTAFINALJUNIO)
  tablaPredA3
#B --> Con el grupo, y las practicas 1 y 2 si podemos obtener resultados
  mA3 <- naiveBayes(datos[,c("Grupo","Practica1","Practica2")], datos$NOTAFINALJUNIO)
  mA3$apriori
  
  dfA3 = datos[,c("Grupo","Practica1","Practica2")]
  predA3 = predict(mA3, dfA3)
  predA3
  tablaPredA3 = table(predA3,datos$NOTAFINALJUNIO)
  tablaPredA3
#C --> Al añadir una nueva variable el resultado es algo mas preciso
  mA3 <- naiveBayes(datos[,c("Grupo","Practica1","Practica2","EXAMENJUNIO")], datos$NOTAFINALJUNIO)
  mA3$apriori
  
  dfA3 = datos[,c("Grupo","Practica1","Practica2","EXAMENJUNIO")]
  predA3 = predict(mA3, dfA3)
  predA3
  tablaPredA3 = table(predA3,datos$NOTAFINALJUNIO)
  tablaPredA3
#D --> Al usar todos los datos el resultado obviamente es más preciso
  mA3 <- naiveBayes(datos[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")], datos$NOTAFINALJUNIO)
  mA3$apriori
  
  dfA3 = datos[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")]
  predA3 = predict(mA3, dfA3)
  predA3
  tablaPredA3 = table(predA3,datos$NOTAFINALJUNIO)
  tablaPredA3
#Ejercicio 4
  
  






