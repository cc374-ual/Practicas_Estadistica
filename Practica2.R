# Carga un dataset por defecto en R
datos <- read.csv("D:/Año2023/2Cuatrimestre/Estadistica/notasA3.csv", sep=",", dec=",", header=T);

datos
#Instalar paquetes
install.packages("e1071")    # DESCOMENTA ESTA LÍNEA

# carga el paquete
library(e1071)

# Reemplaza los NA
datos[datos == ""] <- '0';
datos

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

#NaiveBayes [Ejercicio 2]
m<-naiveBayes(datos$NOTAFINALJUNIO~.,data=datos)
#Ver el resultado de NBayes
m


# Extrae predicciones para evaluar el clasificador
# A= Predice que aprueba
# B= Aprueba
df = datos[,c("Grupo","Practica1","Practica2","Practica3","TOTALpracticas","EXAMENJUNIO")]
pred = predict(m, df)

table(pred,datos$NOTAFINALJUNIO)



# Seleciona las variabels categóricas para el modelo
df = datos[,c("V1","V2","S")]

# Selecciona los datos de la variable a predecir Y (variable clase)
Y = df[,"S"]
X = df[,c("V1","V2")]


m = naiveBayes(X,Y) #m <-naiveBayes(NF~.,data,)

# Calcula las probabilidades "a mano" y comprara
m$apriori

# Distribución de la clase P(S)
m$apriori
nclase = table(Y)

# P(V1|Species)
m$tables$V1

nconjunta = table(df$S, df$V1)
prop.table(table(df$S, df$V1), margin=1)
nconjunta["setosa","FALSE"]/nclase["setosa"]



# Extrae predicciones para evaluar el clasificador
pred = predict(m, df[,c("V1","V2")])

# Evaluar capacidad de prediccion del modelo ---------
# Probabilidad de que acierte en la predicción dado que el tipo es setosa. 
# Si T = 'El clasificador predice setosa' y S = 'setosa'
# P(T|S) = P(T y S)/P(S)

nconjunta = table(pred, df$S)  #los valores de la izq son los de la primera variable
nS = table(df$S)
nconjunta["setosa","setosa"]/nS["setosa"]




