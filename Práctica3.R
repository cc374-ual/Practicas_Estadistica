#Cargamos los datos

datos <- read.csv("C:/Users/Chira Ciprian/OneDrive - Universidad de Almeria/UAL/4 ING/SegundoCuatri/Estadística/Prácticas/Práctica 1/notasA3.csv", sep=",", dec=",", header=T);

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


#Ejercicio 1
   
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
    
    x=table(datos$NOTAFINALJUNIO,datos$EXAMENJUNIO)
    y=as.numeric(x)
    
    y
    
    
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
    
    
    
    
#Ejercicio 2

    datos <- read.csv("C:/Users/Chira Ciprian/OneDrive - Universidad de Almeria/UAL/4 ING/SegundoCuatri/Estadística/Prácticas/Práctica 1/notasA3.csv", sep=",", dec=",", header=T);
    
    # Reemplaza los NA
    datos[datos == ""] <- '0';
    
    # Reemplaza los NA

    #U1 = Nota Final Junio
    #U2 = Nota Examen Junio
    #
    #H0: La variable U1 = U2. (hipótesis nula)
    #H1: La variable U1 != U2.
    #
    #nivel de significancia alpha=0.5
    
    U1= as.numeric(datos[, "EXAMENJUNIO"])
    U1
    
    U2= as.numeric(datos[,"NOTAFINALJUNIO"])
    U2    
    testProp = t.test(U1,U2,paired=TRUE)
    testProp$p.value    
    
    #Obtenemos un p-valor de 4.479137*10^-36, que, al ser > 0.05, no podemos rechazar
    #la hipótesis nula, con lo cual podemos decir no hay diferencias
    #entre las dos variables


#Ejercicio 3

    
    datos <- read.csv("C:/Users/Chira Ciprian/OneDrive - Universidad de Almeria/UAL/4 ING/SegundoCuatri/Estadística/Prácticas/Práctica 1/notasA3.csv", sep=",", dec=",", header=T);
    
    # Reemplaza los NA
    datos[datos == ""] <- '0';
    
    # Reemplaza los NA
    
    #U1 = Nota Final Septiembre
    #U2 = Nota Examen Septiembre
    #
    #H0: La variable U1 = U2. (hipótesis nula)
    #H1: La variable U1 != U2.
    #
    #nivel de significancia alpha=0.05
    
    
    U1= as.numeric(datos[,"NOTAFINALSEPTIEMBRE"])
    U2= as.numeric(datos[,"EXAMENSEPTIEMBRE"])    
    
    testprop2=t.test(U1,U2, paired=TRUE)    
    testprop2$p.value
    
#---------------------------------------------Ejercicio 4---------------------------------------------
  
  grupoA <- subset(datos, Grupo == "A");
  grupoB <- subset(datos, Grupo == "B");
  
    #U1 = Nota Examen Junio Grupo A
    #U2 = Nota Examen Junio Grupo B
    #
    #H0: La variable U1 es independiente de U2. (hipótesis nula)
    #H1: La variable U1 es dependiente de U2.
    #
    #nivel de significancia alpha=0.05  
  
  notaExamenA <- as.numeric(grupoA[, "EXAMENJUNIO"])
  notaExamenB <- as.numeric(grupoB[, "EXAMENJUNIO"])
  
  
  t1
  
  
  notaFinalJunioA  <- as.numeric(grupoA[, "NOTAFINALJUNIO"])
  notaFinalSeptiembreA  <- as.numeric(grupoA[, "NOTAFINALSEPTIEMBRE"])
  grupoB <- subset(datosNum, Grupo == "B");
  notaExamenB<- as.numeric(grupoB[, "EXAMENJUNIO"])
  notaFinalJunioB <- as.numeric(grupoB[, "NOTAFINALJUNIO"])
  notaFinalSeptiembreB  <- as.numeric(grupoB[, "NOTAFINALSEPTIEMBRE"])
    
  NotaExamen <- t.test(notaExamenA, notaExamenB)
  NotaFinalJunio <- t.test(notaFinalJunioA, notaFinalJunioB)
  NotaFinalSeptiembre <- t.test(notaFinalSeptiembreA, notaFinalSeptiembreB)
    
  NotaExamen
  NotaFinalJunio 
  NotaFinalSeptiembre 
    