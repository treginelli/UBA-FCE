##########################################################
#         COMPUTACI�N CIENTIFICA ACTUARIAL (746)         #
#           CATEDRA DEPARTAMENTO DE MATEMATICA           #
#	                CARRERA DE ACTUARIO                    #
#            FACULTAD DE CIENCIAS ECON�MICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

## Asignatura: COMPUTACI�N CIENTIFICA ACTUARIAL 
## Docente: Rodrigo Del Rosso

################################
####### SETEO DE CARPETA #######
################################

getwd()

dir()

directorio <- choose.dir()

setwd(directorio)

getwd() ## verifico si me modific� la ruta


#########################
####### EJEMPLO 1 #######
#########################

y<-c(1,2,3,-1,0,-1,2,1,2)
x<-c(0,1,2,-2,1,-2,0,-1,1)

lm(y ~ x) 

summary(lm(y ~ x)) 

#########################
####### EJEMPLO 2 #######
#########################

datos<-read.table("PWT_2000.txt",header=TRUE,sep="") 
attach(datos)

head(datos)

plot(K,PIBPCL,col="red")

cor(K,PIBPCL)
cor.test(K,PIBPCL)                     ## significatividad
cor.test(K,PIBPCL,alternative="less")  ## contraste unilateral contra menor


PWT <- lm(PIBPCL ~ K) 

coef(PWT)

summary(PWT)

abline(PWT,col="blue")

hist(PIBPCL,main = "Histograma de PIBPCL", freq=FALSE) 
lines(density(PIBPCL), col = "red")

boxplot(PIBPCL,col="yellow")
title("Boxplot de PIBPCL")

#########################
## DIAGN�STICO GR�FICO ##
#########################

layout(matrix(1:4,2,2))    
plot(PWT) 


#########################
####### EJEMPLO 3 #######
#########################

rm(list = ls())

deuda<-read.csv("deuda_mex.csv",header =T)

attach(deuda)

head(deuda)

summary(y)
summary(x2,x3)
summary(deuda)

cor(deuda) 

pairs(deuda)

modelo <- lm(y ~ x2 + x3) 

summary(modelo) # Analizamos la significaci�n de la regresi�n

anova(modelo)   # otra forma

vcov(modelo)    # Matriz de Varianzas y Covarianzas de los Estimadores #

# Predicci�n

range(x2)
range(x3)
range(y)

modelo

1.381549 + 83 * 0.022279 + 22 *(-0.003898)

1.381549 + 85 * 0.022279 + 24 *(-0.003898)

predict(modelo)  ## predicci�n para cada uno de los valores observados de "y"

modelo$fitted.values  ## otra forma

# Chequeamos

round(predict(modelo)- modelo$fitted.values,0)

# Intervalo de predicci�n

x2<-c(83,95,125)
x3<-c(23,25,42)
new <- data.frame(x2,x3)
predict(modelo,new,interval = "prediction")

predict(modelo,new,interval = "prediction",level = 0.99)
predict(modelo,interval = "prediction")

# Intervalo de confianza
predict(modelo,new,interval = "confidence")         ## m�s preciso estimar una respuesta media que una respuesta global
predict(modelo,new,interval = "confidence",level=0.99)


# Ajustamos la recta de cuadrados m�nimos

lm(y ~ x)

lm(y ~ x1 + x2 + x3)

lm(y ~ x1 + x2 + x1 * x2)

## x1 cuantitativa
## x2 factor


######################################
####### DIAGN�STICO DEL MODELO #######
######################################

residuos<-modelo$residuals

resid(modelo)

summary(residuos)

## NORMALIDAD ##

qqnorm(residuos,col="red")
qqline(residuos,col="blue")

shapiro.test(residuos)

library(tseries)
jarque.bera.test(residuos)

library(nortest)
ad.test(residuos)

## INCORRELACI�N ##

library(lmtest)
dwtest(modelo)

## HOMOCEDASTICIDAD ##

bptest(modelo)

## ERROR DE ESPECIFICACI�N ##

m<-2

resettest(modelo,power=2:m)

resettest(PWT)

#######################################
####### CONTRASTES DE HIP�TESIS #######
#######################################

# Para la media de una poblaci�n normal

# Generamos una muestra de tama�o 30 de una distribuci�n normal con media 5 y desv�o 1. 

set.seed(34)            #Fijamos la semilla
x <- rnorm(30,5,1)      #Generamos una normal con media 5 y desv�o 1
t.test(x,mu=5)
       
# Podemos hacerlo con m�s argumentos

t.test(x, alternative = "two.sided",mu = 5,conf.level = 0.95)

# Si planteamos un test unilateral a izquierda

t.test(x, alternative = "less",mu = 5,conf.level = 0.95)

# Si planteamos un test unilateral a derecha para mu=4 y nivel de confianza 0.99

t.test(x, alternative = "greater",mu = 4,conf.level = 0.99)

# Para comparar la media de dos poblaciones normales independientes

# Por default es un test bilateral, y  un intervalo de confianza del 95%, diferencia 0 y varianzas distintas.
# Asumiendo varianzas iguales
# Por ejemplo, generamos otra muestra de tama�o 25 de una distribuci�n normal con media 6 y desv�o 1. 


set.seed(35)
y<-rnorm(25,6,1)
t.test(x,y,var.equal = TRUE)
     
#Si queremos ver si la media de x es menor en una unidad que la media de y

t.test(x,y,mu=1,alternative = "less",var.equal = TRUE)
      
# Cuando la variable de inter�s est� definida mediante un factor

# Generamos el data frame datos 

set.seed(20)
variable<-rnorm(40,20,2)
set.seed(20)
sexo<-sample(c("F","M"),40,replace=TRUE)
datos<-data.frame(variable,sexo)
t.test(variable ~ sexo, data=datos,var.equal=TRUE)

# Asumiendo varianzas distintas (Test de Welch)

set.seed(29)
x1<-rnorm(25,5,0.5)

set.seed(30)
x2<-rnorm(20,5.2,1)

t.test(x1,x2)

# Para muestras apareadas

set.seed(29)
w1<-rnorm(25,5,0.5)

set.seed(30)
w2<-rnorm(25,3,1)

t.test(w1,w2,paired=TRUE)  ## con la instrucci�n paired igual TRUE le indic� que las muestras son apareadas


# Test F para igualdad de varianzas
# La instrucci�n en R para aplicar el test F es  var.test
# El default es un test bilateral para cociente 1  y  un intervalo de confianza del 95%
# Chequeamos el supuesto para los test realizados anteriormente.

var.test(x,y)
 
var.test(x1,x2)
        
var.test(variable ~ sexo, data=datos)  ### el s�mbolo para categorizar "variable" por "sexo"
       
#Ver el help para otras opciones

#Test de Shapiro Wilks 
#Pone a prueba el supuesto de normalidad.
#Para los ejemplos vistos,

# Ejemplo 1
shapiro.test(x)
       
# Ejemplo 13
shapiro.test(y)

# Ejemplo 14
#Para muestras apareadas la diferencia debe tener distribuci�n normal (el supuesto es que la diferencia tiene distribuci�n normal)
 
shapiro.test(w1 - w2)
       
#Ejemplo 15, los grupos est�n dados por un un factor

tapply(datos$variable,datos$sexo,shapiro.test)

# Ejemplos

Materiales <- read.csv2(file.choose(),header=T)
edit(Materiales)
Regi�n<-Materiales$Regi�n
Precio<-Materiales$Precio
tapply(Precio,Regi�n,shapiro.test)
var.test(Precio~Regi�n)
       
#test de Student
t.test(Precio ~ Regi�n,var.equal=TRUE)


Street<-read.csv2(file.choose(),header=T)
Real<-Street$Real
Est<-Street$Estimaci�n

shapiro.test(Real-Est)
t.test(Real,Est,paired=TRUE)

## para armar una banda de confianza matplot (es un plot de matrices)

?prop.test  ## para igualdad de proporciones (es el asint�tico)

?binom.test ## es para proporciones pero es exacto a diferencia del anterior

?wilcox.test

?kruskal.test

?ts

# Datos en R



 
