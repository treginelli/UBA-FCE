
#  ANALISIS NUMERICO (752)                #
#  Prof. Fabris Julio Eduardo             #
#	 CARRERA DE ACTUARIO                    #
#  FACULTAD DE CIENCIAS ECONIMICAS        #
#  UNIVERSIDAD DE BUENOS AIRES            #

## 2do Cuatrimestre de 2020
## Docente: Julio Fabris


# 1.- MATRICES ------------------------------------------------------------


# 1.1- Generacion de la matriz --------------------------------------------


#a) Definiendo matrix(valores, n filas, n col)

matrix()


A<-1:9
matrix(A,3,3)  #completando la matriz con un valor ya existente (previamente)

matrix(c(3,4,5,1),2,2) #completa por default por columna. 

matrix(c(3,4,5,1),2,2, byrow=T) #completa con el vector por fila
                                #Puede ser T, TRUE o 1


#b) Mediante la funcion scan (permite introducir los valores de cada celda, manualmente):

M<-matrix(scan(),nrow = 4) #Se fijan previamente la cantidad de columnas, las filas 
                           # se determinaran por la cantidad de valores que se ingresen
M

#c) Mediante la union de vectores usando las funciones cbind() o rbind() 
#   para unir vectores en columnas o filas: 

A<-seq(1,20,2)
B<-seq(80,99,2)

J<-rbind(A,B) #vectores fila
J
C<-cbind(B,A) #vectores columna
C

#Para determinar si un objeto es una matriz:
is.matrix(A)

is.matrix(C)


# 1.2- Modificacion de elementos de las matrices --------------------------


A<-matrix(1:9,3,3) ; A

A[3,2]     #llama a un elemento en particular

A[3,]      #llama a un conjunto de elementos en particular
           #(en este caso a la tercer fila)

A[,2]      #llama a la segunda fila

A[2:3,3:3] #llama a una submatriz

A[3]       #llama al tercer elemento, recorriendo la matriz por columnas

A[3,2]<-23 #elemento de la fila 3, columna 2 a ser modificado
A

#Se le puede modificar los nombres a las filas y columnas con las funciones: 

rownames(A)<-c("Curso","De","Numerico")

A

dimnames(A) #devuelve los nombres de las filas y columnas de la matriz

dim(A) #devuelve la dimension de la matriz

A["Curso",]  #si las filas y columnas tienen nombres, 
             #se los puede usar para llamar elementos



# 1.3- Operaciones con matrices -------------------------------------------


B<-matrix(10:18,3,3);B
C<-matrix(11:19,3,3);C

A%*%B     #Producto de matrices

B%*%C  

t(A)      #Traspuesta

det(A)    #Determinante

solve(A)  #Inversa


# 1.4- operacion por filas y columnas -------------------------------------


#Funcion Apply()

args(apply)      #Vemos los argumentos que posee

apply(A,2,mean)  #Calcula la media de las columnas de la matriz 

apply(A,1,mean)  #Calcula la media de las filas de la matriz A 

apply(A,2,var)   #Calcula la varianza de las columnas de la matriz A 



# EJERCICIO 1 -------------------------------------------------------------

# Genere dos matrices de 3x3 y halle sus matrices inversas y sus determinantes.
# Con las matrices generadas, cree  una nueva matriz producto de ellas.
# Halle su traspuesta y sustituya la segunda columna por el vector c(1,5,3).

#Resolucion: 




# 2.- NUMEROS ALEATORIOS --------------------------------------------------



# 2.1-Generacion de n.a. segun distribuciones -------------------------------------


#Distribucion uniforme:
  runif(5,0,100)   #genera un vector con 5 elementos a partir de una dist. Uniforme 

#Distribucion normal: 
  rnorm(17,4,2.7)  #genera un vector con 17 elementos a partir de una dist. normal 
                   #con media 4 y desvio 2.7

#Distribucion binomial:
  rbinom(19,100,0.72) #genera un vector con 19 elementos a partir de una dist. binomial
                      #con parámetros  n = 100  y p = 0.72

#Algunos ejemplos de aplicacion: 

rn<-runif(12,min=0,max=74)
rn

rn2<-round(runif(12,min=0,max=74))
rn2

rn3<-matrix(round(runif(12,0,74)),3,4)
rn3

rn4<-matrix(rbinom(16,100,0.089),4,4)
rn4

#Un comando util para complementar la generacion de n.a. es 'replicate':

replicate(100,runif(1,0,1))
#notese que es lo mismo que correr: runif(100,0,1)

replicate(100,sum(runif(100,0,1)))  
#El comando permite replicar n veces otro comando

args(replicate)

# EJERCICIO 2.1 -------------------------------------------------------------

# Genere mediante las funciones vistas y dos valores "a" y "b" (siendo a<b)
# la forma de crear un vector de 10 numeros aleatorios enteros equiprobables
# en el rango [a,b].

#Resolucion: 



# EJERCICIO 2.2 -------------------------------------------------------------

# Mediante este ejercicio intentaremos demostrar la ley de los grandes numeros:

# Genere un vector con los valores 1 a 6 que represente los valores las caras de
# un dado, llamelo "dado".
# Encuentre una forma de "lanzar" dos dados obteniendo asi dos valores de caras
# Al "lanzarlos" multiples veces, demuestre que la media de la suma de
# ambas caras converge al valor esperado (7).


#Resolucion: 







# RESOLUCIONES

#Resolucion Ejercicio 1:

mat1=matrix(c(2,1,3,-1,2,4,0,1,3),3,3)
mat2=matrix(c(9,7,5,4,8,6,3,1,2),3,3)
inv_mat1=solve(mat1)
inv_mat2=solve(mat2)
det_mat1=det(mat1)
det_mat2=det(mat2)
mat3=mat1%*%mat2
t_mat3=t(mat3)
t_mat3[2,]=c(1,5,3)
t_mat3

#Resolucion Ejercicio 2.1:

a=10
b=50

runif()                  #genera un numero aleatorio entre 0 y 1
round(runif())           #genera un numero aleatorio entero {0,1}
round(runif()+a)         #genera un numero aleatorio entero {a,a+1}
round(runif()*(b-a)+a)   #genera un numero aleatorio entero [a,b]
round(runif(10)*(b-a)+a) #genera un vector de 10 numeros aleatorios enteros [a,b]

#Resolucion Ejercicio 2.2:

dado=1:6
sample(dado,2,T)
sum(sample(dado,2,T))
replicate(10000,sum(sample(dado,2,T)))
mean(replicate(10000,sum(sample(dado,2,T))))  
#Al aumentar las repeticiones del experimento, la media tiende a E(x)

