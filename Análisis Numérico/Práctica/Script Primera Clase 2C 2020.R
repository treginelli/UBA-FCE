
#  ANALISIS NUMERICO(752)                 #
#  Prof. Fabris Julio Eduardo             #
#	 CARRERA DE ACTUARIO                    #
#  FACULTAD DE CIENCIAS ECONIMICAS        #
#  UNIVERSIDAD DE BUENOS AIRES            #

## 2do Cuatrimestre de 2020
## Docente: Julio Fabris


#Esto es un script#

#Todo lo que se escribe detras de un "#" es un comentario. 



# 1.- R COMO CALCULADORA --------------------------------------------------


# 1.1-   Operaciones basicas ----------------------------------------------


4+5

4*4

20/4

4^2

2^(1+4)

2^(1/2)


# 1.2- Asignaciones -------------------------------------------------------


# Los operadores "<-" y "=" se pueden utilizar para asignar un valor 
#  a una variable
A=56 
A
A<-55   ;     A    	# Se pueden ejecutar varios comandos juntos separados por punto
# y coma. El nuevo valor ingresado reemplazó al valor anterior
B<-23*8
C<-A+B               # Aquí se realizan operaciones con los valores ingresados en las 
# variables. Se asignan a otra.
D=C-2*A

#Importante: R distingue entre mayusculas y minusculas (en ingles, es "case sensitive"). 


# 1.3- Funciones varias ---------------------------------------------------


#Se puede pedir informacion sobre una funcion y sus argumentos de las siguientes formas: 

help("cos")
help(cos)
?cos		# Ver que el argumento de las funciones trigonométricas debe 
# estar en radianes. 


cos(pi)#Numerico 

sin(pi)

sqrt(9)

log10(8)     #Logaritmo con base 10

log(8)       #Logaritmo natural (con base e)

exp(log(8))  #Funcion exponencial

abs(-4)      #Valor absoluto

factorial(8)   #Numero factorial 

choose(5,2)   #Numero combinatorio. 

#Derivadas. 

func=expression(x^2+5*x)         #Utilizamos función el comando "expression" para cargar
#la funcion que deseamos derivar.

D(func,"x")
D(expression(x^2+5*x),"x")  #Luego utilizamos D para derivar la funcion y entre comillas  
#indicamos en funcion de que variable derivar.

#Evaluar una funcion en un valor determinado.

x=5             # Le asignamos un valor a la variable x
eval(x^2+7)     # El comando "eval" procedera a reemplazar a x por 5 y arrojar el resultado.
eval(func)	    # Equivalente

#Tratamiento de decimales

round(3.8758397)     # por default cantidad de digitos es 0
round(3.8758397,2)   # Pero se puede indicar otra  cantidad con el 2do argumento
                     # por default cantidad de digitos es 6.
signif(3.8758397,2)   
trunc( 3.8758397)    # En este caso el comando directamente elimina los decimales. 
                     # No tiene 2do argumento
floor( 3.8758397)		 # Redondea al entero inferior
ceiling(3.45141)		 # Redondea al entero superior

# ¿Cómo haría para redondear para abajo, como hace floor, pero con 2 decimales?

# 1.4- Sucesiones y secuencias --------------------------------------------


# La funcion seq() produce secuencias equi-espaciadas, con paso que se puede determinar: 
# seq(valor inicial, valor final, paso).

seq(from=1,to=  20 ,by=3)	  # Cuando se explicitan los argumentos, no importa su orden 
seq(to=  20 , from=1,by=3)	# Es equivalente
seq(1,20,3 )		          	# Cuando no se explicitan deben incluirse en el órden indicado
                            # por la descripción del comando (en el help)
A=seq(0,100,5)

7:26 #para que vaya desde el 7 al 26 de 1 en 1

# La funcion rep produce la repeticion de un valor determinado una cantidad dada de veces: 
# rep(valor, longitud).
uno_cinco=rep(1,5)
cuatro_diez=rep(4,10)


#los objetos se pueden borrar con la funcion rm(obj1, obj2,...)

rm(uno_cinco)
rm(uno_cinco,cuatro_diez)



# EJERCICIO 1 -------------------------------------------------------------

# Generar un objeto que sea una secuencia entre el valor  
# del logaritmo base 10 de 18 y el producto entre el valor 
# redondeado con 2 decimales segun digitos significativos de 55.4248 y la raiz de 15.
# El paso debe ser de 7 unidades.


#Resolucion:
k=seq(from=log10(18),to=signif(55.4248,4)*sqrt(15),by=7)



# 2.- VECTORES ------------------------------------------------------------


# 2.1- Generacion del vector ----------------------------------------------


#La forma mas comun es a traves de la funcion concatenar:
A<-c(1,4,(7*8))		# c(v1,v2,v3) concatena los elementos que se ponen entre comas


#Pero tambien se puede dar a partir de función los comandos "seq" o "rep": 
B<-seq(5,7)
C<-rep(7,5)

#El vector puede no ser numerico: 
D<-c("Econometria","AM2","Micro 1")     #Strings  #Caracteres
E<-c(TRUE,FALSE,TRUE,TRUE)              #Logico
E<-c(T,F,T,T)                           #Logico, otra forma de escribirlo


#para saber si un objeto es un vector:
is.vector(D)
data.class(E) # indica la clase del vector E

#Para saber la longitud (cantidad de elementos un vector):
A <-c(12,1,22)
A
length(A)

#Para ordenar y revertir los elementos de un vector:
rev(A)
J<-rev(A)
sort(A) 		# Por defecto ordena de menor a mayor los elementos pero
A           # el vector no se ve alterado
A=sort(A)   # ahora si porque hay una asignación
A
sort(A,decresing=TRUE) #para ordenarlos decresientemente

K<-c(22,1,12)
# Si pensamos los elementos del vector como J=(1ro,2do,3ro), ¿en que orden
#  los pondríamos si queremos que figuren de menor a mayor?

order(K) 
#  En este caso sería 2do,3ro,1ro. Se usa para ordenar tablas   (ver el help)
#te dice las coordenadas del vector ordenadas



# 2.2- Operaciones con vectores -------------------------------------------


A<-c(3,4,7)    ;   B<-c(5,3,2)
A+B

A*B     #producto componente a  componente

A%*%B   #producto escalar  # A veces se escribe A %*% B para leerlo mejor


A^2     #La potencia se aplica a cada elemento del vector

8*A     #Cada elemento del vector se multiplica por el escalar. 

sum(A)  #Devuelve la suma de los componentes del vector


# 2.3- Modificacion de vectores -------------------------------------------


B[2]      #B[i] llama al elemento con posicion "i" dentro del vector "B"

B[2]=57   #Redefine el elemento llamado

B

sample(A, 7 ,replace = T)  #Del vector A, se extraen 
                           #7 numeros al azar pudiendose repetir (con reposicion) 



# EJERCICIO 2 -------------------------------------------------------------

# genere dos vectores, el primero que sea una secuencia del 1 al 7 y el segundo 
# igual pero revirtiendolo mediante alguna funcion.
# Realice el producto componente a componente.
# al nuevo vector modifiquele el cuarto elemento y sustituyalo por 56


#Resolucion: 
v1=c(seq(1,7))
v2=rev(v1)
v3=v1*v2
v3[4]=56
v3




# RESOLUCIONES

#Resolucion Ejercicio 1: 
X<-seq(log10(18),(signif(55.4248,4)*sqrt(15)),7)
X

#Resolucion Ejercicio 2:
v1=c(1:7)
v2=rev(v1)
v3=v1*v2
v3[4]=56
v3
