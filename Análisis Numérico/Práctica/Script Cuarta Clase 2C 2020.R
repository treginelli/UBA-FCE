
#  ANALISIS NUMERICO(752)                 #
#  Prof. Fabris Julio Eduardo             #
#	 CARRERA DE ACTUARIO                    #
#  FACULTAD DE CIENCIAS ECONIMICAS        #
#  UNIVERSIDAD DE BUENOS AIRES            #

## 2do Cuatrimestre de 2020
## Docente: Julio Fabris

# CREACION DE FUNCIONES -----------------------------------------------


# Utilizando FUNCTION -----------------------------------------------

#ESTRUCTURA

# Nombre de la funcion=FUNCTION(argumentos){
# Acciones a realizar con los argumentos ingresados
# (se los llama por el nombre asignado)
# RETURN(el resultado que se quiere guardar)
# }

#Si la funcion no presenta errores, queda guardada en el environment. 


# 1- Ejemplo 1 -------------------------------------------------------

#Construimos una funcion que verifique si el elemento A
#es una matriz, un vector u otro objeto:
m=matrix(1:9,3,3)
v=sample(1:9,9,replace = T)

esmov<-function(A){                              
  if(is.matrix(A)==1){
    return("Es una matriz")
  }else{                        
    if(is.vector(A)==1){            
      return("Es un vector")
    }else{
      return("No es ni una matriz ni un vector")
    }
  }
}

esmov(m)
esmov(v)

c<-"Hola mundo"                 #Genero un string
esmov(c)                        #Pruebo la funcion

e<-data.frame(v,as.vector(m))   #Genero un data frame     
esmov(e)                        #Pruebo la funcion 

# 2- Ejemplo 2 -------------------------------------------------------

#Generamos una funcion que analice si los elementos
#de un vector son o no mayores a un numero (ej: 24):

#Primero,generamos un vector aleatorio con el que vamos a trabajar:

set.seed(1234)
v<-round(runif(30,min=0,max=100))
v


#La funcion "analizador" va a comparar cada elemento del vector 
#y va a devolver un vector de igual longitud, pero compuesto 
#de 2,1 o 0 segun sea mayor, igual o menor que un numero dado "n".

analizador<-function(a,n){
  b<-a
  for(i in 1:length(a)){
    if(a[i]>n){
      b[i]<-2
    }else{
      if(a[i]==n){
        b[i]<-1
      }else{
        b[i]<-0
      }
    }
  }
  print("Vector resultado")
  return(b)
}

analizador(v,15)   #verificamos la funcion para el vector que generamos

#Ahora hagamos una modificacion a n para verificar 

analizador(v, 60)  #volvemos a correr la función, Obviamente aparecen mas ceros


# EJERCICIO 1 --------------------------------------------------------

# Genere un programa que, dada una matriz "m" y un número "n", 
# analice los elementos de esta y de como resultado una matriz 
# que indique con un 2 si el elemento  es mayor a "n", 
# 1 si es igual y 0 si es menor.

#Ejemplo:
#  m= matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
#[,1] [,2] [,3]
#[1,]   10   12   31
#[2,]   14   25   61
#[3,]   72   40   20

#compare(m,20)

#Resultado:
#  [,1] [,2] [,3]
#[1,]    0    0    2
#[2,]    0    2    2
#[3,]    2    2    1

#Resolucion: 



# 3- Ejemplo 3 -------------------------------------------------------

#Cuando queremos iterar un proceso mientras se 
#cumpla una condicion dada, sin un numero
#prefijado de iteraciones, usamos WHILE().


#Creamos una funcion que haga una sucesion creciente de valores pares hasta "a". 

a=20

secuenciapar<-function(a) { 
  x=0
  vect=c()
  while(x <= a ){     #Hasta que x sea menor o igual a 20 lo repite
    vect=c(vect,x)
    x<-x+2 
  }          
  return(vect) 
}

#Ejemplo
secuenciapar(20)


#Otra forma de usar el WHILE() es de una forma similar al FOR(). 
#Utilizaremos un variable auxiliar (un contador) que va a llevar registro de la
#cantidad de iteraciones realizadas. Si en la condicion del WHILE()
# condicionamos el valor que puede tomar esa variable, 
#replicaremos el FOR().

# 4- Ejemplo 4 -------------------------------------------------------

#Vamos a crear una funcion que haga lo siguiente: 
#tirar un dado n veces y sumar los valores obtenidos
#hasta que, o bien se llegue a la cantidad de tiradas "n" o que la suma
#de los resultados de las tiradas supere el valor "s".
#Por defecto, el dado debe ser de 6 caras

sumadado<-function(n,d = 6,s){
  cont=0      #Usamos un contador para frenar la iteracion cuando 
  tir=0
  sum=0      # se cumpla la condicion cont=n
  tirada<-c()
  while(cont<n & sum<s){
    tir<-sample(1:d,1)
    tirada<-c(tirada,tir)
    cont=cont+1
    sum=sum+tir
  }
  print(tirada)
  print(sum)
  print(cont)
}

set.seed(123)
sumadado(10,d=6,30)
sumadado(10,d=6,30)
sumadado(10,d=6,30)
sumadado(10,d=6,30)

# EJERCICIO 2 --------------------------------------------------------

# Genere una funcion que, dados los argumentos "v" vector de tamaño generico
# y un numero "n", genere una matriz de n * n cuyos elementos seran los
# elementos de v ordenados fila por fila. En caso de faltar elementos se completara con 0.
#vec_mat<- function(v,n)
#  Ejemplo: 
#  v<-c(1,2,3,4,5,6,7)
#vec_mat(v,3)
#Resultado:
#  [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
#[3,]    7    0    0

#Resolucion: 


