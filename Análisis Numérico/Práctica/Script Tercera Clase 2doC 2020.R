#  ANALISIS NUMERICO(752)                 #
#  Prof. Fabris Julio Eduardo             #
#	 CARRERA DE ACTUARIO                    #
#  FACULTAD DE CIENCIAS ECONIMICAS        #
#  UNIVERSIDAD DE BUENOS AIRES            #

## 2do Cuatrimestre de 2020
## Docente: Julio Fabris


# 1.- Estructura IF ELSE --------------------------------------------------

# IF(condicion){
# Instrucciones que se realizan en caso de ser TRUE la condicion
# }else{
#  instrucciones que se realizan en caso de ser FALSE la condicion 
# }


# 1.1.- Ejemplo 1 --------------------------------------------------

#Vamos a crear una funcion que determine si un objeto "a"
# tiene un valor 10

a<-10      #defino "a" como cualquier valor que quiera probar

if(a==10){ 
  #Se lee: si el objeto  "a" es igual a 10 
  print("El valor coincide")
}else{                         
  #Se lee: en caso contrario..
  print("El valor no coincide")
}

# Verificamos
a=11

# Y corremos otra vez el "if"


# 1.2.- Ejemplo 2 --------------------------------------------------

#Se pueden establecer varias condiciones. Por ejemplo, 
#vamos a verificar que, ademas de a=10, b deba valer 20

a<-10
b<-11

if(a==10 & b==20){
  print("Se cumplen ambas condiciones")
}else{
  print("No se cumplen ambas condiciones")
}
# Ahora cambiamos b
b=20
# Y corremos otra vez el "if"
#Podemos ir por distintos caminos a realizar en caso de que se
#verifiquen distintas condiciones.

# 1.3.- Ejemplo 3 --------------------------------------------------

#si ambos valores valen 10 y 20 respectivamente, devuelve un mensaje.
#Si no se cumple esa condicion, pero si que uno de los dos sea el
#valor esperado, emitimos otro mensaje distinto al primero.
#Si no se cumple nada, otro mensaje distinto a los anteriores. 

if(a==10 & b==20){
  print("Se cumplen ambas condiciones")
}else{
  if(a==10 | b==20){
    print("Se cumple una condici?n")
  }else{
    print("No se cumple ninguna condici?n")
  }
}

#Otros operadores logicos

a & b           #y
a | b           #o
a > b           #mayor a
a >= b          #mayor o igual a
a != b          #distinto de
isTRUE(a=b)     #devuelve un valor logico


# EJERCICIO 1 -------------------------------------------------------------

# Genere un programa que, dada una matriz "m", indique su determinante en caso de
# ser cuadrada, en caso contrario que imprima " la matriz no es cuadrada"

#Resolucion: 



# 2.- Estructura FOR --------------------------------------------------

#Esta funcion nos permite realizar una accion  una 
#cantidad de veces determinada.


#FOR(variable IN secuencia){
# acciones a realizar en cada iteracion
#}


# 2.1.- Ejemplo 1 --------------------------------------------------

#Generaremos un loop utilizando FOR para que sume los numeros desde 1
# hasta 20

x<-0  # inicializo la variable en 0
for(i in 1:20){  
  
  #Se lee: Para cada numero entre (incluyendo) el 1 y el 20  
  #"i" es  el contador, puedo asignarle
  #cualquier otro nombre.
  x<-x+i   #Se realiza la siguiente accion: se suma al valor x
  #el valor "i" correspondiente a esa iteracion
  #"i" cambia de valor, Valiendo desde a 1 hasta 20
  #avanzando de 1 en 1.
}
print(x)


#lo cual es igual a sumar los primeros 20 numeros
1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20
# Comprobación, genero un vector con una secuencia de 1 a 20
# y sumo sus elementos
sum(1:20)

# 2.2.- Ejemplo 2 --------------------------------------------------

#Ahora vamos a combinar estas funciones con manejo de vectores
#En el vector x, vamos a reemplazar todos los componentes
#mayores a 20 con un valor 0


x<-c(1,8,99,10,43,3,18) #defino el vector x
x

for(i in 1:length(x)){  # para cada uno de los elementos del vector...
  if(x[i]>20){          #verifica que se cumpla la condicion en cada 
    x[i]<-0             #elemento y realiza la siguiente accion.
  }   }
print(x)

# 2.3.- Ejemplo 3 --------------------------------------------------

#Ahora crearemos una estructura que, al ingresar un vector,
#sume sus elementos hasta encontrar algun elemento que sea igual a 8.
# Para eso tenemos que definir el comando "break" que interrumpe el "for"
v <-c(6,5,8,3)

aux<-0                   
for(i in 1:length(  v )){   #Recorre el vector 
  if(v[i]==8){ break        #Verifica que el elemento sea igual a 8 , si lo es
  }else{                    # deja de sumar
    aux<-aux+( v[i])        #Si no es igual a 8  lo suma 
  }
}
print(aux)

# EJERCICIO 2 -------------------------------------------------------------

# Genere un programa que, dado un vector de numeros enteros "v",
# imprima un vector "rtado" cuyos elementos sean las sumas parciales de los
# elementos de v, siendo el  primer valor de "rtado" igual al primer valor de v,
# el segundo la suma de los dos primeros valores de v, y así, hasta el  enesimo
#  valor de "rtado" que será la suma de todos los elementos de v.
# En caso de ser necesario, pueden crear variables auxiliares.

#Resolucion: 



# 3.- Estructura DOUBLE FOR --------------------------------------------------


# 3.1.- Ejemplo 1 --------------------------------------------------

Matriz<-matrix(c(1,8,99,10,43,3,18,4,77),ncol=3,nrow=3)

Matriz

#La consigna es la misma que antes: reemplazar los valores
#mayores a 20 por ceros. 

for(i in 1:nrow(Matriz)){     #Para cada fila
  for(j in 1:ncol(Matriz)){   #Para cada columna
    if(Matriz[i,j]>20){       #Verificamos si se cumple la condicion
      Matriz[i,j]<-0          #Si se cumple, realizamos la accion
    }
  }
}
print(Matriz)

#Esto significa que el programa avanzara de las siguiente manera:
#Comienza por verificando el elemento Matriz[1,1].
#Luego seguira por Matriz [1,2] y asi hasta completar la primera fila.
#Luego verifica el elemento Matriz[2,1] y asi sucesivamente ira 
#verificando cada elemento. Primero verifica toda una fila para pasar
#a la siguiente

# EJERCICIO 3 -------------------------------------------------------------

# Repita el Ejercicio 2 pero utilizando doble FOR.

#Resolucion:



# 4.- Estructura WHILE --------------------------------------------------

#Si queremos realizar una tarea pero no sabemos cuantas veces sera
#necesario hacerlo, podemos usar la funcion while. 
#Esta realiza una tarea en tanto se cumpla una condicion. 
#Cuando la condicion deja de cumplirse, se corta el loop y se deja
#de realizar la tarea. 


#WHILE(condicion que siempre debe cumplirse){
# Acciones a realizar mientras se cumpla la  condicion 
#}


# 4.1.- Ejemplo 1 --------------------------------------------------

#Queremos contar la cantidad de veces que debemos simular valores
#con  distribucion  normal hasta que nos salga un valor extremo.
#(Por ejemplo, mayor a 3 desvios de la media)


a<-2    #media
b<- 0.3 #desvio estandar

contador<-0     #valor auxiliar que usaremos para contar
#la cantidad de iteraciones

sim<-2          #valor simulado. Le damos valor inicial 2 
#para que cumpla la condicion la primera vez.
while(sim<2.9){
  #Se lee: Mientras sim sea menor a 2.9...
  sim<-rnorm(1,mean=a,sd=b)
  contador<-contador+1
}  

print(sim)      #valor simulador que hizo FALSE la  condicion 
print(contador) #cantidad de iteraciones que fueron necesarias.

#Previo a cada  iteracion , el programa verifica la  condicion . 
#si esta no se cumple, realiza la lista de acciones especificada.
#Si se cumple, se sale del loop.

# EJERCICIO 4 -------------------------------------------------------------

# Realice un programa que, dado un vector "v" de  numeros  enteros y un
# valor " x", sume los elementos del vector hasta que la suma supere a  x.
# Indique el valor de esta suma generada.
# Si es posible, indique tambien en la variable "cant" la cantidad de elementos
# que componen la suma

#Resolucion :



# 5.- Funcion  break --------------------------------------------------

#Break : Es utilizado para interrumpir loops generados por ejemplo con 
#estructuras como FOR o WHILE, incluso si es afirmativa la condicion de los loops.
#Se puede utilizar tambien luego de un IF y un IF ELSE ,indicando una condicion que en caso de
#cumplirse haga detener las iteraciones/comandos. 


# 5.1-Ejemplo 1- break en FOR -------------------------------------------------------

for(i in 2:12){     #Para todos los numeros de 2 a 12. Imprime todos hasta que  
  print(i)           
  if(i>=8)          #se cumple la condicion  dentro de el IF (que i es mayor o igual a 8). 
    break           #En ese momento deja de imprimir los i (interrumpe el comando).  
}

# 5.2-Ejemplo 2- break en WHILE -----------------------------------------------------

#la estructura va a tener como argumento un numero que si es mayor a 0
#va a imprimir una sucesion decreciente hasta llegar al 5. En dicho numero corta la sucesion. 

c=5
while(c>0){     #Para que empiece la sucesion el numero c tiene que ser mayor a 0 como condicion.
  print(c)      #Lo imprime y luego comienza a restarle uno.
  c=c-1
  if(c==5)      #Al llegar al numero 5 como este cumple la condicion de el if , la sucesion se corta. 
    break
}





# RESOLUCIONES

#Resolucion Ejercicio 1:

v=matrix(runif(9)*25,3,3)
if(nrow(v)==ncol(v)){
  det(v)
}else{
  print("no es posible hallar su determinante")
}

#Resolucion Ejercicio 2:

v=c(1,2,3,-4,6,8,9)
aux=c()
for (i in 1:length(v)) {
  aux=c(aux,sum(v[1:i]))
}
print(aux)

#Resolucion Ejercicio 3:

v=c(1,2,3,-4,6,8,9)
aux=c()

for (i in 1:length(v)) {
  aux1=0
  for (j in 1:i) {
    aux1=aux1+v[j]
  }
  aux=c(aux,aux1)
}
print(aux)

#Resolucion Ejercicio 4:

v=c(1,3,4,7,7,9)
n=16
aux=1
suma=0
cant=0
while (suma<n) {
  suma=suma+v[aux]
  cant=cant+1
  aux=aux+1
}
print(suma)
print(cant)
