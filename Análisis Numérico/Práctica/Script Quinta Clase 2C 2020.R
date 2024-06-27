##########################################################
#  ANALISIS NUMERICO(752)                                #
#  Prof. Julio Eduardo Fabris                            #
#	 CARRERA DE ACTUARIO                                   #
#  FACULTAD DE CIENCIAS ECONOMICAS                       #
#  UNIVERSIDAD DE BUENOS AIRES                           #
##########################################################

## 1er Cuatrimestre 2020
## Docente: Julio Fabris


# FUNCIONES MASTER SLAVES -----------------------------------------------

# Luego de haber visto creacion de funciones, veremos como 
# utilizar dichas funciones dentro de otras funciones. 
# Se utilizan los terminos "Master"("Maestro") o "Principal"
# y "Slave"("Esclavo") o "Replica", dado que hay dos funciones: 
# una principal ("master") y otra que actua 
# adentro como auxiliar ("slave").

# 1- Ejemplo 1 ------------------------------------------------------------

# Queremos una funcion que analice si los elementos de un 
# vector "v" son mayores o menores a un numero "d", 
# Si son menores debera reemplazarlos por 0. Si son mayores
# debera   reemplazarlos por 1.  
# El reporte sera el vector transformado

#Ejemplo:
#  v=c(10,12,31,14,25,61,72,40,20)

#[1] 10 12 31 14 25 61 72 40 20

#mayorque_v(v,26)

#Resultado:
#[1] 0  0  1  0  0  1  1  1  0


# 1.1.- Funcion Auxiliar ------------------------------------------------

# La funcion "mayor_que_n" verifica si el numero x es mayor
# que un numero d (ambos son argumentos).
# Si lo es , da como resultado "1".Si no lo es,"0".
# Fijarse que, para evitar confusiones, se le da un 
# nombre al programa esclavo, tal que si se verifica el nombre
# reporta "1" y si no se verifica  reporta "0"

mayor_que_n=function(x,d){
  if (x>d){
    return(1)
  }else{
    return(0)
  }
}




# 1.2.- Funcion Principal ------------------------------------------------

# La funcion "mayorque_v" aplica la funcion "mayorque_n"
# a cada elemento del vector dato y genera el vector resultado.

mayorque_v=function(v,d){
  a=v
  for (i in 1:length(v)){
    a[i]=mayor_que_n(v[i],d)
  }
  return(a)
}


# Ejemplo
v=c(10,12,31,14,25,61,72,40,20)

x=mayorque_v(v,26)

v;x

#Prueben que pasara si en la funcion mayorque_n() se 
#usa PRINT y no RETURN.

# Ejercicio para resolver en casa
# Utilizando el mismo programa mayorque_n escribir un programa
# que reemplace los mayores con "M", los menores con "m"

# 2- Ejemplo 2 -----------------------------------------------------------

# Creamos una funcion que de como resultado la suma de los elementos
# de una matriz. La particularidad de esta funcion es que
# debe verificar si los elementos de dicha matriz son o no pares .
# Si lo son, deben restarse.Si son impares deben sumarse. 
# Reportar el resultado de dicha suma.

#Ejemplo:
#  m=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
#      [,1] [,2] [,3]
#[1,]   10   12   31
#[2,]   14   25   61
#[3,]   72   40   20

#es_par_m(m)

#Resultado:
#-51

# 2.1.- Funcion Auxiliar -------------------------------------------------

# Para crear la funcion auxiliar tenemos varios caminos.
# Por un lado, podemos usarla para detectar aquellos elementos
# que son o no pares y , por otro lado, generar la modificación
# correspondiente a dicho elemento para simplificar la funcion principal


es_par=function(n){
  
}


# 2.2.- Funcion Principal ------------------------------------------------

# Utilizando la funcion auxiliar creamos una nueva funcion que 
# genere la sumatoria pedida en el enunciado






# Ejemplo

A=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)

rtado<-es_par_m(A)

A;rtado

# Ejercicio para resolver en casa
# Idem anterior pero que el esclavo cambie el signo de los pares y 
# mantenga el signo de los impares. Luego el master suma todos
# Verifique que el resultado sea el mismo que con la otra dupla de programas 


# 3- Ejemplo 3 -----------------------------------------------------------

# Creamos una funcion que, dada una matriz "m", ordene sus elementos
# de menor a mayor. Se debera ingresar en un nuevo argumento un "1"
# si el orden se retorna segun filas o un "2" si se quiere segun columnas

#Ejemplo:
#  m=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
#      [,1] [,2] [,3]
#[1,]   10   12   31
#[2,]   14   25   61
#[3,]   72   40   20

#ordenar_m(m,1)

#Resultado:
#      [,1] [,2] [,3]
#[1,]   10   12   14
#[2,]   20   25   31
#[3,]   40   61   72

# 3.1.- Funcion Auxiliar -------------------------------------------------

# La función auxiliar debera ordenar un vector de menor a mayor
# Para simplificar, pueden trabajar con un input transformado
# (Consejo:Ver ejercicio 10 de la Guia 3)


ordenarvector=function(x){
  contador=0
  while (contador==0){
    contador=1
    for (i in 1:(length(x)-1)){
      if (x[i]>x[i+1]){
        contador=0
        aux=x[i+1]
        x[i+1]=x[i]
        x[i]=aux
      }
    }
  }
  return(x)
}






# 3.2.- Funcion Principal ------------------------------------------------

ordenarmatriz=function(x,k){
  u=c(ordenarvector(x))
  if (k==1){
    return(matrix(u,nrow=nrow(x),ncol=ncol(x),byrow=T))
  }
  if (k==2){
    return(matrix(u,nrow=nrow(x),ncol=ncol(x),byrow=F))
  }
}





x=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
y=ordenarmatriz(m,2)
x;y


# 4- Ejemplo 4 -----------------------------------------------------------

# Creamos una funcion que, dada una matriz "m" y un numero "n", indique
# en una nueva matriz resultado con un "1" aquellos elementos que son
# multiplos de "n" y con "0" los que no

#Ejemplo:
#  m=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
#      [,1] [,2] [,3]
#[1,]   10   12   31
#[2,]   14   25   61
#[3,]   72   40   20

#multiplo_m(m,2)

#Resultado:
#      [,1] [,2] [,3]
#[1,]   1    1    0
#[2,]   1    0    0
#[3,]   1    1    1

# 4.1.- Funcion Auxiliar -------------------------------------------------





# 4.2.- Funcion Principal ------------------------------------------------





x=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
y=multiplo_m(m,1)
x;y


# 5- Ejemplo 5 -----------------------------------------------------------

# Generar un programa que, dada una matriz "m" de numeros enteros positivos,
# verifique si cada elemento es o no primo. El programa devolverá una matriz
# cuyos elementos sean "1" cuando el elemento de esa posición sea primo, y "0"
# cuando no lo sea. 
# Definición de numero primo: Número natural mayor que 1 que tiene únicamente
# dos divisores positivos distintos: él mismo y el 1

#Ejemplo:
#  m=matrix(1:36,6,6)
#      [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    7   13   19   25   31
#[2,]    2    8   14   20   26   32
#[3,]    3    9   15   21   27   33
#[4,]    4   10   16   22   28   34
#[5,]    5   11   17   23   29   35
#[6,]    6   12   18   24   30   36

#primo_matriz(m)

#Resultado:
#      [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    0    1    1    1    0    1
#[2,]    1    0    0    0    0    0
#[3,]    1    0    0    0    0    0
#[4,]    0    0    0    0    0    0
#[5,]    1    1    1    1    1    0
#[6,]    0    0    0    0    0    0

# 5.1.- Funcion Auxiliar -------------------------------------------------


# Generar un programa "es_primo"
# y verifique si los numeros de una matriz de enteros
# positivos son primos, indicando 1 para aquellos elementos
# que lo son y 0 para aquellos que no.

# Queremos ver si el numero x tiene algun divisor
# sin contar el 1 y el mismo (desde 2 a (x-1))
# Por defecto sabemos que 1 no es primo y 2 lo es,
# entonces vamos a generar en estos casos respuestas particulares
# ya que la secuencia 2:(x-1) va a fallar en esos casos







# 5.2.- Funcion Principal ------------------------------------------------







# Ejemplo
x=matrix(1:36,6)
y=primo_matriz(x)
x;y


# 6- Ejemplo 6 -----------------------------------------------------------

# Generar una funcion que, dada una matriz "m",
# ordene de menor a mayor la columna "d"  y en consecuencia 
# se ordenen las filas de la matriz.


#Ejemplo:
#  m=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
#      [,1] [,2] [,3]
#[1,]   10   12   31
#[2,]   14   25   61
#[3,]   72   40   20

#m_orden(m,3)

#Resultado:
#      [,1] [,2] [,3]
#[1,]   72   40   20
#[2,]   10   12   31
#[3,]   14   25   61


# 6.1.- Funcion Auxiliar --------------------------------------------------

# Dadas dos variables,¿Como podemos permutar su contenido?

# Consejo:"Replicar" la funcion ORDER (Ver HELP).
# (utilizando el mismo input y el mismo output)


# Una vez que tengan la funcion SORT replicada ( ejercicio 4) ,
# lo distinto es que, a medida que se van ordenando los elementos,
# los indices originales tambien se deben ordenar , generando asi
# el output de la funcion SORT





# 6.2.- Funcion Principal ------------------------------------------------

ordenarmat=function(x,d){
  p=matrix(0,ncol=ncol(x),nrow=nrow(x))
  for (i in 1:ncol(x)){
    p[,i]=x[,i][order(x[,d])]
  }
  return(p)
}



# Ejemplo

m=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
y=ordenarmat(m,3)
m;y

