#PRACTICA 3 DE R

#EJERCICIO 1
#1) Escriba un programa - funci�n cuya salida sea el elemento n-�simo de la sucesi�n de Fibonacci.
#La sucesi�n de Fibonacci tiene como sus dos primeros elementos el 0 y el 1. 
#Los restantes t�rminos se calculan como la suma de los dos anteriores.
fibonacci=function(n){
  aux=c(0,1)
  for (i in 3:n){
    aux[i]=aux[i-1]+aux[i-2]
  }
  return (aux[n])
}
fibonacci(10)


#EJERCICIO 2
#2) Escriba un programa (funci�n) que determine si un n�mero entero positivo dado es par o impar.
#Recuerde que puede hacerlo comprobando si el n�mero es divisible exactamente por 2.
#La salida deber� ser 1 si es par y 0 si no es par.
#Luego, utilizando ese programa como esclavo,
#escriba una nueva funci�n que cuente los n�mero pares e impares de una matriz
#de tama�o arbitrario que contiene elementos enteros positivos.
#La salida de este programa ser� una matriz cuyo primer elemento sea la cantidad de valores pares en
#A y el segundo elemento la cantidad de valores impares en A.

es_par=function(n){
  if (n%%2==0){
    return (1)
  }else{
    return(0)
  }
}

contador_de_par_impar=function(m){
  pares=0
  impares=0
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (es_par(m[i,j])==1){
        pares=pares+1
      }
      if (es_par(m[i,j])==0){
        impares=impares+1
      }
    }
  }
  z=c(pares,impares)
  return(z)
}
A=matrix(c(10,12,31,14,25,61,72,40,20),3,byrow=T)
A;contador_de_par_impar(A)


#EJERCICIO 3
#3) Escriba un programa - funci�n que reporte una aproximaci�n del n�mero pi sumando "n" t�rminos
#de la serie de Leibnitz.
#??(???1)????2????+1????????=0=????4
#Verificar que, al aumentar "n", disminuye el error.

aprox_pi=function(n){
  aux=c()
  for (i in 0:n){
    aux[i+1]=((-1)^i)/(2*i+1)
  }
  b=sum(aux)
  return(4*b)
}
aprox_pi(10000000)



#EJERCICIO 4
#4) Escriba un programa (funci�n) que calcule el n�mero pi mediante simulaci�n de 'lluvia' al azar,
#usando n observaciones. El proceso es el siguiente:
#Se generan puntos al azar (x, y) con x, y pertenecientes a [0, 1] y se verifica
#si pertenecen o no al interior de un cuarto de c�rculo de radio uno 
#(Es decir, se comprueba si x^2+y^2 < 1).
#La proporci�n de puntos en el interior del c�rculo converge en probabilidad al �rea
#de ese cuarto de c�rculo a medida que se usan m�s puntos. 
#Nota: El �rea de un cuarto de c�rculo de radio 1 es A=pi/4.

aproximacion_pi=function(n){
  contador=0
  for (i in 1:n){
    x=runif(1)
    y=runif(1)
    if ((x^2+y^2)<1){
      contador=contador+1
    }
  }
  proporcion=contador/n
  return(4*proporcion)
}

aproximacion_pi(10000)



#EJERCICIO 5
#5) Escriba una funci�n en R que devuelva el m�nimo elemento del vector x.
minimo=function(v){
  return(min(v))
}
c=c(100,12,31,14,25,61,72,40,20)
c;minimo(c)



#EJERCICIO 7
#Escriba una funci�n en R que se aplique sobre un escalar x que sea entero y positivo
#y determine si x es primo o no.
#Un n�mero primo es aquel que s�lo es divisible por 1 y por s� mismo.
#Para saberlo deber� dividir el n�mero por todos los otros entre 1 y x y ver si surge alg�n resto cero.
#Si el n�mero es primo el programa retorna un valor 1. Si no lo es retorna un valor 0.


es_primo=function(x){
  contador=0
  for (i in 1:x){
    if (x%%i==0){
      contador=contador+1
    }
  }
  if (contador==2){
    return(1)
  }else{
    return(0)
  }
}
es_primo(8)


#EJERCICIO 8
#Escriba una funci�n en R que aplique el programa anterior
#como esclavo para determinar si los elementos de una matriz o vector y de enteros positivos son primos.
#El programa deber� retornar un vector o matriz de igual dimensi�n que y,
#pero con 1s donde los elementos correspondientes de y son primos y 0s donde los elementos de y son no primos.

primo_m=function(m){
  aux=m
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (es_primo(m[i,j])==1){
        aux[i,j]="1s"
      }else{
        aux[i,j]="0s"
      }
    }
  }
  return(aux)
}

x=matrix(1:36,6)
x;primo_m(x)


#EJERCICIO 9
#Escriba un programa tipo funci�n que genere una matriz A de dimensi�n m x n.
#Dicha matriz deber� tener como elementos n�meros primos entre 0 y 101.
m_primos=function(m,n){
  v=seq(0:101)
  c=c()
  j=0
  for (i in v){
    if (es_primo(v[i])==1){
      j=j+1
      c[j]=v[i]
    }
  }
  return(matrix(c,m,n))
}
m_primos(5,5)


#EJERCICIO 10
#Escriba una funci�n en R tal que tome a un vector x como argumento de entrada y
#devuelva un vector y cuyos elementos surgen de ordenar x de menor a mayor mediante el siguiente
#procedimiento ("M�todo de la Burbuja" o "bubble sort"):
#Se recorre todo el vector x comparando cada elemento con el anterior.
#Si est�n en orden incorrecto se permutan y se contin�a avanzando, comparando y si es necesario, permutando.
#Una vez que se llega al final de x se vuelve a comenzar.
#El proceso termina cuando, ante un recorrido completo en x no se realiza ninguna permutaci�n.

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

v=c(10,12,31,14,25,61,72,40,20)
v;ordenarvector(v)


#EJERCICIO 11
#Escriba una funci�n similar a la anterior pero que admita un segundo argumento,
#tal que si vale 0 ordena de menor a mayor, mientras que si vale 1 ordena de mayor a menor.

ordenarvector=function(x,y){
  contador=0
  if (y==0){
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
  }
  if(y==1){
    while (contador==0){
      contador=1
      for (i in 1:(length(x)-1)){
        if (x[i]<x[i+1]){
          contador=0
          aux=x[i+1]
          x[i+1]=x[i]
          x[i]=aux
        }
      }
    } 
  }
  return(x)
}

v=c(10,12,31,14,25,61,72,40,20)
v;ordenarvector(v,1)


#EJERCICIO 12
#Escriba un programa con formato de funci�n que realice lo siguiente:
#Encontrar el n�mero m�s peque�o en una matriz dada y reportarlo,
#as� como su posici�n en la matriz. Si se repite, reportar todas las posiciones en que se encuentra.
#El input deber� ser una matriz arbitraria de m*n y el output, el escalar correspondiente al valor m�nimo,
#as� como el vector con la posici�n del n�mero encontrado (todas las posiciones, si hubiera m�s de una).



min_m=function(x){
  minimo=min(x)
  aux=matrix(nrow=nrow(x),ncol=ncol(x))
  for (i in 1:nrow(x)){
    for ( j in 1:ncol(x)){
      if (x[i,j]==minimo){
        aux[i,j]="Minimo"
      }
    }
  }
  print(minimo)
  return(aux)
}                                                         
c=c(14,13,13,12,31,14,25,12,20)
f=matrix(c,3,3)
f;min_m(f)



#EJERCICIO 13
#En una lista de n�meros enteros consecutivos desde "a" hasta "b" encontrar 
#aquellos que son divisibles por "c". 
#Reportar un vector con los n�meros que cumplan la condici�n.
#Los argumentos deber�n ser un vector y el escalar por el cual se quiere dividir.

divisibles=function(v,k){
  aux=c()
  p=1
  for (i in 1:length(v)){
    if (v[i]%%k==0){
      aux[p]=v[i]
      p=p+1
    }
  }
  return(aux)
}
c=c(100,12,31,14,25,61,72,40,20)
c;divisibles(c,3)

#EJERCICIO 14
#Dada una matriz dato genere otra que sea la imagen espejada de la original respecto del eje vertical.

espejo=function(m){
  aux=m
  for (i in 1:nrow(m)){
    columna=ncol(m)
    for (j in 1:ncol(m)){
      aux[i,j]=m[i,columna]
      columna=columna-1
    }
  }
  return(aux)
}
x=matrix(1:36,6)
x;espejo(x)

#EJERCICIO 15
#Generar una funci�n que, para cada elemento de una matriz de n*m,
#determine si cada elemento es primo y/o par,
#y exprese los resultados en una sola matriz. Si el n�mero fuera primo,
#en la matriz de output se deber�a ver 1, si fuera PAR, se deber�a ver 1, y si fuera ambas,
#se deber�a ver el n�mero 2

primo_par=function(m){
  aux=m
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (es_par(m[i,j])==1&es_primo(m[i,j])==1){
        aux[i,j]=2
      }
      if (es_par(m[i,j])==1&es_primo(m[i,j])==0){
        aux[i,j]=1
      }
      if (es_par(m[i,j])==0&es_primo(m[i,j])==1){
        aux[i,j]=1
      }
      if (es_par(m[i,j])==0&es_primo(m[i,j])==0){
        aux[i,j]=0
      }
    }
  }
  return(aux)
}
x=matrix(1:36,6)
x;primo_par(x)

#EJERCICIO 16
#Generar una funci�n que, dado un escalar que indique el n�mero de caras de un dado,
#itere tiradas hasta alcanzar una de las dos siguientes condiciones:
#el n�mero de tiradas "n" (argumento de la funci�n) o el valor "s" (argumento de la funci�n)
#de la suma de las tiradas. Se recomienda usar la funci�n sample.
#Recuerde que dos condiciones pueden incluirse utilizando el s�mbolo "&".


tiradas=function(n,s){
  contador=0
  aux=c()
  while (contador<n & sum(aux)<s){
    dado=sample(1:6,1,replace=T)
    contador=contador+1
    aux=c(aux,dado)
  }
  print(contador)
  return(aux)
}
tiradas(100,100)

