#EJERCITACION EXTRA - EJERCICIOS DE PARCIAL

#1) Dado un vector x de longitud n de elementos enteros positivos entre 1 y b,
#generar un vector h de longitud b cuyos elementos h[i] sean la cantidad de veces
#que aparece el numero i en el vector x
#Histograma=function(x,b) ... return(h)
#Ejemplo:
# i= 1 2 3 4 5
# x=[3,5,4,3,1,1,5,1] ; h=[3 0 2 1 2]

funcion1=function(x){
  a=max(x)
  h=c()
  for (j in 1:a){
    contador=0
    for (i in 1:length(x)){
      if (x[i]==j){
        contador=contador+1
        h[j]=contador
      }
    }
  }
  return(h)
}

f=c(3,5,4,3,1,1,5,1)
f;funcion1(f)


#2) Dado un vector x de longitud n siendo n múltiplo de k (o sea n=p*k),
#obtenga un vector suma de longitud p cuyos elementos sean la suma de los valores de x de k en k.
#Suma_vec=function(x,k) ... return(suma)
#Ejemplo:
#x=[1 5 3 9 4 7 2 3 9 8 7 4] ;n=12,k=3,p=4 ; suma=[9 20 14 19]



suma_vec=function(x,k){
  h=c()
  n=length(x)
  p=n/k
  for (i in 1:p){
    h[i]=sum(x[1+((i-1)*k):(i*k)])
  }
  return(h)
}
f=c(1, 5, 3, 9, 4, 7, 2, 3, 9, 8, 7, 4)
f;suma_vec(f,3)



#3) Escriba un programa con formato de función que realice lo siguiente:
#Encontrar el número más pequeño en una matriz dada y reportarlo, así como su posición en la matriz .
#Si se repite, reportar todas las posiciones en que se encuentra.
#Input: Matriz arbitraria m x n Output: escalar con el número más pequeño y vector 
#con las coordenadas de su posición (si se repite el vector debe indicar todas las posiciones
                                                                                                                      
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
c=c(100,12,31,14,25,61,72,40,20)
f=matrix(c,3,3)
f;min_m(f)

#4) Escriba un programa con formato de función que realice lo siguiente:
#En una lista de números enteros consecutivos desde a hasta b, encontrar aquellos que son divisibles por c.
#Reportar un vector con los números que cumplan la condición. Ayuda :
#para saber si un entero es divisible por otro, el resultado de la división no debe tener decimales.
#Input : extremos de la lista de enteros (a y b) y número divisor propuesto (c).
#Ouput: vector con los números de la lista que son múltiplos de c.
#Ejemplo: #calculo(a,b,c)??? [multiplos] #Ejemplo : #a = 10 b = 22 c = 3
#múltiplos : [12 15 18 21]



multiplos=function(a,b,c){
  aux=seq(a,b)
  h=c()
  k=1
  for (i in 1:length(aux)){
    if (aux[i]%%c==0){
      h[k]=aux[i]
      k=k+1
    }
  }
  return(h)
}
multiplos(10,22,3)


#5) Escriba un programa con formato de función que realice lo siguiente: 
#Dada un matriz dato genere otra que sea la imagen espejada de la original respecto del eje vertical.
#Repórtela.
#[espejadas]=espejo(A)

espejada=function(m){
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

c=c(100,12,31,14,25,61,72,40,20)
f=matrix(c,3,3)
f;espejada(f)

#6) El supermercado Market ha lanzado una promoción para todos sus clientes que posean la tarjeta Market.
#La promoción consiste en aplicar un descuento por cada n productos que pasan por caja.
#El primer descuento es de 20%, y se aplica sobre los primeros n productos ingresados. 
#Luego, cada descuento es la mitad del anterior, y es aplicado sobre los siguientes n productos.
#Por ejemplo, si n = 3 y la compra es de 11 productos, entonces los tres primeros tienen 20% de descuento, 
#los tres siguientes 10%, los tres siguientes 5%, y los dos últimos no tienen descuento. 
#Escriba una función "Descuento" que tenga como argumentos n, c (la cantidad de productos) y
#M (vector con los precios de cada producto). La salida debe ser el precio total, el descuento total y
#el precio final después de aplicar el descuento. Si al aplicar el descuento el precio queda con decimales,
#redondee el valor hacia abajo.
#Ejemplo:
#Descuento (3, 8, M)
#M<-c (400, 800, 500, 100, 400, 300, 200,500)
#SALIDA: Total: 3200
#Descuento: 420
#Por pagar: 2780

descuentos=function(x,n){
  desc=c()
  preciototal=sum(x)
  if (length(x)>3*n){
    v=c(rep(0.8,n),rep(0.9,n),rep(0.95,n),rep(1,length(x)-3*n))
    for (i in 1:length(x)){
      desc[i]=x[i]*v[i]
    }
  }
  if (length(x)==3*n){
    v=c(rep(0.8,n),rep(0.9,n),rep(0.95,n))
    for (i in 1:length(x)){
      desc[i]=x[i]*v[i]
    }
  }
  if (length(x)<3*n){
    for (i in 1:length(x)){
      if (i<=n){
        desc[i]=x[i]*0.8
      }
      if (i<=2*n&i>n){
        desc[i]=x[i]*0.9
      }
      if (i<3*n&i>2*n){
        desc[i]=x[i]*0.95
      }
    }
  }
  preciofinal=sum(desc)
  descuentototal=preciototal-preciofinal
  print("Total:")
  print(preciototal)
  print("Descuento:")
  print(descuentototal)
  print("Por pagar:")
  print(preciofinal)
}

M<-c (400, 800, 500, 100, 400, 300, 200,500)
descuentos(M,3)

  




#7) Debe crear una función "Romano" que tenga como único argumento N,
#número que se desea convertir a número romano. La salida será el número romano.


romanos=function(n){
  aux=c()
  while (n>=1000){
    aux=c(aux,"M")
    n=n-1000
  }
  while (n>=500){
    aux=c(aux,"D")
    n=n-500
  }
  while (n>=100){
    aux=c(aux,"C")
    n=n-100
  }
  while (n>=50){
    aux=c(aux,"L")
    n=n-50
  }
  while (n>=10){
    aux=c(aux,"X")
    n=n-10
  }
  while (n>=9){
    aux=c(aux,"IX")
    n=n-9
  }
  while (n>=8){
    aux=c(aux,"VIII")
    n=n-8
  }
  while (n>=7){
    aux=c(aux,"VII")
    n=n-7
  }
  while (n>=6){
    aux=c(aux,"VI")
    n=n-6
  }
  while (n>=5){
    aux=c(aux,"V")
    n=n-5
  }
  while (n>=4){
    aux=c(aux,"IV")
    n=n-4
  }
  while (n>=3){
    aux=c(aux,"III")
    n=n-3
  }
  while (n>=2){
    aux=c(aux,"II")
    n=n-2
  }
  while (n>=1){
    aux=c(aux,"I")
    n=n-1
  }
  return(aux)
}

romanos(1928)







#EJERCICIO PARCIAL

registro=896079
u=9
s=39
p=1


#a) FUNCION ESCLAVO
esclavo1=function(v){
  y=c()
  k=1
  for (i in 1:length(v)){
    if (v[i]%%u==0){
     y[k]=v[i]
     k=k+1
    }
  }
  return(y)
}
#b
#FUNCION MAESTRA
funcionmaestro1=function(x){
  v=rep(0,10)
  r=esclavo1(x)
  for (i in 1:length(r)){
   v[i]=r[i]
  }
  return(matrix(v,nrow=2,ncol=3,byrow=F))
}

  
  
t=c(5,2,1,7,18,4,81)
esclavo1(t)
t;funcionmaestro1(t)



#c)
r=c(8,9,6,0,7,9)
V=funcionmaestro1(t)
funcionmaster=function(V,r){
  suma=sum(V)
}



registro=896079
u=9
s=39
p=1

punto1a=function(v){
  aux=c()
  p=1
  for (i in 1:length(v)){
    if (v[i]%%u==0){
      aux[p]=v[i]
      p=p+1
    }
  }
  return(aux)
}

c=c(100,12,31,14,25,61,72,40,20,18)
c;punto1a(c)

punto1b=function(v){
  a=punto1a(v)
  n=length(a)
  if (n<3){
    n=4
  }
  b=c(rep(0,n*n))
  for (i in 1:length(a)){
    b[i]=a[i]
  }
  return(matrix(b,nrow=n,ncol=3,byrow=F))
}
punto1b(c)


#PUNTO C


punto1c=function(V,r){
  V=punto1b(V)
  r=c(8,9,6,0,7,9)
  suma=sum(V)
  d=suma%%10
  es_par=function(n){
    for (i in 1:length(n)){
    if (n[i]%%2==0){
      return (n[i])
    }else{
      return(n[i]*d)
    }}
  }
  es_par2=function(n){
    for (i in 1:length(n)){
    if (n[i]%%2==0){
      return (n[i]*d)
    }else{
      return(n[i])
    }}
  }
  mayor_que_5=function(n){
    for (i in 1:length(n)){
      if (n[i]>5){
        n[i]=0
      }
    }
  }
  menor_que_5=function(n){
    for (i in 1:length(n)){
      if (n[i]<5){
        n[i]=7
      }
    }
  }
  f=matrix(rep(0,36),6,6)
  v_segundafila=es_par(r)
  v_tercerafila=es_par2(r)
  v_cuartafila=rev(r)
  v_quintafila=mayor_que_5(r)
  v_sextafila=menor_que_5(r)
  for (i in 1:ncol(f)){
    f[1,i]=r[i]
  }
  for (i in 1:ncol(f)){
    f[2,i]=v_segundafila[i]
  }
  for (i in 1:ncol(f)){
    f[3,i]=v_tercerafila[i]
  }
  for (i in 1:ncol(f)){
    f[4,i]=v_cuartafila[i]
  }
  for (i in 1:ncol(f)){
    f[5,i]=v_quintafila[i]
  }
  for (i in 1:ncol(f)){
    f[6,i]=v_sextafila[i]
  }
  S=sum(f[1,])+sum(f[6,])+sum(f[2:5,1])+sum(f[2:5,6])
  print(S)
  return(f)
}
punto1c(c)



#MODELOS DE PARCIAL
modelo1=function(a){
  b=matrix(rep(0,nrow(a)*ncol(a)),nrow=nrow(a),ncol=ncol(a))
  for (i in 1:nrow(a)){
    for (j in 1:ncol(a)){
      b[i,j]=a[i,j]+i+j
    }
  }
  return(b)
}
n=c(9,5,3,12,1,4,6,7)
A=matrix(n,2,4,byrow=T)
A;modelo1(A)


modelo2=function(x,a){
  if (x>a){
    return(1)
  }else{
    return(0)
  }
}
detecta=function(v,h){
  o=c()
  for (i in 1:length(v)){
    if (modelo2(v[i],h)==1){
      o[i]=1
    }else{
      o[i]=0
    }
  }
  return(o)
}
v=c(3,7,5,1,9,2)
detecta(v,3)


modelo3=function(A,b){
  c=A
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (i==j){
        c[i,j]=A[i,j]*b
      }else{
        c[i,j]=A[i,j]/b
      }
    }
  }
  return(c)
}
n=c(9,5,3,12,1,4,6,7)
A=matrix(n,2,2,byrow=T)
A;modelo3(A,2)


elemento.R=function(a){
  i=round(runif(1,1,nrow(a)))
  j=round(runif(1,1,ncol(a)))
  return(a[i,j])
}

n=c(9,5,3,12,1,4,6,7)
A=matrix(n,2,4,byrow=T)
A;elemento.R(A)

modelo4=function(A,k){
  b=c()
  for (i in 1:k){
    b[i]=elemento.R(A)
  }
  return(b)
}
modelo4(A,10)



mayorquemodelo5=function(a,b){
  if(a>b){
    return(1)
  }else{
    return(0)
  }
}

v=c(round(runif(9,7,60)))
modelo5=function(v){
  aux=matrix(v,3,3)
  aux2=0
  for (i in 1:nrow(aux)){
    for (j in 1:ncol(aux)){
      if (i==j){
        aux2=aux2+a[i,j]
      }
    }
  }
  if (aux2>0.7*mean(v)){
    p=1
    for (i in 1:ncol(aux)){
      for (j in 1:nrow(aux)){
        if (mayorquemodelo5(a[p],aux[i,j])==1){
          aux[i,j]=a[p]
        }
        p=p+1
      }
    }
  }
  return(aux)
}
modelo5(v)













