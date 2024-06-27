#EJERCICIO 1
x=round(rnorm(1,4,5));x
if (x<4){
  print("Inferior a la media")
}else{
  print("Mayor o igual a la media")
}


#EJERCICIO 2
v=c(round(rnorm(10,4,5),2));v
media=mean(v);media
for (i in 1:length(v)){
  if (v[i]<media){
    v[i]=0
  }else{
    if (v[i]==media){
     v[i]=1
  }else{
    if (v[i]>media){
      v[i]=2
  }}
  }
}  
print(v)

#EJERCICIO 3
v=c(round(rnorm(10,4,5),2));v
media=mean(v);media
w=c()
for (i in 1:length(v)){
  if (v[i]<media){
    w[i]="I"
  }else{
    if (v[i]>media){
      w[i]="S"
  }else{
    if (v[i]==media){
    w[i]="E"
  }
  }
  }
}
print(w)


#EJERCICIO 4
S=c(rbinom(17,77,0.368));S
mean(S) #ES IGUAL AL ANTERIOR

#EJERCICIO 5
A=matrix(round(rnorm(12,37,9)),3,4);A
aux=matrix(,3,4)
for (i in 1:nrow(A)){
  for (j in 1:ncol(A)){
    if (A[i,j]>=37){
      aux[i,j]=1
    }else{
      if (A[i,j]<37){
        aux[i,j]=0
      }
    }
  }
}
print(aux)


#EJERCICIO 6
B=matrix(round(rexp(200,0.007)),20,9);B
b=c()
for (i in 1:ncol(B)){
  if (sum(B[1:nrow(B),i])>1800){
    b[i]=1
  }else{
    if (sum(B[1:nrow(B),i])<=1800){
      b[i]=0
    }
  }
}
print(b)


#EJERCICIO 7
k=runif(1,76,245);k
contador=1
suma=0
while (suma<k){
  suma=suma+runif(1,0,1)
  contador=contador + 1
}
print(contador)
