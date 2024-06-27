A=matrix(round(c(runif(20)*10)),4,5)
########################################
# Suma por filas
suma=0
for(i in 1:nrow(A))   {
  for(j in 1:ncol(A)) {
    suma=suma+A[i,j]
}  
}
print(suma)

# Comprobación
sum(A)

# Suma por columnas
suma=0
for(j in 1:ncol(A))   {
  for(i in 1:nrow(A)) {
    suma=suma+A[i,j]
  }  
}
print(suma)


#####################################
# Matriz cuadrada
B=matrix(round(c(runif(16)*10)),4,4)

# Suma de los elementos de la diagonal
# y por encima de la diagonal (Dirichlet)
suma=0
for(i in 1:nrow(B))   {
  for(j in i:ncol(B)) {
    suma=suma+B[i,j]
  }  
}
print(suma)
Dirichlet = suma

# Otra forma
suma=0
for(j in 1:ncol(B))   {
  for(i in 1:j) {
    suma=suma+B[i,j]
  }  
}
print(suma)

#######################
# Suma de los elementos por encima de la diagonal
suma=0
for(i in 1:(nrow(B)-1))   {
  for(j in (i+1):ncol(B)) {
    suma=suma+B[i,j]
  }  
}
print(suma)

# Otra forma
suma=0
for(j in 2:ncol(B))   {
  for(i in 1:(j-1))    {
    suma=suma+B[i,j]
  }  
}
print(suma)

# Comprobación
Dirichlet-sum(diag(B))

# Suma de los elementos de la diagonal
suma=0
for(i in 1:nrow(B))   {
  for(j in i)         {
    suma=suma+B[i,j]
  }  
}
print(suma)

# Comprobación
sum(diag(B))

# Otra forma
suma=0
for(j in 1:ncol(B))   {
  for(i in j)         {
    suma=suma+B[i,j]
  }  
}
print(suma)




