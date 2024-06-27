

lambda = 0.50
U = runif(1)
print(U)

X = -1/lambda * log(U)
print(X)

lambda = 0.50
n = 10
y = rep(NA,n)
for(i in 1:length(y)){
  U = runif(1)
  y[i] = -1/lambda * log(U)
}
print(y)


simulated_exp <- function(n,lambda){
  # n es la cantidad de simulaciones
  # lambda es el valor del parámetro de la exponencial
  y = rep(NA,n)
  for(i in 1:length(y)){
    U = runif(1)
    y[i] = -1/lambda * log(U)
  }
  return(y)
}

simulated_exp(10,0.50)
simulated_exp(10000,0.50)

rexp(10000,0.50)
