############################################################################################
#####################################    Interpolación   ###################################
#####################################    por Lagrange    ###################################
############################################################################################



x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A=cbind(x,fx)

#Los argumentos de la función son:

#Matriz A con dos columnas: x,fx
#x :Los valores de x
#fx :Los valores de la función en los respectivos xi.

#xs :Valor en el cual se desea interpolar

# La función genera el polinomio interpolador,
# el valor interpolado y el grafico del polinomio.

lagrange<-function(A,xs){
  m<-A[,1]
  n<-A[,2]
  d=0.01
  lm<-length(m)
  ln<-length(n)
  if(lm!=ln){
    print("La cantidad de argumentos no coincide")
  }else{
    M<-(m[length(m)]-m[1])/900   
    B<-seq(m[1],m[length(m)],M)
    vint<-c()
    for(j in 1:length(B)){
      interpolador=0
      coeficientes<-c()
      a<-B[j]
      names<-c()
      for(i in 1:lm){
        u<-1
        for(y in 1:length(m)){
          if(m[y]!=m[i]){
            u=u*(a-m[y])
          }else{
            u=u*1
          }
        }
        r<-u
        v=1
        for(g in 1:length(m)){
          if(m[g]!=m[i]){
            v=v*(m[i]-m[g])
          }else{
            v=v*1
          }
        }
        rr<-v
        t=(n[i]*r)/rr
        coeficientes<-c(coeficientes,(r/rr))
        interpolador=interpolador+t
        u<-"Coef"
        uu<-i
        name<-paste(u,uu, sep = ".", collapse = "")
        names<-c(names,name)
      }
      vint<-c(vint,interpolador)
    }
    
    #Ahora interpolo el punto xs deseado
    
    interpolador=0
    coeficientes<-c()
    a<-xs
    names<-c()
    for(i in 1:lm){
      u<-1
      for(y in 1:length(m)){
        if(m[y]!=m[i]){
          u=u*(a-m[y])
        }else{
          u=u*1
        }
      }
      r<-u
      v=1
      for(g in 1:length(m)){
        if(m[g]!=m[i]){
          v=v*(m[i]-m[g])
        }else{
          v=v*1
        }
      }
      rr<-v
      t=(n[i]*r)/rr
      coeficientes<-c(coeficientes,(r/rr))
      interpolador=interpolador+t
      u<-"L"
      uu<-(i-1)
      name<-paste(u,uu, sep = ".", collapse = "")
      names<-c(names,name)
      imp<-interpolador
      names(imp)=c("f(x)")
      coefi<-coeficientes
      nombre<-names
    }
    coefi<-data.frame(nombre,coefi)
    print(coefi)
    dev.new()
    plot(B,vint,type = "l",xlab = "X",ylab = "Fx",col="black",lwd=3,main = "Polinomio Interpolador de Lagrange")
    points(m,n,pch = 21,cex=1.5,lwd=0.5,bg="red")
    points(xs,imp,pch = 21,cex=1.8,lwd=0.9,bg="yellow")
    return(imp)
  }
}
lagrange(A,4.5)
