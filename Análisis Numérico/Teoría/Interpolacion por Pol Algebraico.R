x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A=cbind(x,fx)

# A es la matriz de datos
# xs es el punto en el cual se desea interpolar
# La función devuelve el polinomio,
# el valor interpolado y el grafico del polinomio

pol_Algebraico<-function(A,xs){
  d=0.01
  m<-A[,1]
  n<-A[,2]
  M<-matrix(1,length(m),length(m))
  for(i in 1:nrow(M)){
    for(j in 1:ncol(M)){
      M[i,j]<-m[i]^(length(m)-j)
    }
  }
  Minv<-solve(M)
  coef<-Minv%*%(as.matrix(n))

  B<-seq(m[1],m[length(m)],by=d)
  vint<-c()
  for(j in 1:length(B)){
  interpolacion=0
  a<-B[j]
  for(i in 1:nrow(coef)){
    interpolacion=interpolacion+a^((nrow(coef))-i)*(coef[i,1])
  }
  if(a==xs){
    imp<-interpolacion
  }
  vint<-c(vint,interpolacion)
  }
  coef=round(rev(coef),2)
  u=c(coef[1])
  for(i in 2:length(x)){
    j="(x^i)*c"
    uu=gsub("c",coef[i],gsub("i",i-1,j))
   u=c(u,uu)
    }
  S=parse(text=paste(u,collapse = "+"))
  names(S)<-c("f(x)")
  names(imp)=c("f(xs)")
  print(imp)
  dev.new()
  par(mar=c(1,1,1,1))
  plot(B,vint,type = "l",xlab = "X",ylab = "Fx",col="black",lwd=3,main = "Polinomio Algebraico")
  points(m,n,pch = 21,cex=1.5,lwd=0.5,bg="red")
  points(xs,imp,pch = 21,cex=1.8,lwd=0.9,bg="yellow")
  return(S)
}


pol_Algebraico(A,5.5)
