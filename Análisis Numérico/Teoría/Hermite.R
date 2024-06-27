x<-c(3,2,5,6,9)
fx<-c(27,6,135,234,783)
dx<-c(33,NA,85,NA,NA)
dxx<-c(20,NA,NA,NA,NA)
A<-data.frame(x,fx,dx,dxx)

################################ Interpolación #####################################
################################    Hermite    #####################################

#Los argumentos de la función son: -
#aa, que es el valor en el cual se desea interpolar y
#La matriz A con cuatro columnas- 
#Primer columna -los valores de x
#Segunda columna -los valores de la función en los respectivos xi.
#Tercera columna - los valores conocidos de la primera derivada en xi
#Cuarta columna -los valores conocidos de la segunda derivada  en xi.
#ACLARACIÓN- En caso de no conocer alguna de las derivadas en  xi , en su lugar rellenar con NA(not available/no disponible).





Hermite<-function(A,xs){
  x<-A[,1]
  fx<-A[,2]                   #Se separa la matriz por columnas para repetir los x y fx , en los cuales se tiene 
  dx<-A[,3]                   #la primera y/o la segunda derivada 
  dxx<-A[,4]
  a<-c()
  b<-c()
  c<-c(is.na(dx)) 
  d<-c(is.na(dxx))
  for(j in 1:length(dxx)){
    if(c[j]==FALSE & d[j]==FALSE){
      a<-c(a,j)}
    else if(c[j]==FALSE & d[j]==TRUE){
      b<-c(b,j)
    }
    else if(c[j]==TRUE & d[j]==FALSE) print("Hay un dato mal ingresado en las primeras derivadas")
  }
  x<-c(x,x[b],x[a],x[a])
  fx<-c(fx,fx[b],fx[a],fx[a])
  dx<-c(dx,dx[b],dx[a],dx[a])
  dxx<-c(dxx,rep(NA,(length(a)*2+length(b))))
  B1<-data.frame(x,fx,dx,dxx)
  B<-B1[order(B1[,1],B1[,2],B1[,3],B1[,4]),]          #Se genera una matriz B que incluye los valores repetidos
  # de x y fx. Estan ordernados de menor a mayor.  
  dx=B[,3]
  fx=B[,2]                                                     
  x=B[,1]
  dxx<-B[,4]
  Dif.1.<-c(rep(NA,length(x))) 
  d2<-c(rep(NA,length(x)))
  for(i in 1:(length(x)-1)){                                    #Se calcula las primeras diferencias divididas, utilizando las primeras
    if(x[i]!=x[i+1])   Dif.1.[i]=(fx[i]-fx[i+1])/(x[i]-x[i+1])     #derivadas si es que existen.
    if(x[i]==x[i+1]) Dif.1.[i]=dx[i] 
  }
  for(i in 1:(length(x)-2)){
    if(x[i]==x[i+2]) d2[i]=dxx[i]/2                    #Se calculan las segundas diferencias , utilizando las segundas derivadas si es 
    else d2[i]=(Dif.1.[i]-Dif.1.[i+1])/(x[i]-x[i+2])            #que existen.
  }
  P<-(cbind(x,fx,Dif.1.))
  
  x<-P[,1]                                          #Se calculan las diferencias faltantes, en las cuales no se utilizan derivadas.
  n<-length(x)
  diferencia <-rep(NA,n*(n-2))
  dim(diferencia)<-c(n,n-2)
  diferencia[,1]<-d2
  for (j in 2:(n-2)) {
    for (i in 1:(n-2-j+1)){
      k<-j+i+1
      diferencia[i,j] <-(diferencia[i,j-1]-diferencia[i+1,j-1])/(x[i]-x[k])
    }
  }
  tabla<-as.matrix(data.frame(P,diferencia))
  ñ<-ncol(tabla)
  S<-tabla[1,(2:ñ)]
  x<-as.vector(tabla[,1])     #Creamos el vector conformado por (aa-x) siendo aa el valor en el cual se busca interpolar.
  vint<-c()
  C<-seq(x[1],x[length(x)],by=0.001)
  for(i in 1:length(C)){
    a<-C[i]
    s<-rep(1,length(x))
    for(k in 2:length(x)){
      s[k]=(a-x[k-1])*s[k-1]
    }
    Interpolacion_en_aa<-sum(s*S)
    vint<-c(vint,Interpolacion_en_aa)
    if(C[i]==xs){ 
      imp<-Interpolacion_en_aa
    }
  }
  coef=round(tabla[1,2:ncol(tabla)],2)
  j=("(x-a)")
  e=c("1")
  u=c(coef[1])
  for(c in 2:length(coef)){
    a=x[c-1]
    e=paste(c(e,gsub("a",a,j)),collapse ="*")
    u=c(u,paste(c(coef[c],e),collapse = "*"))
  }
  e=paste(u,collapse="+")
  names(imp)<-c("f(xs)")
  names(e)<-c("f(x)")
  print(imp)
  print(e)
  dev.new()
  plot(C,vint,type = "l",xlab = "X",ylab = "Fx",col="black",lwd=3,main = "Polinomio Hermite") 
  points(x,fx,pch = 21,cex=1.5,lwd=0.5,bg="red") 
  points(xs,imp,pch = 21,cex=1.8,lwd=0.9,bg="yellow")
  }
  

Hermite(A,4.5)      
      
      
      
      
  