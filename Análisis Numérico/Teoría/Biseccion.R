############################################################################################
#############################  Resolución de Ecuaciones  ###################################
#############################    Método por Bisección    ###################################
############################################################################################


#Se debe gerenar una función "func" con la ecuación deseada.

#func<-expression(Ecuación deseada)

#Ejemplo
func<-expression(x^2-2)

#Se crea la función "bisección" con argumentos:
#n: Total de iteraciones
#a: Limite Inferior del intervalo en el que se busca raiz
#b: Limite Superior del intervalo

biseccion<-function(n,a,b){
  options(digits=15)

#Para simplificar código, creamos una función que nos permita valuar la función 
#de forma f(valor).
  
f=function(x){
  eval(func)
  }  
    
#Se verifica el teorema de Bolzano
  
  if(f(a)*f(b)<0){
  
    if(a>=b){
      print("a debe ser el valor más chico, b el valor más grande del intervalo [a,b]")
      c<-a
      a<-b
      b<-c
    }
    
    cont=1
    m<-0
    vv<-c(0)
    Iteracion<-c(0)
    while(cont<=n){

#Aqui se encuentra el punto medio de [a,b] que reemplazará a uno de los mismos.
#El proceso seguirá durante "n" iteraciones,si es que no se haya la raíz antes.
      
      x1<-(a+b)/2   
      if(f(x1)==0){
        cont=n
      }else{
        
        if(f(a)*f(x1)<0){
          a<-a
          b<-x1
          cont=cont+1
          aprox=f(x1)
       
           }else{
          a<-x1
          b<-b
          aprox=f(x1)
          cont=cont+1
          }
      }
      
      m=m+1
      Iteracion<-c(Iteracion,m)
      v<-c(x1,f(x1))
      vv<-rbind(vv,v)
    }
    
  vv<-as.data.frame(vv)
  colnames(vv)<-c("xi","f(xi)")
  rownames(vv)<-Iteracion
  print(vv[(2:nrow(vv)),])
    
  }else{
    print("No se verifica el Teorema de Bolzano")
  }
}
