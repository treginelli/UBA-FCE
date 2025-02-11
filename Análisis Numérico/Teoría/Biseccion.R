############################################################################################
#############################  Resoluci�n de Ecuaciones  ###################################
#############################    M�todo por Bisecci�n    ###################################
############################################################################################


#Se debe gerenar una funci�n "func" con la ecuaci�n deseada.

#func<-expression(Ecuaci�n deseada)

#Ejemplo
func<-expression(x^2-2)

#Se crea la funci�n "bisecci�n" con argumentos:
#n: Total de iteraciones
#a: Limite Inferior del intervalo en el que se busca raiz
#b: Limite Superior del intervalo

biseccion<-function(n,a,b){
  options(digits=15)

#Para simplificar c�digo, creamos una funci�n que nos permita valuar la funci�n 
#de forma f(valor).
  
f=function(x){
  eval(func)
  }  
    
#Se verifica el teorema de Bolzano
  
  if(f(a)*f(b)<0){
  
    if(a>=b){
      print("a debe ser el valor m�s chico, b el valor m�s grande del intervalo [a,b]")
      c<-a
      a<-b
      b<-c
    }
    
    cont=1
    m<-0
    vv<-c(0)
    Iteracion<-c(0)
    while(cont<=n){

#Aqui se encuentra el punto medio de [a,b] que reemplazar� a uno de los mismos.
#El proceso seguir� durante "n" iteraciones,si es que no se haya la ra�z antes.
      
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
