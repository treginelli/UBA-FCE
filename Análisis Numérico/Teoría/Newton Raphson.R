############################################################################################
#########################        Resoluci�n de Ecuaciones     ##############################
#########################       Metodo por Newton  Raphson    ##############################
############################################################################################


Y<-expression(x^2-2)    #Defino la ecuaci�n manualmente con expression 

#Argumentos de la funci�n:

#Se crea la funci�n "NewtonR" con argumentos:
#n: Total de iteraciones
#a: L�mite Inferior del intervalo en el que se busca ra�z
#b: L�mite Superior del intervalo 
#xo: Indica cual es el xo. Si es -1 xo=a , si es 1 xo=b.
# Y : es la ecuaci�n deseada. 

NewtonR<-function(a,b,n,xo,Y){
  options(digits=15)
  D<-D(Y,"x")
  
  if(a>=b){
    print(" 'a' debe ser el valor mas chico, b el valor mas grande del intervalo [a,b]")
  }
  else{
    
    #Teorema de Bolzano
    
    x<-c(a,b)
    t=eval(Y)
    D2<-eval(D(D,"x"))
    if(t[1]*t[2]<0){                              #Si se cumple proceder� a la b�squeda de las ra�ces. 
      cont=1
      m<-0
      vv<-c()
      Iteracion<-c()
      
      
      if(sign(t[1])==sign(D2[1])){                 #Elige el xo y verifica que el elegido es correcto.
         x<-a                                      #Utilizamos el extremo izquierdo. 
         if(xo==1){
           print("El valor elegido de xo es incorrecto, xo es A")
           }
        while(cont<=n){
          x<-a-eval(Y)/eval(D) 
          if(eval(Y)==0){ 
            cont<-n       
            break
          }else{
            a<-x          
            aprox=eval(Y)
            cont=cont+1    
          }
          
          v<-c(x,eval(Y))
          vv<-rbind(vv,v)
        }
      }
        else{                                                 #Utilizamos el extremo derecho.
        if(xo==-1){
          print("El valor elegido de xo es incorrecto,xo es B")
          }
        x<-b                                                  
        while(cont<=n){
          x<-b-eval(Y)/eval(D) 
          if(eval(Y)==0){ 
            cont<-n       
            break
          }else{
            b<-x          
            aprox=eval(Y)
            cont=cont+1    
          }
          
          v<-c(x,eval(Y))
          vv<-rbind(vv,v)
        }
      }
      vv<-as.data.frame(vv)
      colnames(vv)<-c("xi","f(xi)")
      rownames(vv)<-c(paste(1:nrow(vv)))            #La salida es una matriz conformada por las n iteraciones 
      print(vv)                                     # y sus respectivos valores en la funci�n.
    }else print("No se cumple el teorema de Bolzano")
    
  }
}






















