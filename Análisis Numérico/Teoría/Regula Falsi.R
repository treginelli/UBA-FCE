############################################################################################
#########################        Resoluci�n de Ecuaciones     ##############################
#########################        M�todo por Regula Falsi      ##############################
############################################################################################

#Se debe gerenar una funci�n "func" con la ecuaci�n deseada.

#func<-expression(Ecuaci�n deseada)
 
#Ejemplo
func<-expression(x^2-2)

#Se crea la funci�n "regulafalsi" con argumentos:
#n: Total de iteraciones
#a: Limite Inferior del intervalo en el que se busca raiz
#b: Limite Superior del intervalo

regulafalsi<-function(n,a,b){
  options(digits=15)

#Para simplificar c�digo, creamos una funci�n que nos permita valuar la funci�n 
#de forma f(valor)
  
f=function(x){
  eval(func)
  }   

#Para que la funci�n no produzca error,"a" debe ser mayor a "b". Igualmente si se 
#ingresan al rev�s, la funci�n lo corregir�
    
  if(a>=b){
      print("a debe ser el valor m�s chico, b el valor m�s grande del intervalo [a,b]")
    c<-a
    a<-b
    b<-a
  }
  
#Verificamos el Terorema de Bolzano  
  
  if(f(a)*f(b)<0){
    cont=1
    m<-0
    Iteracion<-c(0)
    vv<-c(0)
    
#Ingresamos la condici�n que realice el proceso un m�ximo de "n" iteraciones.
        
      while(cont<=n){

#Generamos los "xn" valores,se ir�n acercando a la ra�z de la funci�n 
# comprendida en el intervalo.
        
        xn<-b-f(b)*((b-a)/(f(b)-f(a)))
        if(f(xn)==0){
          cont=n
        }else{
          if(f(a)*f(xn)<0){
            a<-a
            b<-xn
            cont=cont+1
            aprox=f(xn)
          }else{
            a<-xn
            b<-b
            aprox=f(xn)
            cont=cont+1
          }
        }
        
        m=m+1
        Iteracion<-c(Iteracion,m)
        v<-c(xn,aprox)
        vv<-rbind(vv,v)
      }
    
#Guardamos los valores de "xn" en cada iteraci�n junto a "func(xn)".
    
    vv<-as.data.frame(vv)
    colnames(vv)<-c("xi","f(xi)")
    rownames(vv)<-Iteracion
    print(vv[2:nrow(vv),])
    
    }else{
      print("No se verifica el Teorema de Bolzano")
    }
 
}  
