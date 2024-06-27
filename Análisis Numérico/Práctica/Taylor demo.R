############################################################################################
#########################        Aproximación numérica        ##############################
#########################        Polinomio de Taylor          ##############################
############################################################################################


#Ejemplo del uso de las funciones expression, D y eval. 
X<-expression(x^2+8*x+14)
X
Y<-D(X,"x")
Y
x<-2
eval(Y)


#Cálculo de una aproximación en un punto "y" en un entorno "x0" a través de Taylor. 

#expr: objeto del tipo "expression" que se corresponde a una función de una sola variable (x)
#x0: valor numerico referente al punto en cuyo entorno se busca la aproximación. Por default es 0.
#n: cantidad de derivadas a utilizar. Por default es 2
#y: valor en el cual se quiere aproximar

aproxtaylor<-function(expr,x0=0,n=2,y){
  x<-x0                      #se define el valor en el cual se evalúa la función y sus derivadas
  Aproximacion<-eval(expr)   #se define el primer término del polinomio de taylor
  for(i in 1:n){             #loop que recorre el numero indicado de derivadas
    expr=D(expr,"x")         #Se deriva la expresion y se la graba en el valor "expr"
    aux<-expr                #se usa un valor auxiliar "aux" para no modificar "expr" que sera usado luego para volver a derivar
    aux<-eval(expr)          #usando aux, se evalua la derivada en el punto x0
    Aproximacion=Aproximacion+(aux*(y-x0)^i)/(factorial(i)) #se arma el termino de la sumatoria. 
  }
  print(Aproximacion)        #se imprime el resultado   
}


############################################################################

P<-expression(exp((x)))
P
aproxtaylor(P,x0=0,n=5,1.67)
# Verdadero valor
exp(1.67)
###############################################################################################
#    FIN DEL PROGRAMA
###############################################################################################


###############################################################################################
# DEMO  1  :   TABLA DE CALCULO DE exp(1,67) CON n DE 1 A 10
###############################################################################################


X<-expression(exp((x)))
salida=c()
for(j in 1:10)  {
  a=aproxtaylor(X,x0=0,n=j,1.67)
  b=exp(1.67)
  fila=c(j,a,b,b-a)
  salida=rbind(salida,fila)
}
salida<-as.data.frame(salida)
colnames(salida)<-c("Iteracion","Taylor","Valor","Error")
print(salida)

###############################################################################################
#    FIN DEL DEMO 1
###############################################################################################


###############################################################################################
# DEMO  2:GRAFICACIÓN DE LAS APROXIMACIONES DE TAYLOR PARA exp(x) DE ORDEN 1 A 6 EN [-2;2]
###############################################################################################
color=c("red","blue","orange","green", "grey","black")
ancho=seq(1,3.5,0.3)
eje_x=matrix(seq(-2,2,0.2),ncol=1)
eje_ym=c()
eje_y=c()
as.data.frame(eje_ym)
for(j in seq(1,6,1))  {
for(i in seq(-2,2,0.2)) {
eje_y=c(eje_y,aproxtaylor(X,x0=0,n=j,i))
}
eje_ym=cbind(eje_ym,eje_y)
eje_y=c()
}
matplot(eje_x,eje_ym,type="l",col=color,lwd=ancho)

####################################################


