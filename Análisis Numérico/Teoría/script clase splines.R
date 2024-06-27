# Datos pájaro
x=c(0.9, 1.3, 1.9, 2.1, 2.6, 3, 3.9, 4.4, 4.7, 5, 6, 7, 8, 9.2, 10.5, 11.3, 11.6, 12, 12.6, 13, 13.3)

fx=c(1.3, 1.5,1.85,2.1,2.6,2.7,2.4, 2.15,2.05, 2.1, 2.25, 2.3, 2.25, 1.95, 1.4, 0.9, 0.7, 0.6, 0.5, 0.4, 0.25)
A=cbind(x,fx)    ; A
plot(x,fx,type="l",col="red", lwd=2)

plot.new()
Newton(A,11)
lines(x,fx,type="l",col="red", lwd=2)

##EJEMPLO DATOS

m<-c(0,1,3,4)
n<-c(0,0,2,2)
A=cbind(m,n)

# Frontera libre
spline_LG(A,3.5)

# Frontera sujeta
spline_SG(A,3.5,1,-1)

# Not a knot
spline_NaK(A,3.5)

# Datos pájaro
x=c(0.9, 1.3, 1.9, 2.1, 2.6, 3, 3.9, 4.4, 4.7, 5, 6, 7, 8, 9.2, 10.5, 11.3, 11.6, 12, 12.6, 13, 13.3)

fx=c(1.3, 1.5,1.85,2.1,2.6,2.7,2.4, 2.15,2.05, 2.1, 2.25, 2.3, 2.25, 1.95, 1.4, 0.9, 0.7, 0.6, 0.5, 0.4, 0.25)
A=cbind(x,fx)    ; A
plot(x,fx,type="l",col="red", lwd=2)


# Frontera libre
spline_LG(A,3.5)

# Frontera sujeta
spline_SG(A,3.5,1,-1)

# Not a knot
spline_NaK(A,3.5)

