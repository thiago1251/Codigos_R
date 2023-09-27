#Hipotesis en dos muestras
#Prueba de hipotesis para U1-U2 con varianzas conocidas
d0=0
x1=81
x2=76
o1=5.2
o2=3.4
n1=25
n2=36
Zcal=((x1-x2)-d0)/(sqrt(((o1^2)/(n1))+((o2^2)/n2)))
p_value=2*(pnorm(abs(Zcal), lower.tail = F))
p_value
vc=pnorm(abs(Zcal), lower.tail = F)
#############PRUEBAS DE HIPOTESIS PARa U1-U2 con varianzas iguales desconocidas##############
x1=198
x2=206
n1=150
n2=200
s1=32
s2=29

Sp=sqrt((((s1^2)*(n1-1))+((s2^2)*(n2-1)))/(n1+n2-2))

Tcal=(x1-x2)/((Sp)*(sqrt((1/n1)+(1/n2))))

p_value=pt(Tcal,n1+n2-2, lower.tail = T)
p_value
#############PRUEBAS DE HIPOTESIS PARa U1-U2 con varianzas diferentes desconocidas##############
x1=8.5
x2=7.9
n1=90
n2=80
s1=1.8
s2=2.4

Tcal=(x1-x2)/(sqrt(((s1^2)/(n1))+((s2^2)/(n2))))

Vnumerador = ((s1^2/n1)+(s2^2/n2))^2
Vdenominador = (((s1^2/n1)^2)/(n1-1))+(((s2^2/n2)^2)/(n2-1))

V=(Vnumerador)/(Vdenominador)

p_value=pt(Tcal,V, lower.tail = F)
p_value


#############PRUEBAS DE HIPOTESIS PARA LA IGUALDAD DE VARIANZAS##############
S1=32
S2=29
n1=150
n2=200
  

Fcal=(S1^2)/(S2^2)

#SE TOMA EL VALOR MAS PEQUEÑO
p_value=pf(Fcal,n1-1,n2-1,lower.tail = F)
p_value2=pf(Fcal,n1-1,n2-1,lower.tail = T)



p_value=pf(0.5625,89,79,lower.tail = F)
p_value2=pf(0.5625,89,79,lower.tail = T)


#############PRUEBAS DE HIPOTESIS PARA PROPORCIONES##############
x1=166
x2=205
n1=400
n2=380
p1=x1/n1
p2=x2/n2
p=(x1+x2)/(n1+n2)
q= 1-p

Zcal=(p1-p2)/(sqrt((p*q)*((1/n1)+(1/n2))))
            
p_value=pnorm(abs(Zcal), lower.tail = F)
p_value

