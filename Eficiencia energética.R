#--------------- Cargar los datos-------------------------------------------
Efienerg=read.table("base.txt",header=TRUE, dec = ",")
Tem=Efienerg[,c(1,2,7:10)]
View(Tem)
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# --->  1.- Describir la temperatura en los cuatro ambientes
#a través de la estimación de la densidad y realizar constrasters de igualdad de k 
#poblaciones para determinar si el sistema de climatización regula la temperatura
#en los ambientes internos
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

par(mfrow=c(1,4))

#Densidad de la temperatura en el exterior de la oficina:
  Text=Tem[,3]
  hist(Text,freq=FALSE,main="Temperatura en el exterior")
  H=density(Text, bw="nrd0", kernel="epanechnikov",weights = NULL,
            window=kernel, give.Rkern = FALSE)
  print(H)# Bandwidth 'bw'= 0.8373 #Silverman Method
  lines(H,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la 
      Temperatura en el Exterior de la Oficina", sub="bw=0.8373",lwd=2)

#Densidad de la temperatura en la entrada de la oficina:
  Teno=Tem[,5]
  hist(Teno,freq=FALSE,main="Temperatura en la entrada")
  I=density(Teno, bw="nrd0", kernel="epanechnikov",weights = NULL,
  window=kernel, give.Rkern = FALSE)
  print(I)# Bandwidth 'bw' = 0.377 #Silverman Method
  lines(I,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la 
       Temperatura en la Entrada de la Oficina", sub="bw=0.377",lwd=2)

#Densidad en el medio de la oficina:
  Tmedo=Tem[,6]
  hist(Tmedo,freq=FALSE,main="Temperatura en el medio")
  G=density(Tmedo, bw="nrd0", kernel="epanechnikov",weights = NULL,
  window=kernel, give.Rkern = FALSE)
  print(G)# Bandwidth 'bw' = 0.3008 #Silverman Method
  lines(G,xlab = "Temperatura", ylab = "Densidad",main = "Densidad
  de la Temperatura en el Medio de la Oficina",ylim=c(0,0.35),sub="bw= 0.3008", lwd=2)

#Densidad de la temperatura en el fondo de la oficina:
  Tfon=Tem[,4]
  hist(Tfon,freq=FALSE,main="Temperatura en el fondo")
  D=density(Tfon, bw="nrd0", kernel="epanechnikov",weights = NULL,
            window=kernel, give.Rkern = FALSE)
  print(D)# Bandwidth 'bw' = 0.3082 #Silverman Method
  lines(D,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la 
       Temperatura en el Fondo de la Oficina", sub="bw=0.3082",lwd=2)
  
par(mfrow=c(1,1))

#Gráfico de las tres densidades en un solo plano
library(lattice)
  Espacios=data.frame(Temperatura.exterior=Text,Temperatura.entrada=Teno,
   Temperatura.medio=Tmedo, Temperatura.fondo=Tfon)
  densityplot(~Temperatura.exterior+ Temperatura.entrada
              + Temperatura.medio+Temperatura.fondo, 
              data = Espacios, auto.key = TRUE)
# Contrastes de comparación de densidades
  ntext = length(Text)
  nteno = length(Teno)  
  ntmedo = length(Tmedo)  
  ntfon = length(Tfon)  
  
  Grupos = factor(c(rep("Text",ntext),rep("Teno",nteno),rep("Tmedo",ntmedo),rep("Tfon",ntfon)))
  Temp = c(Text,Teno,Tmedo,Tfon)
  datos=data.frame(Temp,Grupos);View(datos)

library(sm)

par(mfrow=c(1,5))

# Todas las temperaturas
  sm.density.compare(Temp,group=Grupos, model="equal")
#Ho: f_ext=f_ent=f_med=f_fon  
#p-value = 0

# Las temperaturas del interior
  Tempint=Temp[Grupos!="Text"]
  Gruposint=Grupos[Grupos!="Text"]
  sm.density.compare(Tempint,group=Gruposint, model="equal")
#Ho: f_ent=f_med=f_fon  
#p-value = 0

# La temperatura med vs. temperatura fon
  sm.density.compare(Tempint[Gruposint!="Tfon"],group=Gruposint[Gruposint!="Tfon"], model="equal")
#Ho: f_med=f_eno  
#p-value = 0

# La temperatura med vs. temperatura ent
  sm.density.compare(Tempint[Gruposint!="Teno"],group=Gruposint[Gruposint!="Teno"], model="equal")
#Ho: f_fon=f_med  
#p-value = 0

# La temperatura ent vs. temperatura fon
  sm.density.compare(Tempint[Gruposint!="Tmedo"],group=Gruposint[Gruposint!="Tmedo"], model="equal")
#Ho: f_en=f_fon  
#p-value = 0

par(mfrow=c(1,1))

#---------------------------------------------------------------------------
# CONCLUSION:el sistema de climatización regula (mejora) la temperatura en 
#el interior de la oficina pero que no es la misma en los tres ambientes 
#del interior, se procede nuevamente a realizar un análisis gráfico y 
#mediante contrastes de hipótesis comparar en cada ambiente si el sistema es 
#capaz de mantener una temperatura regulada constante durante la mañana y la tarde. 
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# --->  2.- Describir la temperatura para los tres ambientes del interior
#a través de la estimación de la densidad y la realizaciión de constrastes de igualdad de k 
#muestras para determinar si el sistema de climatización regula la temperatura
# de manera constante durante la tarde y la mañana.
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

###TEMPERATURA EN EL EXTERIOR
#Temperatura exterior para las primeras 5 horas 9:00-13:00 :)
  hora=unique(Tem$Tiemp)
  n=length(hora)  
  dTex.1=density(Tem$Texte[Tem$Tiemp=="9:00"], bw="nrd0", kernel="epanechnikov",weights = NULL,
            window=kernel, give.Rkern = FALSE)
  plot(dTex.1,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la Temperatura en el exterior de la oficina",
            type="n", ylim=c(0,0.30), xlim=c(0,30), sub="9:00-13:00")  
  colores=c("darkorange1","darkviolet","red","seagreen","blue","yellow")
  k=1
  for(i in hora[1:5]){
  dex.1=density(Tem$Texte[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
            window=kernel, give.Rkern = FALSE)
  lines(dex.1,col=colores[k])    
  k=k+1
  }
  legend(12,0.29, hora[1:5], bty="n",
  col=colores[1:5],lty=1,lwd=2,cex=0.6)

#Temperatura exterior para las siguientes 5 horas 14:00-18:00 
  hora=unique(Tem$Tiemp)
  n=length(hora)  
  dTex.2=density(Tem$Texte[Tem$Tiemp=="9:00"], bw="nrd0", kernel="epanechnikov",weights = NULL,
             window=kernel, give.Rkern = FALSE)
  plot(dTex.2,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la Temperatura en el exterior de la oficina",
       type="n", ylim=c(0,0.30), xlim=c(0,30),sub="14:00-18:00")  
  colores=c("darkorange1","darkviolet","red","seagreen",
  "blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[6:10]){
    dex.2=density(Tem$Texte[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
              window=kernel, give.Rkern = FALSE)
    lines(dex.2,col=colores[k])    
    k=k+1
  }
  legend(12,0.29, hora[6:10], bty="n",
         col=colores[1:5],lty=1,lwd=2,cex=0.6)

par(mfrow=c(3,2))

#TEMPERATURA EN LA ENTRADA DE LA OFICINA ENTRE LAS 09:00 A 13:00 HORAS
#----------------------------------------------------------------------
#Temperatura entrada de la oficina para las primeras 5 horas 9:00-13:00 :)
  hora=unique(Tem$Tiemp)
  n=length(hora) 
  dTen.1=density(Tem$Taofientrada[Tem$Tiemp=="9:00"], bw="nrd0",
   kernel="epanechnikov",weights = NULL,window=kernel, give.Rkern = FALSE)
  
  plot(dTen.1,xlab = "Temperatura", ylab = "Densidad",
   main = "Densidad de la Temperatura en la entrada de la oficina",
   type="n", ylim=c(0,0.40), xlim=c(10,30),sub="9:00-13:00")  
  colores=c("darkorange1","darkviolet","red","seagreen","blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[1:5]){
  den.1=density(Tem$Taofientrada[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
    window=kernel, give.Rkern = FALSE)
    lines(den.1,col=colores[k])    
    k=k+1
    }
  legend(12,0.29, hora[1:5], bty="n",col=colores[1:5],lty=1,lwd=2,cex=0.6)


#Temperatura entrada oficina para las siguientes 5 horas 14:00-18:00 
  hora=unique(Tem$Tiemp)
  n=length(hora) 
  dTen.2=density(Tem$Taofientrada[Tem$Tiemp=="9:00"], bw="nrd0", kernel="epanechnikov",weights = NULL,
                 window=kernel, give.Rkern = FALSE)
  plot(dTen.2,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la Temperatura en la entrada de la oficina",
       type="n", ylim=c(0,0.40), xlim=c(10,30),sub="14:00-18:00")  
  colores=c("darkorange1","darkviolet","red",
            "seagreen","blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[6:10]){
    den.2=density(Tem$Taofientrada[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
                  window=kernel, give.Rkern = FALSE)
    lines(den.2,col=colores[k])    
    k=k+1
  }
  legend(12,0.29, hora[6:10], bty="n",
         col=colores[1:5],lty=1,lwd=2,cex=0.6)


#TEMPERATURA EN EL MEDIO DE LA OFICINA
#----------------------------------------
#Temperatura medio de la oficina para las primeras 5 horas 9:00-13:00 :)
  hora=unique(Tem$Tiemp)
  n=length(hora)  
  dTme.1=density(Tem$Taofimedio[Tem$Tiemp=="9:00"], bw="nrd0",
  kernel="epanechnikov",weights = NULL,window=kernel, give.Rkern = FALSE)
  
  plot(dTme.1,xlab = "Temperatura", ylab = "Densidad",
  main = "Densidad de la Temperatura en el medio de la oficina",
  type="n", ylim=c(0,0.38), xlim=c(10,30),sub="9:00-13:00")  
  colores=c("darkorange1","darkviolet","red","seagreen","blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[1:5]){
  dme.1=density(Tem$Taofimedio[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
  window=kernel, give.Rkern = FALSE)
  lines(dme.1,col=colores[k])    
  k=k+1
  }
  legend(12,0.29, hora[1:5], bty="n",
  col=colores[1:5],lty=1,lwd=2,cex=0.6)


#Temperatura medio oficina para las siguientes 5 horas 14:00-18:00 
  hora=unique(Tem$Tiemp)
  n=length(hora)  
  dTme.2=density(Tem$Taofimedio[Tem$Tiemp=="9:00"], bw="nrd0", kernel="epanechnikov",weights = NULL,
             window=kernel, give.Rkern = FALSE)
  plot(dTme.2,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la Temperatura en el medio de la oficina",
       type="n", ylim=c(0,0.36), xlim=c(10,30),sub="14:00-18:00")  
  colores=c("darkorange1","darkviolet","red",
            "seagreen","blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[6:10]){
  dme.2=density(Tem$Taofimedio[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
              window=kernel, give.Rkern = FALSE)
    lines(dme.2,col=colores[k])    
    k=k+1
  }
  legend(12,0.29, hora[6:10], bty="n",
         col=colores[1:5],lty=1,lwd=2,cex=0.6)
  
#TEMPERATURA EN EL FONDO DE LA OFICINA
#----------------------------------------
#Temperatura fondo de la oficina para las primeras 5 horas 9:00-13:00 :)
  hora=unique(Tem$Tiemp)
  n=length(hora)  
  dTfo.1=density(Tem$Taofifondo[Tem$Tiemp=="9:00"], bw="nrd0",
                 kernel="epanechnikov",weights = NULL,window=kernel, give.Rkern = FALSE)
  
  plot(dTfo.1,xlab = "Temperatura", ylab = "Densidad",
       main = "Densidad de la Temperatura en el fondo de la oficina",
       type="n", ylim=c(0,0.45), xlim=c(10,30),sub="9:00-13:00")  
  colores=c("darkorange1","darkviolet","red","seagreen","blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[1:5]){
    dfo.1=density(Tem$Taofifondo[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
                  window=kernel, give.Rkern = FALSE)
    lines(dfo.1,col=colores[k])    
    k=k+1
  }
  legend(12,0.29, hora[1:5], bty="n",
         col=colores[1:5],lty=1,lwd=2,cex=0.6)


#Temperatura fondo oficina para las siguientes 5 horas 14:00-18:00 
  hora=unique(Tem$Tiemp)
  n=length(hora)  
  dTfo.2=density(Tem$Taofifondo[Tem$Tiemp=="9:00"], bw="nrd0", kernel="epanechnikov",weights = NULL,
                 window=kernel, give.Rkern = FALSE)
  plot(dTfo.2,xlab = "Temperatura", ylab = "Densidad",main = "Densidad de la Temperatura en el fondo de la oficina",
       type="n", ylim=c(0,0.45), xlim=c(10,30),sub="14:00-18:00")  
  colores=c("darkorange1","darkviolet","red",
            "seagreen","blue","yellow","navy", "darkred","forestgreen","turquoise4")
  k=1
  for(i in hora[6:10]){
    dfo.2=density(Tem$Taofifondo[Tem$Tiemp==i], bw="nrd0", kernel="epanechnikov",weights = NULL,
                  window=kernel, give.Rkern = FALSE)
    lines(dfo.2,col=colores[k])    
    k=k+1
  }
  legend(12,0.29, hora[6:10], bty="n",
         col=colores[1:5],lty=1,lwd=2,cex=0.6)
  
#---------------------------------------------------------------------------
# CONCLUSION: en los tres ambientes en la tarde, el sistema de climatización 
#  logra mantener una temperatura regulada constante, que en contraste 
# con el funcionamiento en la mañana no se logra ya que se puede observar 
#  que existe una mayor variación entre las densidades de las temperaturas 
#  en la mañana, por ejemplo se nota que a las 9:00 de la mañana el sistema 
# de climatización no logra regular la temperatura, puesto es más fría que en 
#  las siguientes horas.
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
  #------> # 3.- Comparar la temperatura en dos 
  #instantes (9h00 vs. 10h00) en un día para determinar 
  #si el sistema de climatización es capaz de mantener 
  #una temperatura constante o no. Esto se haría para 
  #cada sitio y se realizaría un test de comparación de dos poblaciones
  #COMPARACIÓN UNIVARIANTE DE DENSIDADES: (sm.density.compare)
  #(Comparison of univariate density estimates)
  #test de igualdad de densidades entre dos funciones de densidad
  #---------------------------------------------------------------------------
#---------------------------------------------------------------------------

library(sm)
par(mfrow=c(2,2))

#Temperatura a las 9 y 10 a.m. en la entrada de la oficina:
  Teno.9=which(Efienerg$Tiemp=="9:00")
  #Teno[Teno.9]
  #Teno[Teno.9[1]]#la primera temperatura que se ve a las 9am [1] 46.99583
  Teno.10=which(Efienerg$Tiemp=="9:00")
  T.9.eno=length(Teno.9)
  T.10.eno=length(Teno.10)
  GRUPO.eno=c(rep(1,T.9.eno),rep(2,T.10.eno))
  x.eno=c(Teno[Teno.9],Teno[Teno.10])
  compTeno.9am=sm.density.compare(x.eno,group=GRUPO.eno, model="equal")
  title(main="Temperatura a las 9:00 y 10:00 a.m.\nEntrada de la oficina",
  col="red",cex=1.2,pch=19)
  legend(36,0.10,c("9 a.m.","10 a.m."), bty="n",col=c("red", "green"),
  lty=c(1,3),lwd=2,cex=1,pch=c(NA_integer_))

#Temperatura a las 9 y 10 a.m. en el medio de la oficina:
  Tmedo.9=which(Efienerg$Tiemp=="9:00")
  Tmedo.10=which(Efienerg$Tiemp=="10:00")
  T.9.medo=length(Tmedo.9)
  T.10.medo=length(Tmedo.10)
  GRUPO.medo=c(rep(1,T.9.medo),rep(2,T.10.medo))
  x.medo=c(Tmedo[Tmedo.9],Tmedo[Tmedo.10])
  compTmedo.9am=sm.density.compare(x.medo,group=GRUPO.medo, model="equal")         
  title(main="Temperatura a las 9:00 y 10:00 a.m.\n Mitad de la Oficina",
        col="red",cex=1.2,pch=19)
  legend(30,0.10,c("9 a.m.","10 a.m."), bty="n",col=c("red", "green"),
  lty=c(1,3), lwd=c(3),cex=1,pch=c(NA_integer_))

#Temperatura a las 9 y 10 a.m. en el fondo de la oficina:
  Tfon.9=which(Efienerg$Tiemp=="9:00")
  Tfon.10=which(Efienerg$Tiemp=="10:00")
  T.9.fon=length(Tfon.9)
  T.10.fon=length(Tfon.10)
  GRUPO.fon=c(rep(1,T.9.fon),rep(2,T.10.fon))
  x.fon=c(Tfon[Tfon.9],Tfon[Tfon.10])
  compTfon.9am=sm.density.compare(x.fon,group=GRUPO.fon, model="equal")
  title(main="Temperatura a las 9:00 y 10:00 a.m.\n Fondo de la Oficina",
        col="red",cex=1.2,pch=19)
  legend(15,0.07,c("9 a.m.","10 a.m."),bty="n",col=c("red", "green"),
  lty=c(1,3), lwd=c(3),cex=0.9,pch=c(NA_integer_))
  
  
  
  
  
  
  
  
  
  
  
  
#-----------------Contraste de normalidad------------------------

  library(nortest)  
    
hist(Tem$Texte,freq = FALSE)  
curve(dnorm(x,mean(Tem$Texte),sd(Tem$Texte)),add = TRUE)  

lillie.test(Tem$Texte)  
# temperatura exterior es normal

hist(Tem$Taofifondo,freq = FALSE)  
curve(dnorm(x,mean(Tem$Taofifondo),sd(Tem$Taofifondo)),add = TRUE)  

lillie.test(Tem$Taofifondo)  
#temperatura fondo no es normal

hist(Tem$Taofientrada,freq = FALSE)  
curve(dnorm(x,mean(Tem$Taofientrada),sd(Tem$Taofientrada)),add = TRUE)  

lillie.test(Tem$Taofientrada)  
#temperatura entrada no es normal

hist(Tem$Taofimedio,freq = FALSE)  
curve(dnorm(x,mean(Tem$Taofimedio),sd(Tem$Taofimedio)),add = TRUE)  

lillie.test(Tem$Taofimedio)  
#temperatura medio no es normal


#----------------------transformacion de las variables de temperatura mediante logaritmo-----

fondo<-log(Tem$Taofifondo)
hist(fondo,freq = FALSE)  
curve(dnorm(x,mean(fondo),sd(fondo)),add = TRUE)  

lillie.test(fondo)  
# log(fondo) no es normal


entrada<-log(Tem$Taofientrada)
hist(entrada,freq = FALSE)  
curve(dnorm(x,mean(entrada),sd(entrada)),add = TRUE)  

lillie.test(entrada)  
#log(entrada) no es normal


medio<-log(Tem$Taofimedio)
hist(medio,freq = FALSE)  
curve(dnorm(x,mean(medio),sd(medio)),add = TRUE)  

lillie.test(medio)  
#log(medio) no es normal

#library(EnvStats)
library(MASS)


b<-boxcox(Tem$Taofifondo~1,lambda = seq(-6,6,0.01))



# completar parar ve si funciona BOX-COX

library(ash)
c<-ash1()

x <- rnorm(100)         # data
hist(x,freq = FALSE)
curve(dnorm(x,mean(x),sd(x)),add = TRUE)
f <- ash1(bin1(x,nbin=50),5) # compute ash estimate
plot( f , type="l" )  
  