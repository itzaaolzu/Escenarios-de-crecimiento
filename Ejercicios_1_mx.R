# Paquetes ----
require(openxlsx) # leer excel
require(readxl) #leer excel
require(tidyverse) # indispensable
require(janitor) #limpieza de datos
require(skimr) #resumen de datos

# Este comando deshabilita la notación científica.
options(scipen=999)

#Pr�ctica 1. Escenarios de crecimiento de la poblaci�n de M�xico
#Autor: Itza Akari Olgu�n Z��iga
#Fecha: 13/01/2021
#Principales fuente: Indicadores demogr�ficos CONAPO, Demography Through Problems-Keyfitz

#Indicadores demogr�ficos de la poblaci�n en M�xico
ind_demo<- read.csv("https://raw.githubusercontent.com/itzaaolzu/Data-online/main/ind_dem_proyecciones.csv", encoding = "latin1")

#Para un mejor manejo de los objetos se recomienda nombrarlos seg�n el n�mero de problema e inciso


#################### Problema 1##################


# (a) Suponga que la poblaci�n de la Republica Mexicana de 1960 crece geometricamente con una tasa de crecimiento anual de r=0.02.
# Si r esta compuesta anualmente, por ejemplo, la poblaci�n P1 al final del a�o 1 es igual a P0(1+r)^1. Determina el tama�o
# de la poblaci�n al final de 10 a�os.

# �Cu�l es nuestra poblaci�n inicial, es decir P0?
P0_1a <- ind_demo %>%
  dplyr::filter(CVE_GEO==0) %>% #filtramos republica mexicana
  dplyr::filter(A�O==1960) %>% #filtramos el a�o
  dplyr::count(A�O, wt=POB_MIT_A�O) #creamos en dataframe con los elementos que nso interesan

# �Cu�l es nuestra tasa de crecimiento anual (r)?
r_1a<- 0.02

#�Al final de cuanto tiempo (t) queremos saber la poblaci�n?
t_1a<-10

#Entonces...�Cu�l es nuestra poblaci�n final con crecimiento geom�trico
P10a = (P0_1a$n*(1+r_1a)^t_1a)
P10_1a <-data.frame(A�O=c(1970),n=c(P10))

#Poblaciones del problema 1
Problema_1a<-rbind(P0_1a,P10_1a)

#�C�mo se ve este crecimiento?
matplot(Problema1$A�O,Problema1$n,type="l", xlab="A�os", ylab= "Poblaci�n") #Geom�trico!


# (b) Suponiendo que la misma p�blaci�n crece aritm�ticamente, es decir P0(1+tr)
# �Cu�l ser� la poblaci�n al final de 10 a�os?
p10b= P0_1a$n*(1+t_1a*r_1a)
P10_1b <-data.frame(A�O=c(1970),n=c(p10b))

#Poblaciones del problema 
Problema_1b<-rbind(P0_1a,P10_1b)

#�C�mo se ve este crecimiento?
matplot(Problema_1b$A�O,Problema_1b$n,type="l", xlab="A�os", ylab= "Poblaci�n") #Arim�tico!


################# Problema 2#######################

# 2.
# Identifica la poblaci�n que tuvo M�xico a mitad de 1965 y a la mitad de 1970.
# Extrapola las poblaciones asumiendo los siguentes supuestos:

#Existen otras formas de extraer valores de de los data frame y formar uno nuevo...

#Para obtener la poblaci�n de M�xico en 1965
p0_2 <- ind_demo[(ind_demo$A�O==1965) & (ind_demo$CVE_GEO==0), c("A�O","POB_MIT_A�O")]

#Para obtener la poblaci�n de M�xico en 1970
p5_2 <- ind_demo[(ind_demo$A�O==1970) & (ind_demo$CVE_GEO==0), c("A�O","POB_MIT_A�O")]

# (a) Aumento absoluto fijo, es decir progresi�n aritm�tica
# Recordando que el crecimiento aritm�tico Pt = P0*(1+t*r)
# Entonces la tasa de crecimiento ser�a r = ((Pt/P0)-1)/t

r_2a<- ((p5_2$POB_MIT_A�O/p0_2$POB_MIT_A�O)-1)/5
P102a = p0_2$POB_MIT_A�O*(1+10*r_2a)
P10_2a <- data.frame(A�O=c(1975),POB_MIT_A�O=c(P102a))

#Unir bases
Problema_2a<-rbind(p0_2,p5_2,P10_2a)

#�C�mo se ve este crecimiento?
matplot(Problema_2a$A�O, Problema_2a$POB_MIT_A�O, type="l", xlab="A�os", ylab= "Poblaci�n") #Arim�tico!



#################Problema 6#######################

# No se prioriza el acceso a m�todos anticonceptivos y hay fallas en su distribuci�n, a partir de 1995
# la poblaci�n comienza a incrementarse a un 2% por a�o durante 100 a�os. Qu� diferencia hace si el 2% est� compuesto
# anualmente o continuamente?

# (Crecimiento compuesto anual: Pt= P0(1+r)^t)
# (Crecimeinto compuesto contunuamente: Pt= P0(e^rt))

#Datos
P0_6a<-ind_demo[(ind_demo$A�O==1995) & (ind_demo$CVE_GEO==0), c("A�O", "POB_MIT_A�O")] #Poblaci�n de M�xico en 1995
r_6a<-.02 
t_6a<-100

#Crecimiento anual compuesto
Pta_6a = P0_6a$POB_MIT_A�O*((1+r_6a)^t_6a)

#Crecimiento compuesto continuamente
Ptc_6a = P0_6a$POB_MIT_A�O*(exp(r_6a*t_6a))



#################Problema 9#######################


# En 1985 M�xico sufre una crisis sanitaria, se cerraron las fronteras y comienza un periodo en el cual hay menos 
# nacimientos que muertes. Si decrece a una tasa anual de 0.0075 �Cu�ntos a�os tardar� en alcanzar la mitad de 
# su poblaci�n actual?


# Tasa anual Pt = P0(1+r)^t (crecimiento ge�metrico)

#Poblaci�n de 1985
P0_9a<-ind_demo[(ind_demo$A�O==1985) & (ind_demo$CVE_GEO==0), c("A�O", "POB_MIT_A�O")] #Poblaci�n de M�xico en 1985
Pt_9a<-P0_9a$POB_MIT_A�O/2
r_9a = -0.0075

#tiempo t=log(Pt/P0)/log(1+r)

t_9a = (log(P0_9a$POB_MIT_A�O/Pt_9a))/log(1-r_9a)


#########################Problema 10 #######################################

#Cuantas duplicaciones ocurrieron entre la poblaci�n de M�xico de 1950 y la del 2050

Pt_10a<-ind_demo[(ind_demo$A�O==2050) & (ind_demo$CVE_GEO==0), c("A�O", "POB_MIT_A�O")] #Poblaci�n de M�xico en el 2050
P0_10a<-ind_demo[(ind_demo$A�O==1950) & (ind_demo$CVE_GEO==0), c("A�O", "POB_MIT_A�O")] #Poblaci�n de M�xico en el 1950

#Formula duplicaci�n Pt= P0 *2^n

#N�mero de duplicaciones
n_10a=(log(Pt_10a$POB_MIT_A�O)/log(P0_10a$POB_MIT_A�O))-1


  
