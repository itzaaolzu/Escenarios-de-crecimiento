# Paquetes ----
require(openxlsx) # leer excel
require(readxl) #leer excel
require(tidyverse) # indispensable
require(janitor) #limpieza de datos
require(skimr) #resumen de datos

# Este comando deshabilita la notaciÃ³n cientÃ­fica.
options(scipen=999)

#Práctica 1. Escenarios de crecimiento de la población de México
#Autor: Itza Akari Olguín Zúñiga
#Fecha: 13/01/2021
#Principales fuente: Indicadores demográficos CONAPO, Demography Through Problems-Keyfitz

#Indicadores demográficos de la población en México
ind_demo<- read.csv("https://raw.githubusercontent.com/itzaaolzu/Data-online/main/ind_dem_proyecciones.csv", encoding = "latin1")

#Para un mejor manejo de los objetos se recomienda nombrarlos según el número de problema e inciso


#################### Problema 1##################


# (a) Suponga que la población de la Republica Mexicana de 1960 crece geometricamente con una tasa de crecimiento anual de r=0.02.
# Si r esta compuesta anualmente, por ejemplo, la población P1 al final del año 1 es igual a P0(1+r)^1. Determina el tamaño
# de la población al final de 10 años.

# ¿Cuál es nuestra población inicial, es decir P0?
P0_1a <- ind_demo %>%
  dplyr::filter(CVE_GEO==0) %>% #filtramos republica mexicana
  dplyr::filter(AÑO==1960) %>% #filtramos el año
  dplyr::count(AÑO, wt=POB_MIT_AÑO) #creamos en dataframe con los elementos que nso interesan

# ¿Cuál es nuestra tasa de crecimiento anual (r)?
r_1a<- 0.02

#¿Al final de cuanto tiempo (t) queremos saber la población?
t_1a<-10

#Entonces...¿Cuál es nuestra población final con crecimiento geométrico
P10a = (P0_1a$n*(1+r_1a)^t_1a)
P10_1a <-data.frame(AÑO=c(1970),n=c(P10))

#Poblaciones del problema 1
Problema_1a<-rbind(P0_1a,P10_1a)

#¿Cómo se ve este crecimiento?
matplot(Problema1$AÑO,Problema1$n,type="l", xlab="Años", ylab= "Población") #Geométrico!


# (b) Suponiendo que la misma póblación crece aritméticamente, es decir P0(1+tr)
# ¿Cuál será la población al final de 10 años?
p10b= P0_1a$n*(1+t_1a*r_1a)
P10_1b <-data.frame(AÑO=c(1970),n=c(p10b))

#Poblaciones del problema 
Problema_1b<-rbind(P0_1a,P10_1b)

#¿Cómo se ve este crecimiento?
matplot(Problema_1b$AÑO,Problema_1b$n,type="l", xlab="Años", ylab= "Población") #Arimético!


################# Problema 2#######################

# 2.
# Identifica la población que tuvo México a mitad de 1965 y a la mitad de 1970.
# Extrapola las poblaciones asumiendo los siguentes supuestos:

#Existen otras formas de extraer valores de de los data frame y formar uno nuevo...

#Para obtener la población de México en 1965
p0_2 <- ind_demo[(ind_demo$AÑO==1965) & (ind_demo$CVE_GEO==0), c("AÑO","POB_MIT_AÑO")]

#Para obtener la población de México en 1970
p5_2 <- ind_demo[(ind_demo$AÑO==1970) & (ind_demo$CVE_GEO==0), c("AÑO","POB_MIT_AÑO")]

# (a) Aumento absoluto fijo, es decir progresión aritmética
# Recordando que el crecimiento aritmético Pt = P0*(1+t*r)
# Entonces la tasa de crecimiento sería r = ((Pt/P0)-1)/t

r_2a<- ((p5_2$POB_MIT_AÑO/p0_2$POB_MIT_AÑO)-1)/5
P102a = p0_2$POB_MIT_AÑO*(1+10*r_2a)
P10_2a <- data.frame(AÑO=c(1975),POB_MIT_AÑO=c(P102a))

#Unir bases
Problema_2a<-rbind(p0_2,p5_2,P10_2a)

#¿Cómo se ve este crecimiento?
matplot(Problema_2a$AÑO, Problema_2a$POB_MIT_AÑO, type="l", xlab="Años", ylab= "Población") #Arimético!



#################Problema 6#######################

# No se prioriza el acceso a métodos anticonceptivos y hay fallas en su distribución, a partir de 1995
# la población comienza a incrementarse a un 2% por año durante 100 años. Qué diferencia hace si el 2% está compuesto
# anualmente o continuamente?

# (Crecimiento compuesto anual: Pt= P0(1+r)^t)
# (Crecimeinto compuesto contunuamente: Pt= P0(e^rt))

#Datos
P0_6a<-ind_demo[(ind_demo$AÑO==1995) & (ind_demo$CVE_GEO==0), c("AÑO", "POB_MIT_AÑO")] #Población de México en 1995
r_6a<-.02 
t_6a<-100

#Crecimiento anual compuesto
Pta_6a = P0_6a$POB_MIT_AÑO*((1+r_6a)^t_6a)

#Crecimiento compuesto continuamente
Ptc_6a = P0_6a$POB_MIT_AÑO*(exp(r_6a*t_6a))



#################Problema 9#######################


# En 1985 México sufre una crisis sanitaria, se cerraron las fronteras y comienza un periodo en el cual hay menos 
# nacimientos que muertes. Si decrece a una tasa anual de 0.0075 ¿Cuántos años tardará en alcanzar la mitad de 
# su población actual?


# Tasa anual Pt = P0(1+r)^t (crecimiento geómetrico)

#Población de 1985
P0_9a<-ind_demo[(ind_demo$AÑO==1985) & (ind_demo$CVE_GEO==0), c("AÑO", "POB_MIT_AÑO")] #Población de México en 1985
Pt_9a<-P0_9a$POB_MIT_AÑO/2
r_9a = -0.0075

#tiempo t=log(Pt/P0)/log(1+r)

t_9a = (log(P0_9a$POB_MIT_AÑO/Pt_9a))/log(1-r_9a)


#########################Problema 10 #######################################

#Cuantas duplicaciones ocurrieron entre la población de México de 1950 y la del 2050

Pt_10a<-ind_demo[(ind_demo$AÑO==2050) & (ind_demo$CVE_GEO==0), c("AÑO", "POB_MIT_AÑO")] #Población de México en el 2050
P0_10a<-ind_demo[(ind_demo$AÑO==1950) & (ind_demo$CVE_GEO==0), c("AÑO", "POB_MIT_AÑO")] #Población de México en el 1950

#Formula duplicación Pt= P0 *2^n

#Número de duplicaciones
n_10a=(log(Pt_10a$POB_MIT_AÑO)/log(P0_10a$POB_MIT_AÑO))-1


  
