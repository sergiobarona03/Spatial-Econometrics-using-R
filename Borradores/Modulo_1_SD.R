
####################################
## Análisis espacial I:           ##
## Análisis exploratorio de datos ##
####################################

#---------------------------------#
# Cargar las librerías necesaria  #
#---------------------------------#
library(classInt)
library(car)
library(spdep)
library(RColorBrewer)
library(StatDA) 
library(sf)
library(MASS)
library(tidyverse)

file.choose()
setwd()
setwd("C:/Users/Portatil/Desktop/Summer Course Spatial Econometrics using R 2015/Labs/Data")
setwd("C:/Users/danie/OneDrive - Gimnasio Central del Valle/Documentos/LEA/Data")
#---------------------------------------#
# Cargar datos y convertir en polígonos #
#---------------------------------------#
ny <- read_sf(dsn=getwd(), layer="NYAIDS")
summary(ny)

# Convertir SpatialPolygonsDataFrame en data.frame
ny.df <- data.frame(ny)

head(ny.df)            # Primeras filas
tail(ny.df)            # Últimas filas
names(ny.df)           # Nombres de las variables
class(ny.df)           # Clase del objeto
summary(ny.df)         # Resumen descriptivo

# Gráfica de los polígonos
par(mfrow=c(1,1))
plot(ny)   

#------------------------------------------------------#
# Histograma y densidad normal: tasa x 1000 habitantes #
#------------------------------------------------------#
attach(ny.df)

me <- mean(log(Rate1000))
sd <- sd(log(Rate1000))

#Opción 1: histograma de Rate1000 con densidad normal usando lines()

min(log(Rate1000))
max(log(Rate1000))
hist(log(Rate1000), breaks=seq(-1,5, by = 0.15),
     col='lightgoldenrod2',
     border='white',
     probability=T, ylab='Density',
     main='Histogram of Transformed AIDS Rate [log(Rate1000)]',
     xlab='log(Rate1000)')
box()

x <- seq(-1,5, by = 0.2)
y <- dnorm(x, mean=me, sd=sd, log=FALSE)
lines(x,y, lty='solid', col='red', lwd=2)
leg.txt <- c("   log(Rate1000)", "Min. =    -4.92",
             "Max.=    -0.45", "Median =    -2.23",
             "Mean =  -2.33", "Std. dev. =   0.93")
legend (x=-5, y=.6, bty="n", leg.txt)

# Opción 2: histograma Rate1000 con densidad normal usando curve()
# MANTENER ÚNICAMENTE ESTE HISTOGRAMA: PENDIENTE
me <- mean(log(Rate1000))
sd <- sd(log(Rate1000))
hist(log(Rate1000), breaks=seq(-1,5, by = 0.15), col='papayawhip',
     probability=T,
     ylab='Density', main='Histogram of log of Rate1000',
     xlab='log(Rate1000)')
box()
curve(dnorm(x, mean=me, sd=sd), from=-1, to=5, add=T, col='red', lwd=2)
legend (x=-1, y=.5, bty="n", leg.txt)

#-------------------------------#
# Verificar normalidad: QQ-plot #
#-------------------------------#
car::qqPlot(Rate1000, envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Rate1000',
            pch=1, cex=1, col="black")

# Prueba con la transformación logarítmica (mejor)
car::qqPlot(log(Rate1000), envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Log(Rate1000)',
            pch=1, cex=1, col="black")

# Se verifican otras transformaciones
par(mfrow=c(4,1))
boxplot(Rate1000, xlab='Rate1000', horizontal=TRUE, cex=0.9, width=2,
        outcol="red", cex.lab=1.8)
boxplot(log(Rate1000), xlab='LOG Rate1000', horizontal=TRUE, cex=0.9,
        width=2, outcol="red", cex.lab=1.8)
boxplot(sqrt(Rate1000), xlab='SQRT Rate1000', horizontal=TRUE, cex=0.9,
        width=2, outcol="red", cex.lab=1.8)
boxplot(Rate1000^(1/3), xlab='Cube root Rate1000', horizontal=TRUE, cex=0.9,
        width=2, outcol="red", cex.lab=1.8)
title("\nComparing Various Transformations of Rate1000", outer=TRUE)


#-------------------------------------------------------#
# Construir el mapa con base en la variable dependiente #
#-------------------------------------------------------#
min(log(Rate1000))      # -0.3147
max(log(Rate1000))      # 4.159664

# Seleccionar una paleta de colores (RColorBrewer package)
brewer.pal(5, "YlGnBu")
display.brewer.pal(5, "YlGnBu")    # That should work
colors <- brewer.pal(5, "YlGnBu")  # Stores our colors in object color

# La función classIntervals divide los datos en clases.
# En este caso, se dividen de acuerdo con el cuantil
color.cat.rate<-classIntervals(log(ny$Rate1000), 
                               n=5, style="quantile", dataPrecision=1)
colcode <- findColours(color.cat.rate, colors)

plot(ny[c("Rate1000", "geometry")], col=colcode)
title('Map of AIDS Rate per 1,000 in NYC')
legend('topleft', legend=c(names(attr(colcode, 'table'))),
       fill=c(attr(colcode, 'palette')), title='log(AIDS Rate) - Quantile')

# Lo mismo se puede hacer para la sd, por ejemplo
color.cat.rate<-classIntervals(log(ny$Rate1000), n=5, style="hclust", dataPrecision=1)
colcode <- findColours(color.cat.rate, colors)

plot(ny[c("Rate1000", "geometry")], col=colcode)
title('Map of AIDS Rate per 1,000 in NYC')
legend('topleft', legend=c(names(attr(colcode, 'table'))),
       fill=c(attr(colcode, 'palette')), title='log(AIDS Rate) - Clustering(H)',
)

#---------------------------------------------------------------------------#
# EDA/ESDA bivariado: Ratex1000 vs. PctBlack (porcentaje de población negra)#
#---------------------------------------------------------------------------#

#########################################################################
###
### HABIENDO ARGUMENTADO QUE LA TRANSFORMACIÓN LOGIT DE Rate1000 ES UNA DECENTE
### VARIABLE DEPENDIENTE, COMENCEMOS BIIVARIADA EDA/ESDA. ESTO GENERALMENTE
### IMPLICA GRÁFICOS DE DISPERSIÓN BIVARIADOS, OTRAS TRANSFORMACIONES Y
### EXAMEN CUIDADOSO DE POSIBLES VALORES EXTREMOS
###
#########################################################################

### Veamos algunas posibles variables independientes: pctblk, gini, medinc, pctpov
### pcthsed (o, si es necesario, una transformación adecuada de cada uno)

#### Primero: PctBlack (porcentaje de la población que declara ser de raza negra)
plot(PctBlk, logit(Rate1000), xlim=c(0,100), ylim=c(-5,1),
     main="logit(Rate1000) by PctBlk")

# ¿Quizás un indicio de curvilinealidad? Tendremos que comprobarlo.
# También verifique los valores cero en caso de que prefiramos un
# transformación de registro:

row.names(ny.df) <- ny.df$POSTAL
which(PctBlk == 0)     # ¡Sin ceros, por lo que no necesitamos trabajar con registros!

row.names(ny.df)[c(1, 25, 50, 100)] # Solo te dice qué ZIPS son

# Inspección visual de linealidad:

plot(PctBlk, logit(Rate1000), main="logit(Rate1000) by PctBlk")
abline(lm(logit(Rate1000) ~ PctBlk), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctBlk), lwd=2, col="blue")

# ¿Qué vemos? Ciertamente va en la dirección hipotética: como
# PctBlk aumenta, la tasa de SIDA aumenta. ¿No linealidad? Probablemente.
# (La función lowess, para "regresión ponderada localmente", es muy
# útil para esto). Entonces, ¿qué hacer? A ver que tronco
#la transformación nos compra:

plot(log(PctBlk), logit(Rate1000), main="logit(Rate1000) by log(PctBlk)")

# La linealidad parece mejorarse; ¡Pero esos valores atípicos!
# Ponlos uno al lado del otro para compararlos:

par(mfrow=c(1,2))
plot(PctBlk, logit(Rate1000))
abline(lm(logit(Rate1000) ~ PctBlk), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctBlk), lwd=2, col="blue")
plot(log(PctBlk), logit(Rate1000))
abline(lm(logit(Rate1000) ~ log(PctBlk)), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ log(PctBlk)), lwd=2, col="blue")
par(mfrow=c(1,1))

# La transformación de registros de PctBlk mejora las cosas, pero realmente
# destaca un valor atípico. Dejémoslos por ahora y probemos ambos
# raíz cuadrada y transformación logarítmica. La configuración se parece a esto:

which(log(PctBlk) < -1)
row.names(ny.df)[c(175)]
nyexcl <- ny.df[-c(175),]
detach(ny.df)
attach(nyexcl)
length(PctBlk)       # 174 looks right
which(log(PctBlk) < -1)

# Ahora veamos la transformación logarítmica sin el valor atípico:

plot(log(PctBlk), logit(Rate1000))
abline(lm(logit(Rate1000) ~ log(PctBlk)), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ log(PctBlk)), lwd=2, col="blue")

# Comprobemos el término R-cuadrado en una configuración de regresión lineal simple:

reglog <- lm(logit(Rate1000) ~ log(PctBlk))
summary(reglog)

# Vamos con la transformación logarítmica, pero quedémonos también
# con el conjunto de datos excluyendo los valores atípicos por ahora.
# Aquí es donde R hace la vida considerablemente más fácil que GeoDa.

################################################## #######################
#####
##### Ahora deberíamos hacer el mismo tipo de prueba con un par más
##### posibles variables independientes. Hice esto para las variables pct blk,
##### pcthisp, gini, pctpov. Los detalles de lo que hice se muestran a continuación.
##### al final de este tutorial. Pero deberías intentar esto por tu cuenta.
##### para ver si estás de acuerdo con mi valoración.
##### Cuando todo se calmó, mi elección para las variables independientes son las
##### versión sin transformar. Quedémonos con estos.
#####
#########################################################################

detach(nyexcl)
attach(ny.df)

#########################################################################
#####
##### UNA MATRIZ DE DISPERSIÓN UTILIZANDO LA FUNCIÓN pares ES UNA BUENA MANERA DE
##### MIRA CÓMO QUEDAN LAS COSAS DESPUÉS DE LAS TRANSFORMACIONES:
#####
#########################################################################

# Cambie el nombre de las variables para que los nombres sean lo suficientemente cortos como para ajustarse al
# ventanas de trazado

LogitRate <- logit(Rate1000)
LogBlack <- log(PctBlk)
pairs(cbind(LogitRate, LogBlack, PctWht, PctHisp, Gini, PctPov, PctHSEd), cex.labels=1.6,
      panel=function(x,y){
        points(x,y, cex=0.3)
        abline(lm(y~x), lty=2, lwd=2, col="red")
        lines(lowess(x,y), lwd=2, col="blue")
      },
      diag.panel=function(x){
        par(new=T)
        hist(x, main="", axes=F, col="wheat", nclass=20)
      }
)

##########################################################################
####                                                                  ####
#### FIN DEL BIVARIADO EDA/ESDA MIRANDO PctBlk ####
#### ####
#### PERO ASÍ ES COMO EXAMINÉ LAS OTRAS VARIABLES:                   ####
##########################################################################

# OMITIR ESTA PARTE Y ENFOCARSE EN EL GRÁFICO ANTERIOR QUE RESUME TODO.

attach(ny.df)

summary(PctWht)    # Sin valores cero
summary(Gini) # Sin valores cero

# Primero, veamos hacia dónde queremos llegar con PctWht:

plot(PctWht, logit(Rate1000), main="logit(Rate1000) by PctWht")
abline(lm(logit(Rate1000) ~ PctWht), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctWht), lwd=2, col="blue")

# Se ve bien, no creo que necesitemos transformar esto

# Ahora el coeficiente de Gini

plot(Gini, logit(Rate1000), main="logit(Rate1000) by Gini")
abline(lm(logit(Rate1000) ~ Gini), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ Gini), lwd=2, col="blue")

# Nuevamente, se ve bien con la excepción de algunos valores atípicos que atraen la atención.
# línea lowess un poco torcida al final.

# Ahora PctPov

plot(PctPov, logit(Rate1000), main="logit(Rate1000) by PctPov")
abline(lm(logit(Rate1000) ~ PctPov), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctPov), lwd=2, col="blue")  # Looks good!  No transformation

# Ahora PctHSEd

plot(PctHSEd, logit(Rate1000), main="logit(Rate1000) by PctHSEd")
abline(lm(logit(Rate1000) ~ PctHSEd), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctHSEd), lwd=2, col="blue") # Looks good!  No transformation

# Ahora PctHisp

plot(PctHisp, logit(Rate1000), main="logit(Rate1000) by PctHisp")
abline(lm(logit(Rate1000) ~ PctHisp), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctHisp), lwd=2, col="blue") # Looks good!  No transformation

# Ahora PctFemHH

plot(PctFemHH, logit(Rate1000), main="logit(Rate1000) by PctFemHH")
abline(lm(logit(Rate1000) ~ PctFemHH), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ PctFemHH), lwd=2, col="blue") # Looks a bit skewed

plot(sqrt(PctFemHH), logit(Rate1000), main="logit(Rate1000) by sqrt(PctFemHH)")
abline(lm(logit(Rate1000) ~ sqrt(PctFemHH)), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ sqrt(PctFemHH)), lwd=2, col="blue")

plot(log(PctFemHH), logit(Rate1000), main="logit(Rate1000) by log(PctFemHH)")
abline(lm(logit(Rate1000) ~ log(PctFemHH)), lty=2, lwd=2, col="red")
lines(lowess(logit(Rate1000) ~ log(PctFemHH)), lwd=2, col="blue")

reg1 <- lm(logit(Rate1000) ~ PctFemHH)
reg2 <- lm(logit(Rate1000) ~ log(PctFemHH))
reg3 <- lm(logit(Rate1000) ~ sqrt(PctFemHH))
summary(reg1)
summary(reg2)
summary(reg3)

# Una revisión rápida del R2 y el Resid SE, la variable sin transformar parece
# para rendir mejor

# Vale, ninguna de las transformaciones lo mejora, así que probablemente deberíamos quedarnos
# con la variable no transformada si decidimos usarla.

################################################## ######################
### ###
### FIN DE LA DISCUSIÓN SOBRE LA TRANSFORMACIÓN PARA femhh y edu_hs ###
### ###
################################################## ######################

# Limpieza interna:

ls()
rm(list=ls())
ls()

########################################################################
########################################################################
###                                                                  ###
### FINALIZAR LA DEMOSTRACIÓN PARA EDA/ESDA BIVARIADA                                  ###
###                                                                  ###
########################################################################
########################################################################