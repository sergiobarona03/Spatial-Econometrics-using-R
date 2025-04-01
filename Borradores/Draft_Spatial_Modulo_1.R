

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
library(ggpubr)

setwd("C:/Users/Portatil/Desktop/SpatialData/EOD_Bogota_2019/Zonificacion_shapefiles/ZONAS")
#---------------------------------------#
# Cargar datos y convertir en polígonos #
#---------------------------------------#
bg <- read_sf(dsn=getwd(), layer="UTAM")
bg <- st_zm(bg)
bg %>% head() %>% print(width = 120)

summary(bg)
View(bg)
# Convertir SpatialPolygonsDataFrame en data.frame
bg.df <- data.frame(bg)

head(bg.df)            # Primeras filas
tail(bg.df)            # Últimas filas
names(bg.df)           # Nombres de las variables
class(bg.df)           # Clase del objeto
summary(bg.df)         # Resumen descriptivo

#--------------------------------------------------------------------#
# Gráfica de los polígonos (variables independientes y dependientes) #
#--------------------------------------------------------------------#

##############################
## Variables independientes ##
##############################

# Cargar la base de datos
dataset = readxl::read_excel("C:/Users/Portatil/Desktop/SpatialData/Final.input.dataset.xlsx")

dataset_plot <- bg %>% 
  mutate(Utam = UTAM) %>%
  left_join(dataset, by = "Utam") %>% na.omit()

# Remover área rual
dataset_plot <- dataset_plot %>% filter(!Utam %in%
                                          c("UPR1",
                                            "UPR2",
                                            "UPR3"))

# Figura 1A: densidad poblacional
f1a <- dataset_plot %>% ggplot() +
  aes(fill = `Número de habitantes por Km2*`) +
  geom_sf() + scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.2,0.82),
        legend.title = element_blank()) + labs(title = 
                                                 "Número de habitantes por km2")


# Figura 1b: tamaño promedio de los hogares
f1b <- dataset_plot %>% ggplot() +
  aes(fill = mean_size) +
  geom_sf() +  scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.2,0.82),
        legend.title = element_blank()) + labs(title = 
                                                 "Tamaño promedio del hogar")

# Figura 1c: Ingresos
f1c <- dataset_plot %>% ggplot() +
  aes(fill = ingresos_bajos) +
  geom_sf() +  scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.2,0.82),
        legend.title = element_blank()) + labs(title = 
                                                 "Ingresos bajos")

# Figura 1d: educación
f1d <- dataset_plot %>% ggplot() +
  aes(fill = higher) +
  geom_sf() +  scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.2,0.82),
        legend.title = element_blank()) + labs(title = 
                                                 "Nivel de educación superior")


# Gráfica de los polígonos para variables independientes
# (densidad, tamaño, ingresos y educación)
ggarrange(f1a, f1b,
          f1c, f1d, ncol = 4, nrow = 1)

############################
## Variables dependientes ##
############################

f2a <- dataset_plot %>% ggplot() +
  aes(fill = `Tasa de viajes en transporte público por persona al día*`) +
  geom_sf() +  scale_fill_gradientn(colours = rev(
    grDevices::hcl.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.2,0.82),
        legend.title = element_blank()) + labs(title = 
                                                 "Tasa de viajes en SITP por persona al día")

f2b <- dataset_plot %>% ggplot() +
  aes(fill = `Tasa de viajes en transporte público por persona que viaja al día*`) +
  geom_sf() +  scale_fill_gradientn(colours = rev(
    grDevices::hcl.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.2,0.82),
        legend.title = element_blank()) + labs(title = 
                                                 "Tasa de viajes en SITP por persona que viaja al día")

ggarrange(f2a, f2b, ncol = 2, nrow = 1)


#-----------------------------------------------------------#
# Histograma, densidad y scatter plot con StatDA            #
#-----------------------------------------------------------#

# Cambiar el nombre de la variable dependiente
dataset_plot = dataset_plot %>%
  mutate(Rate = `Tasa de viajes en transporte público por persona al día*`)

# Además de los histogramas con densidad normal,
# hay paquetes con funcionalidades flexibles (v.g. StatDA)
attach(dataset_plot)
me = mean(log(Rate))
sd = sd(log(Rate))
min(log(Rate))
max(log(Rate))
windows()
StatDA::edaplot(log(Rate), scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(-3,0.5, by = 0.15),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(-3, 0.5), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram, Density Plot, Scatterplot,
	and Boxplot of log(Rate)",
                P.xlab="log(Rate)", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(log(Rate)), lwd=2, col='blue')
curve(dnorm(x, mean=me, sd=sd), from=-3, to=0.5, add=T,
      col='red', lwd=3)
leg.txt <- c("   log(Rate)", "Min. =    -2.5602",
             "Max.=    0.0520", "Median =    -0.5016",
             "Mean =  -0.5616", "Std. dev. =   0.3830")
legend (x=-2.5, y=1.3, bty="n", leg.txt)

#--------------------------------------------------#
# ¿El problema son los outliers? Usar criterio IQR #
#--------------------------------------------------#

#-------------------------------#
# Verificar normalidad: QQ-plot #
#-------------------------------#
car::qqPlot(Rate, envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Rate',
            pch=1, cex=1, col="black")

# Prueba con la transformación logarítmica (mejor)
car::qqPlot(log(Rate), envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Log(Rate)',
            pch=1, cex=1, col="black")

# Se verifican otras transformaciones
par(mfrow=c(4,1))
boxplot(Rate, xlab='Rate', horizontal=TRUE, cex=0.9, width=2,
        outcol="red", cex.lab=1.8)
boxplot(log(Rate), xlab='LOG Rate', horizontal=TRUE, cex=0.9,
        width=2, outcol="red", cex.lab=1.8)
boxplot(sqrt(Rate), xlab='SQRT Rate', horizontal=TRUE, cex=0.9,
        width=2, outcol="red", cex.lab=1.8)
boxplot(Rate^(1/3), xlab='Cube root Rate', horizontal=TRUE, cex=0.9,
        width=2, outcol="red", cex.lab=1.8)
title("\nComparing Various Transformations of Rate", outer=TRUE)

# En lo sucesivo, se trabaja con la transformación logarítimica

#-------------------------------------------------------#
# Construir el mapa con base en la variable dependiente #
#-------------------------------------------------------#
min(log(Rate))      # -0.3147
max(log(Rate))      # 4.159664


# Seleccionar una paleta de colores (RColorBrewer package)
brewer.pal(5, "YlGnBu")
display.brewer.pal(5, "YlGnBu")    # That should work
colors <- brewer.pal(5, "YlGnBu")  # Stores our colors in object color

# La función classIntervals divide los datos en clases.
# En este caso, se dividen de acuerdo con el cuantil
par(mfrow = c(1,1))

color.cat.rate<-classIntervals(log(ny$Rate), 
            n=5, style="quantile", dataPrecision=1)
colcode <- findColours(color.cat.rate, colors)

plot(ny[c("Rate", "geometry")], col=colcode)
title('Map of AIDS Rate per 1,000 in NYC')
legend('topleft', legend=c(names(attr(colcode, 'table'))),
       fill=c(attr(colcode, 'palette')), title='log(AIDS Rate) - Quantile')

# Lo mismo se puede hacer para la sd, por ejemplo
color.cat.rate<-classIntervals(log(ny$Rate), n=5, style="hclust", dataPrecision=1)
colcode <- findColours(color.cat.rate, colors)

plot(ny[c("Rate", "geometry")], col=colcode)
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