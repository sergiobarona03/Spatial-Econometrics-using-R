

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
library(moments)

setwd("C:/Users/PC/Desktop/SpatialData/EOD_Bogota_2019/Zonificacion_shapefiles/ZONAS")
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
dataset = readxl::read_excel("C:/Users/PC/Desktop/SpatialData/Final.input.dataset.xlsx")

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
windows()
ggarrange(f1a, f1b,
          f1c, f1d, ncol = 4, nrow = 1)

############################
## Variables dependientes ##
############################

# ESTO SE DEBERÍA OMITIR (VÉASE LAS LÍNEAS POSTERIORES)

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

# Organizar los mapas de las variables dependientes
windows()
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
me = mean(Rate)
sd = sd(Rate)
min(Rate)
max(Rate)
windows()
StatDA::edaplot(Rate, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(-0.5,1.1, by = 0.08),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(0, 1.2), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram, Density Plot, Scatterplot,
	and Boxplot of Rate",
                P.xlab="Rate", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(Rate), lwd=2, col='blue')
curve(dnorm(x, mean=me, sd=sd), from=-.5, to=1.2, add=T,
      col='red', lwd=3)
leg.txt <- c("Rate", 
             paste0("Min. = ", round(min(Rate),4)),
             paste0("Max. = ", round(max(Rate),4)),
             paste0("Mean = ", round(mean(Rate),4)),
             paste0("Median = ", round(median(Rate),4)),
             paste0("Std. dev. = ", round(sd(Rate),4)),
             paste0("Kurtosis = ", round(kurtosis(Rate),4)),
             paste0("Skewness = ", round(skewness(Rate),4)))
legend (x=0, y=2.5, bty="n", leg.txt)

#-----------------------------------------------------------#
# Outliers: Histograma, densidad y scatter plot con StatDA  #
#-----------------------------------------------------------#

# Detección de outliers
Q1 <- quantile(Rate, .25)
Q3 <- quantile(Rate, .75)
IQR <- IQR(Rate)

outliers <- dataset_plot %>% filter(Rate<(Q1 - 1.5*IQR) | Rate>(Q3 + 1.5*IQR))

# Distribución sin outliers
new_dataset_plot <- dataset_plot %>% filter(!Utam %in%
                                              levels(as.factor(outliers$Utam)))

attach(new_dataset_plot)
me = mean(Rate)
sd = sd(Rate)
min(Rate)
max(Rate)
windows()
StatDA::edaplot(Rate, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(0,1, by = 0.08),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(0.2, 1), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram, Density Plot, Scatterplot,
	and Boxplot of Rate",
                P.xlab="Rate", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(Rate), lwd=2, col='blue')
curve(dnorm(x, mean=me, sd=sd), from=-.5, to=1, add=T,
      col='red', lwd=3)
leg.txt <- c("Rate", 
             paste0("Min. = ", round(min(Rate),4)),
             paste0("Max. = ", round(max(Rate),4)),
             paste0("Mean = ", round(mean(Rate),4)),
             paste0("Median = ", round(median(Rate),4)),
             paste0("Std. dev. = ", round(sd(Rate),4)),
             paste0("Kurtosis = ", round(kurtosis(Rate),4)),
             paste0("Skewness = ", round(skewness(Rate),4)))
legend (x=0.18, y=2.8, bty="n", leg.txt)

#-------------------------------#
# Verificar normalidad: QQ-plot #
#-------------------------------#
attach(new_dataset_plot)
windows()
par(mfrow = c(2,2))
car::qqPlot(Rate, envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Rate',
            pch=1, cex=1, col="black",
            distribution = "norm")

car::qqPlot(log(Rate), envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Log(Rate)',
            pch=1, cex=1, col="black",
            distribution = "norm")

car::qqPlot(sqrt(Rate), envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot SQRT Rate',
            pch=1, cex=1, col="black",
            distribution = "norm")

car::qqPlot(Rate^(1/3), envelope=TRUE, xlab='', 
            main='Quantile Comparison Plot Cube root Rate',
            pch=1, cex=1, col="black",
            distribution = "norm")

# Se verifican otras transformaciones
windows()
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


#-------------------------------------------------------#
# Construir el mapa con base en la variable dependiente #
#-------------------------------------------------------#

# Seleccionar una paleta de colores (RColorBrewer package)
brewer.pal(5, "YlGnBu")
display.brewer.pal(5, "YlGnBu")    # That should work
colors <- brewer.pal(5, "YlGnBu")  # Stores our colors in object color

# La función classIntervals divide los datos en clases.
# En este caso, se dividen de acuerdo con el cuantil
par(mfrow = c(1,1))

color.cat.rate<-classIntervals(new_dataset_plot$Rate, 
            n=5, style="quantile", dataPrecision=1)
colcode <- findColours(color.cat.rate, colors)

plot(new_dataset_plot[c("Rate", "geometry")], col=colcode)
title('Map of Rate x')
legend('topleft', legend=c(names(attr(colcode, 'table'))),
       fill=c(attr(colcode, 'palette')), title='Rate - Quantile')

# Lo mismo se puede hacer para la sd, por ejemplo
color.cat.rate<-classIntervals(new_dataset_plot$Rate, n=5, style="hclust", 
                               dataPrecision=1)
colcode <- findColours(color.cat.rate, colors)

plot(new_dataset_plot[c("Rate", "geometry")], col=colcode)
title('Map of Rate x')
legend('topleft', legend=c(names(attr(colcode, 'table'))),
       fill=c(attr(colcode, 'palette')), title='Rate - Clustering(H)',
       )

#---------------------------------------------------------------------------#
# EDA/ESDA bivariado: Rate X vs. Pct - Personas de ingresos bajos           #
#---------------------------------------------------------------------------#

#########################################################################
###
### HABIENDO ARGUMENTADO QUE LA TRANSFORMACIÓN LOGIT DE Rate1000 ES UNA DECENTE
### VARIABLE DEPENDIENTE, COMENCEMOS BIIVARIADA EDA/ESDA. ESTO GENERALMENTE
### IMPLICA GRÁFICOS DE DISPERSIÓN BIVARIADOS, OTRAS TRANSFORMACIONES Y
### EXAMEN CUIDADOSO DE POSIBLES VALORES EXTREMOS
###
#########################################################################

### Veamos algunas posibles variables independientes: pctblk, gini, medinc,
## pctpov , pcthsed (o, si es necesario, 
## una transformación adecuada de cada uno)

#### Primero: PctBlack (porcentaje de la población que declara ser de raza negra)

plot(new_dataset_plot$ingresos_bajos, 
     new_dataset_plot$Rate, xlim=c(0,100), ylim=c(0.1,1),
     main="Rate by the proportion of low-income households")

# ¿Quizás un indicio de curvilinealidad? Tendremos que comprobarlo.
# También verifique los valores cero en caso de que prefiramos un
# transformación de registro:

# Inspección visual de linealidad:
plot(ingresos_bajos,
     Rate,
     main="Rate by the proportion of low-income households")
abline(lm(Rate ~ ingresos_bajos), 
       lty=2, lwd=2, col="red")
lines(lowess(Rate ~ ingresos_bajos), lwd=2, col="blue")

# ¿Qué vemos? Ciertamente va en la dirección hipotética: como
# PctBlk aumenta, la tasa de SIDA aumenta. ¿No linealidad? Probablemente.
# (La función lowess, para "regresión ponderada localmente", es muy
# útil para esto). Entonces, ¿qué hacer? A ver que tronco
#la transformación nos compra:

plot(log(ingresos_bajos), log(Rate), main="log(Rate) by log(low-income)")

# La linealidad parece mejorarse; ¡Pero esos valores atípicos!
# Ponlos uno al lado del otro para compararlos:

par(mfrow=c(1,2))
plot(ingresos_bajos, Rate)
abline(lm(Rate ~ ingresos_bajos), lty=2, lwd=2, col="red")
lines(lowess(Rate~ ingresos_bajos), lwd=2, col="blue")
plot(log(ingresos_bajos), log(Rate))
abline(lm(log(Rate) ~ log(ingresos_bajos)), lty=2, lwd=2, col="red")
lines(lowess(log(Rate)~ log(ingresos_bajos)), lwd=2, col="blue")
par(mfrow=c(1,1))

# La transformación de registros de PctBlk mejora las cosas, pero realmente
# destaca un valor atípico. Dejémoslos por ahora y probemos ambos
# raíz cuadrada y transformación logarítmica. La configuración se parece a esto:
par(mfrow=c(1,2))
plot(ingresos_bajos, Rate)
abline(lm(Rate ~ ingresos_bajos), lty=2, lwd=2, col="red")
lines(lowess(Rate~ ingresos_bajos), lwd=2, col="blue")
plot(sqrt(ingresos_bajos), sqrt(Rate))
abline(lm(sqrt(Rate) ~ sqrt(ingresos_bajos)), lty=2, lwd=2, col="red")
lines(lowess(sqrt(Rate)~ sqrt(ingresos_bajos)), lwd=2, col="blue")
par(mfrow=c(1,1))


# Comprobemos el término R-cuadrado en una configuración de regresión lineal simple:
reglog <- lm(log(Rate) ~ log(ingresos_bajos))
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



# Conservar únicamente valores positivos
new_dataset_plot <- new_dataset_plot %>% filter(
  elder > 0, minor > 0, ingresos_bajos > 0,
  higher > 0, agre > 0
)
attach(new_dataset_plot)
# Descriptivas
summary(mean_size)    # Tamaño promedio del hogar por UTAM
summary(minor) # Proporción de <18 años por UTAM
summary(elder) # Proporción de >65 años por UTAM
summary(ingresos_bajos) # Proporción de hogares con ingresos < 1'5
summary(higher) # Proporción de personas con educación superior
summary(agre) # Proporción de personas que han recibido agresiones en sus viajes

# Primero, veamos hacia dónde queremos llegar con PctWht:
windows()
par(mfrow = c(1,2))
plot(mean_size, log(Rate), main="Rate by mean households' size")
abline(lm(log(Rate) ~ mean_size), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ mean_size), lwd=2, col="blue")

plot(log(mean_size), log(Rate), main="Rate by mean households' size")
abline(lm(log(Rate) ~ log(mean_size)), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ log(mean_size)), lwd=2, col="blue")

reg1 <- lm(log(Rate) ~ mean_size)
reg2 <- lm(log(Rate) ~ log(mean_size))
summary(reg1)
summary(reg2)

# Se ve bien, no creo que necesitemos transformar esto

# Ahora con laa proporción de <18 años por UTAM
windows()
par(mfrow = c(1,2))
plot(minor, log(Rate), main="Rate by Proportion of under-18 people")
abline(lm(log(Rate) ~ minor), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ minor), lwd=2, col="blue")

plot(log(minor), log(Rate), main="Rate by Proportion of under-18 people")
abline(lm(log(Rate) ~ log(minor)), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ log(minor)), lwd=2, col="blue")

reg1 <- lm(log(Rate) ~ minor)
reg2 <- lm(log(Rate) ~ log(minor))
summary(reg1)
summary(reg2)

# Es preferible la transformación log() para 
# representar la relación lineal

# Ahora elder
windows()
par(mfrow = c(1,2))
plot(elder, log(Rate), main="Rate by Proportion of elder people")
abline(lm(log(Rate) ~ elder), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ elder), lwd=2, col="blue")

plot(log(elder), log(Rate), main="Rate by Proportion of elder people")
abline(lm(log(Rate) ~ log(elder)), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ log(elder)), lwd=2, col="blue")

reg1 <- lm(log(Rate) ~ elder)
reg2 <- lm(log(Rate) ~ log(elder))
summary(reg1)
summary(reg2)
# La variable se mantiene sin transformación

# Ahora higher education
windows()
par(mfrow = c(1,2))
plot(higher, log(Rate), main="Rate by Proportion of higher education graduates")
abline(lm(log(Rate) ~ higher), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ higher), lwd=2, col="blue")

plot(log(higher), log(Rate), main="Rate by Proportion of higher education graduates")
abline(lm(log(Rate) ~ log(higher)), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ log(higher)), lwd=2, col="blue")

reg1 <- lm(log(Rate) ~ higher)
reg2 <- lm(log(Rate) ~ log(higher))
summary(reg1)
summary(reg2)
# La variable se mantiene sin transformación

# Ahora personas que han recibido agresiones en sus viajes
windows()
par(mfrow = c(1,2))
plot(agre, log(Rate), main="Rate by Proportion of Agr")
abline(lm(log(Rate) ~ agre), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ agre), lwd=2, col="blue")

plot(log(agre), log(Rate), main="Rate by Proportion of Agr")
abline(lm(log(Rate) ~ log(agre)), lty=2, lwd=2, col="red")
lines(lowess(log(Rate) ~ log(agre)), lwd=2, col="blue")

reg1 <- lm(log(Rate) ~ agre)
reg2 <- lm(log(Rate) ~ log(agre))
summary(reg1)
summary(reg2)



# Una revisión rápida del R2 y el Resid SE, la variable sin transformar parece
# para rendir mejor


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
### FINALIZAR LA DEMOSTRACIÓN PARA EDA/ESDA BIVARIADA                                
###                                                                  ###
########################################################################
########################################################################