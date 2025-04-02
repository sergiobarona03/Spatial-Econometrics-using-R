
# Caso de estudio 3:
# Modelación espacial


# Cargar librerías
library(tidyverse)
library(sf)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\")

##---------------------------------------##
## Cargar datos a nivel de departamentos ##
##---------------------------------------##

# Cargar datos NBI
nbi <- readxl::read_excel("CNPV_2018\\NBI_MUN_CNPV2018.xlsx")

# Cargar datos de población
pob <- readxl::read_excel("CNPV_2018\\Pob_MUN_CNPV2018.xlsx")

# Cargar datos de ingresos
ing <- readxl::read_excel("CNPV_2018\\INGRESOS_MUN_DNP.xlsx")

# Unir los datos
mun_data <- nbi[c("cod_dpto", "nom_dpto", "nom_mun",
                  "cod", "tasa_nbi", "tasa_miseria",
                  "comp_vivi", "comp_servi",
                  "comp_hacin", "comp_inasist",
                  "comp_dep_eco")] %>% dplyr::left_join(pob[c("cod", "n")], 
                                                        by = c("cod" = "cod")) %>%
  dplyr::left_join(ing[c("cod", "ingresos_totales",
                         "ingresos_corrientes", "corr_tot")], by = c("cod" = "cod"))

# Tasas
mun_data$tasa_nbi <- as.numeric(gsub(",", ".", mun_data$tasa_nbi))
mun_data$tasa_miseria <- as.numeric(gsub(",", ".", mun_data$tasa_miseria))

##---------------------------------------##
## Visualizar NBI en mapas               ##
##---------------------------------------##
# Leer dpto_shape
mun_shape <- st_read(dsn = "SpatialData\\DANE_geodata\\",
                     layer = "MGN_ANM_MPIOS") %>% mutate(
                       cod = as.numeric(
                         paste0(DPTO_CCDGO, MPIO_CCDGO))
                     )

# Unir las bases de datos
mun_merged <-  mun_shape[c("cod",
                           "geometry")] %>%
  left_join(mun_data,by = c("cod" = "cod"))

# Por simplicidad, se omite San Andrés
mun_merged <- mun_merged %>% filter(nom_dpto != "ARCHIPIÉLAGO DE SAN ANDRÉS")

# Se incorpora la variable AREA (km2) como variable de control
area <- readxl::read_excel("CNPV_2018\\AREA_MUN_COL.xlsx")
area$cod <- paste0(area$cod_dpto, area$cod_mun)

mun_merged <- merge(mun_merged, area[c("cod", "area_km")],
                    by = "cod")

# Eliminar región sin vecinos
mun_merged = mun_merged[-1114,]

# Se eliminan NAs
mun_merged <- mun_merged %>% na.omit()

##------------------------------##
## I de Moran Global            ##
##------------------------------##
library(spdep)

# Eliminar polígono sin vecino


# Revisión de las conexiones o links del shapefile 
nb_mun <- poly2nb(mun_merged)
nb_mun

# Se crea la matriz W
nb2_mun<- nb2listw(nb_mun)
summary(nb2_mun)

# Examinar la variable dependiente
summary(mun_merged$tasa_nbi)

# Véase la información de los pesos de la matriz
names(attributes(nb2_mun))  # Nombres de los atributos
card(nb2_mun$neighbours)    # Número de vecinos para cada observación
range(card(nb2_mun$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_mun$neighbours))) # Rango (pesos)
summary(nb2_mun, zero.policy=T)   # Resumen

# - Rechazo o no?
moran.test(mun_merged$tasa_nbi, 
           nb2_mun, alternative="two.sided", zero.policy=T)

##-------------------------------------##
## Modelación de datos espaciales      ##
##-------------------------------------##



# Logaritmo natural de la tasa
mun_merged$tasa_nbi <- log(mun_merged$tasa_nbi)

# Se propone una regresión lineal múltiple con las siguientes variables
reg_mco <- lm(tasa_nbi ~  n + corr_tot + log(area_km), data=mun_merged)
summary(reg_mco)

reg_mco_res <- reg_mco$residuals

#---------------------------------#
# Diagnóstico de los residuales   #
#---------------------------------#

# A continuación examinamos los residuos
me1 <- mean(residuals(reg_mco))
me1    
sd1 <- sd(residuals(reg_mco))
sd1    
summary(residuals(reg_mco))  # Simetría razonable

hist(residuals(reg_mco),  breaks=seq(-2, 4.2, 0.1), col=8, probability=T,
     ylab='Density', main='Histogram of Residuals(reg_mco)',
     xlab='Residuals(reg_mco)')
box()
curve(dnorm(x, mean=me1, sd=sd1), from=-2, to=4.2, add=T,
      col='red', lwd=2)

# Hay señales de ouliers. Veamos el QQ-plot
library(car)
car::qqPlot(residuals(reg_mco), distribution="norm",
       xlab='', main='Quantile Comparison Plot reg_mco residuals',
       envelope=.95, las=0, pch=NA, lwd=2, col="red",
       line="quartiles")

par(new=TRUE)
qqPlot(residuals(reg_mco), distribution="norm", envelope=FALSE,
       pch=1, cex=1, col="black")
par(new=FALSE)

# Nuestra intuición se verifica con numerosas pruebas de normalidad univariada
library(nortest)
library(tseries) 

ad.test(residuals(reg_mco))       
lillie.test(residuals(reg_mco))   
pearson.test(residuals(reg_mco))  
cvm.test(residuals(reg_mco))      
sf.test(residuals(reg_mco))       
shapiro.test(residuals(reg_mco)) 
# No hay normalidad en los residuos

# A partir del siguiente gráfico, estudiamos el supuesto de homocedasticidad
plot(fitted(reg_mco), residuals(reg_mco), xlab="Fitted y", ylab= "Residuals",
     main="Plot of Residuals against Fitted y")
abline(h=0)

# Usemos una prueba formal:
library(lmtest)
bptest(reg_mco) # No se rechaza Ho (Homocedasticidad)

#----------------------------------------------#
# Se verifica la independencia de los residuos #
#----------------------------------------------#

# Test de Durbin-Watson para autocorrelación de los errores
dwtest(reg_mco)

# Véase la información de los pesos de la matriz
names(attributes(nb2_mun))  # Nombres de los atributos
card(nb2_mun$neighbours)    # Número de vecinos para cada observación
range(card(nb2_mun$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_mun$neighbours))) # Rango (pesos)
summary(nb2_mun, zero.policy=T)   # Resumen

moran.test(reg_mco$residuals, nb2_mun, alternative="two.sided", zero.policy=T)

# Seleccionar la paleta
colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(reg_mco$residuals, n=5, style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

## Figura
par(mfrow=c(1,1))
plot(bg_data[c("UTAM", "geometry")], col=colcode)
title('Map of Regression Residuals')
legend('topleft', legend=c(names(attr(colcode, 'table'))), fill=c(attr(colcode, 'palette')), 
       title='Regression Residuals')

moran.plot(reg_mco$residuals, bgQ1.gal, zero.policy=T, labels=as.character(bg.df$UTAM),
           xlab=NULL, ylab=NULL, type="p", col="#AE017E",
           cex=0.8, pch=1)
