
###############################################################
###############################################################
#### Sesión 2: Introducción a la Modelación de             ####
####                Datos Espaciales usando R              ####
####                                                       ####
#### Elaborado por:                                        ####
#### Mauricio Quiñones Domínguez, Ph.D. Política Pública   ####
#### Departamento de Economía y Finanzas                   ####
#### Pontificia Universidad Javeriana - Cali               ####
####                                                       ####
#### Elaboración conjunta con:                             ####
#### Laboratorio de Economía Aplicada                      ####
####                                                       ####
#### Fecha: 30 de abril de 2025                            ####
###############################################################
###############################################################

##------------------------------------------##
## 1. Cargar librerías y definir directorio ##
##------------------------------------------##

# Si es necesario, instalar las librerías
# install.packages("tidyverse")...

# Librerías principales
library(tidyverse)     # ggplot2, dplyr, y otras herramientas
library(sf)            # Manejo de datos vectoriales
library(spdep)         # Análisis de patrones espaciales
library(spatialreg)    # Estimación de modelos e indicadores

# Otras librerías
library(RColorBrewer)  # Paletas de colores 
library(classInt)      # Definir intervalos de clases  
library(kableExtra)    # Mejorar la presentación de tablas en general
library(texreg)        # Presentación de resultados de modelos

# Consulta: ayuda
??sf

# Ayuda sobre paquetes y funciones (?, help)
?sf          # Documentación del paquete sf
help("sf")   # Equivalente

# Definir el directorio de trabajo
# IMPORTANTE: cambiar por su ruta específica
# setwd("path")

setwd("C:\\Users\\Portatil\\Desktop\\Spatial-Econometrics-using-R\\")

##-----------------------------------------------------##
## 1. Cargar y explorar datos NBI por municipios       ##
##-----------------------------------------------------##

# Cargar la base de datos que contiene los indicadores por municipios:
mun.data <- readxl::read_excel("CNPV_2018/Dataset_MUN_CNPV2018.xlsx")

# Definición de columnas numéricas de interés
numeric_cols <- mun.data[, sapply(mun.data, is.numeric)]
numeric_cols <- numeric_cols %>% dplyr::select(-c("cod_dpto", "cod"))

# Calcular estadísticas descriptivas para cada columna numérica
stats <- data.frame(
  Variable = names(numeric_cols),
  Min = sapply(numeric_cols, min, na.rm = TRUE),
  Max = sapply(numeric_cols, max, na.rm = TRUE),
  Mean = sapply(numeric_cols, mean, na.rm = TRUE),
  SD = sapply(numeric_cols, sd, na.rm = TRUE),
  Median = sapply(numeric_cols, median, na.rm = TRUE),
  Q1 =  sapply(numeric_cols, quantile, na.rm = TRUE, probs = 0.25),
  Q3 = sapply(numeric_cols, quantile, na.rm = TRUE, probs = 0.75),
  stringsAsFactors = FALSE
)

# Se recodifican las variables para mejorar la comprensión de las descriptivas
stats$Variable = factor(stats$Variable,
                        levels = c("comp_dep_eco", "comp_hacin", "comp_inasist", "comp_servi", "comp_vivi", "ingresos_corrientes", "ingresos_totales", "corr_tot", "n" ,"ocup_rate" ,"tasa_miseria","tasa_nbi", "area_km"),
                        labels = c("Dependencia economica",
                                   "Hacinamiento",
                                   "Inasistencia escolar",
                                   "Servicios inadecuados",
                                   "Viviendas inadecuadas",
                                   "Ingresos corrientes",
                                   "Ingresos totales",
                                   "Razon corr-tot",
                                   "N", 
                                   "Ocupacion",
                                   "Miseria",
                                   "NBI", "Area (km2)"))


# Presenta las estadísticas en una tabla
kable(stats, caption = "Resumen descriptivo", digits = 2, row.names = F)


##----------------------------------------------##
## 2. Cargar datos vectoriales de municipios    ##
##----------------------------------------------##

# Cargamos el shapefile de municipios
mun_shape <- st_read(dsn = "SpatialData\\DANE_geodata\\",
                     layer = "MGN_ANM_MPIOS") %>% mutate(
                       cod = as.numeric(
                         paste0(DPTO_CCDGO, MPIO_CCDGO))
                     )

# Unir los datos vectoriales (mun_shape) con los indicadores (mun.data)
mun_merged <-  mun_shape[c("cod",
                           "geometry")] %>%
  left_join(mun.data,by = c("cod" = "cod"))


# Mapa sobre la tasa NBI
ggplot(data = mun_merged) +
  geom_sf(aes(fill = tasa_nbi), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = c("#1a9850", "#fee08b", "#d73027"), 
                       values = c(0, 0.5, 1), 
                       name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribucion espacial del NBI por Municipios (% en NBI)",
       caption = "Fuente: DANE. CNPV-2018.") 

# Otra visualización: intervalos de clase
mun_merged$tasa_nbi_cat <- cut(mun_merged$tasa_nbi, 
                               breaks = c(0, 10, 20, 30, 40, 50, Inf), 
                               labels = c("0-10%", "10-20%",
                                          "20-30%", "30-40%", 
                                          "40-50%", "50%+"),
                               include.lowest = TRUE)

# Definir colores para cada categoría
colors_intervals <- c("0-10%" = "#1a9850",  # Verde
                      "10-20%" = "#66bd63", # Verde claro
                      "20-30%" = "#fee08b", # Amarillo-naranja
                      "30-40%" = "#fdae61", # Naranja
                      "40-50%" = "#f46d43", # Rojo claro
                      "50%+" = "#d73027")   # Rojo fuerte

ggplot(data = mun_merged) +
  geom_sf(aes(fill = tasa_nbi_cat), color = "black", size = 0.2) + 
  scale_fill_manual(values = colors_intervals, name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribucion espacial del NBI por Municipios (en % de NBI)",
       caption = "Fuente: DANE. CNPV-2018.") 

##----------------------------------------------##
## 3. Cargar datos vectoriales de municipios    ##
##----------------------------------------------##

# Conexiones: ¿qué municipios son vecinos de quién?
nb_mun <- poly2nb(mun_merged)
nb_mun

# Para la modelación, eliminamos los municipios sin enlaces
mun_merged <- mun_merged[-c(629, 630),]

# Revisión de las nuevas conexiones
nb_mun <- poly2nb(mun_merged)
nb_mun

# Matriz W
nb2_mun<- nb2listw(nb_mun)
summary(nb2_mun)

# Información de la matriz W
names(attributes(nb2_mun))  # Nombres de los atributos
card(nb2_mun$neighbours)    # Número de vecinos para cada observación
range(card(nb2_mun$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_mun$neighbours))) # Relación inversa
summary(nb2_mun, zero.policy=T)   # Resumen

# - Rechazo o no?
moran.test(mun_merged$tasa_nbi, 
           nb2_mun, alternative="two.sided", zero.policy=T)

# Diagrama de dispersión de Moran, siguiendo el código de la sesión 1,
# seleccionando municipios de interés
x <- mun_merged$tasa_nbi
y <- lag.listw(nb2_mun, x, zero.policy = TRUE)
labels <- as.character(mun_merged$nom_mun)

municipios_seleccionados <- c(
  labels[order(x)][1:50],                
  labels[order(x, decreasing = TRUE)][1:25] 
)

etiquetas <- ifelse(labels %in% municipios_seleccionados, labels, "")

moran.plot(x, nb2_mun,
           zero.policy = TRUE,
           labels = etiquetas,
           xlab = "mun_merged$tasa_nbi",
           ylab = "spatially lagged mun_merged$tasa_nbi",
           type = "p", col = "#AE017E", cex = 1, pch = 1)

##------------------------------------------##
## 4. Modelo de regresión MCO               ##
##------------------------------------------##

# Para el análisis, se eliminan los valores faltantes
mun_merged2 = mun_merged %>% filter(!is.na(corr_tot) &
                                      !is.na(n) & !is.na(area_km) &
                                      !is.na(ocup_rate))

# Eliminar otras regiones sin enlaces
mun_merged2 <- mun_merged2[-c(202, 205),]

# Trabajamos con el logaritmo de la tasa NBI
mun_merged2$log_nbi <- log(mun_merged2$tasa_nbi)

# Regresión lineal múltiple (estimación MCO)
reg_mco <- lm(log_nbi ~  n + corr_tot + log(area_km) + ocup_rate, data=mun_merged2)
summary(reg_mco)

# Residuales:
reg_mco_res <- reg_mco$residuals

##----------------------------------------------##
## 5. Regresión MCO: diagnóstico de residuales  ##
##----------------------------------------------##

# Examinamos la media y la desviación estándar
me1 <- mean(residuals(reg_mco))
me1    
sd1 <- sd(residuals(reg_mco))
sd1    
summary(residuals(reg_mco))

# Histograma de los residuos vs. curva normal
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
car::qqPlot(residuals(reg_mco), distribution="norm", envelope=FALSE,
            pch=1, cex=1, col="black")
par(new=FALSE)

# Nuestra intuición se verifica con pruebas de normalidad univariada
# No hay normalidad en los residuos
library(nortest)
library(tseries) 

ad.test(residuals(reg_mco))       
lillie.test(residuals(reg_mco))   
#pearson.test(residuals(reg_mco))  
#cvm.test(residuals(reg_mco))      
#sf.test(residuals(reg_mco))       
shapiro.test(residuals(reg_mco)) 

# Evaluamos homocedasticidad
plot(fitted(reg_mco), residuals(reg_mco), xlab="Ajustados y", ylab= "Residuales",
     main="Plot de Residuales vs. ajustados y")
abline(h=0)

# Prueba formal
library(lmtest)
bptest(reg_mco) # Rechaza Ho (Heterocedasticidad)

##------------------------------------------##
## 6. Independencia de los residuos         ##
##------------------------------------------##

# Definimos nuevamente la matriz W
nb_mun <- poly2nb(mun_merged2)
nb2_mun<- nb2listw(nb_mun)

# Resumen: 
summary(nb2_mun, zero.policy=T)   

# Realizamos la prueba de Moran sobre los residuos
moran.test(reg_mco$residuals, 
           nb2_mun, alternative="two.sided", zero.policy=T)

# Representación espacial de los residuos mediante un mapa
colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(reg_mco$residuals, n=5, style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

par(mfrow = c(1, 1))
plot(st_geometry(mun_merged), 
     col = "grey40", 
     border = "grey40",  
     lwd = 0.3)   
plot(mun_merged2["nom_mun"], 
     col = colcode, 
     border = "grey40",  
     lwd = 0.3, add = TRUE)           
title("Mapa de Residuales de la Regresión")
legend("topleft",
       legend = names(attr(colcode, "table")),
       fill = attr(colcode, "palette"),
       title = "Residuales de la Regresión")

# Finalmente, se analiza el diagrama de dispersión de Moran
moran.plot(reg_mco$residuals, nb2_mun, zero.policy=T, labels=as.character(mun_merged2$nom_mun),
           xlab=NULL, ylab=NULL, type="p", col="#AE017E",
           cex=0.8, pch=1)

##--------------------------------------------##
## 7.   Modelo espacial autorregresivo (SAR)  ##
##--------------------------------------------##

# Estimación:
model.lag.eig <- spatialreg::lagsarlm(log_nbi ~  n + corr_tot + log(area_km) + ocup_rate,
                                      data=mun_merged2, nb2_mun, method="eigen", quiet=FALSE)
summary(model.lag.eig)

# Calculamos los impactos directos, indirectos y totales
model.lag.eig.imp <- impacts(model.lag.eig, listw = nb2_mun, R = 999)
summary(model.lag.eig.imp, zstats = TRUE, short = TRUE)

##------------------------------------------##
## 8.  Modelo de error espacial (SEM)       ##
##------------------------------------------##

# Estimación
model.err.eig <- spatialreg::errorsarlm(log_nbi ~  n + corr_tot + log(area_km) + ocup_rate,
                                        data=mun_merged2, nb2_mun, method="eigen", quiet=FALSE)
summary(model.err.eig)

# Exportamos la comparación de los modelos a un archivo HTML
# Mejor la comparación
# IMPORTANTE: Se guardan en su directorio de trabajo

htmlreg(list(reg_mco, model.lag.eig, model.err.eig),
        file = "OLS_SAR_SEM.html",
        custom.model.names = c("OLS", "SAR", "SEM"),
        single.row = TRUE) # El resultado se muestra en otro archivo


##------------------------------------------------------##
## 9.  Identificación del tipo de modelación espacial   ##
##------------------------------------------------------##

# Test de multiplicadores de Lagrange (i.e. GeoDa-style Lagrange multiplier tests)
four_test <- lm.LMtests(reg_mco, nb2_mun, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
summary(four_test)

# Comparamos la autocorrelación espacial de los residuos entre los modelos
moran.test(reg_mco$residuals, nb2_mun, alternative="two.sided")
moran.test(model.lag.eig$residuals, nb2_mun, alternative="two.sided")
moran.test(model.err.eig$residuals, nb2_mun, alternative="two.sided")

# Veamos la comparación según los criterios de información
AIC(reg_mco)       # 1358.818
AIC(model.lag.eig)    # 779.9292
AIC(model.err.eig)    # 877.2476

BIC(reg_mco)       # 1388.82
BIC(model.lag.eig)    # 814.9316
BIC(model.err.eig)    # 912.25


########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## 
########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## 
########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## 
########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## 
########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## 
########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## ANEXO ########## 

##--------------------------------##
## 10.  Otros modelos: SLX y SAC  ##
##--------------------------------##

# Estimación del modelo de rezago Espacial de las X's (SLX)
model.slx.eig <- spatialreg::lmSLX(tasa_nbi ~ n + corr_tot + log(area_km) + ocup_rate,
                                   data=mun_merged2, nb2_mun)
summary(model.slx.eig)

# Estimación del modelo SAC 
model.lag.error <- spatialreg::sacsarlm(tasa_nbi ~  n + corr_tot + log(area_km) + ocup_rate,
                                        data=mun_merged2, nb2_mun, method = "eigen", quiet=TRUE)
summary(model.lag.error)

# Calculamos impactos del modelo SAC
model.lag.error.imp <- impacts(model.lag.error, listw = nb2_mun, R = 999)
summary(model.lag.error.imp, zstats = TRUE, short = TRUE)

# Test de multiplicadores de Lagrange (i.e. GeoDa-style Lagrange multiplier tests)
lm.LMtests(reg_mco, nb2_mun, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

# Veamos la comparación de la autocorrelación en los residuos
moran.test(reg_mco$residuals, nb2_mun, alternative="two.sided")
moran.test(model.lag.eig$residuals, nb2_mun, alternative="two.sided")
moran.test(model.err.eig$residuals, nb2_mun, alternative="two.sided")
moran.test(model.slx.eig$residuals, nb2_mun, alternative="two.sided")
moran.test(model.lag.error$residuals, nb2_mun, alternative="two.sided")

# Veamos la comparación según los criterios de información
AIC(reg_mco)       # 1358.818
AIC(model.lag.eig)    # 779.9292
AIC(model.err.eig)    # 877.2476
AIC(model.slx.eig)    # 8429.426
AIC(model.lag.error)    # 7957.535

BIC(reg_mco)       # 1388.82
BIC(model.lag.eig)    # 814.9316
BIC(model.err.eig)    # 912.25
BIC(model.slx.eig)    # 8479.429
BIC(model.lag.error)    # 8479.429

########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## FIN SESIÓN 2 ########## 
