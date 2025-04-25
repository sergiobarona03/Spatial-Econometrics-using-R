# Cargar librerías

library(tidyverse)
library(RColorBrewer)
library(classInt)
library(sf)
library(spdep)
library(spatialreg)
library(kableExtra)
library(texreg)


# Cargar marco de datos
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Spatial-Econometrics-using-R\\")
dpto.data <- readxl::read_excel("CNPV_2018//Dataset_DPTO_CNPV2018.xlsx")

library(knitr)
library(kableExtra)

# Examinar datos
head(dpto.data)

numeric_cols <- dpto.data[, sapply(dpto.data, is.numeric)]
numeric_cols <- numeric_cols %>% dplyr::select(-cod)

# Calcula estadísticas para cada columna numérica
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

# Recodificar variable
stats$Variable = factor(stats$Variable,
                        levels = c("comp_dep_eco", "comp_hacin", "comp_inasist", "comp_servi", "comp_vivi", "ingresos_corrientes", "ingresos_totales", "corr_tot", "n" ,"ocupacion_dpto" ,"tasa_miseria","tasa_nbi"),
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
                                   "NBI"))


# Presenta las estadísticas en una tabla
kable(stats, caption = "Resumen descriptivo (n = 33)", digits = 2, row.names = F)

# Datos vectoriales
dpto_shape <- st_read(dsn = "SpatialData/dptos_col/",
                      layer = "clean2_dpto_shape")

# Unir las bases de datos
dpto_merged <-  dpto_shape[c("ID",
                             "DPTO",
                             "AREA",
                             "geometry")] %>%
  left_join(dpto.data[c("dpto_clean", "dpto", "tasa_nbi",
                        "tasa_miseria", "comp_vivi",
                        "comp_servi", "comp_hacin",
                        "comp_inasist", "comp_dep_eco",
                        "n", "ingresos_totales",
                        "ingresos_corrientes", "corr_tot")],
            by = c("DPTO" = "dpto_clean"))

# Por simplicidad, se omite San Andrés
dpto_merged <- dpto_merged %>% filter(DPTO != "ARCHIPIELAGO DE SAN ANDRES")


# Variable de interés: tasa NBI
summary(dpto_merged$tasa_nbi)
min(dpto_merged$tasa_nbi)      
max(dpto_merged$tasa_nbi)      

# Se presenta el mapa sobre NBI
colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(dpto_merged$tasa_nbi, n=5, style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

# I de Moran
# Figura 1
ggplot(data = dpto_merged) +
  geom_sf(aes(fill = tasa_nbi), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = c("#1a9850", "#fee08b", "#d73027"), 
                       values = c(0, 0.5, 1), 
                       name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribucion espacial del NBI por Departamento (% en NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",  
        legend.background = element_rect(fill = "white", color = "black"))

# Datos vectoriales
dpto_shape <- st_read(dsn = "SpatialData/dptos_col/",
                      layer = "clean2_dpto_shape")

# Unir las bases de datos
dpto_merged <-  dpto_shape %>%
  dplyr::left_join(dpto.data[c("dpto_clean", "dpto", "tasa_nbi",
                               "tasa_miseria", "comp_vivi",
                               "comp_servi", "comp_hacin",
                               "comp_inasist", "comp_dep_eco",
                               "n", "ingresos_totales",
                               "ingresos_corrientes", "corr_tot")],
                   by = c("DPTO" = "dpto_clean"))

# Por simplicidad, se omite San Andrés
dpto_merged <- dpto_merged %>% filter(DPTO != "ARCHIPIELAGO DE SAN ANDRES")


# Variable de interés: tasa NBI
summary(dpto_merged$tasa_nbi)
min(dpto_merged$tasa_nbi)      
max(dpto_merged$tasa_nbi)      

# Se presenta el mapa sobre  bg.df$Rate. Seleccionar la paleta
colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(dpto_merged$tasa_nbi, n=5, style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

# Figura 2
# Visualizar la variable "tasa_nbi" a partir de intervalos
dpto_merged$tasa_nbi_cat <- cut(dpto_merged$tasa_nbi, 
                                breaks = c(0, 10, 20, 30, 40, 50, Inf), 
                                labels = c("0-10%", "10-20%",
                                           "20-30%", "30-40%", 
                                           "40-50%", "50%+"),
                                include.lowest = TRUE)

colors_intervals <- c("0-10%" = "#1a9850",  # Verde
                      "10-20%" = "#66bd63", # Verde claro
                      "20-30%" = "#fee08b", # Amarillo-naranja
                      "30-40%" = "#fdae61", # Naranja
                      "40-50%" = "#f46d43", # Rojo claro
                      "50%+" = "#d73027")   # Rojo fuerte

map_nbi <- ggplot(data = dpto_merged) +
  geom_sf(aes(fill = tasa_nbi_cat), color = "black", size = 0.2) + 
  scale_fill_manual(values = colors_intervals, name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribucion espacial del NBI por Departamento (en % de NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",  
        legend.background = element_rect(fill = "white", color = "black"))
map_nbi

# Revisión de las conexiones o links del shapefile 
nb_dpto <- poly2nb(dpto_merged)
nb_dpto

# Se crea la matriz W
nb2_dpto <- nb2listw(nb_dpto)
summary(nb2_dpto)

# Véase la información de los pesos de la matriz
names(attributes(nb2_dpto))  # Nombres de los atributos
card(nb2_dpto$neighbours)    # Número de vecinos para cada observación
range(card(nb2_dpto$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_dpto$neighbours))) # Rango (pesos)
summary(nb2_dpto, zero.policy=T)   # Resumen

# I de Moran
moran.test(dpto_merged$tasa_nbi, 
           nb2_dpto, alternative="two.sided", zero.policy=T)

# Moran plot
# Se presenta el mapa sobre  bg.df$Rate. Seleccionar la paleta
library(RColorBrewer)
library(classInt)

colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(dpto_merged$tasa_nbi, n=5, 
                              style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

## Figura: Distribución espacial del NBI (%)
par(mfrow=c(1,1))
plot(dpto_merged[c("tasa_nbi", "geometry")], col=colcode)
title('Distribucion tasa NBI (%)')
legend('topleft', legend=c(names(attr(colcode, 'table'))), fill=c(attr(colcode, 'palette')), 
       title='NBI (%)')

# Moran plot
dptos_a_etiquetar <- c(
  "ANTIOQUIA", "BOGOTA, D.C.", "BOYACA",
  "ATLANTICO", "BOLIVAR", "MAGDALENA",
  "NORTE DE SANTANDER", "VALLE DEL CAUCA"
)

moran.plot(dpto_merged$tasa_nbi, nb2_dpto,
           zero.policy = TRUE,
           labels = as.character(dpto_merged$DPTO),  
           xlab = "dpto_merged$tasa_nbi",
           ylab = "spatially lagged dpto_merged$tasa_nbi",
           type = "p", col = "#AE017E", cex = 0.8, pch = 1)

x <- dpto_merged$tasa_nbi
y <- lag.listw(nb2_dpto, x, zero.policy = TRUE)
labels <- as.character(dpto_merged$DPTO)

idx <- which(labels %in% dptos_a_etiquetar)
text(x[idx], y[idx], labels = labels[idx], cex = 0.7, pos = 4, col = "black")

# Cargar datos
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Spatial-Econometrics-using-R\\")
mun.data <- readxl::read_excel("CNPV_2018/Dataset_MUN_CNPV2018.xlsx")

# Selección de columnas numéricas
numeric_cols <- mun.data[, sapply(mun.data, is.numeric)]
numeric_cols <- numeric_cols %>% dplyr::select(-c("cod_dpto", "cod"))

# Calcula estadísticas para cada columna numérica
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

# Recodificar variable
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
kable(stats, caption = "Resumen descriptivo (n = 33)", digits = 2, row.names = F)

# Cargar mapa
# Leer dpto_shape
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Spatial-Econometrics-using-R\\")

# Unir las bases de datos
mun_shape <- st_read(dsn = "SpatialData\\DANE_geodata\\",
                     layer = "MGN_ANM_MPIOS") %>% mutate(
                       cod = as.numeric(
                         paste0(DPTO_CCDGO, MPIO_CCDGO))
                     )

# Unir las bases de datos
mun_merged <-  mun_shape[c("cod",
                           "geometry")] %>%
  left_join(mun.data,by = c("cod" = "cod"))


# Figura 3:
library(viridis)

ggplot(data = mun_merged) +
  geom_sf(aes(fill = tasa_nbi), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = c("#1a9850", "#fee08b", "#d73027"), 
                       values = c(0, 0.5, 1), 
                       name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribucion espacial del NBI por Municipios (% en NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",  
        legend.background = element_rect(fill = "white", color = "black"))

# Figura 4:
mun_merged$tasa_nbi_cat <- cut(mun_merged$tasa_nbi, 
                               breaks = c(0, 10, 20, 30, 40, 50, Inf), 
                               labels = c("0-10%", "10-20%",
                                          "20-30%", "30-40%", 
                                          "40-50%", "50%+"),
                               include.lowest = TRUE)

colors_intervals <- c("0-10%" = "#1a9850",  # Verde
                      "10-20%" = "#66bd63", # Verde claro
                      "20-30%" = "#fee08b", # Amarillo-naranja
                      "30-40%" = "#fdae61", # Naranja
                      "40-50%" = "#f46d43", # Rojo claro
                      "50%+" = "#d73027")   # Rojo fuerte

map_nbi <- ggplot(data = mun_merged) +
  geom_sf(aes(fill = tasa_nbi_cat), color = "black", size = 0.2) + 
  scale_fill_manual(values = colors_intervals, name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribucion espacial del NBI por Municipios (en % de NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",  
        legend.background = element_rect(fill = "white", color = "black"))
map_nbi


# Revisión de las conexiones o links del shapefile 
nb_mun <- poly2nb(mun_merged)
nb_mun

# Eliminar regiones sin enlaces
mun_merged <- mun_merged[-c(629, 630),]

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

# Generar el gráfico base SIN etiquetas
moran.plot(mun_merged$tasa_nbi, nb2_mun,
           zero.policy = TRUE,
           labels = NA,
           xlab = "mun_merged$tasa_nbi",
           ylab = "spatially lagged mun_merged$tasa_nbi",
           type = "p", col = "#AE017E", cex = 1, pch = 1)

# Calcular los ejes para poner etiquetas selectivas
x <- mun_merged$tasa_nbi
y <- lag.listw(nb2_mun, x, zero.policy = TRUE)
labels <- as.character(mun_merged$nom_mun)

# Mostrar etiquetas solo en los extremos (cuartiles)
qx <- quantile(x, probs = c(0.25, 0.75))
qy <- quantile(y, probs = c(0.25, 0.75))

# Municipios fuera del rango intercuartílico en x o y
idx_etiquetar <- which(x < qx[1] | x > qx[2] | y < qy[1] | y > qy[2])

# Añadir etiquetas seleccionadas
text(x[idx_etiquetar], y[idx_etiquetar],
     labels = labels[idx_etiquetar],
     cex = 0.7, pos = 4, col = "black")


# Para el análisis, se eliminan los valores faltantes
mun_merged2 = mun_merged %>% filter(!is.na(corr_tot) &
                                      !is.na(n) & !is.na(area_km) &
                                      !is.na(ocup_rate))

# Eliminar regiones sin enlaces
mun_merged2 <- mun_merged2[-c(202, 205),]

# Logaritmo natural de la tasa
mun_merged2$log_nbi <- log(mun_merged2$tasa_nbi)

# Se propone una regresión lineal múltiple con las siguientes variables
reg_mco <- lm(log_nbi ~  n + corr_tot + log(area_km) + ocup_rate, data=mun_merged2)
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
car::qqPlot(residuals(reg_mco), distribution="norm", envelope=FALSE,
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
bptest(reg_mco) # Rechaza Ho (Heterocedasticidad)


#----------------------------------------------#
# Se verifica la independencia de los residuos #
#----------------------------------------------#

# Se define nuevamente la matriz de pesos
nb_mun <- poly2nb(mun_merged2)
nb_mun

# Se crea la matriz W
nb2_mun<- nb2listw(nb_mun)
summary(nb2_mun)

# Véase la información de los pesos de la matriz
names(attributes(nb2_mun))  # Nombres de los atributos
card(nb2_mun$neighbours)    # Número de vecinos para cada observación
range(card(nb2_mun$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_mun$neighbours))) # Rango (pesos)
summary(nb2_mun, zero.policy=T)   # Resumen

moran.test(reg_mco$residuals, 
           nb2_mun, alternative="two.sided", zero.policy=T)

# Seleccionar la paleta
colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(reg_mco$residuals, n=5, style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

## Figura
par(mfrow = c(1, 1))
plot(mun_merged2["nom_mun"], 
     col = colcode, 
     border = "grey40",  
     lwd = 0.3)           
title("Map of Regression Residuals")
legend("topleft",
       legend = names(attr(colcode, "table")),
       fill = attr(colcode, "palette"),
       title = "Regression Residuals")

moran.plot(reg_mco$residuals, nb2_mun, zero.policy=T, labels=as.character(mun_merged2$nom_mun),
           xlab=NULL, ylab=NULL, type="p", col="#AE017E",
           cex=0.8, pch=1)

# Modelo SAR
model.lag.eig <- spatialreg::lagsarlm(log_nbi ~  n + corr_tot + log(area_km) + ocup_rate,
                                      data=mun_merged2, nb2_mun, method="eigen", quiet=FALSE)
summary(model.lag.eig)

model.lag.eig.imp <- impacts(model.lag.eig, listw = nb2_mun, R = 999)
summary(model.lag.eig.imp, zstats = TRUE, short = TRUE)


# Modelo SEM
model.err.eig <- spatialreg::errorsarlm(log_nbi ~  n + corr_tot + log(area_km) + ocup_rate,
                                        data=mun_merged2, nb2_mun, method="eigen", quiet=FALSE)
summary(model.err.eig)

htmlreg(list(reg_mco, model.lag.eig, model.err.eig),
        file = "OLS_SAR_SEM.html",
        custom.model.names = c("OLS", "SAR", "SEM"),
        single.row = TRUE)

# Test de multiplicadores de Lagrange (i.e. GeoDa-style Lagrange multiplier tests)
lm.LMtests(reg_mco, nb2_mun, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))

# Veamos la comparación de la autocorrelación en los residuos
moran.test(reg_mco$residuals, nb2_mun, alternative="two.sided")
moran.test(model.lag.eig$residuals, nb2_mun, alternative="two.sided")
moran.test(model.err.eig$residuals, nb2_mun, alternative="two.sided")

# REVISAR CONCLUSIÓN: Lag, SARAR, & SDM all pretty good.  Error is OK.  The rest: not so much.

# Veamos la comparación según los criterios de información
AIC(reg_mco)       # 1358.818
AIC(model.lag.eig)    # 779.9292
AIC(model.err.eig)    # 877.2476

BIC(reg_mco)       # 1388.82
BIC(model.lag.eig)    # 814.9316
BIC(model.err.eig)    # 912.25

# Modelo SLX
model.slx.eig <- spatialreg::lmSLX(tasa_nbi ~ n + corr_tot + log(area_km) + ocup_rate,
                                   data=mun_merged2, nb2_mun)
summary(model.slx.eig)


# Modelo SAC
model.lag.error <- spatialreg::sacsarlm(tasa_nbi ~  n + corr_tot + log(area_km) + ocup_rate,
                                        data=mun_merged2, nb2_mun, method = "eigen", quiet=TRUE)
summary(model.lag.error)

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


