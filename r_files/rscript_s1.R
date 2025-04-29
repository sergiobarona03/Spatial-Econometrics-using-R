
###############################################################
###############################################################
#### Sesión 1: Introducción a la Modelación de             ####
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
#### Fecha: 29 de abril de 2025                            ####
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

##--------------------------------------------------------##
## 2. Exploración de datos: distribución espacial del NBI ##
##--------------------------------------------------------##

# Cargar la base de datos que contiene los indicadores por departamento:
dpto.data <- readxl::read_excel("CNPV_2018/Dataset_DPTO_CNPV2018.xlsx")

# Examinar datos:
head(dpto.data)

# Definir variables numéricas de interés
numeric_cols <- dpto.data[, sapply(dpto.data, is.numeric)]
numeric_cols <- numeric_cols %>% dplyr::select(-cod)    # Excluir el código

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


# Tabla de estadísticas descriptivas:
kable(stats, caption = "Resumen descriptivo (n = 33)", digits = 2, row.names = F)


##----------------------------------##
## 3. Distribución espacial del NBI ##
##----------------------------------##

# Cargamos el shapefile de departamentos (datos vectoriales)
dpto_shape <- st_read(dsn = "SpatialData/dptos_col/",
                      layer = "clean2_dpto_shape")

# Unimos los datos vectoriales (dpto_shape) con los indicadores (dpto.data)
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


# Examinamos las descriptivas de la variable de interés (tasa NBI)
summary(dpto_merged$tasa_nbi)
min(dpto_merged$tasa_nbi)      
max(dpto_merged$tasa_nbi)      

# Elaboramos el mapa sobre la tasa NBI
ggplot(data = dpto_merged) +
  # Añadir el mapa basado en la geometría, controlando por "tasa_nbi"
  geom_sf(aes(fill = tasa_nbi), 
          color = "black",  # Color: bordes del polígono
          size = 0.2) +     # Grosor de los bordes
  
  # Definir la escala de colores para el relleno (verde, amarillo, rojo)
  scale_fill_gradientn(colors = c("#1a9850", "#fee08b", "#d73027"), 
                       values = c(0, 0.5, 1), 
                       name = "Tasa NBI (%)") + 
  labs(title = "Distribucion espacial del NBI por Departamento (% en NBI)",
       caption = "Fuente: DANE. CNPV-2018.")

# Otra visualización se puede elaborar a partir de intervalos:
dpto_merged$tasa_nbi_cat <- cut(dpto_merged$tasa_nbi, 
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

ggplot(data = dpto_merged) +
  geom_sf(aes(fill = tasa_nbi_cat), color = "black", size = 0.2) + 
  scale_fill_manual(values = colors_intervals, name = "Tasa NBI (%)") + 
  labs(title = "Distribucion espacial del NBI por Departamento (en % de NBI)",
       caption = "Fuente: DANE. CNPV-2018.") 

##-------------------------------##
## 4. I de Moran Global          ##
##-------------------------------##

# Definir las conexiones (qué departamentos son vecinos de quién) 
nb_dpto <- poly2nb(dpto_merged)
nb_dpto

# Creamos W (matriz de pesos espaciales)
nb2_dpto <- nb2listw(nb_dpto)
summary(nb2_dpto)

# Revisamos la información de W
names(attributes(nb2_dpto))  # Nombres de los atributos
card(nb2_dpto$neighbours)    # Número de vecinos por departamento
range(card(nb2_dpto$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_dpto$neighbours))) # Rlación inversa
summary(nb2_dpto, zero.policy=T)   # Resumen

# Calculamos el I de Moran
moran.test(dpto_merged$tasa_nbi, 
           nb2_dpto, alternative="two.sided", zero.policy=T)


##---------------------------------------##
## 5. Diagrama de dispersión de Moran    ##
##---------------------------------------##

# Para empezar, creamos un mapa simple sobre la tasa NBI
# Definimos una paleta de colores
colors <- brewer.pal(5, "YlOrBr")

# Clasificamos las tasas de NBI usando quintiles
color.cat.reg<-classIntervals(dpto_merged$tasa_nbi, n=5, 
                              style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

# Distribución espacial del NBI (%) por quintiles
plot(dpto_merged[c("tasa_nbi", "geometry")], col=colcode)
title('Distribucion tasa NBI (%)')
legend('topleft', legend=c(names(attr(colcode, 'table'))),
       fill=c(attr(colcode, 'palette')), 
       title='NBI (%)')


# Diagrama de dispersión de Moran:
# Cada punto representa un departamento
moran.plot(dpto_merged$tasa_nbi, 
           nb2_dpto,
           zero.policy = TRUE,
           labels = as.character(dpto_merged$DPTO),  
           xlab = "dpto_merged$tasa_nbi",
           ylab = "spatially lagged dpto_merged$tasa_nbi",
           type = "p", 
           col = "#AE017E",
           cex = 0.8, 
           pch = 1)

# Nota: podemos añadir etiquetas para examinar algunos 
#       departamentos de interés

# Departamentos para añadir etiquetas
dptos_a_etiquetar <- c(
  "ANTIOQUIA", "BOGOTA, D.C.", "BOYACA",
  "ATLANTICO", "BOLIVAR", "MAGDALENA",
  "NORTE DE SANTANDER", "VALLE DEL CAUCA"
)

# Coordenada X: valores originales del NBI
x <- dpto_merged$tasa_nbi
# Coordenada Y: lag espacial
y <- lag.listw(nb2_dpto, x, zero.policy = TRUE)
# Obtenemos las etiquetas
labels <- as.character(dpto_merged$DPTO)

# Identificamos los departamentos que queremos etiquetar
idx <- which(labels %in% dptos_a_etiquetar)


# Añadimos las etiquetas al gráfico
text(x[idx], y[idx],
     labels = labels[idx], 
     cex = 0.7,             # Tamaño del texto
     pos = 4,               # Posición respecto del punto
     col = "black")         # Color del texto






########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## FIN SESIÓN 1 ########## 
