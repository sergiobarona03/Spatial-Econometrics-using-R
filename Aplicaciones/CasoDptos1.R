
# Caso de estudio 1:
# NBI a nivel de departamentos

# Cargar librerías
library(tidyverse)
library(sf)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\")

##---------------------------------------##
## Cargar datos a nivel de departamentos ##
##---------------------------------------##

# Cargar datos NBI
nbi <- readxl::read_excel("CNPV_2018\\NBI_DPTO_CNPV2018.xlsx")

# Cargar datos de población
pob <- readxl::read_excel("CNPV_2018\\Pob_DPTO_CNPV2018.xlsx")
  
# Cargar datos de ingresos
ing <- readxl::read_excel("CNPV_2018\\INGRESOS_DPTO_DNP.xlsx") %>% mutate(cod = cod_dpto/1000)

# Unir los datos
dpto_data <- nbi %>% dplyr::left_join(pob[c("cod_dpto", "n")], 
                                      by = c("cod" = "cod_dpto")) %>%
  dplyr::left_join(ing, by = c("cod" = "cod"))

##---------------------------------------##
## Visualizar NBI en mapas               ##
##---------------------------------------##
# Leer dpto_shape
dpto_shape <- st_read(dsn = "SpatialData\\dptos_col\\",
                      layer = "departamentos")

# Limpiar los nombres de los departamentos
library(stringi)
dpto_data <- dpto_data %>%
  mutate(dpto_clean = str_to_upper(str_trim(stri_trans_general(dpto, "Latin-ASCII"))))

dpto_shape <- dpto_shape %>%
  mutate(NOMBRE_DPT_clean = str_to_upper(str_trim(stri_trans_general(NOMBRE_DPT, "Latin-ASCII"))))

# Corrección manual de casos específicos
dpto_shape <- dpto_shape %>%
  mutate(NOMBRE_DPT_clean = case_when(
    NOMBRE_DPT_clean == "ATLANTICO" ~ "ATLANTICO",
    NOMBRE_DPT_clean == "BOGOTA" ~ "BOGOTA, D.C.",
    NOMBRE_DPT_clean == "CORDOBA" ~ "CORDOBA",
    NOMBRE_DPT_clean == "GUAINIA" ~ "GUAINIA",
    NOMBRE_DPT_clean == "QUINDIO" ~ "QUINDIO",
    NOMBRE_DPT_clean == "VAUPES" ~ "VAUPES",
    NOMBRE_DPT_clean == "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA" ~ "ARCHIPIELAGO DE SAN ANDRES",
    TRUE ~ NOMBRE_DPT_clean
  ))

# Unir las bases de datos
dpto_merged <-  dpto_shape[c("ID_ESPACIA",
                             "NOMBRE_DPT_clean",
                             "AREA_OFICI",
                             "geometry")] %>%
  left_join(dpto_data[c("dpto_clean", "dpto", "tasa_nbi",
                        "tasa_miseria", "comp_vivi",
                        "comp_servi", "comp_hacin",
                        "comp_inasist", "comp_dep_eco",
                        "n", "ingresos_totales",
                        "ingresos_corrientes", "corr_tot")],
            by = c("NOMBRE_DPT_clean" = "dpto_clean"))

# Por simplicidad, se omite San Andrés
dpto_merged <- dpto_merged %>% filter(NOMBRE_DPT_clean != "ARCHIPIELAGO DE SAN ANDRES")

# Visualizar la variable "tasa_nbi"
library(viridis)
dpto_merged$tasa_nbi <- as.numeric(gsub(",", ".", dpto_merged$tasa_nbi))
dpto_merged$tasa_miseria <- as.numeric(gsub(",", ".", dpto_merged$tasa_miseria))

ggplot(data = dpto_merged) +
  geom_sf(aes(fill = tasa_nbi), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = c("#1a9850", "#fee08b", "#d73027"), 
                       values = c(0, 0.5, 1), 
                       name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribución espacial del NBI por Departamento (% en NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.15, 0.9),  
        legend.background = element_rect(fill = "white", color = "black"))

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
  labs(title = "Distribución espacial del NBI por Departamento (en % de NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.15, 0.88),  
        legend.background = element_rect(fill = "white", color = "black"))

##---------------------------------------##
## Visualizar en mapas: NBI vs. N        ##
##---------------------------------------##

# Crear intervalos para la variable "n"
dpto_merged$n_cat <- cut(dpto_merged$n, 
                         breaks = c(0, 200000, 500000, 1000000, 3000000,
                                    7000000, Inf), 
                         labels = c("< 200K", "200K - 500K", "500K - 1M", 
                                    "1M - 3M", "3M - 7M", "7M+"),
                         include.lowest = TRUE)

colors_intervals <- c("< 200K" = "#1a9850",  # Verde
                      "200K - 500K" = "#66bd63", # Verde claro
                      "500K - 1M" = "#fee08b", # Amarillo-naranja
                      "1M - 3M" = "#fdae61", # Naranja
                      "3M - 7M" = "#f46d43", # Rojo claro
                      "7M+" = "#d73027")   # Rojo fuerte

map_n <- ggplot(data = dpto_merged) +
  geom_sf(aes(fill = n_cat), color = "black", size = 0.2) + 
  scale_fill_manual(values = colors_intervals, name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribución espacial de la población por Departamento (en % de NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.15, 0.88),  
        legend.background = element_rect(fill = "white", color = "black"))

library(gridExtra)
grid.arrange(map_nbi, map_n, ncol = 2)

##------------------------------##
## I de Moran Global            ##
##------------------------------##
library(spdep)

# Revisión de las conexiones o links del shapefile 
nb_dpto <- poly2nb(dpto_merged)
nb_dpto

# Se crea la matriz W
nb2_dpto <- nb2listw(nb_dpto)
summary(nb2_dpto)

# Examinar la variable dependiente
summary(dpto_merged$tasa_nbi)

# Véase la información de los pesos de la matriz
names(attributes(nb2_dpto))  # Nombres de los atributos
card(nb2_dpto$neighbours)    # Número de vecinos para cada observación
range(card(nb2_dpto$neighbours))  # Rango (número de vecinos)
1/rev(range(card(nb2_dpto$neighbours))) # Rango (pesos)
summary(nb2_dpto, zero.policy=T)   # Resumen

# - Rechazo o no?
moran.test(dpto_merged$tasa_nbi, 
           nb2_dpto, alternative="two.sided", zero.policy=T)

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
title('Distribución tasa NBI (%)')
legend('topleft', legend=c(names(attr(colcode, 'table'))), fill=c(attr(colcode, 'palette')), 
       title='NBI (%)')

# Moran plot
moran.plot(dpto_merged$tasa_nbi, nb2_dpto, 
           zero.policy=T, labels=as.character(dpto_merged$NOMBRE_DPT_clean),
           xlab=NULL, ylab=NULL, type="p", col="#AE017E",
           cex=1.2, pch=1)

##------------------------------##
## Estadísticos locales         ##
##------------------------------##


