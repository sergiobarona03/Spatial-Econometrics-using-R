
# Caso de estudio 2:
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

# Visualizar la variable "tasa_nbi"
library(viridis)

ggplot(data = mun_merged) +
  geom_sf(aes(fill = tasa_nbi), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = c("#1a9850", "#fee08b", "#d73027"), 
                       values = c(0, 0.5, 1), 
                       name = "Tasa NBI (%)") + 
  theme_bw() + 
  labs(title = "Distribución espacial del NBI por Municipios (% en NBI)",
       caption = "Fuente: DANE. CNPV-2018.") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.15, 0.9),  
        legend.background = element_rect(fill = "white", color = "black"))

# Visualizar la variable "tasa_nbi" a partir de intervalos
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
  labs(title = "Distribución espacial del NBI por Municipios (en % de NBI)",
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

mun_merged$n_cat <- cut(mun_merged$n, 
                         breaks = c(0, 20000, 50000, 100000, 300000,
                                    700000, Inf), 
                         labels = c("< 20K", "20K - 50K", "50K - 100k", 
                                    "100k - 300k", "300k - 700k", "700k+"),
                         include.lowest = TRUE)

colors_intervals <- c("< 20K" = "#1a9850",  # Verde
                      "20K - 50K" = "#66bd63", # Verde claro
                      "50K - 100k" = "#fee08b", # Amarillo-naranja
                      "100k - 300k" = "#fdae61", # Naranja
                      "300k - 700k" = "#f46d43", # Rojo claro
                      "700k+" = "#d73027")   # Rojo fuerte

map_n <- ggplot(data = mun_merged) +
  geom_sf(aes(fill = n_cat), color = "black", size = 0.2) + 
  scale_fill_manual(values = colors_intervals, name = "N") + 
  theme_bw() + 
  labs(title = "Distribución espacial de la población por Municipios",
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

# Se presenta el mapa sobre  bg.df$Rate. Seleccionar la paleta
library(RColorBrewer)
library(classInt)

colors <- brewer.pal(5, "YlOrBr")  
color.cat.reg<-classIntervals(dpto_merged$tasa_nbi, n=5, 
                              style="quantile", dataPrecision=2)
colcode <- findColours(color.cat.reg, colors)

## Figura: Distribución espacial del NBI (%)
par(mfrow=c(1,1))
plot(mun_merged[c("tasa_nbi", "geometry")], col=colcode)
title('Distribución tasa NBI (%)')
legend('topleft', legend=c(names(attr(colcode, 'table'))), fill=c(attr(colcode, 'palette')), 
       title='NBI (%)')

# Moran plot
moran.plot(mun_merged$tasa_nbi, nb2_mun, 
           zero.policy=T, labels=as.character(mun_merged$nom_mun),
           xlab=NULL, ylab=NULL, type="p", col="#AE017E",
           cex=1, pch=1)

##------------------------------##
## Estadísticos locales         ##
##------------------------------##


