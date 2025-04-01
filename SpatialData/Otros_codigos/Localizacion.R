
################################################
## Localización: Zona urbana y rural en Cali  ##
################################################
library(gdata)
library(cartography)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(tidyverse)
library(sf)
library(ggspatial)
library(ellipse)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggplot2)
library(maptools)

setwd("C:/Users/Portatil/Desktop/Georef_R")
#############################################
## Unión de la zona urbana y la zona rural ##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#############################################
urbano <- shapefile('Comunas/Comunas_WGS84.shp')
urbano_st <- st_read('Comunas/Comunas_WGS84.shp')
rural <- shapefile('Corregimientos/Corregimientos_WGS84.shp')
rural_st <- st_read('Corregimientos/Corregimientos_WGS84.shp')
plot(st_geometry(urbano_st))
plot(st_geometry(rural_st))

# Completar los polígonos de la zona rural (polígono Hormiguero)
exp = shapefile('Planificación/Expansion.shp')
exp_st = st_read('Planificación/Expansion.shp')
exp_st = exp_st[c("nombre_upu", "geometry")]
exp_st$id = nrow(rural_st) + 1
exp_st = exp_st[c("id", "nombre_upu", "geometry")]
colnames(exp_st) = c("id_correg", "corregimie", "geometry")

rural_exp = rbind(rural_st,exp_st)

# Unión de la Expansión y El Hormiguero
df_k = rural_exp %>% filter(corregimie %in% c("El Hormiguero", "EXPANSION"))
udf_k = data.frame(id_correg = 52,
                   corregimie = "El Hormiguero", geometry = st_union(df_k))
# Convertir en GEODETIC CRS: WGS 84
wgs_udf_k = st_as_sf(x = udf_k,
                     crs = "WGS84")
rural_exp = rural_exp %>% filter(!corregimie %in% c("El Hormiguero", "EXPANSION"))
rural_exp_st <- rbind(rural_exp, wgs_udf_k)

# Unión entre la zona urbana y la zona rural
colnames(rural_exp_st) = c("COMUNA", "NOMBRE", "geometry")
urbano_rural = rbind(urbano_st[c("COMUNA", "NOMBRE", "geometry")],
                     rural_exp_st)


####################################
## Humedales y cuerpos de agua    ##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####################################
humedales_rurales <- shapefile('Humedales/Humedales_Rurales_WGS84.shp')
humedales_rurales_st <- st_read('Humedales/Humedales_Rurales_WGS84.shp')

humedales_urbanos <- shapefile('Humedales/Humedales_urbanos_WGS84_Corregido.shp')
humedales_urbanos_st <- st_read('Humedales/Humedales_urbanos_WGS84_Corregido.shp')

rios = shapefile('Rios Cali/Rios Cali_WGS84.shp')
rios_st = st_read('Rios Cali/Rios Cali_WGS84.shp')

rio_cauca = shapefile('Rio Cauca/Rio Cauca_WGS84.shp')
rio_cauca_st = st_read('Rio Cauca/Rio Cauca_WGS84.shp')

# Cali: zona rural y urbana
colnames(urbano_rural) = c("comuna", "nombre", "geometry")
urbano_rural$zona = rep("Total", nrow(urbano_rural))

cali_humedales = rbind(urbano_rural[c("nombre", "zona", "comuna", "geometry")],
                       humedales[c("nombre", "zona", "comuna", "geometry")])

plot_local <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st), fill = "white") +
  geom_sf(data = st_as_sf(rural_exp_st), fill = "lightgray") +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_label(data = st_as_sf(rio_cauca),aes(label = nombre),size = 2.5)+
  geom_sf_label(data = st_as_sf(rios),aes(label = nombre),size = 2.5) +
  ggtitle("Cali: zona urbana y rural") +
  theme(panel.background=element_rect(fill="white"), #Color del plot
        plot.background = element_rect(fill="gray100"), #Color del fondo del plot
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"),
        panel.grid=element_blank()) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.55, "in"), pad_y = unit(0.55, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"))


ggsave(filename = "Propuestas/Propuesta1/plot_local.png",
       plot = plot_local)
