
################################################
## Microlocalización: Humedales según nivel   ##
##   socioeconómico (urbano y peri-urbano)    ##
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
####################################
## .shp en la zona urbana y rural ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####################################
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


#######################################################
## Recuperación de la variable: nivel socioeconómico ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################################

estrato_urbano = readxl::read_excel("Estrato_moda_urbano.xlsx")
estrato_urbano = as.data.frame(estrato_urbano)
colnames(estrato_urbano) = c("COMUNA", "Estrato")
urbano_st = merge(urbano_st, estrato_urbano, by = "COMUNA")

estrato_rural = data.frame(COMUNA = c(53, 52, 51), Estrato = c("Alto",
                                                               "Bajo", 
                                                               "Bajo"))
rural_exp_st = merge(rural_exp_st, estrato_rural, by = "COMUNA",
                     all.x = TRUE)
##################################################
## Humedales rurales, urbanos, ríos y río Cauca ##
##################################################
humedales_rurales <- shapefile('Humedales/Humedales_Rurales_WGS84.shp')
humedales_rurales_st <- st_read('Humedales/Humedales_Rurales_WGS84.shp')

humedales_urbanos <- shapefile('Humedales/Humedales_urbanos_WGS84_Corregido.shp')
humedales_urbanos_st <- st_read('Humedales/Humedales_urbanos_WGS84_Corregido.shp')

rios = shapefile('Rios Cali/Rios Cali_WGS84.shp')
rios_st = st_read('Rios Cali/Rios Cali_WGS84.shp')

rio_cauca = shapefile('Rio Cauca/Rio Cauca_WGS84.shp')
rio_cauca_st = st_read('Rio Cauca/Rio Cauca_WGS84.shp')

#######################################
## Mapa microlocalizado: zona urbana ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################
urbano_st$Estrato = factor(urbano_st$Estrato,
                          levels = c(1,2,3,4,5,6),
                          labels = c("Bajo", "Bajo",
                                     "Medio", "Medio",
                                     "Alto", "Alto"))

coord_zoom <- urbano_st %>%  
  as(., 'Spatial') %>% 
  extent() 

urbano_micro <- ggplot() + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = alpha("gray", 0.005),
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), 
          aes(fill = Estrato)) + scale_fill_manual(values = c("#E6E6E6", "#B3B3B3", "#999999"))+
  geom_sf(data = st_as_sf(humedales_urbanos), fill = "green4",color = "green4")+
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_label(data = st_as_sf(rural_exp_st),aes(label = NOMBRE),size = 2) +
  geom_sf_label(data = st_as_sf(urbano_st),aes(label = NOMBRE),size = 2)+
  coord_sf(xlim = c(coord_zoom@xmin,
                    coord_zoom@xmax), 
           ylim = c(coord_zoom@ymin,
                    coord_zoom@ymax), expand = FALSE) +
  ggtitle("Humedales urbanos") +
  theme(panel.background=element_rect(fill="white"), #Color del plot
        plot.background = element_rect(fill="gray100"), #Color del fondo del plot
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"),
        panel.grid=element_blank()) +
  annotation_scale()   +                                                           #Este comando a?ade la escala en el mapa
  guides(colour = "colorbar", size = "legend", shape = "legend") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))



#######################################
## Mapa microlocalizado: zona rural  ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################
zoom_rural = rural_exp_st %>% filter(COMUNA %in% c(51,52,53))

coord_zoom <- zoom_rural %>%  
  as(., 'Spatial') %>% 
  extent() 

rural_micro <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("gray", 0.005),
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_exp_st), fill = alpha("gray", 0.005),
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(zoom_rural), aes(fill = Estrato)) +
  scale_fill_manual(values = c("#E6E6E6", "#B3B3B3", "#999999"))+
  geom_sf(data = st_as_sf(humedales_rurales), fill = "green4",color = "green4")+
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_label(data = st_as_sf(rural_exp_st),aes(label = NOMBRE),size = 2) +
  geom_sf_label(data = st_as_sf(urbano_st),aes(label = NOMBRE),size = 2)+
  coord_sf(xlim = c(coord_zoom@xmin + 0.1,
                    coord_zoom@xmax), 
           ylim = c(coord_zoom@ymin,
                    coord_zoom@ymax - 0.05), expand = FALSE) +
  ggtitle("Humedales rurales") +
  theme(panel.background=element_rect(fill="white"), #Color del plot
        plot.background = element_rect(fill="gray100"), #Color del fondo del plot
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"),
        panel.grid=element_blank()) +
  annotation_scale()   +                                                           #Este comando a?ade la escala en el mapa
  guides(colour = "colorbar", size = "legend", shape = "legend") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))



ggsave(filename = "Propuestas/Propuesta1/rural_micro.png",
       plot = rural_micro)
ggsave(filename = "Propuestas/Propuesta1/urbano_micro.png",
       plot = urbano_micro)


