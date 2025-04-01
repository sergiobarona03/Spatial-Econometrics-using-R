
#######################
## Macrolocalizacion ##
#######################
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
library(ggpubr)

setwd("C:/Users/Portatil/Desktop/Georef_R")

###############################
## América del sur: Colombia ##
###############################
world <- shapefile('World/world.shp')
plot(world)

world_st <- st_read('World/world.shp')
south <- world_st %>% filter(continent == "South America")

colombia <- world_st %>% filter(name == "Colombia")

ext_1 <- colombia %>%  
  as(., 'Spatial') %>% 
  extent() 

coord_zoom <- south %>%  
  as(., 'Spatial') %>% 
  extent() 

macro0 <- ggplot() +
  geom_sf(data = world_st, fill = alpha("white", 0.5),
          color = alpha("black", 0.5)) +
  geom_sf(data = st_as_sf(colombia),fill = 'gray',color = 'black') +
  geom_rect(aes(xmin = ext_1@xmin, xmax = ext_1@xmax, 
                ymin = ext_1@ymin, ymax = ext_1@ymax),
            color = "red", fill = NA) +
  coord_sf(xlim = c(coord_zoom@xmin,
                    coord_zoom@xmax), 
           ylim = c(coord_zoom@ymin,
                    coord_zoom@ymax), expand = FALSE) +
  theme(panel.background=element_rect(fill="white"), #Color del plot
        plot.background = element_rect(fill="gray100"), #Color del fondo del plot
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"),
        panel.grid=element_blank()) +
  annotation_scale()   +                                                            
  guides(colour = "colorbar", size = "legend", shape = "legend") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))


####################################################
## Ubicación en Colombia: Valle del Cauca y Cauca ##
####################################################
Colombia <- shapefile('depto_col/dptos_col.shp')
plot(Colombia)

# Quitar San Andrés
dpt <- st_read("depto_col/dptos_col.shp") %>% mutate(NOMBRE_DPT = str_to_sentence(NOMBRE_DPT))
dpt <- dpt %>% filter(NOMBRE_DPT != 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA')

valle_cauca <- dpt %>% filter(NOMBRE_DPT %in% c("Valle del cauca",
                                                "Cauca"))
ext_2 <- valle_cauca %>%  
  as(., 'Spatial') %>% 
  extent() 

macro1 <- ggplot() +
  geom_sf(data = dpt, fill = alpha("white", 0.5),
          color = alpha("black", 0.5)) +
  geom_sf(data = st_as_sf(valle_cauca),fill = 'gray',color = 'black') +
  geom_rect(aes(xmin = ext_2@xmin, xmax = ext_2@xmax, 
                ymin = ext_2@ymin, ymax = ext_2@ymax),
            color = "red", fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"), 
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"))




##########################################
## Ubicación en Valle del Cauca y Cauca ##
##########################################
am <- shapefile('Area metropolitana/AreaInfluencia_WGS84.shp')
am <- st_read("Area metropolitana/AreaInfluencia_WGS84.shp")
am$NOMBRE_DPT = "Área de influencia"
am_valle_cauca <- rbind(valle_cauca[c("OBJECTID", "NOMBRE_DPT",
                                      "Shape_Leng", "Shape_Area",
                                      "geometry")],
                        am[c("OBJECTID", "NOMBRE_DPT",
                             "Shape_Leng", "Shape_Area",
                             "geometry")])

ext_am <- am %>%  
  as(., 'Spatial') %>% 
  extent() 

macro2 <- ggplot() +
  geom_sf(data = am_valle_cauca, fill = alpha("white", 0.5),
          color = alpha("black", 0.5)) +
geom_sf(data = st_as_sf(am),fill = 'gray',color = 'black') +
  geom_rect(aes(xmin = ext_am@xmin, xmax = ext_am@xmax, 
                ymin = ext_am@ymin, ymax = ext_am@ymax),
            color = "red", fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"), 
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"))


################################################
## Ubicación de Cali en el Área de influencia ##
################################################
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

######## ¿Y si es una geometría? P.S. Sí es una geometría.
colnames(urbano_rural) = c("OBJECTID", "NOMBRE", "geometry")
urbano_rural$region = "Cali"

am <- shapefile('Area metropolitana/AreaInfluencia_WGS84.shp')
am_st <- st_read("Area metropolitana/AreaInfluencia_WGS84.shp")
am_st$NOMBRE = "Área de influencia"
am_st = am_st[c("OBJECTID", "NOMBRE", "geometry")]
am_st$region = "No-Cali"

am_urbano_rural <- rbind(am_st, urbano_rural)

ext_cali <- urbano_rural %>%  
  as(., 'Spatial') %>% 
  extent() 

macro3 <- ggplot() + 
  geom_sf(data = am_urbano_rural, aes(fill = region,
                                      col = region), show.legend = FALSE) +
  scale_fill_manual(values=c("gray", "white"))+
  scale_color_manual(values=c("gray", "black")) + 
  geom_rect(aes(xmin = ext_cali@xmin, xmax = ext_cali@xmax, 
                ymin = ext_cali@ymin, ymax = ext_cali@ymax),
            color = "red", fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1.2, "in"), pad_y = unit(0.55, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"), 
        plot.title=element_text(hjust = 0.5,size = 16,family="serif",face = "bold"))



ggsave(filename = "Propuestas/Propuesta1/macro0.png",
       plot = macro0)

ggsave(filename = "Propuestas/Propuesta1/macro1.png",
       plot = macro1)

ggsave(filename = "Propuestas/Propuesta1/macro2.png",
       plot = macro2)

ggsave(filename = "Propuestas/Propuesta1/macro3.png",
       plot = macro3)
  


  

