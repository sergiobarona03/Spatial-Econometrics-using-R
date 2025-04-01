###############################################
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
library(terra)
library(RColorBrewer)
setwd("C:/Users/Portatil/Desktop/Georef_R")
####################################
## .shp en la zona urbana y rural ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####################################
urbano <- shapefile('Comunas/Comunas_WGS84.shp')
urbano_st <- st_read('Comunas/Comunas_WGS84.shp')
rural <- shapefile('Corregimientos/Corregimientos_WGS84.shp')
rural_st <- st_read('Corregimientos/Corregimientos_WGS84.shp')

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
## Humedales rurales, urbanos, ríos y río Cauca ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################################################
# Filtrar los humedales que aparecen en la encuesta
encuesta <- readxl::read_excel("Datos_recod.xlsx")
humedales_encuesta <- levels(as.factor(encuesta$wetland))

count_encuesta <- dplyr::count(encuesta, wetland)
colnames(count_encuesta) = c("nombre", "n")
# Nota: el filtro anterior sólo se usará para los humedales ubicados en el sur de la ciudad

humedales_rurales <- shapefile('Humedales/Humedales_Rurales_WGS84.shp')
humedales_rurales_st <- st_read('Humedales/Humedales_Rurales_WGS84.shp')

humedales_urbanos <- shapefile('Humedales/Humedales_urbanos_WGS84_Corregido.shp')
humedales_urbanos_st <- st_read('Humedales/Humedales_urbanos_WGS84_Corregido.shp')

rios = shapefile('Rios Cali/Rios Cali_WGS84.shp')
rios_st = st_read('Rios Cali/Rios Cali_WGS84.shp')

rio_cauca = shapefile('Rio Cauca/Rio Cauca_WGS84.shp')
rio_cauca_st = st_read('Rio Cauca/Rio Cauca_WGS84.shp')

humedales_urbanos$HumNom[humedales_urbanos$HumNom == "Humedal Batallón"] = "Batallón"

humedales_urbanos$nom_recod = humedales_urbanos$HumNom
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre I"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre II"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre III"] = "Club Campestre" 
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre IV"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre V"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre VI"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre VII"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club Campestre VIII"] = NA

humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Univalle I"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Univalle II"] = "Univalle"

humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "U. Javeriana I"] = "Javeriana"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "U. Javeriana II"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "U.Javeriana III"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Javeriana IV"] = NA

humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom I"] = "Shalom"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom II"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom III"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom IV"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom V"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom VI"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom VII"] = NA
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales club shalom VIII"] = NA

humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "El Retiro I"] = NA

humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal San Buenaventura"] = "San Buenaventura"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Club del Municipio"] = "Club del Municipio"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Panamericano"] = "Panamericano"


humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Charco Azul"] = "Charco Azul"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal El Pondaje"] = "El Pondaje"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedales Isaias Duarte Cancino"] = "Isaias Duarte Cancino"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Rerservorio Puerto Mallarino"] = "Puerto Mallarino"

humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Limonar"] = "El Limonar"
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal La Babilla Zanjón del Burro"] = "La Babilla Zanjón del Burro"


humedales_rurales <- merge(humedales_rurales,
                           count_encuesta, by = "nombre")
colnames(count_encuesta) = c("nom_recod", "n")
humedales_urbanos <- merge(humedales_urbanos,
                           count_encuesta, by = "nom_recod")

#######################################
## Mapa microlocalizado: zona urbana ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################

##################
## Estrato bajo ##-------------------------------------------------------------------------
##################
urbano_st$Estrato = factor(urbano_st$Estrato,
                           levels = c(1,2,3,4,5,6),
                           labels = c("Bajo", "Bajo",
                                      "Medio", "Medio",
                                      "Alto", "Alto"))

urbano_bajo <- urbano_st %>% filter(Estrato == "Bajo")
urbano_medio <- urbano_st %>% filter(Estrato == "Medio")
urbano_alto <-  urbano_st %>% filter(Estrato == "Alto")

humedales_bajo =  subset(humedales_urbanos, Comuna %in% urbano_bajo$COMUNA)
humedales_medio =  subset(humedales_urbanos, Comuna %in% urbano_medio$COMUNA)
humedales_alto =  subset(humedales_urbanos, Comuna %in% urbano_alto$COMUNA)
humedales_alto = subset(humedales_alto, nom_recod %in% humedales_encuesta)

zoom_bajo <- urbano_bajo %>% filter(COMUNA %in% humedales_bajo$Comuna)

coord_bajo <- zoom_bajo %>%  
  as(., 'Spatial') %>% 
  extent() 

center_bajo <- st_centroid(st_as_sf(humedales_bajo))

urbano_micro_bajo <- ggplot() + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = alpha("antiquewhite"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_bajo), fill = alpha("white", 0.8), col = "black") +
  geom_sf(data = st_as_sf(humedales_bajo), aes(fill = nom_recod, col = nom_recod), lwd = 3) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_sf(data = center_bajo, aes(size = n, col = nom_recod), size = 18, alpha = 0.7)+
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_bajo),aes(label = NOMBRE),size = 2.8) +
  geom_sf_text(data = center_bajo,aes(label = n),size = 3) +
  coord_sf(xlim = c(coord_bajo@xmin + 0.008,coord_bajo@xmax), 
           ylim = c(coord_bajo@ymin + 0.003, coord_bajo@ymax), expand = FALSE) + 
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")) + 
  scale_color_manual(values = brewer.pal(n = 4, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))


urbano_micro_bajo_theme <- urbano_micro_bajo + ggtitle("Humedales urbanos (Estrato bajo)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(.99, .18),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale()+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 

###################
## Estrato medio ##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################
zoom_medio <- urbano_medio %>% filter(COMUNA %in% humedales_medio$Comuna)

coord_medio <- zoom_medio %>%  
  as(., 'Spatial') %>% 
  extent() 

center_medio <- st_centroid(st_as_sf(humedales_medio))

urbano_micro_medio <- ggplot() + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = alpha("antiquewhite"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_medio), fill = alpha("white", 0.8), col = "black") +
  geom_sf(data = st_as_sf(humedales_medio), aes(fill = nom_recod, col = nom_recod), lwd = 3) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_sf(data = center_medio, aes(size = n, col = nom_recod), size = 18, alpha = 0.7)+
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_medio),aes(label = NOMBRE),size = 2.8) +
  geom_sf_text(data = center_medio,aes(label = n),size = 3) +
  coord_sf(xlim = c(coord_medio@xmin,coord_medio@xmax), 
           ylim = c(coord_medio@ymin - 0.004, coord_medio@ymax - 0.025), expand = FALSE) + 
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")) + 
  scale_color_manual(values = brewer.pal(n = 4, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

urbano_micro_medio_theme <- urbano_micro_medio + ggtitle("Humedales urbanos (Estrato medio)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.5, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 


##################
## Estrato alto ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################
urbano_alto_plot = urbano_alto %>% filter(COMUNA %in% humedales_alto$Comuna)
humedales_17 <- subset(humedales_alto, nom_recod %in% c("La Babilla Zanjón del Burro",
                                                        "El Limonar", "Club Campestre", "Univalle"))
humedales_22 <-subset(humedales_alto, !nom_recod %in% c("La Babilla Zanjón del Burro",
                                                       "El Limonar", "Club Campestre", "Univalle"))

zoom_17 <- urbano_alto %>% filter(COMUNA == 17)
coord_17 <- zoom_17 %>%  
  as(., 'Spatial') %>% 
  extent() 

zoom_22 <- urbano_alto %>% filter(COMUNA == 22)
coord_22 <- zoom_22 %>%  
  as(., 'Spatial') %>% 
  extent() 

center_alto <- st_centroid(st_as_sf(humedales_17))

urbano_micro_alto_17 <- ggplot() + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = alpha("antiquewhite"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_alto_plot), fill = alpha("white", 0.8), col = "black") +
  geom_sf(data = st_as_sf(humedales_17), aes(fill = nom_recod, col = nom_recod), lwd = 3) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_sf(data = center_alto, aes(size = n, col = nom_recod), size = 18, alpha = 0.7)+
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_alto),aes(label = NOMBRE),size = 2.8) +
  geom_sf_text(data = center_alto,aes(label = n),size = 3) +
  coord_sf(xlim = c(coord_17@xmin - 0.002,coord_17@xmax), 
           ylim = c(coord_17@ymin-0.004, coord_17@ymax), expand = FALSE) + 
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")) + 
  scale_color_manual(values = brewer.pal(n = 4, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

urbano_micro_alto_17_theme <- urbano_micro_alto_17 + ggtitle("Humedales urbanos (Estrato alto en Comuna 17-22)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.5, 0.99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.38, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 


center_alto <- st_centroid(st_as_sf(humedales_22))
urbano_micro_alto_22 <- ggplot() + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = alpha("antiquewhite"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_alto_plot), fill = alpha("white", 0.8), col = "black") +
  geom_sf(data = st_as_sf(humedales_22), aes(fill = nom_recod, col = nom_recod), lwd = 3) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_sf(data = center_alto, aes(size = n, col = nom_recod), size = 18, alpha = 0.7)+
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_alto),aes(label = NOMBRE),size = 2.8) +
  geom_sf_text(data = center_alto,aes(label = n),size = 3) +
  coord_sf(xlim = c(coord_22@xmin,coord_22@xmax + 0.02), 
           ylim = c(coord_22@ymin, coord_22@ymax - 0.02), expand = FALSE) + 
  scale_fill_manual(values = hcl.colors(13, "Greens")) + 
  scale_color_manual(values = hcl.colors(13, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

urbano_micro_alto_22_theme <- urbano_micro_alto_22 + ggtitle("Humedales urbanos (Estrato alto en Comuna 22-22)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(1.4, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 


ggsave(filename = "Propuestas/Propuesta2/urbano_micro_bajo.png",
       plot = urbano_micro_bajo_theme)
ggsave(filename = "Propuestas/Propuesta2/urbano_micro_medio.png",
       plot = urbano_micro_medio_theme)
ggsave(filename = "Propuestas/Propuesta2/urbano_micro_alto_17.png",
       plot = urbano_micro_alto_17_theme)
ggsave(filename = "Propuestas/Propuesta2/urbano_micro_alto_22.png",
       plot = urbano_micro_alto_22_theme)

#######################################
## Mapa microlocalizado: zona rural  ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################

##################
## Estrato bajo ##-------------------------------------------------------------------------
##################
rural_bajo <- rural_exp_st %>% filter(Estrato == "Bajo")
rural_alto <-  rural_exp_st %>% filter(Estrato == "Alto")

zoom_bajo <- rural_bajo
zoom_bajo_rural <- zoom_bajo

coord_bajo_rural <- zoom_bajo %>%  
  as(., 'Spatial') %>% 
  extent() 

humedales_rurales_bajos <- subset(humedales_rurales, !is.na(n))
humedales_rurales_bajos <- subset(humedales_rurales_bajos, nombre != "Las Garzas")
humedales_rurales_bajos <- humedales_rurales_bajos[c(1,3:7),]


center_alto <- st_centroid(st_as_sf(humedales_rurales_bajos))
rurales_micro_bajo <- ggplot()  + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = "antiquewhite", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_bajo), fill = alpha("white", 0.8),
          col = "black") + geom_sf(data = st_as_sf(humedales_rurales_bajos), 
                                   aes(fill = nombre, col = nombre), lwd = 3) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") + 
  geom_sf(data = center_alto, aes(size = n, col = nombre), size = 18
          , alpha = 0.7)+
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(rural_bajo),aes(label = NOMBRE),size = 2.8) +
  geom_sf_text(data = center_alto,aes(label = n),size = 3) +
  coord_sf(xlim = c(coord_bajo_rural@xmin + 0.006,coord_bajo_rural@xmax + 0.04), 
           ylim = c(coord_bajo_rural@ymin, coord_bajo_rural@ymax - 0.03), expand = FALSE) + 
  scale_fill_manual(values = hcl.colors(20, "Greens")) + 
  scale_color_manual(values = hcl.colors(20, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

rurales_micro_bajo_theme <- rurales_micro_bajo + ggtitle("Humedales rurales (Estrato bajo)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.99, 0.35),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.38, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 




##################
## Estrato alto ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################
zoom_alto <- rural_alto
zoom_alto_rural <- zoom_alto
coord_alto_rural <- zoom_alto %>%  
  as(., 'Spatial') %>% 
  extent() 

humedales_rurales_altos <- subset(humedales_rurales, nombre == "Las Garzas")



center_alto <- st_centroid(st_as_sf(humedales_rurales_altos))
rurales_micro_alto <- ggplot()  + 
  geom_sf(data = st_as_sf(rural_exp_st), fill = "antiquewhite", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_alto), fill = alpha("white", 0.8),
          col = "black") + geom_sf(data = st_as_sf(humedales_rurales_altos), 
                                   aes(fill = nombre, col = nombre), lwd = 3) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") + 
  geom_sf(data = center_alto, aes(size = n, col = nombre), size = 18
          , alpha = 0.7)+
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(rural_alto),aes(label = NOMBRE),size = 2.8) +
  geom_sf_text(data = center_alto,aes(label = n),size = 3) +
  coord_sf(xlim = c(coord_alto@xmin + 0.08,coord_alto@xmax), 
           ylim = c(coord_alto@ymin, coord_alto@ymax), expand = FALSE) + 
  scale_fill_manual(values = hcl.colors(20, "Greens")) + 
  scale_color_manual(values = hcl.colors(20, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

rurales_micro_alto_theme <- rurales_micro_alto + ggtitle("Humedales rurales (Estrato alto)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.38, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 

ggsave(filename = "Propuestas/Propuesta2/rurales_micro_bajo.png",
       plot = rurales_micro_bajo_theme)
ggsave(filename = "Propuestas/Propuesta2/rurales_micro_alto.png",
       plot = rurales_micro_alto_theme)




#################################################################
## Localización: hacer zoom a cada parte con un recuadro negro ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#################################################################

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

rios = shapefile('Rios Cali/Rios Cali_WGS84.shp')
rios_st = st_read('Rios Cali/Rios Cali_WGS84.shp')

rio_cauca = shapefile('Rio Cauca/Rio Cauca_WGS84.shp')
rio_cauca_st = st_read('Rio Cauca/Rio Cauca_WGS84.shp')

# Cali: zona rural y urbana
colnames(urbano_rural) = c("comuna", "nombre", "geometry")
urbano_rural$zona = rep("Total", nrow(urbano_rural))

cali_humedales = rbind(urbano_rural[c("nombre", "zona", "comuna", "geometry")],
                       humedales[c("nombre", "zona", "comuna", "geometry")])


ext_1 <- subset(humedales_urbanos, nom_recod == "El Pondaje")
coord_bajo <- ext_1 %>%  
  as(., 'Spatial') %>% 
  extent() 
rect1 <- geom_rect(aes(xmin = coord_bajo@xmin, xmax = coord_bajo@xmax -0.002, 
                       ymin = coord_bajo@ymin, ymax = coord_bajo@ymax - 0.002),
                   color = "black", fill = NA, lwd = 1)

ext_2 <- subset(humedales_urbanos, nom_recod == "Batallón")
coord_medio <- ext_2 %>%  
  as(., 'Spatial') %>% 
  extent() 
rect2 <- geom_rect(aes(xmin = coord_medio@xmin, xmax = coord_medio@xmax + 0.005, 
                       ymin = coord_medio@ymin, ymax = coord_medio@ymax + 0.005),
                   color = "black", fill = NA, lwd = 1)

ext_3 <- subset(humedales_urbanos, nom_recod == "El Limonar")
coord_17 <- ext_3 %>%  
  as(., 'Spatial') %>% 
  extent()
rect3 <- geom_rect(aes(xmin = coord_17@xmin, xmax = coord_17@xmax + 0.006, 
                       ymin = coord_17@ymin, ymax = coord_17@ymax + 0.006),
                   color = "black", fill = NA, lwd = 1)

ext_4 <- subset(humedales_urbanos, nom_recod == "San Buenaventura")
coord_22 <- ext_4 %>%  
  as(., 'Spatial') %>% 
  extent()
rect4 <- geom_rect(aes(xmin = coord_22@xmin, xmax = coord_22@xmax+ 0.0045, 
                       ymin = coord_22@ymin, ymax = coord_22@ymax+ 0.0045),
                   color = "black", fill = NA, lwd = 1)

ext_5 <- subset(humedales_rurales, nombre == "Humedal Ibis")
coord_bajo_rural <- ext_5 %>%  
  as(., 'Spatial') %>% 
  extent()
rect5 <- geom_rect(aes(xmin = coord_bajo_rural@xmin, xmax = coord_bajo_rural@xmax+ 0.0045, 
                       ymin = coord_bajo_rural@ymin, ymax = coord_bajo_rural@ymax+ 0.0045),
                   color = "black", fill = NA, lwd = 1)

ext_6 <- subset(humedales_rurales, nombre == "Las Garzas")
coord_alto_rural <- ext_6 %>%  
  as(., 'Spatial') %>% 
  extent()


rect6 <- geom_rect(aes(xmin = coord_alto_rural@xmin, xmax = coord_alto_rural@xmax + 0.0045, 
                       ymin = coord_alto_rural@ymin, ymax = coord_alto_rural@ymax + 0.0045),
                   color = "black", fill = NA, lwd = 1)

zoom 

plot_local <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("antiquewhite", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_exp_st), fill = "antiquewhite", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_label(data = st_as_sf(rio_cauca),aes(label = nombre),size = 2.5)+
  geom_sf_label(data = st_as_sf(rios),aes(label = nombre),size = 2.5) 

plot_local_theme <- plot_local + ggtitle(" ") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.38, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 

ggsave(filename = "Propuestas/Propuesta2/plot_local.png",
       plot = plot_local_theme)





