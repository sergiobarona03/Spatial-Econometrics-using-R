

############################################
## Valoración monetaria y no-monetaria    ##
############################################
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
library(dplyr)
library(plyr)
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

#######################################################
## Recuperación de la variable: nivel socioeconómico ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################################
estrato_urbano = readxl::read_excel("Estrato_moda_urbano.xlsx")
estrato_urbano = as.data.frame(estrato_urbano)
colnames(estrato_urbano) = c("COMUNA", "Estrato")
urbano_st = merge(urbano_st, estrato_urbano, by = "COMUNA")

estrato_rural = data.frame(id_correg = c(53, 52, 51), Estrato = c("Alto",
                                                                  "Bajo", 
                                                                  "Bajo"))
rural_st = merge(rural_st, estrato_rural, by = "id_correg",
                 all.x = TRUE)

###########################################
###########################################
## Cargar: bases de datos de la encuesta ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###########################################
###########################################
encuesta <- readxl::read_excel("Datos_recod.xlsx")
colnames(encuesta)[3] <- "nombre"

# Nota: el filtro anterior sólo se usará para los humedales ubicados en el sur de la ciudad
humedales_rurales <- shapefile('Humedales/Humedales_Rurales_WGS84.shp')
humedales_rurales_st <- st_read('Humedales/Humedales_Rurales_WGS84.shp')

humedales_urbanos <- shapefile('Humedales/Humedales_urbanos_WGS84_Corregido.shp')
humedales_urbanos_st <- st_read('Humedales/Humedales_urbanos_WGS84_Corregido.shp')

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

humedales_urbanos_encuesta <- humedales_urbanos@data[c("nom_recod", "Comuna")]
colnames(humedales_urbanos_encuesta) <- c("nombre", "comuna")
humedales_urbanos_encuesta <- merge(humedales_urbanos_encuesta,
                           encuesta, by = "nombre")


encuesta_rural <- encuesta %>% filter(nombre %in% levels(as.factor(humedales_rurales$nombre)))

humedales_rurales_comuna <- data.frame(nombre = c("Las Garzas",
                                           "Humedal Ibis",
                                           "Humedal Pacheco",
                                           "Laguna El Sombrerito",
                                           "Madrevieja La Pailita",
                                           "Reservorio Agricola"),
                                comuna = c(53,51,51,52,52,52))
humedales_rurales_comuna <- merge(encuesta_rural, humedales_rurales_comuna, by = "nombre")

humedales_urbanos_rbind <- humedales_urbanos_encuesta[c("ID", "nombre", "comuna", "TCiES18", "TCiES19",
                                         "TCiES20", "TCiES21", "TCiES22", "TCiES23", "es18", "es19","es20","es21" , "es22" )]
humedales_rurales_comuna_rbind <- humedales_rurales_comuna[c( "ID","nombre", "comuna", "TCiES18", "TCiES19",
                                         "TCiES20", "TCiES21", "TCiES22", "TCiES23", "es18", "es19","es20","es21" , "es22" )]

humedales_totales <- rbind(humedales_urbanos_rbind, humedales_rurales_comuna_rbind)

################################################
## Resumen descriptivo (media): base de datos ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
################################################
for (k in 1:nrow(humedales_totales)) {
  humedales_totales$mean_mon[k] <- mean(c(humedales_totales$TCiES18[k],
                                       humedales_totales$TCiES19[k],
                                       humedales_totales$TCiES20[k],
                                       humedales_totales$TCiES21[k],
                                       humedales_totales$TCiES22[k],
                                       humedales_totales$TCiES23[k]), na.rm = TRUE)
  
  humedales_totales$mean_nomon[k] <- mean(c(humedales_totales$es18[k],
                                         humedales_totales$es19[k],
                                         humedales_totales$es20[k],
                                         humedales_totales$es21[k],
                                         humedales_totales$es22[k]), na.rm = TRUE)
  
}
humedales_totales = humedales_totales %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
valor_monetario_humedal_comuna <- humedales_totales %>% dplyr::group_by(nombre, comuna) %>% dplyr::summarize(mean_monetario = mean(mean_mon, 
                                                                                            na.rm = TRUE)) %>% as.data.frame()
valor_no_monetario_humedal_comuna <-humedales_totales %>% dplyr::group_by(nombre, comuna) %>% dplyr::summarize(mean_no_monetario = mean(mean_nomon,
                                                                                              na.rm = TRUE))
valor_monetario_humedal_comuna <- valor_monetario_humedal_comuna %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
valor_monetario_humedal_comuna <- na.omit(valor_monetario_humedal_comuna)


valor_monetario_comuna <- humedales_totales %>% dplyr::group_by(comuna) %>% dplyr::summarize(mean_monetario = mean(mean_mon, 
                                                                                                                           na.rm = TRUE)) %>% as.data.frame()
valor_no_monetario_comuna <- humedales_totales %>% dplyr::group_by(comuna) %>%  dplyr::summarize(mean_no_monetario = mean(mean_nomon,
                                                                                                                             na.rm = TRUE)) %>% as.data.frame()
valor_monetario_comuna <- valor_monetario_comuna %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
valor_monetario_comuna <- na.omit(valor_monetario_comuna)


##############################
## Recuperar las geometrías ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##############################
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
colnames(exp_st) = c("id_correg", "comuna", "geometry")
colnames(urbano_st) = c("id", "comuna", "nombre", "shape_leng",
                       "shape_area", "area_ha", "area_km2", "geometry")
colnames(rural_st) = c("comuna", "corregi", "geometry")

# Añadir valoración monetaria y no monetaria
urbano_st <- merge(urbano_st, valor_monetario_comuna, by = "comuna", all.x = TRUE)
urbano_st <- merge(urbano_st, valor_no_monetario_comuna, by = "comuna", all.x = TRUE)

rural_st <- merge(rural_st, valor_monetario_comuna, by = "comuna", all.x = TRUE)
rural_st <- merge(rural_st, valor_no_monetario_comuna, by = "comuna", all.x = TRUE)

urbano_st_fill <- na.omit(urbano_st)
rural_st_fill <- na.omit(rural_st)

rios = shapefile('Rios Cali/Rios Cali_WGS84.shp')
rios_st = st_read('Rios Cali/Rios Cali_WGS84.shp')

rio_cauca = shapefile('Rio Cauca/Rio Cauca_WGS84.shp')
rio_cauca_st = st_read('Rio Cauca/Rio Cauca_WGS84.shp')

zoom_monetario <- urbano_st %>%  
  as(., 'Spatial') %>% 
  extent() 

plot_monetario <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), 
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st_fill), aes(fill = mean_monetario)) +
  geom_sf(data = st_as_sf(rural_st), fill = "wheat2", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_st_fill), aes(fill = mean_monetario)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_text(data = st_as_sf(urbano_st),aes(label = nombre),size = 1.5) +
  geom_sf_text(data = st_as_sf(rural_st),aes(label = corregi),size = 1.5) +
  scale_fill_gradient(low = "palegreen3", high = "darkgreen") +
  coord_sf(xlim = c(zoom_monetario@xmin,zoom_monetario@xmax), 
           ylim = c(zoom_monetario@ymin - 0.05, zoom_monetario@ymax - 0.05), expand = FALSE)


plot_monetario_theme <- plot_monetario + ggtitle(" ") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.45, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(4, 4, 4, 4),
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
                         width = unit(2, "cm")) +
  guides(fill=guide_legend(title="Individual´s travel cost (COP)"))

ggsave(filename = "Propuestas/Propuesta3/plot_monetario.png",
       plot = plot_monetario_theme)


zoom_no_monetario <- urbano_st %>%  
  as(., 'Spatial') %>% 
  extent() 


plot_no_monetario <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), 
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st_fill), aes(fill = mean_no_monetario)) +
  geom_sf(data = st_as_sf(rural_st), fill = "wheat2", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_st_fill), aes(fill = mean_no_monetario)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_text(data = st_as_sf(urbano_st),aes(label = nombre),size = 1.5) +
  geom_sf_text(data = st_as_sf(rural_st),aes(label = corregi),size = 1.5)  +
  scale_fill_gradient(low = "palegreen3", high = "darkgreen") +
  coord_sf(xlim = c(zoom_no_monetario@xmin,zoom_no_monetario@xmax), 
           ylim = c(zoom_no_monetario@ymin - 0.05, zoom_no_monetario@ymax - 0.05), expand = FALSE)


plot_no_monetario_theme <- plot_no_monetario + ggtitle(" ") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.3, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(4, 4, 4, 4),
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
                         width = unit(2, "cm"))  +
  guides(fill=guide_legend(title='Escala Likert'))

ggsave(filename = "Propuestas/Propuesta3/plot_no_monetario.png",
       plot = plot_no_monetario_theme)
