
#####################
## Preparar datos  ##
#####################

######################
## Cargar librerías ##---------------------------------------------------------------------------------------------------------
######################
library(readxl)
library(gdata)
library(cartography)
library(lubridate)
library(raster)
library(stringr)
library(tidyverse)
library(sf)
library(ggspatial)
library(ellipse)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggplot2)
library(terra)
library(RColorBrewer)

library(here)
source(here::here("Modular/Valoracion","0.Recod_Encuesta.R"), encoding = "UTF8")

###########################
## Cargar bases de datos ##---------------------------------------------------------------------------------------------------------
###########################
# Zona urbana como shapefile y data.frame
urbano <- shapefile('Comunas/Comunas_WGS84.shp')
urbano_st <- st_read('Comunas/Comunas_WGS84.shp')
colnames(urbano_st)[colnames(urbano_st) == "COMUNA"] = "comuna"

# Zona rural como shapefile y data.frame
rural <- shapefile('Corregimientos/Corregimientos_WGS84.shp')
rural_st <- st_read('Corregimientos/Corregimientos_WGS84.shp')
colnames(rural_st)[colnames(rural_st) == "id_correg"] = "comuna"

# Expansión urbana como shapefile y data.frame
exp = shapefile('Planificación/Expansion.shp')
exp_st = st_read('Planificación/Expansion.shp')[c("nombre_upu", "geometry")] # conservar geometría

#######################
##  Ríos y río Cauca ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################
# Cargar ríos como shapefile y data.frame
rios = shapefile('Rios Cali/Rios Cali_WGS84.shp')
rios_st = st_read('Rios Cali/Rios Cali_WGS84.shp')

# Cargar río Cauca como shapefile y data.frame
rio_cauca = shapefile('Rio Cauca/Rio Cauca_WGS84.shp')
rio_cauca_st = st_read('Rio Cauca/Rio Cauca_WGS84.shp')

#######################
## Humedales urbanos ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################

# cargar shapefile y data.frame
humedales_urbanos <- shapefile('Humedales/Humedales_urbanos_WGS84_Corregido.shp')
humedales_urbanos_st <- st_read('Humedales/Humedales_urbanos_WGS84_Corregido.shp')

# Recodificación
humedales_urbanos$nom_recod = humedales_urbanos$HumNom
humedales_urbanos$nom_recod[humedales_urbanos$nom_recod == "Humedal Batallón"] = "Batallón"
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

#######################
## Humedales rurales ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################

# Cargar shapefile y data.frame
humedales_rurales <- shapefile('Humedales/Humedales_Rurales_WGS84.shp')
humedales_rurales_st <- st_read('Humedales/Humedales_Rurales_WGS84.shp')

#######################
## Datos: encuesta   ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################
# Cargar encuesta y cambio del nombre (wetland-nombre)
encuesta <- readxl::read_excel("Datos_recod.xlsx")
colnames(encuesta)[which(colnames(encuesta) == "wetland")] <- "nombre"

# Merge: humedales urbanos y la encuesta
humedales_urbanos_encuesta <- humedales_urbanos@data[c("nom_recod", "Comuna")]
colnames(humedales_urbanos_encuesta) <- c("nombre", "comuna")
humedales_urbanos_encuesta <- merge(humedales_urbanos_encuesta,
                                    encuesta, by = "nombre")

# Merge: humedales rurales y la encuesta
encuesta_rural <- encuesta %>% filter(nombre %in% levels(as.factor(humedales_rurales$nombre)))
humedales_rurales_comuna <- data.frame(nombre = c("Las Garzas",
                                                  "Humedal Ibis",
                                                  "Humedal Pacheco",
                                                  "Laguna El Sombrerito",
                                                  "Madrevieja La Pailita",
                                                  "Reservorio Agricola"),
                                       comuna = c(53,51,51,52,52,52))
humedales_rurales_comuna <- merge(encuesta_rural, humedales_rurales_comuna, by = "nombre")

# Los humedales totales: rbind entre humedales urbanos y rurales
humedales_urbanos_rbind <- humedales_urbanos_encuesta[c("ID", "nombre", "comuna", "TCi_usd", "mean_es")]
humedales_rurales_comuna_rbind <- humedales_rurales_comuna[c( "ID","nombre", "comuna", "TCi_usd", "mean_es")]

humedales_totales <- rbind(humedales_urbanos_rbind, humedales_rurales_comuna_rbind)
