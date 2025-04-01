
################################
## Preparación: base de datos ##
##    Encuesta de movilidad   ##
################################
library(readxl)
library(reshape2)
library(tidyverse)
library(sf)
library(colorspace)
library(grDevices)
library(ggpubr)

setwd("C:/Users/Portatil/Desktop/SpatialData/EOD_Bogota_2019/Anexos/Anexo D - Valores absolutos de indicadores")
#------------------------#
# Nivel 1: Municipio     #
#------------------------#
var_vector <- c("hogares", "n",
                "ingresos",
                "poblacion",
                "sexo", "edad",
                "educ", "ocup",
                "tipologia")

sheet_vector <- c(3, 7, 14, 16, 20, 
                  22, 28, 29, 57)

mun.list <- vector("list", 
                     length = length(sheet_vector))

for (k in 1:length(sheet_vector)) {
  data.aux <- readxl::read_excel("02_Anexo D_Socioeconomicos.xlsx",
                                 sheet = sheet_vector[k],
                                range = cell_rows(10:100)) %>% na.omit()
  mun.list[[k]] <- data.aux
  names(mun.list)[k] <- var_vector[k]
}



#---------------------#
# Nivel 2. Localidad  #
#---------------------#
var_vector <- c("hogares", "n",
                "ingresos",
                "poblacion", "densidad",
                "sexo", "edad",
                "educ")

sheet_vector <- c(2, 6, 13, 15, 18, 
                  21, 23, 25)

loc.list <- vector("list", 
                     length = length(sheet_vector))

for (k in 1:length(sheet_vector)) {
  data.aux <- readxl::read_excel("02_Anexo D_Socioeconomicos.xlsx",
                                 sheet = sheet_vector[k],
                                 range = cell_rows(10:100)) %>% na.omit()
  loc.list[[k]] <- data.aux
  names(loc.list)[k] <- var_vector[k]
}

#---------------------#
# Nivel 3: UTAM       #
#---------------------#
var_vector <- c("hogares","densidad",
                "motorizacion",
                "autos",
                "motos",
                "bicis")

sheet_vector <- c(4, 19, 41, 45,
                 49,53)

utam.list <- vector("list", 
                   length = length(sheet_vector))

for (k in 1:length(sheet_vector)) {
  data.aux <- readxl::read_excel("02_Anexo D_Socioeconomicos.xlsx",
                                 sheet = sheet_vector[k],
                                 range = cell_rows(10:100)) %>% na.omit()
  utam.list[[k]] <- data.aux
  names(utam.list)[k] <- var_vector[k]
}

#------------------------------#
# Recuperar .shp por municipio #
#------------------------------#
setwd("C:/Users/Portatil/Desktop/SpatialData/EOD_Bogota_2019/Zonificacion_shapefiles/ZONAS")
utam <- read_sf(dsn=getwd(), layer="UTAM")
utam <- st_zm(utam)
utam %>% head() %>% print(width = 120)

# Ajustar base de datos de ingresos, sexo, edad, educación
# ocupación y tipología
for (k in c(3,6,7,8)) {
  head(loc.list[[k]])
  loc.list[[k]] <- melt(loc.list[[k]], id.vars = "Localidad")
}

#############################################################
## Parte I: Número de hogares, tamaño promedio del hogar,  ##
## ingresos  y educación de los hogares según localidad    ##
#############################################################
# Mapa plano
utam_z <- st_zm(utam)
utam_z$LOCNombre <- tolower(utam_z$LOCNombre) %>% str_to_title()
utam_z %>% ggplot() +
  geom_sf() + theme_bw()

# El tema de las UPRs
#loc.list[[1]]$`Número de hogares*`[which(loc.list[[1]]$Localidad == "Upr")] = 1000000
#loc.list[[1]]$Localidad[which(loc.list[[1]]$Localidad == "Upr")] = "Sumapaz"

# Figura 1A
data_f1a <- utam_z %>% 
  mutate(Localidad = LOCNombre) %>%
  left_join(loc.list[[1]], by = "Localidad")

# Nota: con el left_join(), se eliminan las UPRs
# Drop NAs
data_f1a = data_f1a %>% na.omit()
f1a <- data_f1a %>% ggplot() +
  aes(fill = `Número de hogares*`) +
  geom_sf() + theme_bw() +
  theme(legend.position = c(0.15,0.88),
        legend.title = element_blank()) + labs(title = 
                                                 "Número de hogares por localidad")

# Figura B
data_f1b <- utam_z %>% 
  mutate(Localidad = LOCNombre) %>%
  left_join(loc.list[[2]], by = "Localidad")

# Nota: con el left_join(), se eliminan las UPRs
# Drop NAs
data_f1b = data_f1b %>% na.omit()
f1b <- data_f1b %>% ggplot() +
  aes(fill = `Tamaño promedio del hogar*`) +
  geom_sf() + scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.15,0.88),
        legend.title = element_blank()) +
  labs(title = "Tamaño promedio del hogar según localidad")

# Figura C
# Hallar el intervalo de ingresos moda
ing.loc <- loc.list[[3]] %>% group_by(Localidad) %>% mutate(Mode =
                                      variable[which.max(value)]) %>%
  select(c("Localidad", "Mode")) %>% distinct()

data_f1c <- utam_z %>% 
  mutate(Localidad = LOCNombre) %>%
  left_join(ing.loc, by = "Localidad")

# Nota: con el left_join(), se eliminan las UPRs
# Drop NAs
data_f1c = data_f1c %>% na.omit()
f1c <- data_f1c %>% ggplot() +
  aes(fill = `Mode`) +
  geom_sf() + theme_bw() +
  theme(legend.position = c(0.3,0.88),
        legend.title = element_blank()) +
  labs(title = "Nivel de ingresos por localidad")

# Figura D
# Hallar el intervalo de ingresos moda
educ.loc <- loc.list[[8]] %>% group_by(Localidad) %>% mutate(Mode =
                                                              variable[which.max(value)]) %>%
  select(c("Localidad", "Mode")) %>% distinct()

data_f1d <- utam_z %>% 
  mutate(Localidad = LOCNombre) %>%
  left_join(educ.loc, by = "Localidad")

# Nota: con el left_join(), se eliminan las UPRs
# Drop NAs
data_f1d = data_f1d %>% na.omit()
f1d <- data_f1d %>% ggplot() +
  aes(fill = `Mode`) +
  geom_sf() + theme_bw() +
  theme(legend.position = c(0.35,0.9),
        legend.title = element_blank()) +
  labs(title = "Nivel de educación por localidad")

ggarrange(f1a, f1b, ncol = 2, nrow = 1)
ggarrange(f1c, f1d, ncol = 2, nrow = 1)

##############################################################
## Parte II: población y densidad por localidad             ##
##############################################################

# Figura 2A
data_f2a <- utam_z %>% 
  mutate(Localidad = LOCNombre) %>%
  left_join(loc.list[[4]], by = "Localidad")

# Nota: con el left_join(), se eliminan las UPRs
# Drop NAs
data_f2a = data_f2a %>% na.omit()
f2a <- data_f2a %>% ggplot() +
  aes(fill = `Número de habitantes*`) +
  geom_sf() + scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.15,0.88),
        legend.title = element_blank()) + labs(title = 
                                                 "Número de habitantes por localidad")

# Figura 2B
data_f2b <- utam_z %>% 
  mutate(Localidad = LOCNombre) %>%
  left_join(loc.list[[5]], by = "Localidad")

# Nota: con el left_join(), se eliminan las UPRs
# Drop NAs
data_f2b = data_f2b %>% na.omit()
f2b <- data_f2b %>% ggplot() +
  aes(fill = `Número de habitantes por Km2*`) +
  geom_sf() + scale_fill_gradientn(colours = rev(
    grDevices::heat.colors(10)), name = NULL) + theme_bw() +
  theme(legend.position = c(0.15,0.88),
        legend.title = element_blank()) +
  labs(title = "Densidad poblacional según localidad")

ggarrange(f2a, f2b, ncol = 2, nrow = 1)


