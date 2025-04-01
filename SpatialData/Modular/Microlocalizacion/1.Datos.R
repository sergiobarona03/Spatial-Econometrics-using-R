
#####################
## Preparar datos  ##
#####################
library(here)
source(here::here("Modular/Microlocalizacion","0.Recod_Encuesta.R"), encoding = "UTF8")


###########################
## Cargar bases de datos ##---------------------------------------------------------------------------------------------------------
###########################
# Zona urbana como shapefile y data.frame
urbano <- shapefile('Comunas/Comunas_WGS84.shp')
urbano_st <- st_read('Comunas/Comunas_WGS84.shp')

# Zona rural como shapefile y data.frame
rural <- shapefile('Corregimientos/Corregimientos_WGS84.shp')
rural_st <- st_read('Corregimientos/Corregimientos_WGS84.shp')

# Expansión urbana como shapefile y data.frame
exp = shapefile('Planificación/Expansion.shp')
exp_st = st_read('Planificación/Expansion.shp')[c("nombre_upu", "geometry")] # conservar geometría

#############################################################
## Recuperación de variable: estrato socioeconómico (moda) ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#############################################################
# Estrato socioeconómico: zona urbana
estrato_urbano = as.data.frame(read_excel("Estrato_moda_urbano.xlsx"))# Cargar
colnames(estrato_urbano) = c("COMUNA", "Estrato") # Cambiar nombres
urbano_st = merge(urbano_st, estrato_urbano, by = "COMUNA") # Merge: geometría urbana y estrato moda

# Estrato socioeconómico: zona rural
estrato_rural = data.frame(id_correg = c(53, 52, 51), Estrato = c("Alto",
                                                                  "Bajo", 
                                                                  "Bajo")) # crear data.frame (nivel socioeconómico)
rural_st = merge(rural_st, estrato_rural, by = "id_correg", all.x = TRUE) # Merge: geometría rural y nivel socieconómico

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
encuesta <- readxl::read_excel("Datos_recod.xlsx") # cargar encuesta
humedales_encuesta <- levels(as.factor(encuesta$wetland)) # vector de humedales en la encuesta

count_encuesta <- dplyr::count(encuesta, wetland) # Conteo de encuestas por humedal
colnames(count_encuesta) = c("nombre", "n") # Cambiar nombres

humedales_rurales <- merge(humedales_rurales, count_encuesta, by = "nombre") #Merge: humedal rural y encuesta

colnames(count_encuesta) = c("nom_recod", "n")
humedales_urbanos <- merge(humedales_urbanos, count_encuesta, by = "nom_recod") #Merge: humedal urbano y encuesta


######################################################
## División de los datos según nivel socioeconómico ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################################
urbano_st$Estrato = factor(urbano_st$Estrato,
                           levels = c(1,2,3,4,5,6),
                           labels = c("Bajo", "Bajo",
                                      "Medio", "Medio",
                                      "Alto", "Alto"))  # Recodificación estrato-nivel

# Geometrías urbanas diferenciadas según nivel socioeconómico
urbano_bajo <- urbano_st %>% filter(Estrato == "Bajo")
urbano_medio <- urbano_st %>% filter(Estrato == "Medio")
urbano_alto <-  urbano_st %>% filter(Estrato == "Alto")

# Humedales urbanos diferenciados según nivel socieconómico
humedales_bajo =  subset(humedales_urbanos, Comuna %in% urbano_bajo$COMUNA)
humedales_medio =  subset(humedales_urbanos, Comuna %in% urbano_medio$COMUNA)
humedales_alto =  subset(humedales_urbanos, Comuna %in% urbano_alto$COMUNA)
humedales_alto = subset(humedales_alto, nom_recod %in% humedales_encuesta)

# Coordenadas para ubicar el número de encuestas (humedales urbanos)
urbano_sites = data.frame(Estrato = c("Bajo", "Medio",
                                      "Alto"),
                          n = c(sum(humedales_bajo@data$n),
                                sum(humedales_medio@data$n),
                                sum(humedales_alto@data$n)),
                          x = c(-76.49525,
                                -76.54468,
                                -76.53343),
                          y = c(3.42,
                                3.41549,
                                3.37728))

# Humedales rurales diferenciados según nivel socieconómico
humedales_rurales_bajos <- subset(humedales_rurales, !is.na(n))
humedales_rurales_bajos <- subset(humedales_rurales_bajos, nombre != "Las Garzas")
humedales_rurales_bajos <- humedales_rurales_bajos[c(1,3:7),]
humedales_rurales_altos <- subset(humedales_rurales, nombre == "Las Garzas")

# Coordenadas para ubicar el número de humedales (rurales)
rural_sites <- data.frame(c("Bajo", 
                           "Alto"),
                         n = c(sum(humedales_rurales_bajos@data$n),
                               sum(humedales_rurales_altos@data$n)),
                         x = c(-76.48294,
                               -76.590203),
                         y = c(3.35099,
                               3.326778))
urbano_sites$Area <-  rep("Urbana", nrow(urbano_sites))
rural_sites$Area <-  rep("Rural", nrow(rural_sites))
sites <- rbind(urbano_sites[c("Area", "n", "x", "y")],
               rural_sites[c("Area", "n", "x", "y")])


# Definición de las geometrías fill y NA
urbano_bajo_fill <- subset(urbano_bajo, COMUNA %in% humedales_bajo$Comuna) 
urbano_bajo_na <- subset(urbano_bajo, !COMUNA %in% humedales_bajo$Comuna) 

urbano_medio_fill <- subset(urbano_medio, COMUNA %in% humedales_medio$Comuna) 
urbano_medio_na <- subset(urbano_medio, !COMUNA %in% humedales_medio$Comuna)

urbano_alto_fill <- subset(urbano_alto, COMUNA %in% humedales_alto$Comuna) 
urbano_alto_na <- subset(urbano_alto, !COMUNA %in% humedales_alto$Comuna)


###############################################################
## Paleta de colores: diferencias según nivel socioeconómico ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###############################################################
palette_ses <- hcl.colors(6, "OrYel")
palette_ses <-  palette_ses[2:6]
col_point <- alpha("red", 0.65)


