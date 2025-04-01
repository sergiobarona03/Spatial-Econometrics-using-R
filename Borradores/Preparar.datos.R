

library(caret)
library(tidyverse)

#################################################
## Construir data frame final para los hogares ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#################################################
data <- readxl::read_excel("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics Using R\\EOD_Bogota_2019\\Encuesta_de_Movilidad_2019\\Archivos XLSX\\HogaresEODH2019.xlsx")

# Aplicación de factores de expansión
exp_sample <- data[c("Id_Hogar",
                     "Utam",
                     "id_rango_ingresos",
                     "Factor")]

exp_sample2 = exp_sample %>% mutate(value = 1)  %>% spread(id_rango_ingresos, 
                                                           value,  fill = 0 ) 

exp_sample2[as.character(c(1:10))] = exp_sample2[as.character(c(1:10))]*exp_sample2$Factor

# Determinar la proporción de personas de nivel de ingresos bajos
ingresos = exp_sample2 %>% group_by(Utam) %>%
  dplyr::summarize(ingresos_bajos = round(sum(c(`1`, `2`)), 0)/sum(c(`1`, `2`,
                                                                     `3`, `4`,
                                                                     `5`, `6`,
                                                                     `7`, `8`,
                                                                     `9`, `10`))*100
  )



# Determinar el tamaño promedio de cada hogar por UTAM
size = data %>% group_by(Utam) %>%
  dplyr::summarize(mean_size = weighted.mean(p7_total_personas,
                                             Factor))

df_hogar = merge(ingresos, size, by = "Utam")

##################################################
## Construir data frame final para las personas ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################################################
personas <- read.csv("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics Using R\\EOD_Bogota_2019\\Encuesta_de_Movilidad_2019\\Archivos_CSV\\PersonasEODH2019.csv",
                     sep = ";")
hogar <- read.csv("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics Using R\\EOD_Bogota_2019\\Encuesta_de_Movilidad_2019\\Archivos_CSV\\HogaresEODH2019.csv",
                  sep = ";")

########################################################
## Menor de edad: Aplicación de factores de expansión ##
########################################################
exp_sample <- personas[c("id_hogar",
                         "id_persona",
                         "p4_edad",
                         "f_exp")]
# Recuperar utam
exp_sample <- merge(exp_sample, hogar[c("Id_Hogar",
                                        "Utam")], 
                    by.x = "id_hogar", by.y = "Id_Hogar")

exp_sample$prop_menor <- NA
for (k in 1:nrow(exp_sample)) {
  if(exp_sample$p4_edad[[k]] < 18){
    exp_sample$prop_menor[[k]] = 1*exp_sample$f_exp[[k]]
  } 
}

age <- exp_sample %>% group_by(Utam) %>% dplyr::summarize(
  minor = (sum(prop_menor, na.rm = T)/sum(f_exp))*100
)


##################################################################
### Proporción de ancianos: Aplicación de factores de expansión ##
##################################################################
exp_sample3 <- personas[c("id_hogar",
                          "id_persona",
                          "p4_edad",
                          "f_exp")]
# Recuperar utam
exp_sample3 <- merge(exp_sample3, hogar[c("Id_Hogar",
                                          "Utam")], 
                     by.x = "id_hogar", by.y = "Id_Hogar")

exp_sample3$prop_elder <- NA
for (k in 1:nrow(exp_sample3)) {
  if(exp_sample3$p4_edad[[k]] > 65){
    exp_sample3$prop_elder[[k]] = 1*exp_sample3$f_exp[[k]]
  } 
}

elder <- exp_sample3 %>% group_by(Utam) %>% dplyr::summarize(
  elder = (sum(prop_elder, na.rm = T)/sum(f_exp))*100
)

###########################################################################
## Proporción de educación superior: Aplicación de factores de expansión ##
###########################################################################
exp_sample5 <- personas[c("id_hogar",
                          "id_persona",
                          "p11me_victima_agresion_viaje",
                          "f_exp")]
# Recuperar utam
exp_sample5 <- merge(exp_sample5, hogar[c("Id_Hogar",
                                          "Utam")], 
                     by.x = "id_hogar", by.y = "Id_Hogar")

exp_sample5$agresion <- NA
for (k in 1:nrow(exp_sample5)) {
  if(exp_sample5$p11me_victima_agresion_viaje[[k]] %in% c(1)){
    exp_sample5$agresion[[k]] = 1*exp_sample5$f_exp[[k]]
  } 
}

exp_sample5$noagresion <- NA
for (k in 1:nrow(exp_sample5)) {
  if(exp_sample5$p11me_victima_agresion_viaje[[k]] %in% c(2)){
    exp_sample5$noagresion[[k]] = 1*exp_sample5$f_exp[[k]]
  } 
}

exp_sample5$Utam = as.factor(exp_sample5$Utam)
agresion <- exp_sample5 %>% group_by(Utam) %>% dplyr::summarize(
  agre = (sum(agresion, na.rm = T)/sum(noagresion, na.rm = T))*100
)


###########################################################################
## Proporción de educación superior: Aplicación de factores de expansión ##
###########################################################################
exp_sample6 <- personas[c("id_hogar",
                          "id_persona",
                          "p5_id_nivel_educativo",
                          "f_exp")]
# Recuperar utam
exp_sample6 <- merge(exp_sample6, hogar[c("Id_Hogar",
                                          "Utam")], 
                     by.x = "id_hogar", by.y = "Id_Hogar")

exp_sample6$higher <- NA
for (k in 1:nrow(exp_sample6)) {
  if(exp_sample6$p5_id_nivel_educativo[[k]] %in% c(11,12,13)){
    exp_sample6$higher[[k]] = 1*exp_sample6$f_exp[[k]]
  } 
}

higher <- exp_sample6 %>% group_by(Utam) %>% dplyr::summarize(
  higher = (sum(higher, na.rm = T)/sum(f_exp))*100
)



df_personas_1 = merge(age, elder, by = "Utam")
df_personas_2 = merge(df_personas_1, agresion, by = "Utam")
df_personas_2 = merge(df_personas_2, higher, by = "Utam")


############################
## Base de datos completa ##
############################
input_dataset <- merge(df_hogar,
                       df_personas_2, by = "Utam")

#########################################################
## Se recuperan indicadores sociodemográficos por UTAM ##
#########################################################
setwd("C:/Users/Portatil/Desktop/Spatial Econometrics Using R/EOD_Bogota_2019/Anexos/Anexo D - Valores absolutos de indicadores")
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
                                 range = cell_rows(10:300)) %>% na.omit()
  colnames(data.aux)[1] = "Utam"
  data.aux$Utam = toupper(data.aux$Utam)
  utam.list[[k]] <- data.aux
  names(utam.list)[k] <- var_vector[k]
  input_dataset <- merge(input_dataset,
                         utam.list[[k]], by = "Utam",
                         all.x = T, all.y = T)
}


#####################################################
## Se recupera la variable dependiente por UTAM:   ##
##  Indicador de movilidad: tasa de viajes en SITM ##
#####################################################
setwd("C:/Users/Portatil/Desktop/Spatial Econometrics Using R/EOD_Bogota_2019/Anexos/Anexo D - Valores absolutos de indicadores")

tasa1 <-readxl::read_excel("03_Anexo D_Movilidad.xlsx",
                           sheet = 16,
                           range = cell_rows(10:300)) %>% na.omit()
colnames(tasa1)[1] = "Utam"
tasa1$Utam = toupper(tasa1$Utam)

tasa2 <-readxl::read_excel("03_Anexo D_Movilidad.xlsx",
                           sheet = 31,
                           range = cell_rows(10:300)) %>% na.omit()
colnames(tasa2)[1] = "Utam"
tasa2$Utam = toupper(tasa2$Utam)

# Añadir a dataset_input
input_dataset = merge(input_dataset,
                      tasa1, by = "Utam")
input_dataset = merge(input_dataset,
                      tasa2, by = "Utam")

setwd("C:/Users/Portatil/Desktop/Spatial Econometrics Using R/")
writexl::write_xlsx(input_dataset, "Final.input.dataset.xlsx")


