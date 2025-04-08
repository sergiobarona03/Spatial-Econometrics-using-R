# Cargar librerías
library(tidyverse)
library(stringi)
library(janitor)

# Función de normalización avanzada
normalizar_nombres <- function(x) {
  x %>%
    toupper() %>%
    trimws() %>%
    stri_trans_general("Latin-ASCII") %>%
    gsub("\\s*\\(.*?\\)", "", .) %>%  # Eliminar contenido entre paréntesis
    gsub("Ñ", "N", .) %>%  # Mantener Ñ como única letra especial
    gsub("[^A-Z0-9 ]", "", .) %>%  # Eliminar caracteres especiales
    gsub("\\s+", " ", .) %>%  # Normalizar espacios múltiples
    gsub(" $", "", .)  # Eliminar espacio final
}

# Mapeo completo de municipios
mapeo_municipios <- c(
  "LA VICTORIA" = "LA VICTORIA (PACOA)",
  "MIRITI PARANA" = "MIRITI-PARANA (CAMPOAMOR)",
  "MIRITIPARANA" = "MIRITI-PARANA (CAMPOAMOR)",
  "PUERTO NARINO" = "PUERTO NARIÑO",
  "SAN ANDRES DE CUERQUIA" = "SAN ANDRÉS DE CUERQUÍA",
  "SANTAFE DE ANTIOQUIA" = "SANTA FE DE ANTIOQUIA",
  "PROVIDENCIA" = "PROVIDENCIA Y SANTA CATALINA",
  "RIO VIEJO" = "RÍO VIEJO",
  "RIOVIEJO" = "RÍO VIEJO",
  "TIQUISO" = "TIQUISIO",
  "LABRANZA GRANDE" = "LABRANZAGRANDE",
  "GUACHENE" = "GUACHENÉ",
  "GUACHENE " = "GUACHENÉ",
  "PURACE" = "PURACÉ (COCONUCO)",
  "BECERRILL" = "BECERRIL",
  "MANAURE BALCON DEL CESAR" = "MANAURE",
  "BOJAYA" = "BOJAYÁ (BELLAVISTA)",
  "BOJAYA BELLAVISTA" = "BOJAYÁ (BELLAVISTA)",
  "EL CARMEN" = "EL CARMEN DE ATRATO",
  "RIO IRO" = "RÍO IRÓ (SANTA RITA)",
  "UNION PANAMERICANA" = "UNIÓN PANAMERICANA (ANIMAS)",
  "CERETE" = "CERETÉ",
  "CHIMA" = "CHIMÁ",
  "CHINU" = "CHINÚ",
  "CIENAGA DE ORO" = "CIÉNAGA DE ORO",
  "MONITOS" = "MOÑITOS",
  "MONTELIBANO" = "MONTELÍBANO",
  "MONTERIA" = "MONTERÍA",
  "PURISIMA" = "PURÍSIMA",
  "SAHAGUN" = "SAHAGÚN",
  "SAN ANDRES DE SOTAVENTO" = "SAN ANDRÉS SOTAVENTO",
  "SAN JOSE DE URE" = "SAN JOSÉ DE URÉ",
  "TUCHIN" = "TUCHÍN",
  "UBATE" = "VILLA DE SAN DIEGO DE UBATE",
  "VILLA GOMEZ" = "VILLAGÓMEZ",
  "BARRANCO MINA" = "BARRANCO MINAS (ANM)",
  "PANA PANA" = "PANÁ-PANÁ (CAMPO ALEGRE)",
  "CERRO DE SAN ANTONIO" = "CERRO SAN ANTONIO",
  "PUEBLO VIEJO" = "PUEBLOVIEJO",
  "SAN ANDRES DE TUMACO" = "TUMACO",
  "SANTA CRUZ" = "SANTA CRUZ (GUACHAVÉS)",
  "LEGUIZAMO" = "PUERTO LEGUÍZAMO",
  "JORDAN" = "JORDÁN SUBE",
  "COLOSO" = "RICAURTE (COLOSO)",
  "RICAURTE" = "RICAURTE (COLOSO)",
  "MARIQUITA" = "SAN SEBASTIÁN DE MARIQUITA",
  "GUADALAJARA DE BUGA" = "BUGA",
  "SAN ANDRES DE CUERQUIA" = "SAN ANDRÉS DE CUERQUÍA",
  "RIO VIEJO" = "RÍO VIEJO",
  "EL PEÑON" = "EL PEÑÓN",
  "TUMACO" = "SAN ANDRÉS DE TUMACO",
  "BARRANCO MINAS" = "BARRANCO MINAS (ANM)",
  "GUACHENE" = "GUACHENÉ",
  "INZA" = "INZÁ",
  "PAEZ" = "PÁEZ",
  "MONTELIBANO" = "MONTELÍBANO",
  "PUERTO LEGUIZAMO" = "PUERTO LEGUÍZAMO",
  "VILLA GOMEZ" = "VILLAGÓMEZ",
  "SAN SEBASTIAN DE MARIQUITA" = "SAN SEBASTIÁN DE MARIQUITA",
  "JORDAN SUBE" = "JORDÁN SUBE",
  "PANAPANA" = "PANÁ-PANÁ (CAMPO ALEGRE)",
  "PROVIDENCIA" = "PROVIDENCIA Y SANTA CATALINA",
  
)

# Mapeo de departamentos
mapeo_departamentos <- c(
  "CORDOBA" = "CÓRDOBA",
  "NARINO" = "NARIÑO",
  "QUINDIO" = "QUINDÍO",
  "CHOCO" = "CHOCÓ",
  "GUAVIARE" = "GUAVIARE",
  "VAUPES" = "VAUPÉS"
)

# Cargar y preparar datos DIVIPOLA
divipola <- readxl::read_excel("NBI_MUN_CNPV2018.xlsx") %>%
  select(cod_dpto, nom_dpto, cod_mun, nom_mun, cod) %>%
  mutate(
    nom_mun_norm = normalizar_nombres(nom_mun),
    nom_dpto_norm = normalizar_nombres(nom_dpto) %>%
      recode(!!!mapeo_departamentos)
  ) %>%
  mutate(nom_mun_norm = recode(nom_mun_norm, !!!mapeo_municipios))

# Cargar y preparar datos de ocupación
ocup <- readxl::read_excel("oc_mun_dane.xlsx") %>%
  janitor::clean_names() %>%
  select(departamento, municipio, percent_en_el_municipio) %>%
  rename(
    nom_mun = municipio,
    nom_dpto = departamento
  ) %>%
  mutate(
    nom_mun_norm = normalizar_nombres(nom_mun),
    nom_dpto_norm = normalizar_nombres(nom_dpto) %>%
      recode(!!!mapeo_departamentos)
  ) %>%
  mutate(nom_mun_norm = recode(nom_mun_norm, !!!mapeo_municipios))

# Realizar el merge final
ocup_cods <- merge(
  ocup %>% distinct(nom_dpto_norm, nom_mun_norm, .keep_all = TRUE),
  divipola %>% distinct(nom_dpto_norm, nom_mun_norm, .keep_all = TRUE),
  by = c("nom_dpto_norm", "nom_mun_norm"),
  all = TRUE
) %>%
  arrange(nom_dpto_norm, nom_mun_norm)

# Identificar y reportar casos faltantes
problemas_final <- ocup_cods %>%
  filter(is.na(cod_mun) | is.na(percent_en_el_municipio)) %>%
  select(nom_dpto_norm, nom_mun_norm) %>%
  distinct()

if(nrow(problemas_final) > 0) {
  message("Casos que requieren atención manual (", nrow(problemas_final), "):")
  print(problemas_final)
} else {
  message("¡Merge completado exitosamente! Todas las coincidencias encontradas.")
}


