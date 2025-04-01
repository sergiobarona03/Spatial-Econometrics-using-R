
nom_dpto <-  readxl::read_excel("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\NBI_DPTO_CNPV2018.xlsx") %>%
  dplyr::select(cod, dpto) %>% rename(cod_dpto = cod,
                                      nom_dpto  = dpto)

# Municipios

pob <- readxl::read_excel("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\Poblacion_CNPV2018.xlsx")
pob$cod_dpto <- as.numeric(substr(pob$mun, 1, 2))
pob$cod_mun <- as.numeric(substr(pob$mun, 3, 5))
pob$cod <- as.numeric(substr(pob$mun, 1, 5))
pob$nom_mun <- as.character(substr(pob$mun, 7, nchar(pob$mun)))

# Recuperar el nombre del departamento
pob <- pob %>% left_join(nom_dpto, by = "cod_dpto")

pob <- pob %>% dplyr::select(cod_dpto, nom_dpto, cod_mun, cod, nom_mun, n)

writexl::write_xlsx(pob,
                    "C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\Pob_MUN_CNPV2018.xlsx")

# Departamentos
pob_dpto <- readxl::read_excel("C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\Pob_MUN_CNPV2018.xlsx")
pob_dpto <- pob_dpto %>% group_by(cod_dpto) %>% summarize(
  cod_dpto = first(cod_dpto),
  nom_dpto = first(nom_dpto),
  n = sum(n, na.rm = T)
)

writexl::write_xlsx(pob_dpto,
                    "C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\Pob_DPTO_CNPV2018.xlsx")

# Extensión de departamentos y municipios
mun_ext = st_read(dsn = "C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\SpatialData\\DANE_geodata\\",
        layer = "MGN_ANM_MPIOS") %>% janitor::clean_names() %>%
  dplyr::select(dpto_ccdgo , mpio_ccdgo, mpio_cnmbr,
         area) %>% as.data.frame()

mun_ext$area_km = mun_ext$area/1000000

# Área por departamento
dpto_ext = mun_ext %>% group_by(dpto_ccdgo) %>%
  summarise(area_km = sum(area_km)) %>%
  rename(cod_dpto = dpto_ccdgo) 
dpto_ext$cod_dpto = as.numeric(dpto_ext$cod_dpto)

dpto_ext <- dpto_ext %>% left_join(nom_dpto, by = "cod_dpto")
dpto_ext <- dpto_ext %>% dplyr::select(cod_dpto, nom_dpto, area_km)

writexl::write_xlsx(dpto_ext,
                    "C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\AREA_DPTO_COL.xlsx")

# Área por municipio
mun_ext <- mun_ext %>% dplyr::select(dpto_ccdgo,
                              mpio_ccdgo, 
                              mpio_cnmbr, area_km) %>%
  rename(cod_dpto = dpto_ccdgo,
         cod_mun = mpio_ccdgo,
         nom_mun = mpio_cnmbr)

mun_ext$cod_dpto = as.numeric(mun_ext$cod_dpto)
mun_ext <- mun_ext %>% left_join(nom_dpto, by = "cod_dpto")
writexl::write_xlsx(mun_ext,
                    "C:\\Users\\Portatil\\Desktop\\Spatial Econometrics using R\\CNPV_2018\\AREA_MUN_COL.xlsx")

# Tasa de desempleo


