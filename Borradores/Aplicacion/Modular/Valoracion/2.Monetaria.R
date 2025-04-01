

#######################################################
## Valoración monetaria: humedales urbanos y rurales ##
#######################################################
library(here)
source(here::here("Modular/Valoracion","1.Datos.R"), encoding = "UTF8")

################################################
## Resumen descriptivo (media): base de datos ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
################################################
#Valor monetario según humedales y comunas
humedales_totales = humedales_totales %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
valor_monetario_humedal_comuna <- humedales_totales %>% dplyr::group_by(nombre, comuna) %>% dplyr::summarize(mean_monetario = mean(TCi_usd, 
                                                                                                                                   na.rm = TRUE)) %>% as.data.frame()
# Reemplazar NaN-NA (eliminar missing values)
valor_monetario_humedal_comuna <- valor_monetario_humedal_comuna %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
valor_monetario_humedal_comuna <- na.omit(valor_monetario_humedal_comuna)

# Valor monetario según comunas
valor_monetario_comuna <- humedales_totales %>% dplyr::group_by(comuna) %>% dplyr::summarize(mean_monetario = mean(TCi_usd, 
                                                                                                                   na.rm = TRUE)) %>% as.data.frame()
# Reemplazar NaN-NA (eliminar missing values)
valor_monetario_comuna <- valor_monetario_comuna %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
valor_monetario_comuna <- na.omit(valor_monetario_comuna)
valor_monetario_comuna$comuna = as.numeric(valor_monetario_comuna$comuna)

# Añadir valor monetario a las geometrías
urbano_st_m <- merge(urbano_st, valor_monetario_comuna, by = "comuna", all.x = TRUE)
rural_st_m <- merge(rural_st, valor_monetario_comuna, by = "comuna", all.x = TRUE)

urbano_st_fill_m <- na.omit(urbano_st_m)
rural_st_fill_m <- na.omit(rural_st_m)

#########################################
## Figura: Valoración monetaria (USD) ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
########################################

zoom_monetario <- urbano_st %>%  
  as(., 'Spatial') %>% 
  extent() 

plot_monetario <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st_m), fill = alpha("wheat2", 0.5), 
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st_fill_m), aes(fill = mean_monetario)) +
  geom_sf(data = st_as_sf(rural_st_m), fill = "wheat2", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_st_fill_m), aes(fill = mean_monetario)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_text(data = st_as_sf(urbano_st),aes(label = NOMBRE),size = 1.5) +
  geom_sf_text(data = st_as_sf(rural_st),aes(label = corregimie),size = 1.5) + 
  scale_fill_gradient(low = "palegreen3", high = "darkgreen",
                      limits = c(1,16),
                      breaks = c(1, 4, 8, 12, 16),
                      labels = c(" ", "4.00", "8.00", "12.00", " ")) +
  coord_sf(xlim = c(zoom_monetario@xmin,zoom_monetario@xmax), 
           ylim = c(zoom_monetario@ymin - 0.05, zoom_monetario@ymax - 0.05), expand = FALSE)


plot_monetario_theme <- plot_monetario + ggtitle("Monetary valuation of cultural ecosystem services") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.25, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid'),
        panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) +
  guides(fill=guide_colourbar(title="ITC (USD)"))

ggsave(filename = "Propuestas/Propuesta3/plot_monetario.png",
       plot = plot_monetario_theme,
       width = 7, height = 7)


