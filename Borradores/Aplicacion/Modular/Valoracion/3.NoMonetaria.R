
#######################################################
## Valoración monetaria: humedales urbanos y rurales ##
#######################################################
library(here)
source(here::here("Modular/Valoracion","2.Monetaria.R"), encoding = "UTF8")

# Calcular valor no monetario medio según humedal y comuna
valor_no_monetario_humedal_comuna <-humedales_totales %>% dplyr::group_by(nombre, comuna) %>% dplyr::summarize(mean_no_monetario = mean(mean_es,
                                                                                                                                        na.rm = TRUE))
# Calcular valor no-monetario según comuna
valor_no_monetario_comuna <- humedales_totales %>% dplyr::group_by(comuna) %>%  dplyr::summarize(mean_no_monetario = mean(mean_es,
                                                                                                                          na.rm = TRUE)) %>% as.data.frame()
# Añadir valor no-monetario a las geometrías
urbano_st_nm <- merge(urbano_st, valor_no_monetario_comuna, by = "comuna", all.x = TRUE)
rural_st_nm <- merge(rural_st, valor_no_monetario_comuna, by = "comuna", all.x = TRUE)

urbano_st_fill_nm <- na.omit(urbano_st_nm)
rural_st_fill_nm <- na.omit(rural_st_nm)


##############################################
## Figura: Valoración no-monetaria (Likert) ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##############################################

# Figura: valoración no-monetario by 10-point Likert Scale
zoom_no_monetario <- urbano_st %>%  
  as(., 'Spatial') %>% 
  extent() 

levels(as.factor(urbano_st_fill_nm$mean_no_monetario))


plot_no_monetario <- ggplot() + 
  geom_sf(data = st_as_sf(urbano_st_nm), fill = alpha("wheat2", 0.5), 
          col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st_fill_nm), aes(fill = mean_no_monetario)) +
  geom_sf(data = st_as_sf(rural_st_nm), fill = "wheat2", col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_st_fill_nm), aes(fill = mean_no_monetario)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_sf_text(data = st_as_sf(urbano_st),aes(label = NOMBRE),size = 1.5) +
  geom_sf_text(data = st_as_sf(rural_st),aes(label = corregimie),size = 1.5)  +
  scale_fill_gradient(low = "white", high = "darkgreen",
                      limits = c(1,10),
                      breaks = c(1, 5, 10),
                        labels = c("Completely unimportant", "Neutral", "Completely important")) +
  coord_sf(xlim = c(zoom_no_monetario@xmin,zoom_no_monetario@xmax), 
           ylim = c(zoom_no_monetario@ymin - 0.05, zoom_no_monetario@ymax - 0.05), expand = FALSE)


plot_no_monetario_theme <- plot_no_monetario + ggtitle("Non monetary valuation of cultural ecosystem services") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.98, 0.2),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(8, 6, 10, 6),
        legend.key.size = unit(0.25, 'cm'), #change legend key size
        legend.key.height = unit(0.30, 'cm'), #change legend key height
        legend.key.width = unit(0.18, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid'),
        panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  annotation_scale(location = "bl")+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))  +
  guides(fill=guide_colourbar(title='Likert scale'))

ggsave(filename = "Propuestas/Propuesta3/plot_no_monetario.png",
       plot = plot_no_monetario_theme,
       width = 7, height = 7)
