
##############################
## Figura 2.1: Urbano bajo  ##
##############################
library(here)
source(here::here("Modular/Microlocalizacion","1.Datos.R"), encoding = "UTF8")

zoom_bajo <- urbano_bajo %>% filter(COMUNA %in% humedales_bajo$Comuna)

coord_bajo <- zoom_bajo %>%  
  as(., 'Spatial') %>% 
  extent() 

urbano_micro_bajo <- ggplot() + 
  geom_sf(data = st_as_sf(rural_st), fill = alpha("wheat2"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_bajo_fill), fill = palette_ses[1], col = "black") +
  geom_sf(data = st_as_sf(urbano_bajo_na), fill = alpha("wheat2", 0.5), col = "black") +
  geom_sf(data = st_as_sf(humedales_bajo), aes(fill = nom_recod, col = nom_recod), lwd = 4) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_rect(aes(xmin = coord_bajo@xmin + 0.008, xmax = coord_bajo@xmax, 
                ymin = coord_bajo@ymin + 0.003, ymax = coord_bajo@ymax),
            color = "black", fill = NA, lwd = 2) +
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_bajo),aes(label = NOMBRE),size = 2.8) +
  geom_point(data = urbano_sites, aes(x = x, y = y), size = 27, 
             shape = 21, fill = col_point, col = "black") +
  geom_point(data = rural_sites, aes(x = x, y = y), size = 27, 
             shape = 22, fill = col_point, col = "black") +
  geom_text(data = urbano_sites, aes(x = x, y = y, label = n), size = 6) +
  geom_text(data = rural_sites, aes(x = x, y = y, label = n), size = 6) +
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


ggsave(filename = "Propuestas/Propuesta3/urbano_micro_bajo.png",
       plot = urbano_micro_bajo_theme,width = 7,
      height = 7)
