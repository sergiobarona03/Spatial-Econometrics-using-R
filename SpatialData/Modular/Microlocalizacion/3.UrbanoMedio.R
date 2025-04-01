###############################
## Figura 2.2: Urbano medio  ##
###############################
library(here)
source(here::here("Modular/Microlocalizacion","2.UrbanoBajo.R"), encoding = "UTF8")

zoom_medio <- urbano_medio %>% filter(COMUNA %in% humedales_medio$Comuna)

coord_medio <- zoom_medio %>%  
  as(., 'Spatial') %>% 
  extent() 

urbano_sites_medio = urbano_sites %>% filter(Estrato == "Medio")

urbano_micro_medio <- ggplot() + 
  geom_sf(data = st_as_sf(rural_st), fill = alpha("wheat2"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_medio_fill), fill = palette_ses[2], col = "black") +
  geom_sf(data = st_as_sf(urbano_medio_na), fill = alpha("wheat2", 0.5), col = "black") +
  geom_sf(data = st_as_sf(humedales_medio), aes(fill = nom_recod, col = nom_recod), lwd = 4) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_point(data = urbano_sites_medio, aes(x = x, y = y), size = 27, 
             shape = 21, fill = col_point, col = "black") +
  geom_point(data = rural_sites, aes(x = x, y = y), size = 27, 
             shape = 22, fill = col_point, col = "black") +
  geom_rect(aes(xmin = coord_medio@xmin, xmax = coord_medio@xmax, 
                ymin = coord_medio@ymin - 0.004, ymax = coord_medio@ymax - 0.025),
            color = "black", fill = NA, lwd = 2) +
  geom_text(data = urbano_sites_medio, aes(x = x, y = y, label = n), size = 6) +
  geom_text(data = rural_sites, aes(x = x, y = y, label = n), size = 6) +
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_medio),aes(label = NOMBRE),size = 2.8) +
  coord_sf(xlim = c(coord_medio@xmin,coord_medio@xmax), 
           ylim = c(coord_medio@ymin - 0.004, coord_medio@ymax - 0.025), expand = FALSE) + 
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")) + 
  scale_color_manual(values = brewer.pal(n = 4, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

urbano_micro_medio_theme <- urbano_micro_medio + ggtitle("Humedales urbanos (Estrato medio)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.96, 0.2),
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
  annotation_scale(location = "bl")+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))

ggsave(filename = "Propuestas/Propuesta3/urbano_micro_medio.png",
       plot = urbano_micro_medio_theme, width = 7,
       height = 7)
