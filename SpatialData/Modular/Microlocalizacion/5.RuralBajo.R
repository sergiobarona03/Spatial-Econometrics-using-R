###############################
## Figura 2.4: Rural bajo    ##
###############################
library(here)
source(here::here("Modular/Microlocalizacion","4.UrbanoAlto.R"), encoding = "UTF8")
rural_bajo <- rural_st %>% filter(Estrato == "Bajo")

coord_bajo_rural <- rural_bajo %>%  
  as(., 'Spatial') %>% 
  extent() 

rurales_micro_bajo <- ggplot()  + 
  geom_sf(data = st_as_sf(rural_st), fill = alpha("wheat2"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) + 
  geom_sf(data = st_as_sf(rural_bajo), fill = palette_ses[4], col = "black") +
  geom_sf(data = st_as_sf(humedales_rurales_bajos), 
          aes(fill = nombre, col = nombre), lwd = 4) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") + 
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(rural_bajo),aes(label = corregimie),size = 2.8) +
  geom_point(data = urbano_sites_alto, aes(x = x, y = y), size = 27, 
             shape = 21, fill = col_point, col = "black") +
  geom_point(data = rural_sites, aes(x = x, y = y), size = 27, 
             shape = 22, fill = col_point, col = "black") +
  geom_rect(aes(xmin = coord_bajo_rural@xmin + 0.006, xmax = coord_bajo_rural@xmax + 0.04, 
                ymin = coord_bajo_rural@ymin, ymax = coord_bajo_rural@ymax - 0.03),
            color = "black", fill = NA, lwd = 2) +
  geom_text(data = urbano_sites_alto, aes(x = x, y = y, label = n), size = 6) +
  geom_text(data = rural_sites, aes(x = x, y = y, label = n), size = 6) +
  coord_sf(xlim = c(coord_bajo_rural@xmin + 0.006,coord_bajo_rural@xmax + 0.04), 
           ylim = c(coord_bajo_rural@ymin, coord_bajo_rural@ymax - 0.03), expand = FALSE) + 
  scale_fill_manual(values = hcl.colors(20, "Greens")) + 
  scale_color_manual(values = hcl.colors(20, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

rurales_micro_bajo_theme <- rurales_micro_bajo + ggtitle("Humedales rurales (Estrato bajo)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.99, 0.35),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.38, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid')) +
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 

ggsave(filename = "Propuestas/Propuesta3/rurales_micro_bajo.png",
       plot = rurales_micro_bajo_theme,
       width = 7, height = 7)
