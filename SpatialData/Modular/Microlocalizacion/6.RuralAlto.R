###############################
## Figura 2.5: Rural alto    ##
###############################
library(here)
source(here::here("Modular/Microlocalizacion","5.RuralBajo.R"), encoding = "UTF8")
rural_alto <-  rural_st %>% filter(Estrato == "Alto")

coord_alto_rural <- rural_alto %>%  
  as(., 'Spatial') %>% 
  extent() 

humedales_rurales_altos <- subset(humedales_rurales, nombre == "Las Garzas")


rurales_micro_alto <- ggplot()  + 
  geom_sf(data = st_as_sf(rural_st), fill = alpha("wheat2"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) + 
  geom_sf(data = st_as_sf(rural_alto), fill = palette_ses[5], col = "black") +
  geom_sf(data = st_as_sf(humedales_rurales_altos), 
          aes(fill = nombre, col = nombre), lwd = 4) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(rural_alto),aes(label = corregimie),size = 2.8) +
  geom_point(data = urbano_sites_alto, aes(x = x, y = y), size = 27, 
             shape = 21, fill = col_point, col = "black") +
  geom_point(data = rural_sites, aes(x = x, y = y), size = 27, 
             shape = 22, fill = col_point, col = "black") +
  geom_rect(aes(xmin = coord_alto_rural@xmin, xmax = coord_alto_rural@xmax, 
                ymin = coord_alto_rural@ymin, ymax = coord_alto_rural@ymax - 0.02),
            color = "black", fill = NA, lwd = 2) + 
  geom_text(data = urbano_sites_alto, aes(x = x, y = y, label = n), size = 6) +
  geom_text(data = rural_sites, aes(x = x, y = y, label = n), size = 6) +
  coord_sf(xlim = c(coord_alto_rural@xmin,coord_alto_rural@xmax), 
           ylim = c(coord_alto_rural@ymin, coord_alto_rural@ymax - 0.02), expand = FALSE) + 
  scale_fill_manual(values = hcl.colors(20, "Greens")) + 
  scale_color_manual(values = hcl.colors(20, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

rurales_micro_alto_theme <- rurales_micro_alto + ggtitle("Humedales rurales (Estrato alto)") +
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


ggsave(filename = "Propuestas/Propuesta3/rurales_micro_alto.png",
       plot = rurales_micro_alto_theme, width = 7,
       height = 7)
