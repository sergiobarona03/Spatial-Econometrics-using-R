###############################
## Figura 2.3: Urbano alto   ##
###############################
library(here)
source(here::here("Modular/Microlocalizacion","3.UrbanoMedio.R"), encoding = "UTF8")

zoom_alto <- urbano_alto %>% filter(COMUNA %in% levels(as.factor(humedales_alto$Comuna)))

coord_alto <- zoom_alto %>%  
  as(., 'Spatial') %>% 
  extent() 

urbano_sites_alto = urbano_sites %>% filter(Estrato == "Alto")

urbano_micro_alto <- ggplot() + 
  geom_sf(data = st_as_sf(rural_st), fill = alpha("wheat2"), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_alto_fill), fill = palette_ses[3], col = "black") +
  geom_sf(data = st_as_sf(urbano_alto_na), fill = alpha("wheat2", 0.5), col = "black") +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(humedales_alto), aes(fill = nom_recod, col = nom_recod), lwd = 4) +
  geom_sf(data = st_as_sf(rios), fill = "light blue",color = "light blue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "light blue",color = "light blue") +
  geom_point(data = urbano_sites_alto, aes(x = x, y = y), size = 27, 
             shape = 21, fill = col_point, col = "black") +
  geom_point(data = rural_sites, aes(x = x, y = y), size = 27, 
             shape = 22, fill = col_point, col = "black") +
  geom_rect(aes(xmin = coord_alto@xmin, xmax = coord_alto@xmax + 0.015, 
                ymin = coord_alto@ymin, ymax = coord_alto@ymax),
            color = "black", fill = NA, lwd = 2) +
  geom_text(data = urbano_sites_alto, aes(x = x, y = y, label = n), size = 6) +
  geom_text(data = rural_sites, aes(x = x, y = y, label = n), size = 6) +
  geom_sf_label(data = st_as_sf(rio_cauca_st),aes(label = nombre),size = 2) +
  geom_sf_text(data = st_as_sf(urbano_alto),aes(label = NOMBRE),size = 2.8) +
  coord_sf(xlim = c(coord_alto@xmin,coord_alto@xmax + 0.015), 
           ylim = c(coord_alto@ymin, coord_alto@ymax), expand = FALSE) + 
  scale_fill_manual(values = hcl.colors(15, "Greens")) + 
  scale_color_manual(values = hcl.colors(15, "Greens"))+
  guides(shape = guide_legend(override.aes = list(size = 0.1))) +
  guides(color = guide_legend(override.aes = list(size = 0.1)))

urbano_micro_alto_theme <- urbano_micro_alto + ggtitle("Humedales urbanos (Estrato alto)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.99, 0.99),
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
  annotation_scale(location = "br")+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) 

ggsave(filename = "Propuestas/Propuesta3/urbano_micro_alto.png",
       plot = urbano_micro_alto_theme, width = 7,
       height = 7)
