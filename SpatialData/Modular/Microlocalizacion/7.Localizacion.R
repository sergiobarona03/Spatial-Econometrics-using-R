##################################################
## Figura 2: Localización: Cali rural y urbano  ##
##################################################
library(here)
source(here::here("Modular/Microlocalizacion","6.RuralAlto.R"), encoding = "UTF8")

rural_st_na <- rural_st %>%  filter(is.na(Estrato))
rural_st_estrato <- rural_st %>%  filter(!is.na(Estrato))
rural_st_estrato$Estrato[rural_st_estrato$Estrato == "Alto"] <- "High peri-urban"
rural_st_estrato$Estrato[rural_st_estrato$Estrato == "Bajo"] <- "Low peri-urban"

urbano_st_local <- rbind(urbano_bajo_fill, urbano_medio_fill, urbano_alto_fill)
urbano_st_na <- rbind(urbano_bajo_na, urbano_medio_na, urbano_alto_na)
urbano_st_local$Estrato <- as.character(urbano_st_local$Estrato)
urbano_st_local$Estrato[urbano_st_local$Estrato == "Bajo"] <- "Low urban"
urbano_st_local$Estrato[urbano_st_local$Estrato == "Medio"] <- "Medium urban"
urbano_st_local$Estrato[urbano_st_local$Estrato == "Alto"] <- "High urban"

plot_local <- ggplot() + 
  geom_sf(data = st_as_sf(rural_st_na),fill = alpha("wheat2"),col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st_na),fill = alpha("wheat2", 0.5),col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(exp_st), fill = alpha("wheat2", 0.5), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(urbano_st_local), aes(fill = Estrato), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rural_st_estrato), aes(fill = Estrato), col = alpha("black", 0.3)) +
  geom_sf(data = st_as_sf(rios), fill = "cornflowerblue",color = "cornflowerblue")+
  geom_sf(data = st_as_sf(rio_cauca), fill = "cornflowerblue",color = "cornflowerblue") +
  geom_point(data = sites, aes(x = x, y = y, shape = Area),
             size = 10, col = col_point,
             fill = col_point)+
  geom_text(data = sites, aes(x = x, y = y, label = n), size = 2.5) +
  geom_sf_label(data = st_as_sf(rio_cauca),aes(label = nombre),size = 2.5)+
  geom_sf_label(data = st_as_sf(rios),aes(label = nombre),size = 2.5) +
  scale_fill_manual(values = palette_ses, breaks = c("Low urban",
                                                     "Medium urban",
                                                     "High urban",
                                                     "Low peri-urban",
                                                     "High peri-urban"))+ 
  scale_shape_manual(values = c('Urbana' = 21, 'Rural' = 22), 
                     breaks = c("Urbana", "Rural"), labels = c("Urban area",
                                                               "Peri-urban area"))


plot_local_theme <- plot_local + ggtitle(" ") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        plot.title=element_text(hjust = 0,size = 12,
                                family="serif",face = "italic"),
        panel.grid=element_blank(),
        legend.position = c(0.05, 0.99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(2, 2, 2, 2),
        legend.key.size = unit(0.38, 'cm'), #change legend key size
        legend.key.height = unit(0.38, 'cm'), #change legend key height
        legend.key.width = unit(0.38, 'cm'),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype='solid'),
        panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  annotation_scale(location = "bl", pad_x = unit(0.4, "in"), pad_y = unit(0.3, "in"))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) + 
  guides(fill=guide_legend(title="Socioeconomic Status"),
         shape = guide_legend(title = "Sample size"))

ggsave(filename = "Propuestas/Propuesta3/plot_local.png",
       plot = plot_local_theme,
       width = 7, height = 7)
