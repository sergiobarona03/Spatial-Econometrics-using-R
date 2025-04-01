##########################################################
## Panel de figuras: Localización: Cali rural y urbano  ##
##########################################################
library(cowplot)
library(here)
source(here::here("Modular/Microlocalizacion","7.Localizacion.R"), encoding = "UTF8")


panel_1 <- plot_local_theme + xlab(" ") + ylab(" ") + ggtitle(" ") 
panel_2 <- urbano_micro_bajo_theme + xlab(" ") + ylab(" ") + ggtitle(" ")
panel_3 <- urbano_micro_medio_theme + xlab(" ") + ylab(" ") + ggtitle(" ")
panel_4 <- urbano_micro_alto_theme + xlab(" ") + ylab(" ") + ggtitle(" ")
panel_5 <- rurales_micro_bajo_theme + xlab(" ") + ylab(" ") + ggtitle(" ")
panel_6 <- rurales_micro_alto_theme  + xlab(" ") + ylab(" ") + ggtitle(" ")

panel_micro <-  ggdraw() +
  draw_plot(panel_1, x = 0.2, y = 0.3, width = 0.5, height = 0.68) +
  draw_plot(panel_2, x = 0.68, y = 0.5, width = 0.3, height = 0.5) +
  draw_plot(panel_3, x = 0.02, y = 0, width = 0.2, height = 0.5) +
  draw_plot(panel_4, x = 0.02, y = 0.5, width = 0.2, height = 0.5) +
  draw_plot(panel_5, x = 0.68, y = 0, width = 0.3, height = 0.5) +
  draw_plot(panel_6, x = 0.2, y = 0, width = 0.5, height = 0.3) +
  draw_plot_label(label = c("F. Peri-urban wetlands within high strata",
                            "E. Peri-urban wetlands within low strata",
                            "C. Urban wetlands within medium strata",
                            "A. Cali: Urban and peri-urban area",
                            "B. Urban wetlands within low strata",
                            "D. Urban wetlands within high strata"), size = 14,
                  x = c(0.2, 0.68,
                        -0.02, 0.2, 0.68 , -0.02), y = c(0.3, 0.5, 
                                                       0.5, 0.98, 1, 0.99)) 

ggsave(filename = "Propuestas/Propuesta3/Panel_Micro.png",
       plot = panel_micro,
       width = 24, height = 14,bg = "white")
