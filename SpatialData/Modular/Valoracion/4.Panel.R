
###############################################
## Valoración monetaria: creación del panel  ##
###############################################
library(cowplot)
library(here)
source(here::here("Modular/Valoracion","3.NoMonetaria.R"), encoding = "UTF8")

panel_1 <- plot_monetario_theme + xlab(" ") + ylab(" ") + ggtitle(" ")
panel_2 <- plot_no_monetario_theme + xlab(" ") + ylab(" ") + ggtitle(" ")

panel_valor <-  ggdraw() +
  draw_plot(panel_1, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(panel_2, x = .5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("A. Monetary valuation",
                            "B. Non-Monetary valuation"), size = 15,
                  x = c(0, 0.5), y = c(1, 1)) 

ggsave(filename = "Propuestas/Propuesta3/Panel_Valor.png",
       plot = panel_valor,
       width = 12, height = 7,bg = "white")
