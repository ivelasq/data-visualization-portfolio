# DSIEUR Plot


# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(magick)
library(cowplot)

# image -------------------------------------------------------------------

logo_file <- image_read("image.png")

# FONT --------------------------------------------------------------------

font_add_google("Lato", "lato")

# DATA --------------------------------------------------------------------

dat <-
tibble::tribble(
         ~colors, ~height, ~hex,
        "white",       7,    "#c6d4db",
        "black",     7.5,    "#141619",
       "purple",     8.2,    "#3a2657",
    "dark_blue",     6.9,    "#222b52",
   "dark_blue2",     5.7,    "#222f60",
     "med_blue",     5.1,    "#1689cc",
   "light_blue",     4.4,    "#5dafc5",
         "turq",     5.7,    "#016b72",
   "dark_green",     6.5,    "#06372c",
        "olive",     6.9,    "#505e20",
        "green",     8.1,    "#086333",
  "light_green",     9.5,    "#90bf25",
       "yellow",     9.1,    "#d0bc0a",
        "peach",     8.8,    "#e4a95d",
  "dark_yellow",     8.6,    "#e4830a",
       "orange",     8.1,    "#e14c15",
  "dark_orange",     8.6,    "#be471a",
   "light_pink",     9.5,    "#db5c65",
    "dark_pink",    10.5,    "#ad2247",
       "maroon",    11.5,    "#841d26"
  )

dat$colors <- factor(dat$colors, levels = dat$colors)

# making pencil shape -

# https://stackoverflow.com/questions/62592214/changing-the-shape-of-bars-in-ggplot2-bar-plot

pencil_shape <- data.frame(x=c(0.1, 0.9, 0.9, 0.9, 0.5, 0.1, 0.1),
                           y=c(0, 0, 4.5, 8.5, 10, 8.5, 4.5))

ggplot(pencil_shape, aes(x, y)) + 
  geom_polygon() +
  ylim(0, 14) +
  xlim(0, 10)

stat_windmill <- function(mapping = NULL, data = NULL, geom = "polygon",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, span_x = 1, ...) {
  layer(
    stat = StatWindmill, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, span_x = span_x, ...)
  )
}

StatWindmill <- ggproto("StatWindmill", Stat,
                        compute_group = function(data, scales, span_x = 1) {
                          pencil_frame <- data.frame(x_map = c(0.1, 0.9, 0.9, 0.9, 0.5, 0.1, 0.1),
                                                     y = c(0, 0, 4.5, 8.5, 10, 8.5, 4.5))
                          new_x <- (data$x - span_x/2) + (span_x * pencil_frame$x_map)
                          new_y <- data$y * pencil_frame$y
                          new_pencil <- data.frame(x=new_x, y=new_y)
                          new_pencil
                          
                        },
                        
                        required_aes = c("x", "y")
)

geom_windmill <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          rule = "evenodd",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatWindmill,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rule = rule,
      ...
    )
  )
}

# PLOT --------------------------------------------------------------------

showtext_auto()
quartz()

p <-
  dat %>% 
  ggplot(aes(x = colors, y = height, fill = colors)) +
  geom_windmill() +
  theme_linedraw() +
  scale_fill_manual(values = dat$hex) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill = "#1d1d1b"),
        plot.title = element_text(colour = "#ffffff",
                                  size = 16,
                                  face = "bold",
                                  family = "lato"),
        plot.subtitle = element_text(colour = "#ffffff",
                                     family = "lato"),
        legend.position = "none") +
  labs(title = "Data Science in Education\nUsing R",
       subtitle = "Ryan A. Estrellado, Emily A. Freer, Jesse Mostipak,\nJoshua M. Rosenberg and Isabella C. Velásquez")

p2 <-
  ggdraw() +
  draw_plot(p) +
  draw_image(
    logo_file, x = 0.94, y = 0.45,
    width = 0.04
  )

ggsave(filename = paste0("plot", Sys.time(), ".png"),
       plot = p2,
       device = "png",
       width = 16,
       height = 9,
       units = "in")
