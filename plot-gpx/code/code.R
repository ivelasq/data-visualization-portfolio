# Libraries

library(tidyverse)
library(sf)
library(ggmap)

# Open files

f <- 
  here::here("data", "raw", "GRSM_TRAILS-shp", "GRSM_TRAILS.shp")

shp <- st_read(f)

coords <-
  st_coordinates(shp) %>% 
  as_data_frame()

# Let's try it with the first trail

t1 <-
  coords %>% 
  filter(L1 == 1)

# Get map background

map_bkgrd <- 
  get_map(location = c(lon = mean(t1$X), lat = mean(t1$Y)), 
          zoom = 14,
          maptype = "watercolor", 
          scale = 2)

# Plot coordinates

ggmap(map_bkgrd) +
  geom_point(data = t1, 
             aes(x = X,
                 y = Y),
             fill = "#0000FF",
             alpha = 0.2,
             size = 0.3) +
  guides(fill = FALSE,
         alpha = FALSE,
         size = FALSE) +
  theme_void()

# Save file

ggsave(here::here("output", "map.png"), dpi = "retina", width = 4, height = 4, units = "in")

