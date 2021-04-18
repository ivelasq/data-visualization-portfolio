# washington

# libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, sf, geosphere, geogrid, tmap, viridis)

# cost to drive a mile
# source: https://www.irs.gov/newsroom/standard-mileage-rates-for-2018-up-from-rates-for-2017
mile <- .545

# washington tax data
# shapefiles downloaded from https://dor.wa.gov/find-taxes-rates/sales-tax-jurisdiction-boundaries

wa_sf <- # create a sf object to calculate distance to portland
  st_read('shapefiles/LOCCODE_PUBLIC_18Q4.shp') %>%
  st_transform("+init=epsg:4326") %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

wa_dat <- # tax rate data
  read_csv('data/Rates2018Q4.csv') %>% 
  rename(LOCCODE = Code)

wa_dist <-
  left_join(wa_sf, wa_dat)

st_geometry(wa_dist) <- NULL # to be able to do the following calculation
wa_dist <- as_data_frame(wa_dist)

lon <- wa_dist$lon
lat <- wa_dist$lat
lonlat <- cbind(lon, lat)
port_lon <- rep(-122.6587, 356)
port_lat <- rep(45.5122, 356)
port_lonlat <- cbind(port_lon, port_lat)

d <- distm(lonlat, port_lonlat, fun = distHaversine)/1609
distance <- d[,1]

wa_dist <- cbind(wa_dist, distance)
wa_dist$cost <- wa_dist$d*mile*2/wa_dist$Rate

wa_dist %>% 
  ggplot(aes(cost, d)) +
  geom_point(size = 4)  +
  theme_minimal()

# tilemap -----------------------------------------------------------------

wa_shp <- # need an S4 object
  rgdal::readOGR(dsn = "shapefiles/", layer = "LOCCODE_PUBLIC_18Q4")

wa_tax <- sp::merge(wa_shp, wa_dist, by = "LOCCODE")

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = wa_tax, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

wa_details <- calculate_grid(wa_tax, grid_type = "hexagonal", seed = 6)
wa_hex <- assign_polygons(wa_tax, wa_details)

# tmap plot
tm_shape(wa_hex) + 
  tm_polygons("cost", palette = "viridis")

# function for tidying SpatialPolygonsDataFrame
# https://www.dataplanes.org/notes/2018/02/08/death-penalty-us

clean <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = merge(shape.points, shape@data, by="id")
}

result_hex_df <- clean(wa_hex)
result_hex_df <-
  result_hex_df %>%
  mutate(quartile = ntile(cost, 4))

# ggplot2
result_hex_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat.x, fill = quartile, group = LOCCODE), color = "white") +
  scale_fill_viridis(option = "magma", labels = c("$59-$746", "T888", "t", "3")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
