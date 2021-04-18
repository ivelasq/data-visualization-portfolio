# cRaggy analysis for R Cascadia 2018

# libraries ---------------------------------------------------------------

library(tidyverse)
library(mapproj)
library(ggmap)
library(RColorBrewer)

# data --------------------------------------------------------------------

# reading in data data from: John David Smith - john.smith@learningalliances.net - Twitter / github: smithjd

# url <- "https://s3.amazonaws.com/biketown-tripdata-public/BiketownPublicTripData201804.zip"

# download.file(url, dest = "dataset.zip", mode = "wb")

# unzip("dataset.zip", exdir = "./data")

public_trip_data <- list.files(
  path = "./data/PublicTripData",
  pattern = "*.csv",
  full.names = T ) %>%
  # map_df(~ read_csv(., col_types = cols(.default = "c")))
  map_df(~ read_csv(., col_types = "iccddcccddcccicdTcl"))

str(public_trip_data)

# plot 1 ------------------------------------------------------------------

table(public_trip_data$PaymentPlan)

long_lat_data <- 
  public_trip_data %>%
  filter(PaymentPlan == "Subscriber") %>% 
  select(StartLatitude, StartLongitude, EndLatitude, EndLongitude) %>% 
  rowid_to_column("id") %>%  # add id column
  gather(latlong, value, -id) %>% 
  mutate(latlong =
         case_when(latlong == "StartLatitude" ~ "latitude",
                   latlong == "EndLatitude" ~ "latitude",
                   latlong == "StartLongitude" ~ "longitude",
                   latlong == "EndLongitude" ~ "longitude")) %>% 
  group_by(latlong) %>% 
  mutate(grouped_id = row_number()) %>% 
  spread(latlong, value) %>% 
  select(-grouped_id)

# simple dot plot

plot(long_lat_data$latitude, long_lat_data$longitude, type = "n")
points(long_lat_data$latitude, long_lat_data$longitude, pch = 20, cex = 0.3)

# using map tutorial code sfrom
# https://flowingdata.com/2014/01/28/how-to-map-geographic-paths-in-r/

min_long <- -122.7107239
max_long <- -122.6210475
min_lat <- 45.4643257
max_lat <- 45.5823977

# base map 

plot(x = NULL,
     y = NULL,
     xlim = c(min_lat, max_lat),
     ylim = c(min_long, max_long), type = "n", asp = 1, axes = TRUE, xlab = "", ylab = "")

points(long_lat_data$latitude, long_lat_data$longitude, pch = 20, cex = 0.3)

detailMap <- function(bbox, thedata) {
  basemap <- get_map(location = bbox, maptype = "toner-lines", color="bw")
  ggmap(basemap) + 
    geom_point(aes(x = longitude, y = latitude, group = id), size = 0.6, color = "red", alpha = 0.3, data = thedata)
}

portland <- c(min_long, min_lat, max_long, max_lat)

detailMap(portland, long_lat_data)

# plot 2 ------------------------------------------------------------------

date_dat <-
  public_trip_data %>% 
  mutate(StartDate_t = as.Date(StartDate, format = "%m/%d/%Y"),
         StartDate_d = weekdays(StartDate_t)) %>%
  select(StartDate_d, PaymentPlan, StartLatitude, StartLongitude) %>%
  rowid_to_column("id") %>%  # add id column
  gather(latlong, value, c(-id, -StartDate_d, -PaymentPlan)) %>% 
  mutate(latlong =
           case_when(latlong == "StartLatitude" ~ "latitude",
                     latlong == "StartLongitude" ~ "longitude")) %>% 
  group_by(latlong) %>% 
  mutate(grouped_id = row_number()) %>% 
  spread(latlong, value) %>% 
  select(-grouped_id) %>% 
  filter(!is.na(PaymentPlan),
         !is.na(StartDate_d)) %>% 
  mutate(StartDate_d = factor(StartDate_d, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         PaymentPlan = factor(PaymentPlan, levels = c("Casual", "Subscriber")))

# using map tutorial code sfrom
# https://flowingdata.com/2014/01/28/how-to-map-geographic-paths-in-r/

min_long <- -122.7107239
max_long <- -122.6210475
min_lat <- 45.4643257
max_lat <- 45.5823977

n <-
  date_dat %>% 
  filter(latitude > min_lat,
         latitude < max_lat,
         longitude > min_long,
         longitude < max_long)

n <- length(n$id)

# base map 

plot(x = NULL,
     y = NULL,
     xlim = c(min_lat, max_lat),
     ylim = c(min_long, max_long), type = "n", asp = 1, axes = TRUE, xlab = "", ylab = "")

points(date_dat$latitude, date_dat$longitude, pch = 20, cex = 0.3)

detailMap <- function(bbox, thedata) {
  basemap <- get_map(location = bbox, maptype = "toner-lite", color="bw")
  ggmap(basemap) + 
    geom_point(aes(x = longitude, y = latitude, group = id, color = factor(PaymentPlan)), size = 0.6, alpha = 0.5, data = thedata) +
    facet_wrap(~ StartDate_d, ncol = 7) +
    labs(title = "BIKETOWNPDX RIDE TRIP STARTS BY DAY OF WEEK AND PAYMENT PLAN",
         subtitle = paste0("n = ", n),
         caption = "Isabella Velásquez, github.com/ivelasq/cRaggy, 312-342-1122\nspatial visualization using ggmap, map tiles by Stamen Design, under CC BY 3.0. data by OpenStreetMap, under ODbL") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.title=element_blank()) + 
    scale_color_brewer(palette = "Dark2")
    
}

portland <- c(min_long, min_lat, max_long, max_lat)

detailMap(portland, date_dat)

table(date_dat$StartDate_d, date_dat$PaymentPlan)

ggsave('cRaggy_submission.png', path = "./plot", width = 11, height = 8.5, units = "in")
