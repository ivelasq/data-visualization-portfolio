# Vancouver Dot Map

# Libraries ---------------------------------------------------------------

library(cancensus) # https://github.com/mountainMath/cancensus
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(mapview)
library(ggmap)
library(khroma)

# Data --------------------------------------------------------------------

region <- # Find Vancouver region number: CSD 5915022
  cancensus::list_census_regions("CA16")

vars <- # Find race variables
  list_census_vectors("CA16") %>% 
  filter(type == "Total",
         str_detect(details, "visible minority"))

vars_vector <- vars$vector

census_data <- 
  get_census(dataset = "CA16",
             regions = list(CSD = "5915022"),
             vectors = vars_vector,
             level = "DA",
             geo_format = "sf") %>%
  rename(`South Asian` = `v_CA16_3960: South Asian`,
         Chinese = `v_CA16_3963: Chinese`,
         Black = `v_CA16_3966: Black`,
         Filipino = `v_CA16_3969: Filipino`,
         `Latin American` = `v_CA16_3972: Latin American`,
         Arab = `v_CA16_3975: Arab`,
         `Southeast Asian` = `v_CA16_3978: Southeast Asian`,
         `West Asian` = `v_CA16_3981: West Asian`,
         Korean = `v_CA16_3984: Korean`,
         Japanese = `v_CA16_3987: Japanese`,
         `White / Not a Visible Minority` = `v_CA16_3996: Not a visible minority`)

race_vector <- c("South Asian", "Chinese", "Black", "Filipino", "Latin American", "Arab", "Southeast Asian", "West Asian", "Korean", "Japanese", "White / Not a Visible Minority")

sf_dots_all <- NULL;

createDots <- function(.x){
  
  x_sym <- sym(.x)

  census_filter <-
    census_data %>% 
    filter(!is.na(!!(x_sym))) # cannot have missing values
  
  num_dots <-
    as.data.frame(census_filter) %>% 
    select(.x)
  
  sf_dots <-
    map_df(
      names(num_dots),
      ~ st_sample(census_filter,
                  size = num_dots[, .x],
                  type = "random") %>%
        st_cast("POINT") %>%
        st_coordinates() %>%
        as_tibble() %>%
        setNames(c("lon", "lat")) %>%
        mutate(race = .x)
    )
  
  sf_dots_all <<- bind_rows(sf_dots_all, sf_dots)
  
}

map(.x = race_vector,
    .f = createDots)

set.seed(1111)

sf_dots_all <-
  sf_dots_all %>% 
  slice(sample(1:n())) %>%  # randomize order
  filter(!race %in% c("Arab", "Black", "West Asian", "Korean")) # very small numbers

# Plot --------------------------------------------------------------------

v_town <- get_stamenmap(bbox = c(left = -123.22496118, 
                                 right = -123.02324196, 
                                 top = 49.31617132, 
                                 bottom = 49.19844515), 
                        zoom = 13, 
                        maptype = "toner-lines")

ggmap(v_town) +
  geom_point(data = sf_dots_all, aes(lon, lat, colour = race), size = .1, alpha = 0.2) +
  scale_color_vibrant() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = quote(bold))) +
  labs(colour = "Visible Minority Group, 2016 Census",
       title = "Dot Map of Vancouver, BC",
       subtitle = "One Dot Per Person for the Entire City",
       caption = "Source: Canadian Census via cancensus; map tiles by Stamen Design") +
  guides(colour = guide_legend(override.aes = list(size = 4,
                                                   alpha = 1)))

# Save --------------------------------------------------------------------

ggsave("yvr.png", dpi = "retina", width = 10, height = 6, units = "in")
