# North Dakota Dot Map

# Libraries ---------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(ggmap)
library(khroma)

sf1 <-
  load_variables(2010, "sf1", cache = TRUE)

demo <- # Male & Female
  c("P012002", "P012026")

nd <-
  get_decennial(
    geography = "block",
    variables = demo,
    summary = "P012001", # all races
    year = 2010,
    state = "ND",
    county = "Williams",
    geometry = TRUE)

nd_spread <-
  nd %>% 
  spread(variable, value)

num_dots_m <-
  as.data.frame(nd_spread) %>% 
  select(P012002)

num_dots_f <-
  as.data.frame(nd_spread) %>% 
  select(P012026)

tic()
num_dots_m2 <-
  map_df(names(num_dots_m),
         ~ st_sample(nd_spread,
                     size = num_dots_m[,.x],
                     type = "random") %>% 
           st_cast("POINT") %>% 
           st_coordinates() %>% 
           as_tibble() %>% 
           setNames(c("lon", "lat")) %>%
           mutate(race = .x)
  )
toc()

tic()
num_dots_f2 <-
  map_df(names(num_dots_f),
         ~ st_sample(nd_spread,
                     size = num_dots_f[,.x],
                     type = "random") %>% 
           st_cast("POINT") %>% 
           st_coordinates() %>% 
           as_tibble() %>% 
           setNames(c("lon", "lat")) %>%
           mutate(race = .x)
  )
toc()

nd_dots <- bind_rows(num_dots_m2, num_dots_f2) %>% slice(sample(1:n()))

will <- get_stamenmap(bbox = 
                            c(left = -104.04821202, 
                              right = -102.82891803, 
                              top = 48.6348614, 
                              bottom = 47.95452093), 
                          maptype = "toner-lines", 
                          zoom = 11)
ggmap(will) +
  geom_point(data = nd_dots, aes(lon, lat, colour = race), size = .1) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.key = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = quote(bold))) +
  scale_x_continuous(limits = c(-87.940267, -87.523661)) + 
  scale_y_continuous(limits = c(41.644335, 42.023131)) +
  labs(colour = "Gender, 2010 Census") +
  guides(colour = guide_legend(override.aes = list(size = 4)))

p_dots
