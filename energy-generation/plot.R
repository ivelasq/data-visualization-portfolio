# https://www.eia.gov/electricity/data/state/

library(tidyverse)
library(readxl)
library(janitor)
library(geofacet)
library(extrafont)

dat <-
  read_excel(here::here("data", "annual_generation_state.xls", skip = 1)) %>% 
  clean_names()

crosswalk <-
  tibble::tribble(
    ~energy_source,   ~energy_source_group,
    "Coal",                 "coal",
    "Geothermal",           "renewables",
    "Hydroelectric Conventional",           "renewables",
    "Natural Gas",          "natural gas",
    "Nuclear",              "nuclear",
    "Other", "other non-renewables",
    "Other Biomass",           "renewables",
    "Other Gases", "other non-renewables",
    "Petroleum", "other non-renewables",
    "Pumped Storage",           "renewables",
    "Solar Thermal and Photovoltaic",           "renewables",
    "Wind",           "renewables",
    "Wood and Wood Derived Fuels",           "renewables"
  )

map_dat_group <-
  left_join(dat, crosswalk)

map_dat <-
  map_dat_group %>% 
  filter(type_of_producer == "Total Electric Power Industry",
         energy_source != "Total",
         !state %in% c("US-Total", "US-TOTAL"),
         !is.na(state)) %>% 
  select(year, state, energy_source_group, generation_megawatthours) %>% 
  group_by(year, state, energy_source_group) %>% 
  summarise(generation_megawatthours = sum(generation_megawatthours)) %>% 
  ungroup() %>% 
  group_by(year, state) %>% 
  mutate(percent = generation_megawatthours/sum(generation_megawatthours)) %>% 
  mutate(energy_source_group = factor(energy_source_group, levels = c("coal",
                                                                      "natural gas",
                                                                      "other non-renewables",
                                                                      "renewables",
                                                                      "nuclear")))

ggplot(map_dat, aes(year, percent, fill = energy_source_group)) +
  geom_area(color = "white", size = 0.2) + 
  facet_geo(~ state, 
            grid = "us_state_grid1") +
  theme_void() +
  ggtitle("Net Generation by State and Energy Source, 1990-2017\n") +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#293333", "#4D5340", "#FF6600", "#8FB350", "#00CCCC"))