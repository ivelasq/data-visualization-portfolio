#################################
#                               #
#       DataViz Battle          #
#        January 2018           #
# Specific Growth Rate of Algae #
#                               #
#################################

# libraries ---------------------------------------------------------------

library(XML)
library(tidyverse)
library(ggbeeswarm)
library(viridis)


# data --------------------------------------------------------------------

algae <- "http://aquatext.com/tables/algaegrwth.htm"

algae_tab <-
  readHTMLTable(algae, header = F, which = 2, stringsAsFactors = F, trim = T)

algae_types <- # data downloaded from algaebase
  read_csv("./data/raw/algae_types.csv")


# tidy table --------------------------------------------------------------

algae_tab_names <- # some species downloaded with tab separators
  algae_tab %>%
  mutate(V1 = if_else(V1 == "Chlorella vulgaris\n      (freshwater)", "Chlorella vulgaris (freshwater)", V1)) %>% 
  mutate(V1 = if_else(V1 == "Nannochlorois\n      oculata", "Nannochlorois oculata", V1))

algae_tab_tidy <-
  algae_tab_names %>% 
  rename("species" = "V1", # this creates colnames that can be gathered
         "5.5000" = "V2",
         "5.2500" = "V3",
         "10.5000" = "V4",
         "10.2500" = "V5",
         "25.5000" = "V6",
         "25.2500" = "V7",
         "30.5000" = "V8",
         "30.2500" = "V9") %>% 
  slice(-1:-3) %>% # no longer needed rows
  gather("temp_light", "growth_rate", -1) %>%
  mutate(temp = paste0(str_extract(temp_light, "\\d+"), "°C"),
         light = str_replace(temp_light,"\\d+.",""),
         growth_rate = as.numeric(growth_rate),
         growth_rate = if_else(species == "Isochrysis aff. galbana" & is.na(growth_rate), 0.06, growth_rate)) %>%  # this variable was missing
  select(-temp_light)

algae_tab_type <-
  left_join(algae_tab_tidy, algae_types)


# visualization -----------------------------------------------------------

algae_tab_type$temp <- factor(algae_tab_type$temp, levels = c("5°C", "10°C", "25°C", "30°C"))

algae_tab_type$salinity <- as.character(algae_tab_type$salinity)
algae_tab_type$type <- factor(algae_tab_type$type, levels = c("marine", "freshwater"))

algae_plot <-
  algae_tab_type %>%
  ggplot(aes(y = growth_rate, 
             x = light, 
             color = growth_rate)) +
  geom_beeswarm(dodge.width = 1, cex = 4, size = 4) +
  facet_grid(~temp) +
  labs(x = "LIGHT INTENSITY (LUX)", 
       y = "GROWTH RATE", 
       title = "ALGAE GROWTH RATES",
       subtitle = "Specific growth rates of algae (divisions per day) at different light intensities and temperatures.",
       caption = "Source: Aquatext") +
  scale_color_viridis() +
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  theme(title = element_text(size = 16),
        plot.subtitle = element_text(size = 12, family = "serif", face = "italic"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none"
  )

algae_plot

# outputs ---------------------------------------------------------

write.csv(algae_tab_type, "./data/processed/algae_processed.csv")

ggsave(filename = "./plot/algae_submission.png", plot = algae_plot, width = 17.04, height = 7.69)
