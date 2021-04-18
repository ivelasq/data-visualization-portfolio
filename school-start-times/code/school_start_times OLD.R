# School Start Times

# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(maps)
library(showtext)

# fonts -------------------------------------------------------------------

font_add(family = "schoolbell", regular = here::here("font", "Schoolbell-Regular.ttf"))

# data --------------------------------------------------------------------

colnames <-
  c("state", "num_schools", "avg_start_middle", "before_730", "from_730_759", "from_800_829", "from_830_859", "after_900")

dat <-
  read_excel(here::here("data", "raw", "sass1112_506_s1s.xlsx"), skip = 1) 

clean_dat <-
  dat %>% 
  select(-2, -4, -6, -8, -10, -12, -14, -16) %>% 
  rename_all(~ colnames) %>% 
  slice(-1, -28:-31, -58) %>% 
  mutate_at(vars(before_730:after_900), list(~ as.numeric(.))) %>% 
  pivot_longer(cols = before_730:after_900,
               names_to = "time_span",
               values_to = "percentage") %>% 
  mutate(region = tolower(state)) %>% 
  mutate(time_span = factor(time_span, 
                            levels = c("before_730", "from_730_759", "from_800_829", "from_830_859", "after_900")))

time_span_labels <- list(
  "before_730" = "Before 7:30AM",
  "from_730_759" = "From 7:30AM to 7:59AM",
  "from_800_829" = "From 8:00AM to 8:29AM",
  "from_830_859" = "From 8:30AM to 8:59AM",
  "after_900" = "After 9:00AM"
)

labeller <- function(variable, value){
  return(time_span_labels[value])
}

states_map <- map_data("state")

map_dat <- left_join(states_map, clean_dat)

showtext_auto()
quartz()

map_dat %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percentage), color = "white", size = 0.1) +
  facet_wrap(~ time_span, labeller = labeller, ncol = 5) + 
  scale_fill_viridis_c(option = "B", direction = -1) +
  theme_void(base_family = "schoolbell") +
  ggtitle("What Percentage of Public Middle Schools Start...") +
  theme(plot.title = element_text(size = 18, 
                                  vjust = 5,
                                  hjust = 0.5, 
                                  color = "#301934"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        aspect.ratio = 1)

ggsave(here::here("fig", "school_starts.tiff"), dpi = "retina", width = 10, height = 6, units = "in")
