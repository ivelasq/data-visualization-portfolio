# School Start Times

# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(httr)
library(maps)
library(showtext)

# fonts -------------------------------------------------------------------

font_add_google("Schoolbell", "schoolbell")

# data --------------------------------------------------------------------

colnames <-
  c("state", "avg_start", "before_730", "from_730_759", "from_800_829", "after_830")

url <- "https://nces.ed.gov/surveys/ntps/xls/ntps1718_202000602_s1s.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))

dat <-
  read_excel(tf, skip = 2) 

clean_dat <-
  dat %>% 
  select(-3, -5, -7, -9, -11) %>% 
  rename_all(~ colnames) %>% 
  filter(!is.na(state)) %>% 
  slice(1:62) %>% 
  mutate_at(vars(before_730:after_830), list(~ as.numeric(.))) %>% 
  pivot_longer(cols = before_730:after_830,
               names_to = "time_span",
               values_to = "percentage") %>% 
  mutate(region = tolower(state)) %>% 
  mutate(time_span = factor(time_span, 
                            levels = c("before_730", "from_730_759", "from_800_829", "after_830")))

time_span_labels <- list(
  "before_730" = "Before 7:30AM",
  "from_730_759" = "From 7:30AM to 7:59AM",
  "from_800_829" = "From 8:00AM to 8:29AM",
  "after_830" = "After 8:30AM"
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
  facet_wrap(~ time_span, labeller = labeller, ncol = 4) + 
  scale_fill_viridis_c(option = "B", direction = -1) +
  theme_void(base_family = "schoolbell") +
  labs(title = "What Percentage of Public High Schools Start...",
       caption = "Source: NCES 2018") +
  theme(plot.title = element_text(size = 18, 
                                  vjust = 5,
                                  hjust = 0.5, 
                                  color = "#301934"),
        legend.position = "bottom",
        aspect.ratio = 1)

ggsave(here::here("fig", "school_starts.png"), dpi = "retina", width = 10, height = 6, units = "in")
