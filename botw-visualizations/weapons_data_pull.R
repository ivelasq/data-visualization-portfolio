# BOTW Weapons Data Pull

# libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)
library(polite)

# references --------------------------------------------------------------

# https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d
# https://stackoverflow.com/questions/51287276/bind-rows-column-cant-be-converted-from-integer-to-character-error
# http://calumwebb.uk/posts/spotifyr/

# data --------------------------------------------------------------------

url <- "http://orcz.com/Breath_of_the_Wild:_Weapons_List"
session <- bow(url)

weapons_tab <-
  scrape(session) %>% 
  html_nodes("table.wikitable") %>% 
  html_table(fill = TRUE) 

weapons_tab[[1]][["NA"]] <- weapons_tab[[1]][["empty"]]

weapons_tab %<>% 
  repair_names() %>% 
  lapply(., mutate_if, is.integer, as.character) %>%
  reduce(bind_rows) %>% 
  mutate_at(vars(AP, DUR, `AP*DUR`), list(~as.numeric(.)))

weapons_meta <-
  weapons_tab %>% 
  mutate(category = case_when(str_detect(Name, "Ancient") ~ "Ancient",
                              str_detect(Name, "Guardian") ~ "Ancient",
                              str_detect(Name, "Blizzard") ~ "Elemental",
                              str_detect(Name, "Fire") ~ "Elemental",
                              str_detect(Name, "Flame") ~ "Elemental",
                              str_detect(Name, "Frost") ~ "Elemental",
                              str_detect(Name, "Ice") ~ "Elemental",
                              str_detect(Name, "Lightning") ~ "Elemental",
                              str_detect(Name, "Meteor") ~ "Elemental",
                              str_detect(Name, "Thunder") ~ "Elemental",
                              str_detect(Name, "Boko") ~ "Enemy",
                              str_detect(Name, "Bokoblin") ~ "Enemy",
                              str_detect(Name, "Lynel") ~ "Enemy",
                              str_detect(Name, "Moblin") ~ "Enemy",
                              str_detect(Name, "Lizal") ~ "Enemy",
                              str_detect(Name, "Vicious Sickle") ~ "Enemy",
                              str_detect(Name, "Windcleaver") ~ "Enemy",
                              str_detect(Name, "Demon Carver") ~ "Enemy",
                              str_detect(Name, "Duplex Bow") ~ "Enemy",
                              str_detect(Name, "Zora") ~ "Tribe",
                              str_detect(Name, "Scimitar") ~ "Tribe",
                              str_detect(Name, "Gerudo") ~ "Tribe",
                              str_detect(Name, "Traveler's") ~ "Hylian",
                              str_detect(Name, "Knight's") ~ "Hylian",
                              str_detect(Name, "Soldier's") ~ "Hylian",
                              str_detect(Name, "Royal") ~ "Royal",
                              TRUE ~ "Other")) %>% 
  janitor::clean_names()

write_csv(weapons_meta, here::here("data", "processed", "weapons_meta.csv"))