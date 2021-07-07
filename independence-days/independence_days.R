# Independence Days Around the World --------------------------------------

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)
library(httr)
library(polite)
library(lubridate)
library(janitor)

# Scrape data -------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_national_independence_days"
url_bow <- polite::bow(url)

ind_html <-
  polite::scrape(url_bow) %>% 
  rvest::html_nodes("table.wikitable") %>% 
  rvest::html_table(fill = TRUE)

ind_tab <- 
  ind_html[[1]][1:6] %>% 
  as_tibble() %>% 
  clean_names()

# Clean data --------------------------------------------------------------

ind_clean <-
  ind_tab %>%
  # Cleaning up some dates
  mutate(date_of_holiday = case_when(country == "Croatia" ~ "May 30",
                                     country == "Mexico" ~ "September 16",
                                     country == "Mongolia" ~ "December 29",
                                     country == "Paraguay" ~ "May 14",
                                     country == "Israel" ~ "May 14", # Independence Day exists within a range, but this was the original date.
                                     country == "Slovenia" ~ "June 25", # Slovenia has two dates; this one is "Statehood Day".
                                     TRUE ~ date_of_holiday),
         year = str_sub(year_of_event, start = 1, end = 4),
         date_mdy = case_when(date_of_holiday != "" ~ paste0(date_of_holiday, ", ", year),
                              TRUE ~ ""),
         date_parsed = mdy(date_mdy),
         weekday = weekdays(date_parsed),
         day = day(date_parsed),
         month = month(date_parsed, label = TRUE)) %>% 
  relocate(date_parsed:month, .after = country)

# Heatmap -----------------------------------------------------------------
# Tutorial: https://www.littlemissdata.com/blog/heatmaps

# Create summary table

ind_summary <-
  ind_clean %>% 
  group_by(day, month) %>% 
  tally() %>% 
  filter(!is.na(day))

# Create all combinations of dates

date_combs <-
  crossing(month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
           day = 1:31)

# Join and clean up to reflect actual calendars

ind_cal <-
  left_join(date_combs, ind_summary) %>% 
  filter(month == "Jan" & day %in% c(1:31)|
           month == "Feb" & day %in% c(1:29)|
           month == "Mar" & day %in% c(1:31)|
           month == "Apr" & day %in% c(1:30)|
           month == "May" & day %in% c(1:31)|
           month == "Jun" & day %in% c(1:30)|
           month == "Jul" & day %in% c(1:31)|
           month == "Aug" & day %in% c(1:31)|
           month == "Sep" & day %in% c(1:30)|
           month == "Oct" & day %in% c(1:31)|
           month == "Nov" & day %in% c(1:30)|
           month == "Dec" & day %in% c(1:31)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(month = factor(month, levels = c("Dec",
                                          "Nov",
                                          "Oct",
                                          "Sep",
                                          "Aug",
                                          "Jul",
                                          "Jun",
                                          "May",
                                          "Apr",
                                          "Mar",
                                          "Feb",
                                          "Jan")))

# Create heatmap

ind_cal %>% 
  ggplot(aes(x = day, y = month)) +
  geom_tile(aes(fill = as.factor(n)),colour = "white", na.rm = TRUE) +
  scale_x_continuous(limits = c(1, 32)) +
  scale_fill_manual(values = c("0" = "#f6f6f6", 
                                 "1" = "#FFD1FF", 
                                 "2" = "#FFB3FA", 
                                 "3" = "#FF97E2", 
                                 "4" = "#FF7BC0", 
                                 "5" = "#FF465F")) +
  guides(fill = guide_legend(title = "Independence Day Tally")) +
  theme_bw() +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "When are Independence Days?",
       xlab = "Day",
       ylab = "Month")

ggsave("./independence-days/independence_days.png")
