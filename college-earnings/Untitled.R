library(tidyverse)
library(XML)
library(janitor)

site <- "http://money.com/money/best-colleges/"

table_cols <-
  readHTMLTable(site, header = F, which = 1, stringsAsFactors = F)

table_dat <-
  readHTMLTable(site, header = F, which = 2, stringsAsFactors = F) %>% 
  setNames(table_cols[1,]) %>% 
  clean_names() %>% 
  mutate_at(vars(est_price_2019_20_without_aid,
                 est_price_2019_20_with_avg_grant,
                 average_student_debt,
                 early_career_earnings), funs(str_replace_all(., "\\$|,", "")))  %>% 
  mutate_at(vars(est_price_2019_20_without_aid,
                 est_price_2019_20_with_avg_grant,
                 average_student_debt,
                 early_career_earnings), funs(as.numeric(.))) %>% 
  mutate_at(vars(percent_of_students_who_get_any_grants), 
            funs(str_replace_all(., "%", ""))) %>% 
  mutate_at(vars(percent_of_students_who_get_any_grants),
            funs(as.numeric(.)/100))
