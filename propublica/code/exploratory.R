library(tidyverse)

dat <-
  read_csv(here::here("data", "raw", "allegations_20200726939.csv"))

nrow(dat)
colnames(dat)

dat %>% 
  group_by(command_now) %>% 
  tally() %>% 
  arrange(desc(n))

dat %>% 
  group_by(fado_type) %>% 
  tally() %>% 
  arrange(desc(n))

dat %>% 
  group_by(allegation) %>% 
  tally() %>% 
  arrange(desc(n))

dat %>% 
  group_by(complainant_gender, complainant_ethnicity) %>% 
  tally() %>% 
  arrange(desc(n))

dat_date <- 
  dat %>% 
  mutate(date_received = as.Date(paste0(year_received, "-", month_received, "-01")),
         date_closed = as.Date(paste0(year_closed, "-", month_closed, "-01")),
         complaint_length = as.numeric(date_closed - date_received))

dat %>% 
  group_by(complainant_ethnicity, allegation) %>% 
  tally() %>% 
  arrange(desc(n))

dat %>% 
  filter(str_detect(board_disposition, "Training")) %>% 
  group_by(first_name, last_name) %>% 
  tally() %>% 
  arrange(desc(n))

dat_date %>% 
  filter(year_received >= 2019) %>% 
  ggplot(aes(x = fado_type, y = complaint_length, group = fado_type)) +
  geom_boxplot()
