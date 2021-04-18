
# libraries ---------------------------------------------------------------

library(tidyverse)
library(brolgar)
library(extrafont)
library(directlabels)

# data ----------------------------------------------------------------------

college_dat <-
  read_csv("/Users/shortessay/Downloads/Data_10-7-2019.csv")

colnames(college_dat)


# clean data ------------------------------------------------------------------

college_pct <-
  college_dat %>% 
  mutate(`2017` = `Black or African American total (EF2017A  All students  Undergraduate total)`/`Grand total (EF2017A  All students  Undergraduate total)`,
         `2016` = `Black or African American total (EF2016A_RV  All students  Undergraduate total)`/`Grand total (EF2016A_RV  All students  Undergraduate total)`,
         `2015` = `Black or African American total (EF2015A_RV  All students  Undergraduate total)`/`Grand total (EF2015A_RV  All students  Undergraduate total)`,
         `2014` = `Black or African American total (EF2014A_RV  All students  Undergraduate total)`/`Grand total (EF2014A_RV  All students  Undergraduate total)`,
         `2013` = `Black or African American total (EF2013A_RV  All students  Undergraduate total)`/`Grand total (EF2013A_RV  All students  Undergraduate total)`,
         `2012` = `Black or African American total (EF2012A_RV  All students  Undergraduate total)`/`Grand total (EF2012A_RV  All students  Undergraduate total)`,
         `2011` = `Black or African American total (EF2011A_RV  All students  Undergraduate total)`/`Grand total (EF2011A_RV  All students  Undergraduate total)`,
         `2010` = `Black or African American total (EF2010A_RV  All students  Undergraduate total)`/`Grand total (EF2010A_RV  All students  Undergraduate total)`,
         `Institution Name` = case_when(str_detect(`Institution Name`, "Columbia") ~ "Columbia University",
                                        TRUE ~ `Institution Name`),
         `Institution Name` = case_when(str_detect(`Institution Name`, "Massachusetts") ~ "MIT",
                                        TRUE ~ `Institution Name`)) %>% 
  select(`Institution Name`, `2017`:`2010`) %>% 
  gather(year, percentage, -1) %>%
  mutate(year = as.numeric(year))

college_ts <- # create a time series
  college_pct %>% 
  mutate(key = group_indices(., `Institution Name`)) %>% 
  as_tsibble(index = year, key = key)

# brolgar -----------------------------------------------------------------

college_ts %>%
  features(percentage,
           feat_five_num)

college_ts %>%
  features(percentage,
           feat_monotonic)

# graphs ------------------------------------------------------------------

label_wrap_gen <- function(width = 120) {
  function(variable, value) {
    lapply(strwrap(as.character(value), width = width, simplify = FALSE), 
           paste, collapse="\n")
  }
}

college_pct %>% 
  ggplot(aes(x = year, y = percentage, color = `Institution Name`, group = `Institution Name`)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, .1)) +
  facet_grid(~ `Institution Name`,
             labeller=label_wrap_gen(width = .1)) +
  theme_minimal() +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(size = 18),
        legend.position = "none",
        axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_color_manual(values = c("#293333", "#4D5340", "#FF6600", "#8FB350", "#00CCCC",
                               "#FFAD01", "#B31A01", "#60B332", "#B3A87E", "#09015C", "#9F5BB3")) +
  labs(title = "Percentage of Fall Undergraduates that are Black/African American", 
       caption = "IPEDS Data Center Session Guest_60394508853")

college_pct %>% 
    ggplot(aes(x = as.numeric(year), y = percentage, group = `Institution Name`, color = `Institution Name`)) +
    geom_line() +
  scale_x_continuous(expand = c(0.25, 0)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, .1)) +
    theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Rockwell"),
        plot.title = element_text(size = 18),
        legend.position = "none") +
  geom_dl(aes(label = `Institution Name`), method = list(dl.combine("last.bumpup"), cex = 0.8)) +
  scale_color_manual(values = c("#293333", "#4D5340", "#FF6600", "#8FB350", "#00CCCC",
                                "#FFAD01", "#B31A01", "#60B332", "#B3A87E", "#09015C", "#9F5BB3")) +
  labs(title = "Percentage of Fall Undergraduates that are Black/African American",
       caption = "IPEDS Data Center Session Guest_60394508853")
  
# percent change ----------------------------------------------------------

college_pct_change <-
  college_pct %>% 
  filter(year %in% c("2010", "2017")) %>% 
  group_by(`Institution Name`) %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change = (percentage/lag(percentage) - 1) * 100)
