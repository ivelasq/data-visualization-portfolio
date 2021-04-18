library(tidyverse)
library(readxl)
library(viridis)

endowments <-
  read_excel(here::here("data", "NCSE2017 Endowment Markets Values for Media--Final January 31 2018.xlsx"), skip = 2)

rankings <-
  read_csv(here::here("data", "National Universities Rankings.csv"))

joined_dat <-
  left_join(rankings, endowments, by = c("UnitID" = "Unitid1")) %>% 
  mutate(`Tuition and fees` = as.numeric(gsub("\\D", "", `Tuition and fees`)))

nrow(joined_dat)
sum(is.na(joined_dat$Rank.y))

joined_dat %>% 
  ggplot(aes(x = `Rank.x`, y = `FY2017 Endowment (in $1,000s)`, group = UnitID, color = `Tuition and fees`)) +
  geom_point(size = 2)  +
  labs(title = "2017 University Endowments vs. U.S. News Rankings",
       caption = "Source: NACUBO, U.S. News") +
  xlab("U.S. News Ranking") +
  ylab("FY2017 Endowment (in 000's)") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_viridis(option = "cividis", 
                      discrete = F,
                      labels = scales::dollar) +
  theme_minimal()

joined_dat %>% 
  ggplot(aes(x = `Rank.x`, y = `FY2017 Endowment (in $1,000s)`, group = UnitID, color = `Tuition and fees`)) +
  geom_point(size = 2)  +
  ggtitle("2017 University Endowments (log scale) vs. U.S. News Rankings") +
  xlab("U.S. News Ranking") +
  ylab("FY2017 Endowment (in 000's)") +
  scale_y_continuous(trans = "log10",
                     labels = scales::dollar) +
  scale_color_viridis(option = "cividis", 
                      discrete = F,
                      labels = scales::dollar) +
  theme_minimal()

joined_dat %>% 
  ggplot(aes(x = `Rank.x`, y = `FY2017 Endowment (in $1,000s)`, group = UnitID, color = `Tuition and fees`, size = `Undergrad Enrollment`)) +
  geom_point(size = 2)  +
  ggtitle("2017 University Endowments (log scale) vs. U.S. News Rankings") +
  xlab("U.S. News Ranking") +
  ylab("FY2017 Endowment (in 000's)") +
  scale_y_continuous(trans = "log10",
                     labels = scales::dollar) +
  scale_color_viridis(option = "cividis", 
                      discrete = F,
                      labels = scales::dollar) +
  theme_minimal()

joined_dat %>% 
  ggplot(aes(x = `Rank.x`, y = as.numeric(`Rank.y`), color = `Tuition and fees`)) +
  geom_point(size = 2)  +
  labs(title = "2017 University Endowments Ranking  vs. U.S. News Ranking",
       caption = "Source: NACUBO, U.S. News") +
  xlab("U.S. News Ranking") +
  ylab("FY2017 Endowment Ranking") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_viridis(option = "cividis", 
                      labels = scales::dollar) +
  theme_minimal()

joined_dat %>% 
  ggplot(aes(x = `Rank.x`, y = `FY2017 Endowment (in $1,000s)`, fill = `Tuition and fees`)) +
  geom_bar(stat = "identity") +
  ggtitle("University FY2017 Endowments (in 000's) vs. U.S. News Rankings") +
  xlab("U.S. News Ranking") +
  ylab("FY2017 Endowment (in 000's)") +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_viridis(option = "cividis", 
                      discrete = F,
                      labels = scales::dollar) +
  theme_minimal()
  
