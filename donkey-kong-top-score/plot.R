# Donkey Kong High Score Competition

# libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)
library(polite)
library(lubridate)
library(ggdark)
library(showtext)
library(cowplot)
library(ggimage)

# font --------------------------------------------------------------------

font_add_google("PT Sans", "pt+sans")
font_add_google("VT323", "vt323")

# img ---------------------------------------------------------------------

img <- magick::image_read("https://vignette.wikia.nocookie.net/villains/images/0/09/DonkeyKong.png")

img2 <- ("/Users/shortessay/Desktop/barrel.png")

# data --------------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Donkey_Kong_high_score_competition"
session <- bow(url)

scores_html <-
  scrape(session) %>% 
  html_nodes("table.wikitable") %>% 
  html_table(fill = TRUE) 

scores_df <-
  scores_html %>% 
  lapply(., mutate_if, is.integer, as.character) %>%
  reduce(bind_rows) 

scores_tab <-
  scores_df %>% 
  mutate_at(vars(Score), list(~ as.numeric(str_replace_all(., ",", "")))) %>% 
  mutate_at(vars(Date), list(~ mdy(.))) %>% 
  add_row(Date = Sys.Date(), Score = 1259000, Player = "John McCurdy", Notes = "") %>% 
  mutate(new_record = case_when(Player != lag(Player) | Player == "Billy Mitchell"~  "New Record Holder",
                                TRUE ~ NA_character_),
         image = img2)

# plot --------------------------------------------------------------------

showtext_auto()
quartz()

p <-
  scores_tab %>% 
  ggplot() + 
  geom_step(aes(x = Date,
                y = Score),
            color = "#de042a",
            size = 1.5) +
  geom_image(data = scores_tab %>% filter(!is.na(new_record)),
             aes(x = Date,
                 y = Score, 
                 image = image),
             size = .008) +
  dark_theme_minimal(
    base_family = "vt323"
    ) +
  expand_limits(x = c(as.Date("1982-08-18", "%Y-%m-%d"),
                      as.Date("2024-01-01", "%Y-%m-%d")),
                y = 1300000) +
  annotate(geom = "curve", 
           x = as.Date("1982-08-19", "%Y-%m-%d"), 
           y = 878000, 
           xend = as.Date("1985-01-01", "%Y-%m-%d"), 
           yend = 950000, 
           curvature = -.23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("1985-03-01", "%Y-%m-%d"), 
           y = 950000, 
           family = "pt+sans",
           size = 4,
           label = "Billy Mitchell sets a record score of 874,300 in 1982.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  annotate(geom = "curve", 
           x = as.Date("2000-06-01", "%Y-%m-%d"), 
           y = 879200, 
           xend = as.Date("1999-03-01", "%Y-%m-%d"), 
           yend = 900000, 
           curvature = -.23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("1985-03-01", "%Y-%m-%d"), 
           y = 910000, 
           family = "pt+sans",
           size = 4,
           label = "Tim Sczerby takes the new high score in 2000 with 879,200.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  annotate(geom = "curve", 
           x = as.Date("2005-10-01", "%Y-%m-%d"), 
           y = 980600, 
           xend = as.Date("2007-03-01", "%Y-%m-%d"), 
           yend = 970000, 
           curvature = .23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("2007-05-20", "%Y-%m-%d"),
           y = 970000, 
           family = "pt+sans",
           size = 4,
           label = "Steve Wiebe gains the new high score and later\nis the first player to surpass 1 million points.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  annotate(geom = "curve", 
           x = as.Date("2009-12-01", "%Y-%m-%d"), 
           y = 1061200, 
           xend = as.Date("2007-06-01", "%Y-%m-%d"), 
           yend = 1100000, 
           curvature = -.23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("2000-03-01", "%Y-%m-%d"),
           y = 1135000, 
           family = "pt+sans",
           size = 4,
           label = "Hank Chien surpasses Wiebe.\nWiebe momentarily takes it back,\nbut Chien reclaims the high score.\nHe is the record holder until 2014.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  annotate(geom = "curve", 
           x = as.Date("2015-01-01", "%Y-%m-%d"), 
           y = 1135000, 
           xend = as.Date("2016-01-01", "%Y-%m-%d"), 
           yend = 1125000, 
           curvature = .23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("2016-03-01", "%Y-%m-%d"),
           y = 1125000, 
           family = "pt+sans",
           size = 4,
           label = "In 2014,\nRobbie Lakeman\novertakes Chien.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  annotate(geom = "curve", 
           x = as.Date("2015-06-01", "%Y-%m-%d"), 
           y = 1170000, 
           xend = as.Date("2013-08-01", "%Y-%m-%d"), 
           yend = 1200000, 
           curvature = -.23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("1997-09-01", "%Y-%m-%d"), 
           y = 1225000, 
           family = "pt+sans",
           size = 4,
           label = "Wes Copeland bests Lakeman for the high score in 2015.\nLakeman is able to reclaim the record within six hours.\nThe two alternate holding the record until 2019.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  annotate(geom = "curve", 
           x = as.Date("2019-05-01", "%Y-%m-%d"), 
           y = 1245000, 
           xend = as.Date("2020-06-01", "%Y-%m-%d"), 
           yend = 1237000, 
           curvature = .23, 
           color = "#11f6f9", 
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", 
           x = as.Date("2020-08-01", "%Y-%m-%d"), 
           y = 1237000, 
           family = "pt+sans",
           size = 4,
           label = "John McCurdy\nbecomes the\nnew and current\nrecord holder.", 
           hjust = "left",
           lineheight = 0.9,
           color = "white") +
  # geom_hline(yintercept = 1259000, 
  #            linetype = "dashed", 
  #            size = 0.25, 
  #            color = "#de042a") +
  # annotate(geom = "text", 
  #          x = as.Date("1982-10-01", "%Y-%m-%d"), 
  #          y = 1265000, 
  #          #family = "pt_sans",
  #          size = 3,
  #          label = "Current High Score: 1,259,000", 
  #          hjust = "left",
  #          lineheight = 0.9,
  #          color = "white") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("#f6b90a", "#de042a")) +
  labs(title = "DONKEY KONG HIGH SCORE COMPETITION",
       subtitle = "CURRENT RECORD: 1259000",
       caption = "Source: https://en.wikipedia.org/wiki/Donkey_Kong_high_score_competition") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#de042a", 
                                  family = "vt323",
                                  size = 24,
                                  hjust = 0.5),
        plot.subtitle = element_text(color = "white",
                                     family = "vt323",
                                     size = 18,
                                     hjust = 0.5)) 

ggdraw() +
  draw_plot(p) +
  cowplot::draw_image(img,
                      x = 0.422,
                      y = 0.372,
                      scale = .12)

ggsave2("dk_plot.png", dpi = "retina", width = 10, height = 6, units = "in")

