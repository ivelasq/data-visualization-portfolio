# ProPublica Civilian Complaints Against New York City Police Officers
# Circle Packing

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(packcircles)
library(ggtext)
library(showtext)

# Font --------------------------------------------------------------------

font_add_google("Rubik", "rubik")

# Create Data -------------------------------------------------------------
# Unfortunately, I cannot share the data.
# It is available for download here: https://www.propublica.org/datastore/dataset/civilian-complaints-against-new-york-city-police-officers

dat <-
  read_csv(here::here("data", "raw", "allegations_20200726939.csv"))

d_plot <-
  dat %>% 
  filter(complainant_ethnicity %in% c("Black", "Hispanic", "White"),
         year_received >= "2015",
         str_detect(board_disposition, "Substantiated")) %>% 
  group_by(complainant_ethnicity, fado_type, allegation) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(color = case_when(complainant_ethnicity == "Hispanic" ~ "#ee6a59",
                           complainant_ethnicity == "Black" ~ "#3a3f58",
                           complainant_ethnicity == "White" ~ "#f9ac67"),
         allegation = case_when(allegation == "Word" ~ "Discourtesy (Word)",
                                TRUE ~ allegation))

# Create Circle Packing ---------------------------------------------------
# Tutorial: https://cran.r-project.org/web/packages/packcircles/vignettes/progressive_packing.html

showtext_auto()
quartz()

packing <- 
  circleProgressiveLayout(d_plot$n)

d_labels <- 
  bind_cols(d_plot, packing)

dat.gg <- circleLayoutVertices(packing, npoints=50)

ggplot(data = dat.gg) +
  geom_polygon(aes(x, y, group = id, fill = factor(id)), 
               colour = "white",
               size = .1,
               show.legend = FALSE) +
  geom_text(data = d_labels %>% filter(n > 20), 
            color = "white",
            aes(x, y, size = n*1/nchar(allegation), 
                label = stringr::str_wrap(allegation, width = 10),
                family = "rubik"),
            show.legend = FALSE) +
  scale_fill_manual(values = d_plot$color) +
  scale_y_reverse() +
  coord_equal() +
  theme_void() +
  labs(title = "Most Common Substantiated Allegations Against NYC Police Officers, 2015-2019",
       subtitle = "<b><span style = 'color:#3a3f58;'>Black</span></b>, <b><span style = 'color:#f9ac67;'>White</span></b>, and <b><span style = 'color:#ee6a59;'>Hispanic</span></b> Complainants",
       caption = "<b>Source:</b> ProPublica Civilian Complaints Against New York City Police Officers") +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 2,
                                  size = 16,
                                  family = "rubik"),
        plot.subtitle = element_textbox_simple(halign = 0.5,
                                               family = "rubik",
                                               size = 12),
        plot.caption = element_textbox_simple(halign = 1,
                                              family = "rubik"))

ggsave(here::here("figs", "bubble.png"), dpi = "retina", width = 10, height = 6, units = "in")


# Create Second Plot ------------------------------------------------------

ggplot(data = dat.gg) +
  geom_polygon(aes(x, y, group = id, fill = factor(id)), 
               colour = "white",
               size = .1,
               show.legend = FALSE) +
  geom_text(data = d_labels %>% group_by(fado_type) %>% top_n(20), 
            aes(x, y, size = n*1/nchar(allegation), 
                label = stringr::str_wrap(allegation, width = 10),
                family = "rubik",
                color = "white"),
            show.legend = FALSE) +
  scale_fill_manual(values = d_plot$color) +
  scale_y_reverse() +
  coord_equal() +
  theme_void() +
  labs(title = "Most Common Allegations Against New York City Police Officers, 2015-2019",
       subtitle = "<b><span style = 'color:#3a3f58;'>Black</span></b>, <b><span style = 'color:#f9ac67;'>White</span></b>, and <b><span style = 'color:#ee6a59;'>Hispanic</span></b> Complainants",
       caption = "<b>Source:</b> ProPublica Civilian Complaints Against New York City Police Officers") +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 2,
                                  size = 16,
                                  family = "rubik"),
        plot.subtitle = element_textbox_simple(halign = 0.5,
                                               family = "rubik",
                                               size = 12),
        plot.caption = element_textbox_simple(halign = 1,
                                              family = "rubik")) +
  facet_grid(~ fado_type)

ggsave(here::here("figs", "bubble2.png"), dpi = "retina", width = 10, height = 6, units = "in")

