# BOTW Weapons Data Pull
# Dendrogram 1

# libraries ---------------------------------------------------------------

library(tidyverse)
library(ggdendro)

# read data ---------------------------------------------------------------

weapons_meta <- 
  read_csv(here::here("data", "processed", "weapons_meta.csv"))

# visualization -----------------------------------------------------------

w_distdat <- as.data.frame(weapons_meta)

rownames(w_distdat) <- w_distdat$name

w_distdat <-
  w_distdat %>% 
  select(category, subtype, ap_dur) %>% 
  na.omit()

dist_w <- 
  dist(w_distdat)

clusters_w <- hclust(dist_w)

clusters_data <- as.dendrogram(clusters_w)
clusters_data_ggd <- dendro_data(clusters_data, type = "rectangle")
seg_data <- segment(clusters_data_ggd)

ggdendrogram(clusters_w, rotate = T, size = 2) +
  coord_flip() + 
  theme_dendro()

number_of_bar <- nrow(clusters_data_ggd$labels)
angle <- 90 - 360 * (as.numeric(rownames(clusters_data_ggd$labels)) - 0.5) / number_of_bar
clusters_data_ggd$labels$hjust <- ifelse( angle < -90, 1, 0)
clusters_data_ggd$labels$angle <- ifelse(angle < -90, angle + 180, angle)

label_data <- as_tibble(clusters_data_ggd$labels)

label_data <- 
  label_data %>% 
  mutate(
    ap_dur_y = 8, # most inner
    subtype_y = 7.66, 
    category_y = 7.33 # most outer
  )

label_data <- left_join(label_data, as_tibble(w_distdat, rownames = "label"), by = "label") %>%
  mutate_at(vars(category:ap_dur), scale) %>%
  mutate_at(vars(category:ap_dur), ~ntile(., 10))

endpoint_data <- as_tibble(seg_data %>% filter(yend == 0)) %>% add_row(yend = 0, x = max(seg_data$x) + 1)

clusters_data_ggd$labels <- 
  clusters_data_ggd$labels %>% 
  add_row(label = "", x = max(clusters_data_ggd$labels$x) + 1, y = 0, hjust = 0, angle = 90)

ggplot(seg_data) +
  geom_segment(aes(x = x, y = log(y + 1), xend = xend, yend = log(yend + 1)),
               alpha = 0.6) +
  geom_point(data = endpoint_data,
             aes(x = x, y = log(yend+1))) +
  geom_text(data = clusters_data_ggd$labels,
            aes(x=x, y= log(y+1) - 0.3, label = label, hjust = hjust), 
            size = 1.8,
            angle = clusters_data_ggd$labels$angle, inherit.aes = FALSE) +
  # feature layers ordered from inner to outer
  geom_point(data = label_data,
             aes(x = x, y = category_y), 
             stroke = 0, alpha = 0.99, shape = 18) +
  geom_point(data = label_data,
             aes(x = x, y = subtype_y), 
             stroke = 0, alpha = 0.99, shape = 18) +
  geom_point(data = label_data,
             aes(x = x, y = ap_dur_y), 
             stroke = 0, alpha = 0.99, shape = 18) +
  coord_polar(start = 0) + 
  scale_y_reverse(limits = c(8, -10), 
                  expand = c(0.2, 0)) +
  scale_size(range = c(0.1, 1.5)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    text = element_text(family = "Helvetica"),
    legend.position = "none"
  )

ggsave(here::here("figs", "weapons_plot.tiff"), dpi = "retina", width = 10, height = 10, units = "in")

