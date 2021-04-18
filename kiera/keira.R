# Library
library(ggplot2)
library(dplyr)
library(forcats)
library(ggpomological)

# Lollipop Chart
# https://www.r-graph-gallery.com/303-lollipop-plot-with-2-values.html

# Create data
dat <- 
  tibble::tribble(
    ~movie_year, ~set_year,                      ~movie,
          2002L,     1917L,            "Doctor Zhivago",
          2003L,     1720L,  "Pirates of the Caribbean",
          2004L,     1410L,               "King Arthur",
          2005L,     1992L,                "The Jacket",
          2005L,     1797L,       "Pride and Prejudice",
          2007L,     1935L,                 "Atonement",
          2007L,     1840L,                      "Silk",
          2008L,     1940L,          "The Edge of Love",
          2008L,     1774L,               "The Duchess",
          2010L,     1978L,           "Never Let Me Go",
          2011L,     1904L,        "A Dangerous Method",
          2012L,     1874L,             "Anna Karenina",
          2014L,     2001L, "Jack Ryan: Shadow Recruit",
          2014L,     1939L,        "The Imitation Game",
          2015L,     1996L,                   "Everest",
          2018L,     1893L,                   "Colette",
          2019L,     2003L,          "Official Secrets",
          2019L,     1945L,             "The Aftermath",
          2020L,     1970L,              "Misbehaviour"
    )

# Reorder data using average? Learn more about reordering in chart #267
dat <- 
  dat %>% 
  rowwise() %>% 
  arrange(movie_year)

ggplot(dat) +
  geom_segment(aes(x = fct_reorder(movie, set_year), 
                   xend = movie, 
                   y = movie_year, 
                   yend = set_year), 
               color="grey") +
  geom_point(aes(x = movie, 
                 y = set_year), 
             color = rgb(0.2, 0.7, 0.1, 0.5), size = 7) +
  geom_point(aes(x = movie, y = movie_year), color= "#9400D3", size = 1, alpha = .3) +
  annotate("text", 
           x = dat$movie, 
           y = dat$set_year, 
           label = dat$movie, 
           color = "orange", 
           size = 4 , 
           angle = 0, 
           fontface = "bold", 
           hjust = -0.12, 
           vjust = -0.1) +
  coord_flip() +
  theme_pomological() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  ggtitle(label = "Where in the past is Keira Knightley?")
