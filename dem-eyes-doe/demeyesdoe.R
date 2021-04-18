# DemEyesDoe Flair Analysis

# Python Code -------------------------------------------------------------
# https://towardsdatascience.com/scraping-reddit-data-1c0af3040768

# pip install praw
# pip install pandas
# 
# import praw
# import pandas as pd
# 
# reddit = praw.Reddit(client_id = 'client_id',
#                      client_secret = 'secret',
#                      user_agent = 'user_agent')
# 
# subreddit = reddit.subreddit('DemEyesDoe').hot()
# 
# posts = []
# eyes_subreddit = reddit.subreddit('DemEyesDoe')
# for sub in eyes_subreddit.hot(limit = 1000):
#   posts.append([sub.link_flair_text, sub.score])
# posts = pd.DataFrame(posts, columns = ['flair', 'score'])
# print(posts)

# R Code ------------------------------------------------------------------

library(tidyverse)
library(cowplot)
# library(ggridges)

d <- 
  read_csv("/Users/shortessay/posts.csv") %>% 
  filter(!is.na(flair))

d_avg <-
  d %>% 
  group_by(flair) %>% 
  summarise(avg_score = mean(score, na.rm = T))

d_avg %>% 
  ggplot(aes(y = avg_score, x = reorder(flair, -avg_score), fill = flair)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Number of Upvotes by Flair",
       subtitle = "Subreddit /r/DemEyesDoe, n = 978") +
  scale_fill_manual(values = c("Green" = "#374025", 
                               "Blue" = "#95ACBF", 
                               "Hetrochromia" = "#D9CEB0", 
                               "Hazel" = "#A6896F", 
                               "Brown" = "#592B27", 
                               "Black" = "#000000",
                               "Grey" = "#D8D9C7")) +
  # geom_text(aes(label = round(avg_score, digits = 1)),
  #           vjust = -0.5,
  #           color = "black",
  #           size = 3) +
  theme(axis.title.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") -> p1

# d %>% 
#   ggplot(aes(x = score, y = flair)) +
#   geom_density_ridges()

# d %>% 
#   ggplot(aes(x = score, group = flair, fill = flair)) +
#   geom_density(alpha = .3)

d %>% 
  ggplot(aes(y = score, x = reorder(flair, -score, mean), color = flair)) +
  geom_jitter(position = position_jitter(0.2),
              alpha = 0.5) +
  # stat_summary(fun.y=mean, geom="point", shape=18,
  #                size = 3, color="red") +
  theme_minimal() +
  labs(title = "Distribution of Upvotes by Flair",
       subtitle = "Subreddit /r/DemEyesDoe, n = 978") +
  scale_color_manual(values = c("Green" = "#374025", 
                               "Blue" = "#95ACBF", 
                               "Hetrochromia" = "#D9CEB0", 
                               "Hazel" = "#A6896F", 
                               "Brown" = "#592B27", 
                               "Black" = "#000000",
                               "Grey" = "#D8D9C7")) +
  theme(axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") -> p2

plot_grid(p1,
          p2,
          ncol = 1)

ggsave(here::here("eyes.png"), dpi = "retina", width = 12, height = 8, units = "in")
