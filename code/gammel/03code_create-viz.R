#--create visualizations of distribtions

rm(list = ls())

source("code/00code_color-palettes.R")

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

#--the data 
d <- 
    read_csv("data/tidy/td_dummy-all.csv")



# bar plot ----------------------------------------------------------------

d %>% 
  arrange(weight) %>% 
  mutate(value_binF = as.factor(value_bin),
         impact_categoryF = fct_inorder(impact_category)) %>% 
  ggplot(aes(value_bin, score/100)) + 
  geom_col(aes(fill = value_binF), size = 2, color = "black", show.legend = F) + 
  scale_fill_manual(values = c(cv1, cv2, cv3, cv4, cv5)) + 
  scale_y_continuous(labels = label_percent()) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low value", "Low value", "Medium value", "Highly valuable", "Very highly valuable")) +
  labs(x = NULL,
       y = "Probability") +
  th1 + 
  facet_grid(.~ impact_categoryF + confidence + weight)


# dot plot ----------------------------------------------------------------


d %>% 
  mutate(value_binF = as.factor(value_bin),
         impact_category = str_to_sentence(impact_category)) %>% 
  ggplot(aes(value_bin, impact_category)) + 
  geom_point(aes(fill = value_binF, size = score*10), show.legend = F, pch = 21) + 
  scale_fill_manual(values = c(cv1, cv2, cv3, cv4, cv5)) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low value", "Low value", "Medium value", "Highly valuable", "Very highly valuable"), 
    position = "top") +
  labs(x = NULL,
       y = NULL) +
  th1 +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.y = element_line(color = "gray"),
        axis.text.x = element_text(angle = 45, hjust = 0))


ggsave("fig_dummy-dots.png")
