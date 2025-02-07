#--try to create a summary distribution myself
#--adrian catastrophizes things

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("code_color-palettes.R")

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

#--my data
m <- 
  read_excel("dummy-ratings.xlsx", skip = 5) %>% 
  janitor::clean_names()

#--distribution builder
d <- 
  read_excel("distribution-cheat-sheet.xlsx", skip = 5) %>% 
  select(-tot) %>% 
  fill(confidence) %>% 
  janitor::clean_names() %>% 
  pivot_longer(x1:x5) %>% 
  mutate(name = parse_number(name),
         value_bin = case_when(
           name == 1 ~ 5,
           name == 2 ~ 4,
           name == 3 ~ 3,
           name == 4 ~ 2,
           name == 5 ~ 1,
           TRUE ~ 9999
         )) %>% 
  rename(score = value) %>% 
  select(-name)

d
m

# losses ------------------------------------------------------------------

Make_Fig <- function(imp_cat = "coordination requirements"){
  
  m1 <- 
    m %>% 
    filter(impact_category == imp_cat)
  
  t1 <- str_to_title(m1$title)
  
  p1 <- 
    d %>% 
    filter(rating == m1$rating,
           confidence == m1$confidence) %>% 
    mutate(value_binF = as.factor(value_bin)) %>% 
    ggplot(aes(value_bin, score/100)) + 
    geom_col(aes(fill = value_binF), size = 2, color = "black", show.legend = F) + 
    scale_fill_manual(values = c(cv1, cv2, cv3, cv4, cv5)) + 
    scale_y_continuous(labels = label_percent()) +
    scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      labels = c("Very low value", "Low value", "Medium value", "Highly valuable", "Very highly valuable")) +
    labs(x = NULL,
         y = "Probability",
         title = str_wrap(t1, 25)) +
  th1
  
  return(p1)
  }


m


p1 <- Make_Fig(imp_cat = "losses")
p2 <- Make_Fig(imp_cat = "health and safety")
p3 <- Make_Fig(imp_cat = "direct costs")
p4 <- Make_Fig(imp_cat = "environment")
p5 <- Make_Fig(imp_cat = "time and management")
p6 <- Make_Fig(imp_cat = "coordination requirements")

p1 + p2 + p3 + p4 + p5 + p6

ggsave("fig_dummy.png", width = 12, height = 5.5)
