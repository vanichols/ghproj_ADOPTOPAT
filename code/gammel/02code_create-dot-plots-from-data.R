#--try to create a summary distribution myself
#--use dot plots instead of bar graphs

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("00code_color-palettes.R")

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

Get_Values <- function(f_dat = m, d_dat = d){
  
  t.cats <-
    f_dat %>% 
    pull(impact_category)
  
  i <- 1
  
  t.m <- 
    f_dat %>% 
    filter(impact_category == t.cats[i])
  
  t.sum <- 
    t.m %>% 
    left_join(
      d_dat %>% 
        filter(rating == t.m$rating,
               confidence == t.m$confidence)
    )
  
  
  for(i in 2:length(t.cats)){
  
    t.m <- 
      f_dat %>% 
      filter(impact_category == t.cats[i])
  
    t.st <- 
      t.m %>% 
      left_join(
        d_dat %>% 
          filter(rating == t.m$rating,
                 confidence == t.m$confidence)
      )
    
    t.sum <- 
      bind_rows(t.sum, t.st)
      
    i <- i + 1
    
  }
  
  return(t.sum)
}

b <- Get_Values()

b %>% 
  mutate(value_binF = as.factor(value_bin)) %>% 
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
  facet_grid(.~ impact_category)

b %>% 
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
