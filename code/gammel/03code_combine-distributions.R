#--try to create a summary that combines all of the distributions
#--adrian et al. do funny things

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
