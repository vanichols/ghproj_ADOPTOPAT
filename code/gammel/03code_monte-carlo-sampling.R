#--try to create a summary distribution myself
#--use dot plots instead of bar graphs

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("00code_color-palettes.R")

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

#--elicitaion data
d.elic <- 
  read_excel("dummy-ratings.xlsx", skip = 5) %>% 
  janitor::clean_names()

#--the impact categories
the_impcats <- 
  d.elic %>% 
  pull(impact_category) %>% 
  unique()


#--distribution builder
d.beta <- 
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


# losses ------------------------------------------------------------------

Get_Values <- function(f_dat = d.elic, d_dat = d.beta){
  
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

d.dist <- Get_Values()

d.dist %>% 
  ggplot(aes(value_bin, score)) + 
  geom_col() + 
  facet_grid(.~impact_category + confidence)


# build vectors to sample from-----------------------------------------------------------

d.forsampling <- NULL

for (i in 1:length(the_impcats)){
  
  t.impcat <- the_impcats[i]
  
  t.b <- 
    d.dist %>% 
    select(impact_category, score, value_bin) %>% 
    filter(impact_category == t.impcat)
  
  v.all <- NULL
  
  for (j in 1:nrow(t.b)){
    
    t.b.v <- 
      t.b %>% 
      slice(j)
    
    t.v <- rep(x = t.b.v$value_bin, times = t.b.v$score)
    
    v.all <- c(v.all, t.v)
    
   j <- j + 1 
  }
    
  t.d.all <- tibble(impact_category = t.b.v$impact_category,
                    bayes_value = v.all)
  
  d.forsampling <- bind_rows(d.forsampling, t.d.all)
  
  i <- i + 1
  
}

d.forsampling


# sample ------------------------------------------------------------------

bayes.value.vector <- NULL

#--sample from each imNULL#--sample from each impact category equal to the weight it is given
for(k in 1:length(the_impcats)){
  
  tmp.impact <- the_impcats[k]
  
  tmp.df <- 
    d.dist %>% 
    select(impact_category, value_bin, score) %>% 
    filter(impact_category == tmp.impact)
  
  tmp.wt <- 
    d.elic %>% 
    select(weight, impact_category) %>% 
    filter(impact_category == tmp.impact) %>% 
    pull(weight)

  tmp.samp <- sample(x = tmp.df$value_bin, prob = tmp.df$score, size = 100*tmp.wt, replace = TRUE)
  
  bayes.value.vector <- c(bayes.value.vector, tmp.samp)
  
  k <- k + 1
  
}

tibble(x = bayes.value.vector) %>% 
  ggplot(aes(x = x)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 6))

tibble(x = bayes.value.vector) %>% 
  group_by(x) %>% 
  summarise(n = n()/100)
  