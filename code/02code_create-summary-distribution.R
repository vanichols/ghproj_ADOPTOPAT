#--try to create a summary distribution myself
#--use dot plots instead of bar graphs

rm(list = ls())

library(tidyverse)
library(readxl)

#--elicitaion data
d.elic <- 
  read_excel("data/raw/dummy-ratings.xlsx", skip = 5) %>% 
  janitor::clean_names()

#--the impact categories
the_impcats <- 
  d.elic %>% 
  pull(impact_category) %>% 
  unique()

#--distribution builder cheat sheet
d.beta <- read_csv("data/td_distribution-cheat-sheet.csv")

#--elicitation data as distributions
d.dist <- read_csv("data/")

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
  