#--use beta distributions from uncertainty + values from elicitations to create value distribution
#--should read in data from the 'raw' folder
#--writes data to the 'tidy' folder
#--should be turned into a function to run on the 'raw' elicitation data at some point

rm(list = ls())

library(tidyverse)
library(readxl)

#--my data
d.elic <- 
  read_excel("data/raw/dummy-ratings.xlsx", skip = 5) %>% 
  janitor::clean_names()

#--distribution builder cheat sheet
d.beta <- read_csv("data/td_distribution-cheat-sheet.csv")


# translate ratings to distributions --------------------------------------


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


# write it ----------------------------------------------------------------

d.dist %>% 
  write_csv("data/tidy/td_dummy.csv")
  