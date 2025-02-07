#--try to create a summary distribution myself
#--use dot plots instead of bar graphs

rm(list = ls())

library(tidyverse)
library(readxl)


#--distribution builder cheat sheet
d.beta <- read_csv("data/td_distribution-cheat-sheet.csv")

#--elicitation data as distributions
d.dist <- read_csv("data/tidy/td_dummy.csv")

#--the impact categories



# function to sample ------------------------------------------------------------------

#--sample from each impact category equal to the weight it is given

Do_MC_Sampling <- function(f_dat = d.dist, f_nsamp = 10000){
  
  f_theimps <- 
    f_dat %>% 
    pull(impact_category) %>% 
    unique()
  
  
  for(k in 1:length(f_theimps)){
    
    tmp.impact <- f_theimps[k]
    
    tmp.df <- 
      f_dat %>% 
      select(impact_category, value_bin, score) %>% 
      filter(impact_category == tmp.impact)
    
    tmp.wt <- 
      f_dat %>% 
      select(weight, impact_category) %>% 
      filter(impact_category == tmp.impact) %>% 
      distinct() %>% 
      pull(weight)
    
    tmp.samp <- sample(x = tmp.df$value_bin, prob = tmp.df$score, size = f_nsamp*tmp.wt, replace = TRUE)
    
    bayes.value.vector <- c(bayes.value.vector, tmp.samp)
    
    k <- k + 1
    
  }
  
  return(bayes.value.vector)
  
}



# do it -------------------------------------------------------------------

bayes.value.vector <- NULL

res <- Do_MC_Sampling()

tibble(x = res) %>% 
  ggplot(aes(x = x)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 6))

tibble(x = res) %>% 
  group_by(x) %>% 
  summarise(n = n()/10000)


# write it nicely ---------------------------------------------------------

d.dist

td <- 
  tibble(value_bin = res) %>% 
  group_by(value_bin) %>% 
  summarise(score = n()/10000) %>% 
  mutate(weight = 100, 
         confidence = "-",
         title = "All categories combined",
         impact_category = "total")

d.dist %>% 
  bind_rows(td) %>% 
  write_csv("data/tidy/td_dummy-all.csv")
