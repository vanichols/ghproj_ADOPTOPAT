#--functions


# translate elicitation to distribution -----------------------------------

Ratings_2_Distributions <- function(f_dat = d.elic, d_dat = d.beta){
  
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


# monte carlo sim ---------------------------------------------------------


Do_MC_Sampling <- function(f_dat = d.dist, f_nsamp = 10000){
  
  f_theimps <- 
    f_dat %>% 
    pull(impact_category) %>% 
    unique()
  
  tmp.scen <- 
    f_dat %>% 
    pull(scenario) %>% 
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
  
  dat <- 
    tibble(value_bin = bayes.value.vector) %>% 
    group_by(value_bin) %>% 
    summarise(score = n()/10000) %>% 
    mutate(weight = 100, 
           confidence = "-",
           title = "All categories combined",
           impact_category = "all", 
           scenario = tmp.scen)
  
  return(dat)
  
}


