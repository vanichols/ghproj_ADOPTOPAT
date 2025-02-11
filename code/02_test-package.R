library(ipmopat)
library(tidyverse)
library(readxl)

#--check beta data, good
ipm_binned_betas %>% 
  mutate(ratingF = fct_inorder(rating),
         confidenceF = fct_inorder(confidence)) %>% 
  ggplot(aes(value_bin, score)) + 
  geom_col() + 
  scale_y_continuous(limits = c(0, 100)) +
  facet_grid(ratingF ~confidenceF, labeller = label_wrap_gen(width = 10))

ggsave("figs/binned-betas.png")

#--ANT data
d_ant <- 
  read_excel("data/raw/ANT-compost-lettuce.xlsx") %>% 
  fill(title)

setdiff(names(d_ant), names(ipm_exampdat))

ipm_exampdat

d1 <- Ratings2Distributions(f_dat = d_ant)
d1a <- ipm_exampprocdat

d1

setdiff(names(d1), names(d1a))


d2 <- CombineSixMetrics(f_dat = d1, f_nsamp = 10000)

d2
d2a <- ipm_exampcombdat

VisualizeResultsBarFig(f_dat1 = d1, f_dat2 = d2)

ggsave("figs/ANT-compost-lettuce-bars2.png", width = 7.8, height = 6)
