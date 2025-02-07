#--use beta distributions from uncertainty + values from elicitations to create value distribution
#--should read in data from the 'raw' folder
#--writes data to the 'tidy' folder
#--uses the Trans... function

rm(list = ls())

library(tidyverse)
library(readxl)

source("code/00_fxns.R")
source("code/00_color-palettes.R")

#--distribution builder cheat sheet
d.beta <- read_csv("data/td_distribution-cheat-sheet.csv")



# ANT ---------------------------------------------------------------------

e <- 
  read_excel("data/raw/ANT-compost-lettuce.xlsx", skip = 5) %>% 
  janitor::clean_names() %>% 
  mutate(weight = as.numeric(weight))

e1 <- 
  e %>% 
  filter(scenario == "CCP")

e2 <- 
  e %>% 
  filter(scenario == "ADOPT")

  
# A. translate ratings to distributions --------------------------------------

a1 <- Ratings_2_Distributions(f_dat = e1, d_dat = d.beta)
a2 <- Ratings_2_Distributions(f_dat = e2, d_dat = d.beta)

a3 <- 
  a1 %>% 
  bind_rows(a2) 

#--just to check
a3 %>% 
  ggplot(aes(value_bin, score)) + 
  geom_col() + 
  facet_grid(scenario~impact_category)


# B. MC sampling ----------------------------------------------------------

bayes.value.vector <- NULL
b1 <- Do_MC_Sampling(f_dat = a1, f_nsamp = 10000) 

bayes.value.vector <- NULL
b2 <- Do_MC_Sampling(f_dat = a2, f_nsamp = 10000)

b3 <- 
  b1 %>% 
  bind_rows(b2)


# C. all the data combined ------------------------------------------------

c1 <- 
  a3 %>% 
  bind_rows(b3) %>% 
  mutate(mini_facet = ifelse(impact_category == "all", "Combined", "Individual"),
         title = paste0(title, " (", weight, "%)"))

# D. visualize ------------------------------------------------------------


# dot plot ----------------------------------------------------------------


c1 %>% 
  arrange(scenario, weight) %>% 
  mutate(value_binF = as.factor(value_bin),
         impact_category = str_to_sentence(impact_category),
         titleF = fct_inorder(title),
         scenarioF = factor(scenario, levels = c("CCP", "ADOPT"))) %>% 
  arrange(scenario, titleF) %>% 
  ggplot(aes(value_bin, reorder(title, weight))) + 
  geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 6.5, ymax = 7.5), fill = "white") +
  geom_point(aes(fill = value_binF, size = score*10, alpha = mini_facet), show.legend = F, pch = 21, stroke = 1.2) + 
  scale_size(range = c(0, 7)) +
  scale_fill_manual(values = c(cv1, cv2, cv3, cv4, cv5)) +
  scale_alpha_manual(values = c(1, 0.15)) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(0.5, 5.5),
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low value", "Low value", "Medium value", "Highly valuable", "Very highly valuable"), 
    position = "top") +
  labs(x = NULL,
       y = NULL,
       title = "ANT Compost Lettuce") +
  th2 +
  facet_grid(scenarioF ~.) +
  theme(panel.background = element_rect(fill = 'gray95', color = 'black'),
        panel.grid.major.y = element_line(color = "gray"),
        axis.text.x = element_text(angle = 45, hjust = 0))



c1 %>% 
  arrange(scenario, weight) %>% 
  mutate(value_binF = as.factor(value_bin),
         impact_category = str_to_sentence(impact_category),
         titleF = fct_inorder(title),
         scenarioF = factor(scenario, levels = c("CCP", "ADOPT"))) %>% 
  arrange(scenario, titleF) %>% 
  ggplot(aes(value_bin, reorder(title, weight))) + 
  geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 6.5, ymax = 7.5), fill = "white") +
  geom_point(aes(fill = value_binF, size = score*10, alpha = mini_facet), show.legend = F, pch = 21, stroke = 1.2) + 
  scale_size(range = c(0, 7)) +
  scale_fill_manual(values = c(cv1, cv2, cv3, cv4, cv5)) +
  scale_alpha_manual(values = c(1, 0.15)) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(0.5, 5.5),
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low value", "Low value", "Medium value", "Highly valuable", "Very highly valuable"), 
    #position = "top"
    ) +
  labs(x = NULL,
       y = NULL,
       title = "ANT Compost Lettuce") +
  th2 +
  facet_grid(.~scenarioF) +
  theme(panel.background = element_rect(fill = 'gray95', color = 'black'),
        panel.grid.major.y = element_line(color = "gray"),
        axis.text.x = element_text(angle = 45, hjust = 1))


# bar plot ----------------------------------------------------------------


c1 %>% 
  arrange(scenario, weight) %>% 
  mutate(value_binF = as.factor(value_bin),
         impact_categoryF = fct_inorder(impact_category),
         titleF = fct_inorder(title)) %>% 
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
  facet_grid(scenario~ mini_facet + impact_categoryF)


