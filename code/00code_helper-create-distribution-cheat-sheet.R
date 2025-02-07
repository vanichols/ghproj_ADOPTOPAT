#--recreate the beta distributions the EUCLID project used
#   for assigning uncertainty to the ratings
#   and distribute them amongst five bins

#--I used this code as a helper tool, I created the distribution cheat sheet by hand

rm(list = ls())

library(tidyverse)
library(readxl)


# process cheat sheet into tidy data --------------------------------------

#--distribution builder
d.beta <- 
  read_excel("data/byhand_distribution-cheat-sheet.xlsx", skip = 5) %>% 
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


d.beta %>% 
  write_csv("data/td_distribution-cheat-sheet.csv")


# use this to help: https://mathlets.org/mathlets/beta-distribution/

#### very low confidence rating ####

# get 35% in 1 rating (left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 1, 2))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 35% in 2 rating (second to left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 1.7, 3))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()


# get 35% in 3 rating (middle) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 2.8, 2.8))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()


#### medium confidence rating ####

# get 50% in 3 rating (middle) -------------------------------------------------------
#--ratio of parameters is 1

tst <- tibble(x = rbeta(10000, 5.8, 5.8))

tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 50% in 2 rating (second to left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 5, 8.2))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 50% in 1 rating (left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 1.5, 5.2))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()


#### high confidence rating ####

# get 80% in 3 rating (middle) -------------------------------------------------------
#--ratio of parameters is 1

tst <- tibble(x = rbeta(10000, 20, 20))

tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 80% in 2 rating (second to left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 10, 25))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 80% in 1 rating (left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 1.5, 10.2))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()


#### very high confidence rating ####

# get 90% in 3 rating (middle) -------------------------------------------------------
#--ratio of parameters is 1

tst <- tibble(x = rbeta(10000, 35, 35))

tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 90% in 2 rating (second to left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 15, 35))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

# get 90% in 1 rating (left) -------------------------------------------------------

tst <- tibble(x = rbeta(10000, 1.5, 10.2))


tst %>% 
  mutate(bin = cut(x, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
  group_by(bin) %>% 
  summarise(nu = n()/10000)


tst %>% 
  ggplot(aes(x = x)) + 
  geom_histogram()

