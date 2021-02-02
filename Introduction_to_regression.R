install.packages("Lahman")
install.packages("tidyverse")
install.packages("dslabs")
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Lahman
data("LahmanData")
str(Teams)
?Teams

#home_runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#stolen_bases
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#base_on_balls
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#at_bats
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#win_rates
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rates = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_rates, E_per_game)) + 
  geom_point(alpha = 0.5)

#doubles_vs_triples
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B/ G, X3B_per_game = X3B / G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) + 
  geom_point(alpha = 0.5)

  
  