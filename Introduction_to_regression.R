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

#Galton_data

install.packages("HistData")
library(HistData)
GaltonFamilies
?HistData
str(GaltonFamilies)

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
data("GaltonFamilies")
set.seed(1983)

galton_heights %>% summarize(mean(father), sd(father), mean(son), sd(son))
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)  

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

B <- 1000
n <- 50

R <- replicate(B,{
  sample_n(galton_heights, n, replace = T) %>%
  summarize(r = cor(father,son))  %>%
  pull(r)
  })
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

mean(R)
sd(R)

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(n-2)))

str(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r = cor( AB / G, R / G)) %>%
  pull(r)

Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r = cor( W / G, E / G)) %>%
  pull(r)

Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r = cor( X2B / G, X3B / G)) %>%
  pull(r)
