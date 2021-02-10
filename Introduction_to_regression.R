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

#Stratification

sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

round(galton_heights$father)
?factor

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata =factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()  

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father) 

m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

galton_heights %>%
  ggplot(aes(son, father)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_2, slope = m_2)

#Assessment
set.seed(1989, sample.kind="Rounding") 
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

m_m <-mean(female_heights$mother)
sd_m <- sd(female_heights$mother)
m_d <- mean(female_heights$daughter)
sd_d <-sd(female_heights$daughter)
r <- cor(female_heights$mother,female_heights$daughter)


m <- r* sd_d/sd_m
b <- m_d - m_m*m
sqrt(1-r^2)
r^2

60*m +b