#chapter17 1-2
library(dplyr)
library(tidyverse)
library(HistData)
library(ggplot2)
install.packages("HistData")
data("GaltonFamilies")
head(GaltonFamilies)

galton_heights <-GaltonFamilies %>%
filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarize(mean(father),sd(father), mean(son), sd(son))

galton_heights %>%
  ggplot(aes(father,son))+geom_point(alpha=0.5)

galton_heights %>% summarize(r=cor(father,son))
mean(scale(galton_heights$father)*scale(galton_heights$son))
cor(galton_heights$father,galton_heights$son)

R<- galton_heights %>% sample_n(25, replace = T) %>%
  summarize(r =cor(father,son))

B <- 1000
N <-200
R <- replicate(B,{
  sample_n(galton_heights,N , replace = T) %>%
  summarize(r=cor(father,son)) %>%
  pull(r)
})

#qplot
qplot(R, geom = "histogram", binwidth = 0.01, color = I("black"))

#ggplot
df <-data.frame(R)
ggplot(df,aes(x=R,col="black"))+geom_histogram(binwidth =0.01)

#draw qqplot
ggplot(aes(sample=R), data= data.frame(R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), 
              slope = sqrt((1-mean(R)^2)/(N-2)))

#conditional expectations

conditional_avg <- galton_heights %>% 
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>% 
  pull(avg)
conditional_avg

sd(galton_heights$son)

galton_heights %>% mutate(father_strata = factor(round(father))) %>% 
  ggplot(aes(father_strata, son)) + 
  geom_boxplot() + 
  geom_point()


galton_heights %>%
  mutate(father_strata = factor(round(father))) %>%
  group_by(father_strata) %>%
  summarize(father_strata, avg =mean(son))%>%
  ggplot(aes(father_strata, avg)) +
  geom_point()

scale(galton_heights$father)

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)

s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)

r <- cor(galton_heights$father,galton_heights$son)

galton_heights %>% 
  ggplot(aes(father,son))+
  geom_point(alpha=0.5)+
  geom_abline(slope =r*s_y/s_x, intercept = mu_y-r*(s_y/s_x) *mu_x)

galton_heights %>% 
  ggplot(aes(scale(father),scale(son)))+
  geom_point(alpha=0.5)+
  geom_abline(slope =r, intercept = 0)

#Comparing two methods 1.conditional average 2. regression line
set.seed(1983)
B <- 1000
N <- 50
conditional_avg <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  dat %>% filter(round(father) == 72) %>% 
    summarize(avg = mean(son)) %>% 
    pull(avg)
})

regression_prediction <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  mu_x <- mean(dat$father)
  mu_y <- mean(dat$son)
  s_x <- sd(dat$father)
  s_y <- sd(dat$son)
  r <- cor(dat$father, dat$son)
  mu_y + r*(72 - mu_x)/s_x*s_y
})

mean(conditional_avg, na.rm = T)
mean(regression_prediction)

sd(conditional_avg, na.rm = T)
sd(regression_prediction)

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father) 

z_father <- galton_heights %>% round(father - mean(father)) / sd(father)
galton_heights %>% mean(father)
mean(galton_heights$father)

z_father <- round(galton_heights$father - mean(galton_heights$father) / sd(galton_heights$father))
hist(z_father, breaks =20)

m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2 * mu_y

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b_1, slope = m_1, col = "blue") +
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col = "red") 

#1. Load the GaltonFamilies data from the HistData. 
#The children in each family are listed by gender and then by height. 
#Create a dataset called galton_heights 
#by picking a male and female at random.

names(GaltonFamilies)

galton_heights <- GaltonFamilies %>%
  group_by(family)%>%
  sample_n(1) %>%
  ungroup()
 
#2. Make a scatterplot for heights 
#between mothers and daughters, 
#mothers and sons, 
#fathers and daughters, 
#and fathers and sons.

galton_heights_m_d <- galton_heights%>%
  filter(gender == "female") %>%
  group_by(family)%>%
  sample_n(1)%>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)

m_d <- galton_heights_m_d %>%
  ggplot(aes(mother, daughter))+geom_point(alpha=0.5)

galton_heights_m_s <- galton_heights%>%
  filter(gender=="male") %>%
  select(mother,childHeight) %>%
  rename(son = childHeight)




m_s <- galton_heights_m_s %>%
  ggplot(aes(mother, son))+geom_point(alpha=0.5)

galton_heights_f_d <- galton_heights%>%
  filter(gender=="female") %>%
  select(father,childHeight) %>%
  rename(daughter = childHeight)

f_d <-galton_heights_f_d %>%
  ggplot(aes(father, daughter))+geom_point(alpha=0.5)

galton_heights_f_s <- galton_heights%>%
  filter(gender=="male") %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

f_s <-galton_heights_f_s %>%
  ggplot(aes(father, son))+geom_point(alpha=0.5)

#3. Compute the correlation in heights 
#between mothers and daughters, 
#mothers and sons, 
#fathers and daughters, 
#and fathers and sons.

mu_x_md <- mean(galton_heights_m_d$mother)
mu_y_md <- mean(galton_heights_m_d$daughter)
s_x_md <- sd(galton_heights_m_d$mother)
s_y_md <- sd(galton_heights_m_d$daughter)
r_md <- cor(galton_heights_m_d$mother, galton_heights_m_d$daughter)

galton_heights_m_d %>% 
  ggplot(aes(mother, daughter)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r_md * s_y_md/s_x_md, 
              intercept = mu_y_md - r_md * s_y_md/s_x_md * mu_x_md) 

lm(data=galton_heights_m_d, mother~daughter)
lm(data=galton_heights_m_s, mother~son)
lm(data=galton_heights_f_s, father~son)
lm(data=galton_heights_f_d, father~daughter)

library(gridExtra)
grid.arrange(m_d, m_s, f_d, f_s, ncol=2)
m_d

galton_heights%>%
  filter(gender=="male") %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

set.seed(1989, sample.kind="Rounding")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

set.seed(NULL)