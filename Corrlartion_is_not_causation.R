library(tidyverse)

N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

head(sim_data)

res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))

sim_data %>% filter(group==res$group[which.max(res$r)]) %>%
  ggplot(aes(x,y))+
  geom_point()+
  geom_smooth(method="lm")

res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .),conf.int=T))

set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

qplot(x, y, alpha = 0.5)

cor(x,y)
cor(x[-23], y[-23])
qplot(rank(x), rank(y))
cor(rank(x), rank(y))



