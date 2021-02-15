library(Lahman)
library(tidyverse)
library(dslabs)
Teams

bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) 

r <- cor(bb_slope$R_per_game,bb_slope$BB_per_game)
sd_b <- sd(bb_slope$BB_per_game)
sd_r <- sd(bb_slope$R_per_game)
m <- r*sd_r/sd_b
m

get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
get_slope(bb_slope$BB_per_game,bb_slope$R_per_game)

summary_stats <- Teams %>% 
     filter(yearID %in% 1961:2001 ) %>%
     mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
     summarize(avg_HR = mean(HR_per_game),
                            s_HR = sd(HR_per_game),
                            avg_R = mean(R_per_game),
                            s_R = sd(R_per_game),
                            r = cor(HR_per_game, R_per_game))
summary_stats

bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope 

singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(Singles_per_game, R_per_game))

singles_slope 

Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2) 

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = get_slope(BB_per_game, R_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))

# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
 
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

summary(fit)

# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data=.) %>% 
    .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

install.packages("gridExtra")
library(gridExtra)

#both are normal distribution
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")  
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 

grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

lse %>% summarize(cor(beta_0, beta_1))

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 

#predictions and confidence intervals

galton_heights %>% ggplot(aes(son, father))+
  geom_point()+ geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()
mean(fit)
Y_hat

Y_hat %>% pull(fit)

#assessment

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

Teams %>% 
  filter(yearID %in% 1961:2001 )%>%
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game =HR/G) %>%
  lm(R_per_game ~ BB_per_game+HR_per_game , data= .)

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))




