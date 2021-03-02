rm(list=ls())

library(tidyverse)

library(dslabs)
data("research_funding_rates")
research_funding_rates

award <-research_funding_rates %>%
  summarize(awarded_men = sum(awards_men),non_awarded_men=sum(awards_total)-sum(awards_men),
            awarded_women =sum(awards_women), non_awarded_women =sum(awards_total)-sum(awards_women))

men <- sum(research_funding_rates$awards_men)
women <- sum(research_funding_rates$awards_women)
total <-sum(research_funding_rates$awards_total)

no_men <- sum(research_funding_rates$applications_men)-men
no_women <- sum(research_funding_rates$applications_women)-women

cross <-tibble(men =c(men, no_men), women =c(women, no_women))
rownames(cross) <- c("awarded","not_awarded")

two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat


two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

library(broom)
chisq.test <- two_by_two %>% select(-awarded) %>% chisq.test()%>%tidy()

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% ggplot(aes(discipline, success,  col = gender, size = applications)) + 
  geom_point()

dat %>% filter(gender=="women")%>%

dat %>% gather(gender, value = success)

                                   
