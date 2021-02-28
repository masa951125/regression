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

#confounding

library(dslabs)
data(admissions)
admissions

admissions  %>% group_by(gender) %>%
  summarize(percentage = sum(admitted)/sum(applicants)*100)
sum(admissions$admitted)
sum(admissions$applicants)

admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

library(broom)

admissions %>% group_by(gender) %>%
  summarize(total_admitted = round(sum(admitted/100*applicants)),
            not_admitted = sum(applicants)-sum(admitted/100*applicants))%>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(minus_women_men = women-men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")


admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + 
  geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))
