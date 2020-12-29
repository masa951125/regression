library(tidyverse)
library(HistData)
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