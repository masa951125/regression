#chapter17 1-2

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

