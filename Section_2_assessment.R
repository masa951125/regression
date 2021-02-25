library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, R_per_game =R/G, HR_per_game=HR/G)

fit <- Teams_small %>%
  mutate(R =R/G) %>%
  do(tidy(lm(avg_attendance ~ R, data=.)))
fit

fit <- Teams_small %>%
  mutate(HR =HR/G) %>%
  do(tidy(lm(avg_attendance ~ HR, data=.)))

fit <- Teams_small %>%
  do(tidy(lm(avg_attendance ~ W, data=.)))

fit <- Teams_small %>%
    do(tidy(lm(avg_attendance ~ yearID, data=.)))

cor(Teams_small$HR_per_game, Teams_small$W)

dat <- Teams_small%>%
  mutate( Round_W = round(W/10)) %>%
  filter(Round_W >=5 & Round_W<=10)
 dat$Round_W
 
sum(dat$Round_W==8)

dat %>%
  group_by(Round_W) %>%
  do(tidy(lm(avg_attendance ~ R_per_game, data=.))) %>%
  filter(term== "R_per_game")
  
dat %>%
  group_by(Round_W) %>%
  do(tidy(lm(avg_attendance ~ HR_per_game, data=.))) %>%
  filter(term== "HR_per_game")

dat %>%  
  group_by(Round_W) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

fit <-Teams_small %>%
  lm(avg_attendance ~R_per_game+ HR_per_game + W + yearID, data=.)

-456674.4 +321.8*5 +1798.4*1.2 +80*116.7 +1960*229.6

predict(fit, data.frame(R_per_game=5,HR_per_game=1.2, W=80,yearID=1960))

fit <-Teams_small %>%
  lm(avg_attendance ~R_per_game+ HR_per_game + W + yearID, data=.)

#Question6

fit <- Teams %>%
  filter(yearID==2002)%>%
  mutate(avg_attendance = attendance/G,R_per_game =R/G, HR_per_game=HR/G) %>%
  lm(avg_attendance ~ R_per_game+ HR_per_game + W , data=.)

new_Teams <- Teams %>%
  filter(yearID==2002) %>%
  mutate(avg_attendance = attendance/G,R_per_game =R/G, HR_per_game=HR/G)

actual_dat <- Teams %>%
  filter(yearID==2002)%>%
  mutate(avg_attendance = attendance/G)

newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)

predicted_dat <-predict(fit,new_Teams)
cor(actual_dat$avg_attendance, predicted_dat)
