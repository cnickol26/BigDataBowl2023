library(tidyverse)
library(ggimage)

test <- data.frame(x=1, y=3, image='https://www.pro-football-reference.com/req/20180910/images/headshots/WhitAn20_2021.jpg')

speed_coef <- read_csv('speed_coeff.csv')

power_coef <- read_csv("power_coeff.csv")

power_coef$mean_scaled <- scale(power_coef$mean)[,1]
speed_coef$mean_scaled <- scale(speed_coef$mean)[,1]

power_coef_filt <- power_coef %>% select(mean_scaled, sd, tackle_name) %>%
  rename(power_mean_scaled = mean_scaled, power_sd = sd)
speed_coef_filt <- speed_coef %>% select(mean_scaled, sd, tackle_name) %>%
  rename(speed_mean_scaled = mean_scaled, speed_sd = sd)

plot_data <- full_join(power_coef_filt, speed_coef_filt, by="tackle_name")

ggplot(plot_data, aes(power_mean_scaled, speed_mean_scaled)) + geom_point()


ggplot(test, aes(x, y)) + geom_image(aes(image=image), size=0.1)
