library(tidyverse)
theme_set(theme_light())

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")


# By hectare
nyc_squirrels %>% 
  count(hectare, sort = FALSE) 

by_hectare <- nyc_squirrels %>% 
  group_by(hectare, primary_fur_color) %>% 
  summarize(lon = mean(long), lat = mean(lat), n = n())


# Color graphic by the three primary colors
by_hectare %>% 
  filter(!is.na(primary_fur_color)) %>% 
  group_by(hectare, primary_fur_color) %>% 
  ggplot(aes(lon, lat, size = n, color = primary_fur_color)) +
  geom_point() +
  scale_color_manual(values = c("black", "#d2691e", "darkgray")) +
  labs(title = "Spotted squirrel positions averaged by hectare", 
       color = "primary color",
       size = "# squirrels") +
  facet_wrap(~ primary_fur_color) 