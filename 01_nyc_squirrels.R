library(tidyverse)
theme_set(theme_light())

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Counting zip_codes, if sort = FALSE, it would sort output in ascending order of n
nyc_squirrels %>% 
  count(zip_codes, sort = TRUE)


# Graphics 

# Latitude and Longitude
nyc_squirrels %>% 
  ggplot(aes(long, lat)) +
  geom_point()


# Hectare
nyc_squirrels %>% 
  count(hectare, sort = FALSE) 

by_hectare <- nyc_squirrels %>% 
  group_by(hectare) %>% 
  summarize(meanLong = mean(long), meanLat = mean(lat), n = n())

by_hectare %>% 
  ggplot(aes(meanLong, meanLat, size = n)) +
  geom_point() +
  theme_void()


# Colour counting
nyc_squirrels %>% 
  count(primary_fur_color, sort = TRUE)
nyc_squirrels %>% 
  count(highlight_fur_color, sort = TRUE)
nyc_squirrels %>% 
  count(color_notes, sort = TRUE)
nyc_squirrels %>% 
  count(primary_fur_color, highlight_fur_color, sort = TRUE)

# Colour graphic by gray color
by_hectare <- nyc_squirrels %>% 
  group_by(hectare, primary_fur_color) %>% 
  summarize(meanLong = mean(long), meanLat = mean(lat), n = n(),
            pct_gray = mean(primary_fur_color == "Gray", na.rm = TRUE),
            pct_cinnamon = mean(primary_fur_color == "Cinnamon", na.rm = TRUE),
            pct_black = mean(primary_fur_color == "Black", na.rm = TRUE)) 

by_hectare %>% 
  ggplot(aes(meanLong, meanLat, size = n, color = pct_black)) + 
  geom_point()

# Colour graphic by the three primary colors
by_hectare %>% 
  filter(!is.na(primary_fur_color)) %>% 
  group_by(hectare, primary_fur_color) %>% 
  ggplot(aes(meanLong, meanLat, size = n, color = primary_fur_color)) +
  geom_point() +
  scale_color_manual(values = c("black", "#d2691e", "darkgray")) +
  facet_wrap(~ primary_fur_color)

# Regressão logística
by_hectare %>% 
  filter(n >= 10) %>% 
  ggplot(aes(meanLat, pct_gray)) +
  geom_point() +
  geom_smooth()

by_hectare %>%
  mutate(n_gray = round(pct_gray * n)) %>% 
  glm(cbind(n_gray, n - n_gray) ~ meanLat, data = ., family = "binomial") %>% 
  summary()
  
