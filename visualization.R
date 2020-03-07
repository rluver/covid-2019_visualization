require("gganimate")
require("gifski")
require("plotly")




# load
source("covid-2019_crawling.R")




# visualization
table %>% arrange(desc(date)) %>% select(date, city, confirmed_cumulative) %>% 
  ggplot(aes(x = reorder(city, confirmed_cumulative), y = confirmed_cumulative), fill = 
           as.factor(city)) + geom_col() + coord_flip(clip = "off", expand = F) +
  geom_text(aes(label = str_c(confirmed_cumulative, "")), hjust = 1) +
  transition_states(states = date,
                    transition_length = 4,
                    state_length = 1) +
  ease_aes("cubic-in-out") %>%
  animate(100, fps = 20, duration = 30, width = 950, height = 750,
          renderer = gifski_renderer())


aggregate(confirmed_cumulative ~ city + date, table, sum) %>% 
  group_by(date) %>% mutate(rank = min_rank(-confirmed_cumulative * 1)) %>% ungroup() %>% 
  ggplot(aes(x = reorder(city, confirmed_cumulative), y = confirmed_cumulative), fill = 
           as.factor(city)) + geom_col() + coord_flip(clip = "off", expand = F) +
  geom_text(aes(label = str_c(confirmed_cumulative, "")), hjust = 1) +
  transition_time(date) + enter_fade() -> ani

animate(ani, fps = 20, duration = 30, width = 950, height = 750,
        renderer = gifski_renderer)



