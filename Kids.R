#imports
library(tidytuesdayR)
library(tidyverse)
library(gganimate)
 
#load data
data <- tt_load(2020, 38)

#filter data, get the 10 first states for each year
libraries <- data$kids %>% filter(variable == "lib") %>% group_by(year) %>% mutate(rank = rank(inf_adj_perchild)) %>%
    filter(rank > 41)

#graphic  
p <- libraries %>% ggplot() +
  geom_tile(aes(rank, inf_adj_perchild / 2, height = inf_adj_perchild, width = 0.9, fill = state)) +
  transition_states(year, transition_length = 1, state_length = 3) +
  geom_text(aes(rank, y = 0, label = state, color = state), hjust = "right") +
  geom_text(aes(rank, y = inf_adj_perchild - 0.05, label = as.character(round(inf_adj_perchild*1000, 0)))) + 
  coord_flip() +
  scale_y_continuous(limits = c(-0.2, NA), breaks = c(0, 0.1, 0.2, 0.3, 0.4), labels = c(0, 100, 200, 300, 400)) +
  scale_x_continuous(breaks = NULL) +
  labs(title = "Expanses on libraries,\nper child and inflation adjusted", subtitle = str_c("Year : ", "{closest_state}"), caption = "Data : Urban institute\nVisualisation : @ReyssatF", x = NULL, y = "expanse per child ($ 2016)") +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(face = "bold.italic", size = 15),
        axis.title.y = element_text(size = 15))

#export
animate(p, nframes = 300, end_pause = 10)
anim_save(file = "lib_expanse.gif")

