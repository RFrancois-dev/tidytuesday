#load data
load(file = "friendsData.R")
#tidyverse is the 7th friend
library(tidyverse)

##get every mention of the main chars by the main chars
tab <- tibble(mentionned = str_extract_all(data$friends$text, "Ross|Monica|Chandler|Rachel|Joey|Phoebe"), speaker = data$friends$speaker) %>%
  filter(speaker %in% c("Monica Geller", "Chandler Bing", "Rachel Green", "Joey Tribbiani", "Ross Geller", "Phoebe Buffay")) %>%
  unnest(mentionned) %>% mutate(mentionned_sex = as.factor(if_else(grepl("Ross|Chandler|Joey", mentionned), "man", "woman")),
                                speaker = speaker %>% fct_infreq())

#compute the fequency by speaker
prop_by_spk <- tab %>% group_by(speaker, mentionned_sex) %>% summarize(n = n()) %>% group_by(speaker) %>% mutate(freq = n / sum(n) * 100)

#compute coordinates for the full data bar
prop_full <- tab %>% group_by(mentionned_sex) %>% summarise(n = n()) %>% mutate( freq = n / sum(n) * 100)
x0 <- 0.55
x2 <- 6.45
x1 <- x0 + prop_full$freq[prop_full$mentionned_sex == "man"] * (x2-x0) / 100

#labels coordinates
xmen = x0 + x1 / 2
xwomen = x1 + (x2 - x1) / 2

#the plot!
ggplot(tab, aes(speaker, fill = mentionned_sex)) +
  ##two bars per speaker
  geom_bar(position = "dodge") +
  ##add the proportions
  geom_text(data = prop_by_spk, aes(speaker, n/2, label = str_c(round(freq, 0), "%"), group = mentionned_sex), position = position_dodge(0.9)) + 
  ##full bar
  geom_rect(xmin = x0, xmax = x1, ymin = -300, ymax = -550, fill = "#F8766D") +
  geom_rect(xmin = x1, xmax = x2, ymin = -300, ymax = -550, fill = "#00BFC4") +
  geom_text(data = prop_full, aes(c(xmen, xwomen), -425, label = str_c(round(freq, 0), "%"))) +
  ##adjust axis and clean the area
  theme(axis.text.x = element_text(vjust = 45),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous("Number of mentions" ,breaks = c(0, 500, 1000), limits = c(-550, NA)) +
  scale_x_discrete(name = NULL) +
  ##title and captions
  labs(title = "When Friends speak about each others, which gender is the most mentionned ?",
       subtitle = "Number and frequency of mentions, by gender mentionned and by character speaking",
       caption = "Data : https://github.com/EmilHvitfeldt/friends\nVisualisation : @ReyssatF",
       fill = "Gender of the\nmentionned character") +
  ##annotations
  annotate(geom = "text", x = 3.5, y = -250, label = "All mentions by the 6 friends", fontface = "bold") +
  annotate(geom = "text", x = 4, y = 900,
           label = "Rachel is the one speaking the most.\nShe mentionned Joey, Ross and Chandler\ntwice as much as Monica, Phoebe and herself !",
           fontface = "bold.italic", color = "blue") +

#export the plot
ggsave(filename  = "plt.pdf")
