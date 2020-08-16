library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(tvthemes)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar

avatar %>% 
  filter(character == "Zuko") %>% 
  select(book_num, chapter_num, character_words) %>% 
  group_by(book_num, chapter_num) %>% 
  mutate(linenumber = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = character_words, token = "words")  %>% 
  filter(word %in%  c("avatar", "aang", "aang's", "avatar's")) %>% 
  mutate(av = 
           case_when(word == "avatar"  ~ 1,
                     word == "avatar's" ~ 1)) %>% 
  mutate(aa = 
           case_when(word == "aang"  ~ 1,
                     word == "aang's" ~ 1)) %>% 
  group_by(book_num, chapter_num) %>% 
  summarise(Avatar = sum(av),
            Aang = sum(aa)) %>% 
  gather(key = "Name", value = "Frequency", Avatar, Aang) %>% 
  ungroup() %>% 
  mutate(book_num =
           factor(book_num,
                  labels = c("Water", "Earth", "Fire"))) -> avatar

extrafont::font_import("~/tvthemes/fonts/Slayer",prompt = FALSE)
extrafont::fonts()

ggplot(avatar, aes(x=chapter_num, y=Frequency, fill = Name)) +
  geom_bar(position = "stack", stat = "identity", width = .8) +
  facet_wrap(~book_num) +
  xlab("Chapter") +
  labs(title = "Avatar: The Last Airbender", subtitle = "The number of times Zuko mentions Avatar/Aang") +
  scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
  theme_avatar(text.font = "Slayer", 
               title.color = "#a10000",
               title.size = 15,
               subtitle.color = "#785e3c", 
               legend.title.color = '#c9c9c9', 
               legend.text.color = '#c9c9c9', 
               axis.title.color = '#c9c9c9') +
  scale_fill_manual(values = c('#d0a000', '#600000')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_rect(colour = "#c9c9c9", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "#c9c9c9", size = 10),
        axis.text = element_text(colour = "#785e3c"),
        legend.position = "bottom",
        legend.background = element_rect(fill = NA),
        legend.text = element_text(colour = "#785e3c"))