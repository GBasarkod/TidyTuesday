#Load libraries
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(tvthemes)
library(extrafont)

#Load data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar

#Tidy data and create variables needed
avatar %>% 
  filter(character == "Zuko") %>% #Only keep lines spoken by Zuko
  select(book_num, chapter_num, character_words) %>% #select columns required
  group_by(book_num, chapter_num) %>% 
  mutate(linenumber = row_number()) %>% #Label the lines spoken by Zuko by book and chapter 
  ungroup() %>% 
  unnest_tokens(output = word, input = character_words, token = "words")  %>% #add column with each row = one word spoken by Zuko
  filter(word %in%  c("avatar", "aang", "aang's", "avatar's", "airbender")) %>% #Only retain these words 
  mutate(av = 
           case_when(word == "avatar"  ~ 1,
                     word == "avatar's" ~ 1)) %>% #Create a column where a value of 1 is given to each instance of the word Avatar
  mutate(aa = 
           case_when(word == "aang"  ~ 1,
                     word == "aang's" ~ 1)) %>% #Create a column where a value of 1 is given to each instance of the word Aang 
  mutate(ai = 
           case_when(word == "airbender" ~ 1)) %>% #Create a column where a value of 1 is given to each instance of the word airbender
  group_by(book_num, chapter_num) %>% 
  summarise(Avatar = sum(av, na.rm = T),
            Aang = sum(aa, na.rm = T),
            Airbender = sum(ai, na.rm = T)) %>% #count the number of occurences for each name within each chapter
  gather(key = "Name", value = "Frequency", Avatar, Aang, Airbender) %>% #change to long format, creating one column that has a factor variable with the two names, and a second column with the values
  ungroup() %>% 
  mutate(book_num =
           factor(book_num,
                  labels = c("Water", "Earth", "Fire"))) -> avatar #Change value of book factors to the names of the books
#(there was a variable with book names already, so you could use that instead and change the ordering of it as it would default to alphabetical)

extrafont::font_import("~/tvthemes/fonts/Slayer",prompt = FALSE) #Load specific font that is downloaded to your system.
extrafont::fonts()


#Create plot
ggplot(avatar, aes(x=chapter_num, y=Frequency, fill = Name)) +
  geom_bar(position = "stack", stat = "identity", width = .8) + 
  facet_wrap(~book_num) +
  xlab("Chapter") +
  labs(title = "Avatar: The Last Airbender", subtitle = "The number of times Zuko mentions the Avatar") +
  scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
  theme_avatar(text.font = "Slayer", 
               title.color = "#a10000",
               title.size = 15,
               subtitle.color = "#785e3c", 
               legend.title.color = '#c9c9c9', 
               legend.text.color = '#c9c9c9', 
               axis.title.color = '#c9c9c9') +
  scale_fill_manual(values = c('#d0a000', '#785e3c', '#600000')) +
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
