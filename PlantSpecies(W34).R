library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(rnaturalearthdata)
library(rnaturalearth)
library(viridis)

tuesdata <- tidytuesdayR::tt_load('2020-08-18')
plants <- tuesdata$plants


plants %>% 
  filter(red_list_category == "Extinct") %>%
  select(country, binomial_name, year_last_seen) %>% 
  group_by(country, year_last_seen) %>% 
  mutate(count = n()) %>% 
  ungroup () %>% 
  select(country, year_last_seen, count) %>% 
  distinct() %>% 
  na.omit() %>% 
  pivot_wider(id_cols = country, names_from = year_last_seen, values_from = count) %>%
  pivot_longer(cols = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"), names_to = "year_last_seen", values_to = "count") %>%  
  mutate(count = if_else(is.na(count), 0, as.double(count))) %>% 
  group_by(country) %>% 
  mutate(total = cumsum(count)) %>% 
  ungroup() %>% 
  rename(name = country) %>% 
  mutate(name = 
           case_when(name == "Pitcairn" ~ "Pitcairn Is.",
                     name == "Cook Islands" ~ "Cook Is.",
                     name == "Cabo Verde" ~ "Cape Verde",
                     name == "Viet Nam" ~ "Vietnam",
                     name == "United States" ~ "United States of America", 
                     name == "Sao Tome and Principe" ~ "São Tomé and Principe",
                     name == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
                     name == "French Polynesia" ~ "Fr. Polynesia",
                     TRUE ~ name)) -> threat

world <- ne_countries(scale = "large", returnclass = "sf")
subtitles <- c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020")
data <- list()
final_data <- list()

for (i in 1:7){
  threat %>% 
    select(name, year_last_seen, total) %>% 
    filter(as.factor(year_last_seen) == subtitles[[i]]) -> data[[i]]
  left_join(world, data[[i]], by = "name") -> final_data[[i]]
  plot <- ggplot(final_data[[i]]) +
    geom_sf(color = NA, aes(fill = total)) +
    coord_sf(ylim = c(-50, 80)) + 
    scale_fill_gradientn(colours = viridis(9), values = c(0, 0.01, 0.02, 0.03, 0.05, 0.08, 0.1, 0.4, 1), na.value = "grey75", limits = c(1,100)) +
    theme_void() + 
    labs(title = "Cumulative Extinction of Plant Species",
         subtitle = subtitles[[i]],
         fill = "Count",
         caption = "#TidyTuesday Week 34: Plants in Danger") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, colour = "#440154FF", margin = margin(5,2,5,2), face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 13, colour = "#33638DFF", margin = margin(2,2,2,2), face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(vjust = .8, colour = "#440154FF"),
      legend.text = element_text(colour = "#440154FF"),
      panel.background = element_rect(fill = "grey97", linetype = 0),
      plot.background = element_rect(fill = "grey97", linetype = 0),
      plot.caption = element_text(size = 10, colour = "grey60", hjust = .99),
      text = element_text(family = "serif")
    )

  ggsave(plot = plot, filename=paste("myplot", i, ".png", sep = ""), path = "~/tidytuesday/week 34", dpi = 300, height = 4.5, width = 6)
  
  
}


#stitch together
library(magick)
list.files(path = "~tidytuesday/week 34", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps=1) %>% 
  image_write("PlantsExtinct.gif")

