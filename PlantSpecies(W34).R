#load libraries 
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(rnaturalearthdata) 
library(rnaturalearth)
library(viridis)

#load data
tuesdata <- tidytuesdayR::tt_load('2020-08-18')
plants <- tuesdata$plants


plants %>% 
  filter(red_list_category == "Extinct") %>% #only keep species that have gone extinct
  select(country, binomial_name, year_last_seen) %>% #only keep columns needed, just to make sure the count function will work 
  group_by(country, year_last_seen) %>% 
  mutate(count = n()) %>% #count the number of species that have gone extinct per year period within each country
  ungroup () %>% 
  select(country, year_last_seen, count) %>% 
  distinct() %>% #only keep rows with distinct values (so one for each country)
  na.omit() %>% #remove rows with missing values (some countries had species that had gone extinct with missing values for the year period in which this happened)
#Next I wanted to make sure that I had all the different year periods listed for each country, and could not figure out how to do this
#I landed on first converting the dataframe to wide format and then back to long format, but there is probably a better way to do this. 
  pivot_wider(id_cols = country, names_from = year_last_seen, values_from = count) %>% 
  pivot_longer(cols = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"), names_to = "year_last_seen", values_to = "count") %>%  
  mutate(count = if_else(is.na(count), 0, as.double(count))) %>% #Many countries will have missing values for the number of species going extinct per time period, so convert NAs to 0s
  group_by(country) %>% 
  mutate(total = cumsum(count)) %>% #create a column that has a cumulative count of the number of species going extint over the years, by each country. 
  ungroup() %>% 
  rename(name = country) %>% #change the name of the country column to "name" to later match with the rnaturalearth dataframe
  mutate(name = 
           case_when(name == "Pitcairn" ~ "Pitcairn Is.", #Change the names of a few countries to match the names in the earth dataframe
                     name == "Cook Islands" ~ "Cook Is.",
                     name == "Cabo Verde" ~ "Cape Verde",
                     name == "Viet Nam" ~ "Vietnam",
                     name == "United States" ~ "United States of America", 
                     name == "Sao Tome and Principe" ~ "São Tomé and Principe",
                     name == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
                     name == "French Polynesia" ~ "Fr. Polynesia",
                     TRUE ~ name)) -> threat #for all other countries not listed above, keep original values

world <- ne_countries(scale = "large", returnclass = "sf") #load earth data
subtitles <- c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020") #create a list of all the different time periods, to inform the for loop below 
data <- list() #create empty list
final_data <- list() #create empty list

for (i in 1:7){ #for each of the 7 time periods
  threat %>%  #take the dataset we created above
    select(name, year_last_seen, total) %>% #only keep these columns
    filter(as.factor(year_last_seen) == subtitles[[i]]) -> data[[i]] #only keep the rows that belong to that year period (and repeat for all year periods and save to list)
  left_join(world, data[[i]], by = "name") -> final_data[[i]] #merge these newly created smaller datasets with the earth data one by one, keeping all rows from the earth data, and save to list
  plot <- ggplot(final_data[[i]]) + #plot the smaller datasets one by one. 
    geom_sf(color = NA, aes(fill = total)) + #use the column with the cumulative count to inform the colours in the plot
    coord_sf(ylim = c(-50, 80)) + #which part of the world map to plot
  #Because there is a lot more variation at the lower end of the distribution than the top end (Madagascar has WAY more species going extinct than most other countries),
  #We need to manually create the colour gradient so that there are more colours at the bottom end, and fewer at the top end of the distribution
  #We can still use the viridis colour palette
  #select the number of colours you want, and at what point to change between the colours specifying that on a range between 0 and 1
  #Use a light grey for all the countries that have no data
  #limit the colour scale between 1 and 100 so it is consistent across all 7 graphs
    scale_fill_gradientn(colours = viridis(9), values = c(0, 0.01, 0.02, 0.03, 0.05, 0.08, 0.1, 0.4, 1), na.value = "grey75", limits = c(1,100)) + 
    theme_void() + #remove grid lines etc from the map
    labs(title = "Cumulative Extinction of Plant Species", #provide titles and make pretty...
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

  ggsave(plot = plot, filename=paste("myplot", i, ".png", sep = ""), path = "~/tidytuesday/week 34", dpi = 300, height = 4.5, width = 6) #save images to directory
  
  
}


#stitch together
library(magick)
list.files(path = "~tidytuesday/week 34", pattern = "*.png", full.names = T) %>% #import plots
  map(image_read) %>% #read plots
  image_join() %>% #join plots
  image_animate(fps=1) %>% #specify the speed with which to change between plots
  image_write("PlantsExtinct.gif") #wrtie gif

