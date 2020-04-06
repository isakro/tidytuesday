library(tidyverse)
library(sf) 
library(maptools)
library(gganimate) 

# Retrieve data (one of the multiple avaiable csv-files for this tidytuesday)
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# Read in vector data from the US Census Bureau:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us <- st_read('cb_2018_us_state_500k.shp') %>% 
  st_transform(2163) # set crs to US National Atlas Equal Area

# Tidy the beer data
beer <- beer_states %>% 
  # Remove a few states/terroitories and the 'total' row
  filter(!state %in% c('total', "AS", "GU", "MP", "PR", "VI")) %>%
  # Group the data by state and year
  group_by(state, year) %>% 
  # Create a column that sums the number of barrels for each state per year
  mutate(sum_barrels = sum(barrels, na.rm = TRUE)) %>% 
  # The summing happens irrespective of but once for each 'type'so remove 
  # all rows except for that of one type (this probably has a better solution)
  filter(!type %in% c('On Premises', 'Bottles and Cans')) %>% 
  # And then remove the unsummed barrels and type columns altogether
  select(-type, -barrels) %>% 
  # Group by state
  group_by(state) %>% 
  # First find mean and sd for number of barrels for each state across all years
  mutate(state_mean = mean(sum_barrels),
         state_sd = sd(sum_barrels),
         # Then find the deviance of number of barrels from the state mean 
         # for each year expressed in standard deviations.
         dev_state = (sum_barrels - state_mean) / state_sd)

# Below the map data is prepared. 
# Functions for moving, scaling and rotating Alaska and Hawaii are based the answer 
# user Spacedman gave here:
# https://stackoverflow.com/a/13767984/11376454

fixup <- function(usa, alaskaFix, hawaiiFix){
  
  # Retrieve Alaska
  alaska <- usa %>%  
    filter(NAME == "Alaska")
  alaska <- fix1(sf:::as_Spatial(alaska), alaskaFix)
  alaska <- st_as_sf(alaska)
  st_crs(alaska) <- st_crs(usa)
  
  # Crop the Hawaii data to exlclude Midway an stuff
  # (based on retrieving st_bbox() and manually adjusting values to create
  #  a polygon with which to clip)
  clip_poly <- st_polygon(list(rbind(c(-6420072,-1053158),
                                     c(-6420072, 100000),
                                     c(-5451870, 100000),
                                     c(-5451870, -1053158),
                                     c(-6420072,-1053158)))) %>% 
    st_sfc %>% 
    st_set_crs(2163) 
  
  hawaii <- usa %>%
    filter(NAME == 'Hawaii') %>% 
    st_intersection(clip_poly) 
  
  hawaii <- fix1(sf:::as_Spatial(hawaii), hawaiiFix)
  hawaii <- st_as_sf(hawaii)
  st_crs(hawaii) <- st_crs(usa)
  
  # Retrieve mainland contigous states
  states <- usa %>%
    filter(!NAME %in% c('Alaska', 'Hawaii', 'Samoa', 'Guam',
                        'Commonwealth of the Northern Mariana Islands',
                        'United States Virgin Islands', 'American Samoa',
                        'Puerto Rico')) 
  
  us = rbind(states, alaska, hawaii)
  
  return(us)
}

fix1 <- function(object, params){
  r = params[1]; scale = params[2]; shift = params[3:4]
  object = elide(object, rotate = r)
  size = max(apply(bbox(object), 1, diff)) / scale
  object = elide(object, scale = size)
  object = elide(object, shift = shift)
  return(object)
}

# Run above functions to adjust the position of Hawaii and Alaska
usfix <- fixup(us, c(-35, 1.5, -450000, -3500000), c(-35, 1, 4200000, -1800000))

# Combine tidytuesday data with map data
data_states <- inner_join(usfix, beer, by = c("STUSPS" = "state"))

# Some thematic code and inspiration taken from Timo Grossenbacher:
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
map <- data_states %>%
    ggplot() + 
    geom_sf(aes(group = NAME, fill = dev_state)) +
    scale_fill_viridis_c(option = "inferno") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#f5f5f2", color = NA), 
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.border = element_blank(),
          legend.position = 'bottom') +
    guides(fill = guide_legend(title = "Standard Deviation",
                               direction = 'horizontal',
                               title.position = 'top',
                               title.hjust = 0.5,
                               nrow = 1,
                               byrow = T,
                               label.position = 'bottom')) +
    transition_time(year) +
    labs(title = 'Annual beer production in the United States',
         subtitle = 'Standard deviation from state mean across all years. \nYear: {as.integer(frame_time)}',
         caption = 'Geometries: US Census Bureau, \nData: Alcohol and Tobacco Tax and Trade Bureau')

# Call and save animation
map_anm <- animate(map) 
anim_save("us_beer.gif", map_anm)
