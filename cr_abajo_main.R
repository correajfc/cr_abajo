library(sf)
library(dplyr)
library(tidygeocoder)
library(osrm)

# 1. One World Trade Center, NYC
# 2. Madison Square Park, NYC
adresses <- c("285 Fulton St, New York, NY 10007", 
              "11 Madison Ave, New York, NY 10010")

# geocode the two addresses & transform to {sf} data structure
data <- tidygeocoder::geo(adresses, method = "osm") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

osroute <- osrm::osrmRoute(loc = data,
                           returnclass = "sf")

# sample osroute 50 times regularly, cast to POINT, return sf (not sfc) object
osroute_sampled <- st_sample(osroute, type = 'regular', size = 50) %>%
  st_cast('POINT') %>%
  st_as_sf() 


library(ggplot2)
library(ggmap) # warning: has a naming conflict with tidygeocoder!
library(gganimate)

# ggmap does not quite like geom_sf(), 
# the "old school" geom_point will be easier to work with
osroute_xy <- osroute_sampled %>% 
  mutate(seq = 1:nrow(.),
         x = st_coordinates(.)[,"X"],
         y = st_coordinates(.)[,"Y"]) 

# basemap / the bbox depends on yer area of interest
NYC <- get_stamenmap(bbox = c(-74.05, 40.68, -73.9, 40.8),
                     zoom = 13,
                     maptype = "toner-background")

# draw a map 
animation <- ggmap(NYC) + 
  geom_point(data = osroute_xy,
             aes(x = x, y = y),
             color = "red",
             size = 4) +
  theme_void() +
  transition_reveal(seq) +
  shadow_wake(wake_length = 1/6)

# create animation
gganimate::animate(animation, 
                   nframes = 2*(nrow(osroute_xy)+1), 
                   height = 800, 
                   width = 760,
                   fps = 10, 
                   renderer = gifski_renderer(loop = T))

# save animation  
gganimate::anim_save('animated_nyc.gif')