
source("obtener_datos_strava.R", echo = T)

library(ggplot2)
library(ggmap) # warning: has a naming conflict with tidygeocoder!
library(gganimate)


min_long <- dat_df$lon %>% min() 
max_long <- dat_df$lon %>% max()

min_lat <- dat_df$lat %>% min()
max_lat <- dat_df$lat %>% max()

ampliacion_lon <-  0.011
ampliacion_lat <-  0.005
# basemap / the bbox depends on yer area of interest
CALI <- get_stamenmap(bbox = c(min_long - ampliacion_lon, 
                               min_lat- ampliacion_lat, 
                               max_long +ampliacion_lon, 
                               max_lat + ampliacion_lat),
                     zoom = 15,
                     maptype = "toner",)
ggmap(CALI)

# invert colors in raster
invert <- function(x) rgb(t(255-col2rgb(x))/255)    
CALI_inv <- as.raster(apply(CALI, 2, invert))

# copy attributes from original object
class(CALI_inv) <- class(CALI)
attr(CALI_inv, "bb") <- attr(CALI, "bb")

ggmap(CALI_inv)

# dat_df_fltr <- dat_df %>% 
#   filter(between(ts_POSIXct ,
#                  ymd_hms("2020-11-29 08:40:13",tz = "America/Bogota"),
#                  ymd_hms("2020-11-29 08:50:13",tz = "America/Bogota")))
km_ini <- dat_df %>% 
  filter(ts_POSIXct == ymd_hms("2020-11-29 08:35:13",tz = "America/Bogota")) %>% 
  pull(cum_dist_to_lead_m)
dat_df_fltr <- dat_df %>% 
filter(ts_POSIXct > ymd_hms("2020-11-29 08:35:13",tz = "America/Bogota")) %>% 
  mutate(cum_dist_to_lead_m= cum_dist_to_lead_m - km_ini)

# draw a map
animation <- ggmap(CALI_inv) + 
  geom_point(data = dat_df_fltr ,
             aes(x = lon, y = lat),
             color = "red",
             size = 4) +
  theme_void() +
  transition_reveal(ts_POSIXct) +
  shadow_wake(wake_length = 1/3)

# create animation gif 760x800
gganimate::animate(animation, 
                   nframes = nrow(dat_df_fltr), 
                   height = 800, 
                   width = 760,
                   fps = 30, 
                   renderer = gifski_renderer(loop = T))

# save animation  
gganimate::anim_save('animated_cr_abajo.gif')

# create animation 1920x1080
animacion <- gganimate::animate(animation, 
                   nframes = nrow(dat_df_fltr), 
                   height = 1080, 
                   width = 1920,
                   fps = 30, 
                   renderer = av_renderer())

# save animation  
gganimate::anim_save(animation = animacion,
                     filename = 'animated_cr_abajo_15.mp4')



# create animation 3840x2160
animation <- ggmap(CALI_inv) + 
  geom_point(data = dat_df_fltr ,
             aes(x = lon, y = lat),
             color = "red",
             size = 10) +
  theme_void() +
  transition_reveal(ts_POSIXct) +
  shadow_wake(wake_length = 1/6)

animacion <- gganimate::animate(animation, 
                                nframes = nrow(dat_df_fltr), 
                                height = 2160, 
                                width = 3840,
                                fps = 30, 
                                renderer = av_renderer())

# save animation  
gganimate::anim_save(animation = animacion,
                     filename = 'animated_cr_abajo_15_uhd.mp4')



# create distance elevation profile
min_elev <- min(dat_df_fltr$elev)
max_elev <- max(dat_df_fltr$elev)

animation_elev <- dat_df_fltr %>% 
ggplot()+
  aes(cum_dist_to_lead_m,elev)+
  geom_ribbon(#data = dat_df_fltr %>% select(-ts_POSIXct),
              aes(x=cum_dist_to_lead_m,
                  y = elev, 
                  ymin = min(elev), 
                  ymax = elev), 
              fill = 'grey85', alpha=0.9, position = "identity") +
  geom_point(color = "red",
             size = 5)+
  scale_x_continuous(name = "distancia",labels = scales::label_number_si(unit = "km"))+
  scale_y_continuous(name = "elevación",labels = scales::label_number(suffix = "m"))+
  coord_fixed(ratio = 2)+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.1, linetype = "solid"),
    # panel.grid.major = element_line(size = 0.1, linetype = 'solid',
    #                                 colour = "white"), 
    # panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
    #                                 colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white",size = 6),
    axis.title = element_text(color = "white", size = 6),
  )+
  transition_reveal(ts_POSIXct) +
  shadow_wake(wake_length = 1/6)
  
gganimate::animate(animation_elev, 
                   nframes = nrow(dat_df_fltr), 
                   height = 300, 
                   width = 760,
                   fps = 30, 
                   renderer = gifski_renderer(loop = T))
# save animation  
gganimate::anim_save('animated_cr_abajo_elev.gif')




animation_elev <- dat_df_fltr %>% 
  ggplot()+
  aes(cum_dist_to_lead_m,elev)+
  geom_ribbon(#data = dat_df_fltr %>% select(-ts_POSIXct),
    aes(x=cum_dist_to_lead_m,
        y = elev, 
        ymin = min(elev), 
        ymax = elev), 
    fill = 'grey85', alpha=0.9, position = "identity") +
  geom_point(color = "red",
             size = 9)+
  scale_x_continuous(name = "distancia",labels = scales::label_number_si(unit = "km"))+
  scale_y_continuous(name = "elevación",labels = scales::label_number(suffix = "m"))+
  coord_fixed(ratio = 2)+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.1, linetype = "solid"),
    # panel.grid.major = element_line(size = 0.1, linetype = 'solid',
    #                                 colour = "white"), 
    # panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
    #                                 colour = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white",size = 30),
    axis.title = element_text(color = "white", size = 30),
  )+
  transition_reveal(ts_POSIXct) +
  shadow_wake(wake_length = 1/6)

animacion_elev <- gganimate::animate(animation_elev, 
                                nframes = nrow(dat_df_fltr), 
                                height = 800, 
                                width = 2840,
                                fps = 30, 
                                renderer = av_renderer())

# save animation  
gganimate::anim_save(animation = animacion_elev,
                     filename = 'animated_cr_abajo_elev_uhd.mp4')

