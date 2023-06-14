# obtener datos de strava




# abrir datos gpx

# rm(list = ls())
library(tidyverse)
library(here)
library(XML)
library(lubridate)
library(ggmap)
library(geosphere)
options(digits.secs = 3)
options(scipen = 999)

# parse GPX file
path_tmp <- paste0("Cristo_Rey_Jugo_vuelta_a_la_estatua_.gpx")
parsed <- htmlTreeParse(file = path_tmp, useInternalNodes = TRUE)

# get values via via the respective xpath
coords <- xpathSApply(parsed, path = "//trkpt", xmlAttrs)
elev   <- xpathSApply(parsed, path = "//trkpt/ele", xmlValue)
ts_chr <- xpathSApply(parsed, path = "//trkpt/time", xmlValue)

# combine into df 
dat_df <- tibble(
  ts_POSIXct = ymd_hms(ts_chr, tz = "America/Bogota"),
  lat = as.numeric(coords["lat",]), 
  lon = as.numeric(coords["lon",]), 
  elev = as.numeric(elev)
)
head(dat_df)

dat_df %>%
  # filter(ts_POSIXct > ymd_hms("2020-11-29 08:40:13",tz = "America/Bogota")) %>% 
  ggplot()+aes(ts_POSIXct,elev)+geom_line()+scale_x_datetime()

dat_df %>%
  filter(between(ts_POSIXct ,
                 ymd_hms("2020-11-29 08:40:13",tz = "America/Bogota"),
                 ymd_hms("2020-11-29 08:50:13",tz = "America/Bogota"))) %>% 
  ggplot()+aes(ts_POSIXct,elev)+geom_line()+scale_x_datetime()



# compute distance (in meters) between subsequent GPS points
dat_df <- 
  dat_df %>%
  mutate(lat_lead = lead(lat)) %>%
  mutate(lon_lead = lead(lon)) %>%
  rowwise() %>%
  mutate(dist_to_lead_m = distm(c(lon, lat), c(lon_lead, lat_lead), fun = distHaversine)[1,1]) %>%
  ungroup() %>% 
  mutate(cum_dist_to_lead_m = cumsum(dist_to_lead_m))

dat_df %>% 
ggplot()+aes(cum_dist_to_lead_m,elev)+geom_line()+scale_x_continuous()

# # compute time elapsed (in seconds) between subsequent GPS points
# dat_df <- 
#   dat_df %>%
#   mutate(ts_POSIXct_lead = lead(ts_POSIXct)) %>%
#   mutate(ts_diff_s = as.numeric(difftime(ts_POSIXct_lead, ts_POSIXct, units = "secs"))) 
# 
# # compute metres per seconds, kilometres per hour 
# dat_df <- 
#   dat_df %>%
#   mutate(speed_m_per_sec = dist_to_lead_m / ts_diff_s) %>%
#   mutate(speed_km_per_h = speed_m_per_sec * 3.6)
# 
# # remove some columns we won't use anymore 
# dat_df <- 
#   dat_df %>% 
#   select(-c(lat_lead, lon_lead, ts_POSIXct_lead, ts_diff_s))
# head(dat_df) %>% as.data.frame()  