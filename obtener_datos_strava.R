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
dat_df <- data.frame(
  ts_POSIXct = ymd_hms(ts_chr, tz = "UTC"),
  lat = as.numeric(coords["lat",]), 
  lon = as.numeric(coords["lon",]), 
  elev = as.numeric(elev)
)
head(dat_df)

dat_df %>% ggplot()+aes(ts_POSIXct,elev)+geom_line()
