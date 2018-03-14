library(tidyverse)
library(sf)
library(mapview)

df <- read_csv("data/U_mines.csv") %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

mapview(df)
