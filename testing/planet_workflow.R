library(raster)
library(rasterVis)
# library(fasterize)
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)

mines_sf <- read_csv("data/U_mines.csv") %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

paths <- c("data/planet/20180307_054953_0f29/20180307_054953_0f29_3B_AnalyticMS.tif",
           "data/planet/20180307_054953_1_0f29/20180307_054953_1_0f29_3B_AnalyticMS.tif",
           "data/planet/20180307_054954_0f29/20180307_054954_0f29_3B_AnalyticMS.tif",
           "data/planet/20180307_054955_0f29/20180307_054955_0f29_3B_AnalyticMS.tif")

rast_brick <- paths %>% 
  map(brick) %>% 
  reduce(merge)

names(rast_brick) <- c("blue", "green", "red", "NIR")

qabul_khel <- mines_sf %>% 
  filter(str_detect(Name, "Qabul Khel")) %>% 
  st_transform(paste(crs(rast_brick))) %>% 
  st_buffer(units::set_units(2, km)) %>% 
  as("Spatial")

rast_crop <- rast_brick %>% 
  crop(qabul_khel) %>% 
  trim()

cust_theme <- rasterTheme(region = brewer.pal(11, "Spectral"))

rasterVis::levelplot(rast_crop[["NIR"]], par.settings = cust_theme)

ndvi <- (rast_crop[["NIR"]] - rast_crop[["red"]]) / 
        (rast_crop[["NIR"]] + rast_crop[["red"]])

cust_pal <- brewer.pal(11, "Spectral")
plot(ndvi, col = cust_pal)

filtered_ndvi <- ndvi
filtered_ndvi[filtered_ndvi >= -0.15] <- NA
mask_ndvi <- mask(ndvi, filtered_ndvi)

plotRGB(rast_crop, b = 1, g = 2, r = 3)
# plot(rast_crop[["blue"]], col = cust_pal)

nir_fact <- mean(as.list(summary(values(rast_crop[["NIR"]])))$`1st Qu.`,
                 as.list(summary(values(rast_crop[["NIR"]])))$Min)

# values(rast_crop[["NIR"]]) <- scale(values(rast_crop[["NIR"]]))
# plot(rast_crop[["NIR"]], col = cust_pal)

filtered_nir <- rast_crop[["NIR"]]
filtered_nir[filtered_nir >= nir_fact] <- NA
mask_nir <- mask(rast_crop[["NIR"]], filtered_nir)

plot(mask_nir, col = cust_pal)

