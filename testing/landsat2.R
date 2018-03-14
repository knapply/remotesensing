# library(getlandsat)
library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)

mines_sf <- read_csv("data/U_mines.csv") %>%
  filter(str_detect(Name, "UCIL"), !str_detect(Name, "Tummalapalle")) %>%
  filter(Country == "India", is.na(`Date Closed`)) %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

tiff_files <- list.files("data/landsat8/", full.names = TRUE) %>%
  str_subset("TIF$")

blue <- tiff_files %>%
  str_subset("_B2") %>%
  raster::raster() %>%
  raster::trim()

green <- tiff_files %>%
  str_subset("_B3") %>%
  raster::raster() %>%
  raster::trim()

red <- tiff_files %>%
  str_subset("_B4") %>%
  raster::raster() %>%
  raster::trim()

mines_subset <- mines_sf %>%
  filter(Name %in% c("Bhatin UCIL", "Jaduguda UCIL", "Bagjata UCIL")) %>%
  st_transform("+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 

mines_extent <- mines_sf %>%
  filter(Name %in% c("Bhatin UCIL", "Jaduguda UCIL", "Bagjata UCIL")) %>%
  st_transform("+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_buffer(dist = units::set_units(2, km)) %>%
  as("Spatial") %>%
  raster::extent()  %>% 
  + 20

nir <- tiff_files %>%
  str_subset("_B5") %>%
  raster::raster() %>%
  raster::crop(mines_extent) %>%
  raster::trim()

shortwave_ir <- tiff_files %>%
  str_subset("_B7") %>%
  raster::raster() %>%
  raster::crop(mines_extent) %>%
  raster::trim()

b5_b7_ratio <- nir / shortwave_ir

panchromatic <- tiff_files %>%
  str_subset("_B8") %>%
  raster::raster() %>%
  raster::crop(mines_extent) %>%
  raster::trim()

green_blue_ratio <- (green / blue) %>%
  raster::crop(mines_extent) %>%
  raster::trim()

raster::plot(b5_b7_ratio)
plot(mines_subset$geometry, lwd = 4,
             col = "red", add = TRUE)


rast_brick_rgb <- raster::stack(red, green, blue) %>%
  raster::brick() %>%
  raster::crop(mines_extent) %>%
  raster::trim()

raster::plotRGB(rast_brick_rgb)
raster::plot(red)


ndvi <- (nir - rast_brick_rgb[[1]]) / (nir + rast_brick_rgb[[1]]) %>%
  `/`(1000)


slideview(nir, shortwave_ir)

library(RColorBrewer)
pal <- colorRampPalette(c("blue", "green"))

library(rasterVis)

# pal <- RdBuTheme(region = brewer.pal(11, 'RdBu'))
plot(ndvi, col.regions = pal)

options(viewer = NULL)
mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap"),
               raster.size = 10^15, mapview.maxpixels = 10^10,
               raster.palette = colorRampPalette(c("blue",
                                                   "green",
                                                   "yellow",
                                                   "red"))(255),
               na.color = "transparent",
               layers.control.pos = "topright")

test_index <- green_blue_ratio

mapview(test_index, legend = TRUE) + mapview(mines_sf)

slideview(rast_brick_rgb, ndvi)


