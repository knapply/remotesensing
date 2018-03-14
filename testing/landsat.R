# install.packages("getlandsat")
library(getlandsat)
# library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)

mines_sf <- read_csv("data/U_mines.csv") %>%
  # filter(str_detect(Name, "UCIL"), !str_detect(Name, "Tummalapalle")) %>%
  # filter(Country == "India", is.na(`Date Closed`)) %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# mines_buffered_sf <- mines_sf %>%
#   filter(str_detect(Name, "UCIL"), !str_detect(Name, "Tummalapalle")) %>%
#   st_transform(crs = 32618) %>%
#   st_buffer(dist = units::set_units(50, km)) %>% 
#   st_transform(crs = 4326)

# mines_bbox <- mines_buffered_sf %>%
#   st_bbox() %>%
#   as.list()

# options(viewer = NULL)
# mapview(mines_buffered_sf, 
#         map.types = c("Esri.WorldImagery", "OpenStreetMap"),
#         lwd = 5,=
#         color = "red", fill = NA) +
#   mapview(mines_sf)

india_adm0 <- getData("GADM", country = "India", level = 0) %>%
  st_as_sf()

india_bbox <- india_adm0 %>%
  st_bbox() %>%
  as.list()

pak_adm0 <- getData("GADM", country = "Pakistan", level = 0) %>%
  st_as_sf()

pak_bbox <- pak_adm0 %>%
  st_bbox() %>%
  as.list()

india_pak <- rbind(india_adm0, pak_adm0)

# plot(india_adm0$geometry)

cls <- readr::cols(entityId = "c", 
                   acquisitionDate = "c",  
                   cloudCover = "d",  
                   processingLevel = "c", 
                   path = "i", 
                   row = "i", 
                   min_lat = "d", 
                   min_lon = "d", 
                   max_lat = "d", 
                   max_lon = "d")

scenes <- readr::read_csv("http://landsat-pds.s3.amazonaws.com/scene_list.gz",
                          col_types = cls)

# res <- lsat_scenes(n_max = 10)

scenes_filtered_polys <- scenes %>%
  filter((min_lat >= india_bbox$ymin & 
            max_lat <= india_bbox$ymax &
            min_lon >= india_bbox$xmin & 
            max_lon <= india_bbox$xmax) |
           (min_lat >= pak_bbox$ymin &
              max_lat <= pak_bbox$ymax &
              min_lon >= pak_bbox$xmin &
              max_lon <= pak_bbox$xmax)
         ) %>%
  # mutate(acquisitionDate = as.POSIXct(acquisitionDate)) %>%
  # filter(year(acquisitionDate) > 2016) %>%
  # head(1000) %>% 
  # scenes[100000,] %>% 
  gather(longs, long, min_lon, max_lon) %>%
  gather(lats, lat, min_lat, max_lat) %>%
  dplyr::select(-c(longs, lats)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(entityId) %>%
  summarise() %>% 
  st_convex_hull()

joined_sf <- scenes_filtered_polys %>%
  st_join(mines_sf, st_covers) %>%
  drop_na(Name) %>%
  as_tibble() %>%
  st_as_sf()


scenes_filtered_full <- joined_sf %>%
  inner_join(scenes) %>%
  mutate(acquisitionDate = as.POSIXct(acquisitionDate))

scenes_filtered_full %>%
  filter(year(acquisitionDate) == 2017) %>%
  # dplyr::select(acquisitionDate) %>% 
  arrange(desc(acquisitionDate)) %>%
  # slice(1) %>%
  mapview() %>%
  + mapview(mines_sf)
  

test_slice <- scenes_filtered_full %>%
  # filter(entityId == "LC81400442017006LGN00")
  filter(download_url == "https://s3-us-west-2.amazonaws.com/landsat-pds/L8/140/044/LC81400442017006LGN00/index.html") %>%
  slice(1) %>%
  pull(download_url)

tiff_files <- lsat_scene_files(test_slice) %>%
    filter(str_detect(file, "\\.TIF$"))

build_brick_components <- function(file){
  band <- file %>%
    str_extract("B\\d{1,2}")
  
  file %>%
    lsat_image() %>%
    raster()
}

scene_brick <- tiff_files$file[1:2] %>%
  map(build_brick_components) %>%
  reduce(raster::stack) %>%
  brick()

rast_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))
  
mapview(scene_brick[[1]], maxBytes = 10^10)


blue <- tiff_files %>%
  filter(str_detect(file, "_B2")) %>%
  pull(file) %>%
  lsat_image()

green <- tiff_files %>%
  filter(str_detect(file, "_B3")) %>%
  pull(file) %>%
  lsat_image()

red <- tiff_files %>%
  filter(str_detect(file, "_B4")) %>%
  pull(file) %>%
  lsat_image()

nir <- tiff_files %>%
  filter(str_detect(file, "_B5")) %>%
  pull(file) %>%
  lsat_image() %>%
  raster::raster()

rast_brick_rgb <- raster::stack(red, green, blue) %>%
  raster::brick()

raster::plotRGB(rast_brick_rgb, colNA = NA)

mapview(rast_brick_rgb) + mapview(mines_sf)

# NDVI = (NIR - Red) / (NIR + Red)
ndvi <- (nir - rast_brick_rgb[[1]]) / (nir + rast_brick_rgb[[1]])

plot(ndvi,
     main = "NDVI",
     axes = FALSE, box = FALSE)

mapview(ndvi, map.types = c("Esri.WorldImagery", "OpenStreetMap")) +
  mapview(mines_sf)

# img <- raster::raster(test_image)
# raster::layer(img)
# plot(img)
# img2 <- raster::raster(test_image2)
# plot(img2)

# mapview(img) + mapview(img2)
mapview(rast_brick, legend = TRUE) + mapview(mines_sf)

raster::nbands(img)



mapview::mapview(img)


