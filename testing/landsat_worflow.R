# install.packages("getlandsat")
library(getlandsat)
library(raster)
library(rasterVis)
library(fasterize)
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)
# library(tmap)

mines_sf <- read_csv("data/U_mines.csv") %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

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
                          col_types = cls) %>% 
  filter(year(acquisitionDate) > 2013) %>%
  filter(cloudCover < 3)


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

priority_mines <- c("Banduhurang UCIL", "Baghalchore")

priority_scenes <- scenes_filtered_full %>% 
  filter(Name %in% priority_mines) %>%
  filter(year(acquisitionDate) > 2016) %>% 
  group_by(Name) %>% 
  arrange(desc(acquisitionDate)) %>% 
  slice(1:4)



priority_scenes %>%
  # filter(Name == "Qabul Khel") %>% 
  # filter(year(acquisitionDate) > 2013) %>%
  # filter(cloudCover < 10) %>% 
  # arrange(desc(acquisitionDate), cloudCover) %>%
  # slice(1) %>%
  mapview() %>%
  + mapview(mines_sf)

crs_to_use <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "

# india ==================================================================================
target_scene <- scenes_filtered_full %>%
  filter(download_url == "https://s3-us-west-2.amazonaws.com/landsat-pds/L8/140/044/LC81400442017006LGN00/index.html")
  
target_mines <- mines_sf %>%
  filter(Name %in% target_scene$Name)

test_slice <- target_scene %>%
  pull(download_url) %>%
  unique()

tiff_files <- lsat_scene_files(test_slice) %>%
    filter(str_detect(file, "\\.TIF$"))

bbox_for_crop <- target_mines %>%
  st_transform("+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_buffer(dist = units::set_units(3, km)) %>%
  as("Spatial") %>%
  raster::extent()

build_brick_components <- function(file){
  if(str_detect(file, "_B([1-7]|9)\\.TIF")){
    downloaded <- file %>%
      lsat_image() %>%
      raster() %>%
      crop(bbox_for_crop) %>%
      trim()
  }
}



scene_brick <- tiff_files$file %>%
  map(build_brick_components) %>%
  reduce(raster::stack) %>%
  brick() %>% 
  projectRaster(crs = CRS(crs_to_use))

brick_names <- c("Cirrus", "Aerosol", "Green",
                 "Red", "Blue", "NIR", "SWIR1", "SWIR2")

names(scene_brick) <- brick_names


# crs_to_use <- as.character(raster::crs(scene_brick))

splom(scene_brick)

mines_sp <- target_mines %>%
  st_transform(crs_to_use) %>% 
  as("Spatial")

centroids <- target_mines %>%
  # st_transform(crs_to_use) %>% 
  st_coordinates() %>% 
  as_tibble()

srtms <- map2(centroids$X, centroids$Y,
              ~ getData("SRTM", lon = .x, lat = .y)) %>% 
  reduce(merge) %>% 
  projectRaster(crs = CRS(crs_to_use)) %>% 
  crop(mines_sp) %>% 
  trim()

ndvi <- (scene_brick[["NIR"]] - scene_brick[["Red"]]) / 
        (scene_brick[["NIR"]] + scene_brick[["Red"]])

ndwi <- (scene_brick[["NIR"]] - scene_brick[["SWIR2"]]) /
        (scene_brick[["NIR"]] + scene_brick[["SWIR2"]])

filtered_ndvi <- ndvi
filtered_ndvi[filtered_ndvi >= 0.05] <- NA
# filtered_ndvi[srtm <= as.list(summary(values(srtm)))$`1st Qu.`] <- NA
filtered_ndvi[ndwi >= -0.02] <- NA
mask_ndvi <- mask(ndvi, filtered_ndvi)

plotRGB(scene_brick, r = 4, g = 3, b = 5)
plot(mask_ndvi, add = TRUE)
plot(as(st_transform(target_mines, crs_to_use)$geometry, "Spatial"),
     lwd = 5, pch = 20,
     col = "white", add = TRUE)

mapview(mask_ndvi) + mapview(target_mines)


rainbow_theme <- rasterTheme(region = rainbow(100))

cust_theme <- rasterTheme(region = brewer.pal(11, "Spectral"))

levelplot(green_blue,
          par.settings = cust_theme) +
  latticeExtra::layer(sp.points(mines_sp, lwd = 3, pch = 20, col = "black"))

# streamplot(scene_brick)

# zoom into individual mines =============================================================
jaduguda <- target_mines %>%
  filter(Name == "Jaduguda UCIL") %>%
  st_transform("+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

jaduguda_sp <- jaduguda %>%
  as("Spatial")

bbox_for_crop <- jaduguda %>%
  st_buffer(dist = units::set_units(2.5, km)) %>%
  as("Spatial") %>%
  raster::extent()

bbox_for_crop <- st_point(c(9611058, 2590212)) %>%
  st_sfc(crs = 3857) %>% 
  st_transform("+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_buffer(dist = units::set_units(5, km)) %>%
  as("Spatial") %>%
  raster::extent()

centroid <- st_point(c(9611058, 2590212)) %>%
  st_sfc(crs = 3857) %>% 
  st_transform(4326) %>%
  st_coordinates() %>%
  c()

jaduguda_brick <- tiff_files$file %>%
  map(build_brick_components) %>%
  reduce(raster::stack) %>%
  brick()

srtm <- getData("SRTM", lon = centroid[[1]], lat = centroid[[2]]) %>% 
  raster::projectRaster(jaduguda_brick) %>%
  crop(bbox_for_crop) %>%
  trim()

names(jaduguda_brick) <- brick_names

ndvi <- (jaduguda_brick[["NIR"]] - jaduguda_brick[["Red"]]) / 
        (jaduguda_brick[["NIR"]] + jaduguda_brick[["Red"]])

ndwi <- (jaduguda_brick[["NIR"]] - jaduguda_brick[["SWIR2"]]) /
        (jaduguda_brick[["NIR"]] + jaduguda_brick[["SWIR2"]])

filtered_ndvi <- ndvi
filtered_ndvi[filtered_ndvi >= 0.05] <- NA
# filtered_ndvi[srtm <= as.list(summary(values(srtm)))$`1st Qu.`] <- NA
filtered_ndvi[ndwi >= -0.02] <- NA
mask_ndvi <- mask(ndvi, filtered_ndvi)

# plot(srtm)
plotRGB(jaduguda_brick, r = 4, g = 3, b = 5)
plot(mask_ndvi, add = TRUE)

sdplot(rasterToContour(ndvi), add = TRUE)

levelplot(ndvi,
          par.settings = cust_theme) +
  latticeExtra::layer(sp.points(jaduguda_sp, lwd = 3, pch = 20, col = "black"))


mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap"),
               raster.size = 10^15, mapview.maxpixels = 10^10,
               raster.palette = colorRampPalette(brewer.pal(11, "Spectral")),
               na.color = NULL,
               layers.control.pos = "topright")

mapview(mask_ndvi, legend = TRUE)

green_blue <- (jaduguda_brick[["Green"]] / jaduguda_brick[["Blue"]])

mapview(jaduguda_brick[["SWIR1"]], legend = TRUE)


# pakistan ===============================================================================
target_scene <- scenes_filtered_full %>%
  filter(str_detect(download_url, "LC81510382016161LGN00"))

target_mines <- mines_sf %>%
  filter(Name %in% target_scene$Name)

test_slice <- target_scene %>%
  pull(download_url) %>%
  unique()

tiff_files <- lsat_scene_files(test_slice) %>%
    filter(str_detect(file, "\\.TIF$"))

bbox_for_crop <- target_mines %>%
  st_transform(crs_to_use) %>% 
  # st_transform(crs = 3857) %>% 
  # st_transform("+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_buffer(dist = units::set_units(3, km)) %>%
  as("Spatial") %>%
  raster::extent()

build_brick_components <- function(file){
    downloaded <- file %>%
      lsat_image() %>%
      raster() #%>%
      # crop(bbox_for_crop) %>%
      # trim()
}


scene_brick <- tiff_files$file %>%
  str_subset("_B([1-7]|9)\\.TIF") %>% 
  map(build_brick_components) %>%
  reduce(raster::stack) %>%
  brick() %>% 
  projectRaster(crs = CRS(crs_to_use)) %>% 
  crop(bbox_for_crop) %>%
  trim()

names(scene_brick)

brick_names <- c("B3" = "Green",
                 "B5" = "NIR",
                 "B6" = "SWIR1",
                 "B2" = "Blue", 
                 "B4" = "Red",
                 "B7" = "SWIR2", 
                 "B1" = "Aerosol",
                 "B9" = "Cirrus")

brick_names <- c("layer.3" = "Green",
                 "layer.5" = "NIR",
                 "layer.6" = "SWIR1",
                 "layer.2" = "Blue", 
                 "layer.4" = "Red",
                 "layer.7" = "SWIR2", 
                 "layer.1" = "Aerosol",
                 "layer.8" = "Cirrus")

names(scene_brick) <- names(scene_brick) %>% 
  # str_extract("B\\d$") %>% 
  str_replace_all(brick_names)

# scene_brick <- scene_brick %>% 
  # crop(bbox_for_crop) %>%
  # trim()
  
# crs_to_use <- as.character(raster::crs(scene_brick))

# splom(scene_brick)

mines_sp <- target_mines %>%
  st_transform(crs_to_use) %>% 
  as("Spatial")

# centroids <- target_mines %>%
  # st_transform(crs_to_use) %>% 
  # st_coordinates() %>% 
  # as_tibble()

# srtms <- map2(centroids$X, centroids$Y,
#               ~ getData("SRTM", lon = .x, lat = .y)) %>% 
#   reduce(merge) %>% 
#   projectRaster(crs = CRS(crs_to_use)) %>% 
#   crop(mines_sp) %>% 
#   trim()

ndvi <- (scene_brick[["NIR"]] - scene_brick[["Red"]]) / 
        (scene_brick[["NIR"]] + scene_brick[["Red"]])

ndwi <- (scene_brick[["NIR"]] - scene_brick[["SWIR2"]]) /
        (scene_brick[["NIR"]] + scene_brick[["SWIR2"]])

filtered_ndvi <- ndvi
filtered_ndvi[filtered_ndvi >= as.list(summary(values(ndvi)))$`1st Qu.`] <- NA
# filtered_ndvi[srtm <= as.list(summary(values(srtm)))$`1st Qu.`] <- NA
filtered_ndvi[ndwi >= as.list(summary(values(ndwi)))$`1st Qu.`] <- NA
mask_ndvi <- mask(ndvi, filtered_ndvi)

plotRGB(scene_brick, r = 4, g = 3, b = 2)
plot(mask_ndvi, add = TRUE)
plot(as(st_transform(target_mines, crs_to_use)$geometry, "Spatial"),
     lwd = 10, pch = 20,
     col = "black", add = TRUE)

mapview(mask_ndvi) + mapview(target_mines)
