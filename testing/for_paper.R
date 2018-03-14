library(getlandsat)
library(raster)
library(rasterVis)
# library(fasterisze)
library(sf)
library(RStoolbox)
library(tidyverse)
library(lubridate)
library(mapview)
# library(rgdal)

# get data ===============================================================================

mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap"),
               raster.size = 10^15, mapview.maxpixels = 10^10,
               raster.palette = colorRampPalette(c("blue",
                                                   "green",
                                                   "yellow",
                                                   "red")),
               na.color = "transparent",
               layers.control.pos = "topright")

landsat_crs <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

mines_sf <- read_csv("data/U_mines.csv") %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(landsat_crs)

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
  st_convex_hull() %>% 
  st_transform(landsat_crs)

joined_sf <- scenes_filtered_polys %>%
  st_join(mines_sf, st_covers) %>%
  drop_na(Name) %>%
  as_tibble() %>%
  st_as_sf()


scenes_filtered_full <- joined_sf %>%
  inner_join(scenes) %>%
  mutate(acquisitionDate = as.POSIXct(acquisitionDate))

priority_mines <- c("Banduhurang UCIL", "Baghalchore")

target_scenes <- scenes_filtered_full %>% 
  filter(Name %in% priority_mines) %>%
  filter(year(acquisitionDate) > 2016) %>% 
  group_by(Name) %>% 
  arrange(desc(acquisitionDate)) %>% 
  slice(1:4) %>% 
  ungroup()

target_mines <- target_scenes %>%
  dplyr::select(geometry) %>%
  st_join(mines_sf, st_contains) %>% 
  as_tibble() %>% 
  st_as_sf()

target_urls <- target_scenes %>%
  pull(download_url) %>%
  unique()
  
extract_tif_files <- function(target_url){
  target_url %>% 
    lsat_scene_files() %>% 
    filter(str_detect(file, "\\.TIF$"))
}

get_brick_components <- function(file){
  if(str_detect(file, "_B([1-7]|9)\\.TIF")){
    file %>%
      lsat_image()
  }
}

rasters_retrieved <- target_urls %>% 
  map(extract_tif_files) %>% 
  map(~ distinct(.x, file)) %>% 
  flatten() %>% 
  flatten() %>% 
  map(get_brick_components)
  
scene_meta <- target_scenes %>% 
  `st_geometry<-`(NULL) %>% 
  distinct(entityId, Name, acquisitionDate)

band_names <- c("B3" = "Green", "B5" = "NIR",
                 "B6" = "SWIR1", "B2" = "Blue", 
                 "B4" = "Red", "B7" = "SWIR2", 
                 "B1" = "Aerosol", "B9" = "Cirrus")

pak_meta <- rasters_retrieved %>%
  compact() %>% 
  unlist() %>% 
  data_frame(path = .) %>% 
  mutate(entityId = str_extract(path, "LC[A-z0-9]+\\.TIF$") %>% 
           str_replace("_B\\d\\.TIF", "")) %>%
  left_join(scene_meta) %>% 
  filter(Name == "Baghalchore") %>% 
  mutate(band = path %>% 
           str_extract("_B\\d") %>% 
           str_replace("_", "") %>% 
           str_replace_all(band_names))

india_meta <- rasters_retrieved %>%
  compact() %>% 
  unlist() %>% 
  data_frame(path = .) %>% 
  mutate(entityId = str_extract(path, "LC[A-z0-9]+\\.TIF$") %>% 
           str_replace("_B\\d\\.TIF", "")) %>%
  left_join(scene_meta) %>% 
  filter(Name == "Banduhurang UCIL") %>% 
  mutate(band = path %>% 
           str_extract("_B\\d") %>% 
           str_replace("_", "") %>% 
           str_replace_all(band_names))

india_mines_buffer <- mines_sf %>% 
  filter(Name %in% target_mines$Name) %>% 
  st_union() %>% 
  st_convex_hull() %>% 
  st_buffer(dist = units::set_units(2, km)) %>% 
  as("Spatial")

india_mines <- mines_sf %>% 
  filter(Name %in% c("Turamidih UCIL", "Banduhurang UCIL", "Jaduguda UCIL")) %>% 
  filter(Country == "India")

band_names <- c("B3" = "Green", "B5" = "NIR",
                 "B6" = "SWIR1", "B2" = "Blue", 
                 "B4" = "Red", "B7" = "SWIR2", 
                 "B1" = "Aerosol", "B9" = "Cirrus")
  
# build rasters ==========================================================================
build_layer <- function(path, acquisitionDate){
  layer_name <- str_extract(path, "_B\\d") %>% 
    str_replace("_", "") %>% 
    str_replace_all(band_names) %>% 
    paste(as.Date(acquisitionDate), sep = "-")
  
  raster(path) %>% 
    `names<-`(layer_name) %>% 
    crop(extent(as(st_buffer(india_mines, units::set_units(3, km)),
                 "Spatial"))) %>% 
    trim()
}

india_layers <- map2(india_meta$path, india_meta$acquisitionDate,
                   ~ build_layer(.x, .y))

target_cells <- india_layers %>% 
  map(ncell) %>% 
  unlist() %>%  
  min()

baseline_layer <- india_layers %>% 
  keep(~ ncell(.x) == target_cells) %>% 
  pluck(1)

india_brick <- india_layers %>% 
  brick()

turamidih <- mines_sf %>% 
  filter(Name == "Turamidih UCIL")

jaduguda <- mines_sf %>% 
  filter(Name == "Jaduguda UCIL")

turamidih_brick <- india_brick %>% 
      crop(extent(as(st_buffer(turamidih, units::set_units(2.5, km)),
                 "Spatial"))) %>% 
    trim()

plot(turamidih_brick, pal = cust_pal, axes = FALSE)

jaduguda_brick <- india_brick %>% 
      crop(extent(as(st_buffer(jaduguda, units::set_units(2.5, km)),
                 "Spatial"))) %>% 
    trim()

jaduguda_brick %>% 
  get_indices() %>% 
  plot(pal = cust_pal, axes = FALSE)
  

plot(jaduguda_brick, pal = cust_pal, axes = FALSE)

first_jaduguda_2017.02.23 <-jaduguda_brick[[str_which(names(jaduguda_brick), "2017.02.23")]]
sec_jaduguda_2017.03.27 <-jaduguda_brick[[str_which(names(jaduguda_brick), "2017.03.27")]]
third_jaduguda_2017.04.12 <-jaduguda_brick[[str_which(names(jaduguda_brick), "2017.04.12")]]
fourth_jaduguda_2017.04.28 <-jaduguda_brick[[str_which(names(jaduguda_brick), "2017.04.28")]]

all_jaduguda_bricks <- list(first_jaduguda_2017.02.23, sec_jaduguda_2017.03.27,
                            third_jaduguda_2017.04.12, fourth_jaduguda_2017.04.28)

get_indices <- function(raster_brick){
  blue_band <- names(raster_brick)[[str_which(names(raster_brick), "Blue")]]
  green_band <- names(raster_brick)[[str_which(names(raster_brick), "Green")]]
  red_band <- names(raster_brick)[[str_which(names(raster_brick), "Red")]]
  swir2_band <- names(raster_brick)[[str_which(names(raster_brick), "SWIR2")]]
  nir_band <- names(raster_brick)[[str_which(names(raster_brick), "NIR")]]
  
  spectralIndices(raster_brick,
                  blue = blue_band, green = green_band, red = red_band,
                  nir = nir_band, swir2 = swir2_band)
}

# cust_pal <- colorRampPalette(c(brewer.pal(11, "Spectral")))(1000)
cust_pal <- colorRampPalette(c("blue", "lightblue", "yellow", "orange", "red"))(255)

blue_band <- first_jaduguda_2017.02.23[[str_which(names(first_jaduguda_2017.02.23), "Blue")]]
green_band <- first_jaduguda_2017.02.23[[str_which(names(first_jaduguda_2017.02.23), "Green")]]

green_blue <- sec_jaduguda_indices[["NDVI"]] - sec_jaduguda_indices[["NDWI"]]
plot(normImage(green_blue), col = cust_pal)

first_jaduguda_indices <- get_indices(first_jaduguda_2017.02.23)
sec_jaduguda_indices <- get_indices(sec_jaduguda_2017.03.27)
plot(normImage(first_jaduguda_indices), col = cust_pal)

plot(sec_jaduguda_indices[["NDWI"]], col = cust_pal)

second_jaduguda_indices <- get_indices(sec_jaduguda_2017.03.27)
third_jaduguda_indices <- get_indices(third_jaduguda_2017.04.12)
fourth_jaduguda_indicies <- get_indices(fourth_jaduguda_2017.04.28)

combo_jaduguda_indicies <- merge(first_jaduguda_indices, second_jaduguda_indices,
                                 third_jaduguda_indices, fourth_jaduguda_indicies)
plot(combo_jaduguda_indicies, col = cust_pal)



get_rgb_brick <- function(raster_brick){
  red_band <- raster_brick[[str_which(names(raster_brick), "Red")]]
  green_band <- raster_brick[[str_which(names(raster_brick), "Green")]]
  blue_band <- raster_brick[[str_which(names(raster_brick), "Blue")]]
  
  brick(red_band, green_band, blue_band)
}

# calculate indices
cust_pal <- colorRampPalette(c(brewer.pal(11, "Spectral")))(1000)

get_indices <- function(raster_brick, index = "TVI"){
  blue_band <- names(raster_brick)[[str_which(names(raster_brick), "Blue")]]
  green_band <- names(raster_brick)[[str_which(names(raster_brick), "Green")]]
  red_band <- names(raster_brick)[[str_which(names(raster_brick), "Red")]]
  swir2_band <- names(raster_brick)[[str_which(names(raster_brick), "SWIR2")]]
  nir_band <- names(raster_brick)[[str_which(names(raster_brick), "NIR")]]
  
  spectralIndices(raster_brick, index = index,
                  blue = blue_band, green = green_band, red = red_band,
                  nir = nir_band, swir2 = swir2_band)
}

first_indices <- get_indices(first_2017.02.23)
second_indices <- get_indices(sec_2017.03.27)
third_indices <- get_indices(third_2017.04.12)
fourth_indicies <- get_indices(fourth_2017.04.28)

all_indices <- list(first_indices, second_indices, third_indices, fourth_indicies)

all_bricks %>% 
  map(get_indices) %>% 
  brick() %>% 
  plot()
  brick()
  
  map(unlist) %>% 
  # map(~keep(.x, "TVI"))
  map(~pluck(.x, "TVI"))

plot(first_indices)
plot(first_indices[["TVI"]], col = cust_pal)

second_indices <- get_indices(sec_2017.03.27)

lowResImg_pan <- panSharpen(aggregate(first_2017.02.23), sum(first_2017.02.23[[c(2, 6, 7)]]),
                            r = 7, g = 6, b = 2, method = "brovey")

par(mfrow = c(1,2))
plotRGB(first_2017.02.23, r=7,g=6,b=2, stretch = "hist")
plot(first_indices[["TVI"]], col = cust_pal, legend = FALSE, axes = FALSE)

plotRGB(lowResImg_pan, stretch = "hist")

qacs <- classifyQA(img = first_2017.02.23[["Cirrus.2017.02.23"]],
                   type = "cirrus")

library(mapedit)
training <- mapedit::editFeatures(india_mines)

training_sp <- training %>% 
  as_tibble() %>% 
  mutate(Place = "Talings Pond") %>% 
  st_as_sf() %>% 
  select(Place) %>% 
  st_transform(landsat_crs) %>% 
  as("Spatial")


super_class <- superClass(second_indices, 
                          trainData = training_sp, 
                          responseCol = "Place")

unsuper_class <- unsuperClass(second_indices,
                              norm = TRUE,
                              clusterMap = TRUE,
                              nSamples = 200,
                              # algorithm = "Lloyd",
                              # nClasses = 2,
                              nIter = 1000000)

cloudShadowMask(sec_2017.03.27, cloudMask()  )
piff_match <- pifMatch(sec_2017.03.27, first_2017.02.23)
ggR(piff_match$pifMap, geom_raster = TRUE)



plot(second_indices, col = cust_pal, axes = FALSE)

values(indices[["DVI"]]) <- values(indices[["DVI"]])^2
plot(indices[["DVI"]], col = cust_pal)

second_indices[["GNDVI"]] %>% 
  values() %>% 
  order

filtered <- second_indices[["SR"]]
filtered[filtered >= 2] <- NA
# message(sd(values(NDVI)))
# filtered_ndvi[NDWI >= 0] <- NA
# message(sd(values(NDWI)))
filtered <- mask(indices[["SR"]], filtered)

plot(filtered, col = cust_pal)

mapview(indices[["MSAVI"]])

get_NDVI <- function(raster_brick){
  nir_band <- raster_brick[[str_which(names(raster_brick), "NIR")]]
  red_band <- raster_brick[[str_which(names(raster_brick), "Red")]]
  
  (nir_band - red_band) / (nir_band + red_band)
}

get_NDWI <- function(raster_brick){
  green_band <- raster_brick[[str_which(names(raster_brick), "Green")]]
  nir_band <- raster_brick[[str_which(names(raster_brick), "NIR")]]
  # swir_band <- raster_brick[[str_which(names(raster_brick), "SWIR1")]]
  
  (green_band - nir_band) / (green_band + nir_band)
}

get_green_blue <- function(raster_brick){
  green_band <- raster_brick[[str_which(names(raster_brick), "Green")]]
  blue_band <- raster_brick[[str_which(names(raster_brick), "Blue")]]
  
  green_band / blue_band
}

NDVIs <- map(all_bricks, get_NDVI)

plot(brick(NDVIs))

NDWIs <- map(all_bricks, get_NDWI)

plot(brick(NDWIs))

mapview(NDWIs[[1]])

make_masked <- function(NDVI, NDWI){
  filtered_ndvi <- NDVI
  filtered_ndvi[filtered_ndvi >= sd(values(NDVI))] <- NA
  message(sd(values(NDVI)))
  filtered_ndvi[NDWI >= 0] <- NA
  message(sd(values(NDWI)))
  mask(NDVI, filtered_ndvi)
}

masks <- map2(NDVIs, NDWIs, make_masked)
plot(brick(masks), col = cust_pal)

all_bricks %>% 
  map(get_rgb_brick) %>% 
  brick()%>% 
  plotRGB()

# plot(get_rgb_brick(all_bricks))
plot(masks[[4]], add = TRUE)

mapview(masks[[4]])


cust_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))(255)











##########
green_blues <- map(all_bricks, get_green_blue) %>% 
  brick()

plot(green_blues)









first_ndvi <- get_NDVI(first_2017.02.23$NIR.2017.02.23,
                       first_2017.02.23$Red.2017.02.23)




NIR_layers <- india_brick[[str_which(names(india_brick), "NIR")]]

mean_NIR <- raster::calc(NIR_layers, mean)

red_layers <- india_brick[[str_which(names(india_brick), "Red")]]

mean_red <- raster::calc(red_layers, mean)

get_NDVI <- function(NIR_band, red_band){
  (NIR_band - red_band) / (NIR_band + red_band)
}

get_NDWI <- function(NIR_band, SWIR2_band){
  (NIR_band - SWIR2_band) / (NIR_band + SWIR2_band)
}

mean_ndvi <- get_NDVI(mean_NIR, mean_red)

india_mines <- mines_sf %>% 
  # filter(Name %in% target_mines$Name) %>% 
  filter(Name %in% c("Turamidih UCIL", "Banduhurang UCIL")) %>% 
  # filter(!Name %in% c("Bagjata UCIL")) %>% 
  filter(Country == "India")

cropped_mean_ndvi <- mean_ndvi %>% 
  crop(extent(as(st_buffer(india_mines, units::set_units(2, km)),
                 "Spatial"))) %>% 
  trim()

image(cropped_mean_ndvi)
plot(india_mines$geometry, add = TRUE)


