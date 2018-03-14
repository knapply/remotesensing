library(raster)
library(rasterVis)
library(sf)
library(RStoolbox)
library(tidyverse)
library(lubridate)
library(mapview)
library(units)
library(maptools)

# global =================================================================================
#* vars ==================================================================================
planet_crs <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#** vector ===============================================================================
mines_sf <- read_csv("data/U_mines.csv") %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(planet_crs)

target_india_mines_sf <- mines_sf %>%
  filter(Name %in% c("Turamidih UCIL", "Jaduguda UCIL"))

# target_india_mines_buffered_sf <- target_india_mines_sf %>% 
#   st_buffer(set_units(2.5, km))
# st_write(target_india_mines_buffered_sf, "data/target_india_mines_buffered_sf.kml")

# write_rds(cust_poly_turamidih, "data/cust_poly_turamidih.rds")
cust_poly_turamidih <- read_rds("data/cust_poly_turamidih.rds")

#** dirs =================================================================================
turamidih_dir <- "data/planet/turamidih"
turamidih_orders <- str_c("162378", "162380", "162382", 
                          "162431", "162432", "162433",
                          sep = "|")

#* visualization =========================================================================
# cust_pal <- colorRampPalette(brewer.pal(11, "Spectral"))(255)
cust_pal <- colorRampPalette(c("darkblue", "lightgreen", "yellow", "red"))(10)
cust_theme <- rasterTheme(region = brewer.pal(11, "Spectral"))

#* foos ==================================================================================
get_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  
  green_band / blue_band
}

# unzip planet imagery ===================================================================
#* turamidih =============================================================================
dir("data/planet/", full.names = TRUE) %>% 
  str_subset(turamidih_orders) %>% 
  walk(~ unzip(.x, exdir = turamidih_dir))

dates <- turamidih_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_extract("\\d{8}") %>% 
  unique()

dates_regex <- dates %>% 
  str_c(collapse = "|")

#* turamidih_brick =======================================================================
sep_bricks <- turamidih_dir %>% 
  dir(full.names = TRUE, recursive = TRUE) %>% 
  str_subset(., "MS\\.tif$") %>% 
  map(brick) %>% 
  set_names(str_extract(map(., names), dates_regex))

date_names <- sep_bricks %>% 
  map(names) %>% 
  map(~str_extract(.x, dates_regex)) %>% 
  map(unique) %>% 
  unlist(use.names = FALSE) %>% 
  unique()

combo_bricks <- list(sep_bricks[[1]], 
                     sep_bricks[[2]],
                     merge(sep_bricks[[3]], sep_bricks[[4]]),
                     merge(sep_bricks[[5]], sep_bricks[[6]]),
                     merge(sep_bricks[[7]], sep_bricks[[8]]),
                     merge(sep_bricks[[9]], sep_bricks[[10]])
                     ) %>% 
  set_names(date_names)

plot_names <- combo_bricks %>% 
  names() %>% 
  str_replace("(\\d{4})(\\d{2})(\\d{2})", "Date_\\1-\\2-\\3")

cropped_bricks <- combo_bricks %>% 
  map(crop, cust_poly_turamidih) %>% 
  map(trim)

get_indices <- function(brick, ...){
  spectralIndices(brick,
                  blue = 1, green = 2, red = 3,
                  nir = 4, ...)
}

cropped_bricks %>% 
  walk(~ plotRGB(.x, r = 3, g = 2, b = 1, stretch = "hist"))

brick_indices <- cropped_bricks %>% 
  map(get_indices)

ndvi <- brick_indices %>% 
  map(~ .x$NDVI) %>% 
  brick()

ndvi %>% 
  `names<-`(plot_names) %>% 
  # set_names(str_extract_all(names(.), "\\d{8}")) %>% 
  plot(col = cust_pal, axes = FALSE)

par(mfrow = c(2, 3))
ndvi %>% 
  walk(image, col = cust_pal)

ndwi <- brick_indices %>% 
  map(~ .x$NDWI) %>% 
  brick()

plot(ndwi)
levelplot(ndwi, par.settings = cust_theme)
bwplot(ndvi)

green_blue <- cropped_bricks %>% 
  map(get_green_blue) %>% 
  brick()

plot(green_blue)

unsuperClass()

k_means <- cropped_bricks %>% 
  map(unsuperClass, nClasses = 2, nIter = 1000, norm = TRUE) 

k_means %>% 
  map("map") %>% 
  walk(image)














