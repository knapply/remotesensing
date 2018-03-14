# library(getlandsat)
library(raster)
library(rasterVis)
# library(fasterisze)
library(sf)
library(RStoolbox)
library(tidyverse)
library(lubridate)
library(mapview)
library(units)
# library(rgdal)

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

target_india_mines_buffered_sf <- target_india_mines_sf %>% 
  st_buffer(set_units(2.5, km))
# st_write(target_india_mines_buffered_sf, "data/target_india_mines_buffered_sf.kml")

#** dirs =================================================================================
jaduguda_dir <- "data/planet/jaduguda"
turamidih_dir <- "data/planet/turamidih"

jaduguda_orders <- str_c("162377", "162379", "162381", sep = "|")
turamidih_orders <- str_c("162378", "162380", "162382", sep = "|")

#* visualization =========================================================================
# cust_pal <- colorRampPalette(c("blue", "lightblue", "yellow", "orange", "red"))(255)
cust_pal <- colorRampPalette(brewer.pal(11, "Spectral"))(11)
# cust_pal <- colorRampPalette(c("black", "red", "yellow", "blue", "green"))(255)
# cust_pal <- colorRampPalette(brewer.pal(11, "BrBG"))(255)

#* funs ==================================================================================

# unzip planet imagery ===================================================================
#* jaduguda ==============================================================================
dir("data/planet/", full.names = TRUE) %>% 
  str_subset(jaduguda_orders) %>% 
  walk(~ unzip(.x, exdir = jaduguda_dir))
#* turamidih =============================================================================
dir("data/planet/", full.names = TRUE) %>% 
  str_subset(turamidih_orders) %>% 
  walk(~ unzip(.x, exdir = turamidih_dir))

# build rasters ==========================================================================
#* jaduguda_bricks =======================================================================
jaduguda_2018.03.12 <- jaduguda_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_subset("20180312") %>% 
  .[!str_detect(., "DN_udm")] %>% 
  map(brick) %>% 
  reduce(merge)

plotRGB(jaduguda_2018.03.12, r = 3, g = 2, b = 1, stretch = "hist")
plot(target_india_mines_sf$geometry, add = TRUE, col = "red", lwd = 5)

#* turamidih_brick =======================================================================
turamidih_2018.03.11 <- turamidih_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_subset("20180311") %>% 
  .[!str_detect(., "DN_udm")] %>% 
  map(brick) %>%
  reduce(merge)

plotRGB(turamidih_2018.03.11, r = 3, g = 2, b = 1, stretch = "hist")
plot(target_india_mines_sf$geometry, add = TRUE, col = "red", lwd = 5)

# full extent analysis ===================================================================
#* indices ================================================================================
#** jaduguda ==============================================================================
jaduguda_indices <- spectralIndices(jaduguda_2018.03.12,
                                    blue = 1, green = 2, red = 3,
                                    nir = 4)

plot(jaduguda_indices, col = cust_pal)
plot(jaduguda_indices[[2]], col = cust_pal)
plot(target_india_mines_sf$geometry, add = TRUE, col = "black", lwd = 5)
#** turamidih =============================================================================
turamidih_indices <- spectralIndices(turamidih_2018.03.11,
                                    blue = 1, green = 2, red = 3,
                                    nir = 4)

plot(turamidih_indices, col = cust_pal)
image(turamidih_indices[["NRVI"]], col = cust_pal)

# cropped extent ======== ================================================================
#* jaduguda ==============================================================================
jaduguda_buffer <- target_india_mines_sf %>% 
  filter(Name == "Jaduguda UCIL") %>% 
  st_buffer(set_units(4, km)) %>% 
  as("Spatial")

crop_jaduguda_2018.03.12 <- jaduguda_2018.03.12 %>% 
  crop(jaduguda_buffer) %>% 
  trim()

plotRGB(crop_jaduguda_2018.03.12, r = 3, g = 2, b = 1, stretch = "hist")
image(crop_jaduguda_2018.03.12[[4]], col = cust_pal)

#* turamidih =============================================================================
turamidih_buffer <- target_india_mines_sf %>% 
  filter(Name == "Turamidih UCIL") %>% 
  st_buffer(set_units(2, km)) %>% 
  as("Spatial")

crop_turamidih_2018.03.11 <- turamidih_2018.03.11 %>% 
  crop(turamidih_buffer) %>% 
  trim()

plotRGB(crop_turamidih_2018.03.11, r = 3, g = 2, b = 1, stretch = "hist")
image(crop_turamidih_2018.03.11[[4]], col = cust_pal)

#* cropped indices =======================================================================
#** jaduguda =============================================================================
crop_jaduguda_indices <- spectralIndices(crop_jaduguda_2018.03.12,
                                    blue = 1, green = 2, red = 3,
                                    nir = 4)

plot(crop_jaduguda_indices, col = cust_pal)
image(crop_jaduguda_indices[["RVI"]], col = cust_pal)
plot(target_india_mines_sf$geometry, add = TRUE, col = "black", lwd = 5)

c(1:length(names(crop_jaduguda_indices))) %>% 
  walk(~ image(crop_jaduguda_indices[[.x]], col = cust_pal, main = names(.x)))

#*** RVI mask ============================================================================
# rvi_mask_jaduguda <- crop_jaduguda_indices[["RVI"]]
# rvi_mask_jaduguda[
#   rvi_mask_jaduguda < ( (values(rvi_mask_jaduguda)) < mean(values(rvi_mask_jaduguda)) )
#   |
#     rvi_mask_jaduguda > ( (values(rvi_mask_jaduguda)) > mean(values(rvi_mask_jaduguda)) )
#   ] <- NA
# 
# rvi_mask_jaduguda[ values(rvi_mask_jaduguda) > 
#                      mean(values(rvi_mask_jaduguda), na.rm = TRUE)
#                       ] <- NA
# plot(rvi_mask_jaduguda, col = cust_pal)
#** turamidih ============================================================================
crop_turamidih_indices <- spectralIndices(crop_turamidih_2018.03.11,
                                    blue = 1, green = 2, red = 3,
                                    nir = 4)

image(crop_turamidih_indices[["RVI"]], col = cust_pal)

c(1:length(names(crop_turamidih_indices))) %>% 
  walk(~ image(crop_turamidih_indices[[.x]], col = cust_pal))

# better cropped extent ==================================================================
#* jaduguda ==============================================================================
cust_poly_jaduguda <- mapedit::editFeatures(jaduguda_buffer)
cust_poly_jaduguda <- cust_poly_jaduguda %>% 
  st_transform(planet_crs) %>% 
  as("Spatial")

bet_crop_jaduguda <- jaduguda_2018.03.12 %>% 
  crop(cust_poly_jaduguda) %>% 
  trim()

plotRGB(bet_crop_jaduguda, r = 3, g = 2, b = 1, stretch = "hist")
plot(bet_crop_jaduguda)
#** indices ==============================================================================
bet_crop_jaduguda_indices <- spectralIndices(bet_crop_turamidih,
                                             blue = 1, green = 2, red = 3,
                                             nir = 4)
plot(bet_crop_jaduguda_indices, col = cust_pal)
c(1:length(names(bet_crop_jaduguda_indices))) %>% 
  walk(~ image(bet_crop_jaduguda_indices[[.x]], col = cust_pal, main = names(.x)))

#** turamidih ============================================================================
cust_poly_turamidih <- mapedit::editFeatures(turamidih_buffer) %>% 
# cust_poly_jaduguda <- cust_poly_jaduguda %>% 
  st_transform(planet_crs) %>% 
  as("Spatial")

bet_crop_turamidih <- turamidih_2018.03.11 %>% 
  crop(cust_poly_turamidih) %>% 
  trim()

plotRGB(bet_crop_turamidih, r = 3, g = 2, b = 1, stretch = "hist")
plot(bet_crop_turamidih)
#** indices ==============================================================================
bet_crop_turamidih_indices <- spectralIndices(bet_crop_turamidih,
                                             blue = 1, green = 2, red = 3,
                                             nir = 4)
image(bet_crop_turamidih_indices[["MSAVI"]], col = cust_pal)
c(1:length(names(bet_crop_turamidih_indices))) %>% 
  walk(~ plot(bet_crop_turamidih_indices[[.x]], col = cust_pal, main = .x))

# temporal ===============================================================================
#* turamidih =============================================================================
turamidih_2018.01.30 <- turamidih_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_subset("20180130") %>% 
  .[!str_detect(., "DN_udm")] %>% 
  map(brick) %>%
  reduce(merge) %>% 
  crop(cust_poly_turamidih) %>% 
  trim()

turamidih_2018.01.30_indices <- spectralIndices(turamidih_2018.01.30,
                                             blue = 1, green = 2, red = 3,
                                             nir = 4)

turamidih_2017.12.25 <- turamidih_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_subset("20171225") %>% 
  .[!str_detect(., "DN_udm")] %>% 
  map(brick) %>%
  reduce(merge) %>% 
  crop(cust_poly_turamidih) %>% 
  trim()

turamidih_2017.12.25_indices <- spectralIndices(turamidih_2017.12.25,
                                             blue = 1, green = 2, red = 3,
                                             nir = 4)
plot(turamidih_2017.12.25_indices, col = cust_pal)

turamidih_ndvi_brick <- list(turamidih_2017.12.25_indices[["NDVI"]],
                             turamidih_2018.01.30_indices[["NDVI"]],
                             bet_crop_turamidih_indices[["NDVI"]]
                             ) %>% 
  brick()

plot(turamidih_ndvi_brick, col = cust_pal)

par(mfrow = c(2, 2))
c(1:length(names(turamidih_ndvi_brick))) %>% 
  walk(~ plot(turamidih_ndvi_brick[[.x]], 
              axes = FALSE,
              col = cust_pal, 
              main = .x))

animate(turamidih_ndvi_brick, pause = 0.75, col = cust_pal)

# green_blue =============================================================================
get_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  
  green_band / blue_band
}

turamidih_green_blue_brick <- list(turamidih_2017.12.25, 
                                   turamidih_2018.01.30,
                                   bet_crop_turamidih) %>% 
  map(get_green_blue) %>% 
  brick()

plot(turamidih_green_blue_brick, col = cust_pal)

animate(turamidih_green_blue_brick, pause = 0.75, col = cust_pal)
  
# NDWI ===================================================================================
turamidih_ndwi_brick <- list(turamidih_2017.12.25_indices[["NDWI"]],
                             turamidih_2018.01.30_indices[["NDWI"]],
                             bet_crop_turamidih_indices[["NDWI"]]
                             ) %>% 
  brick()

plot(turamidih_ndwi_brick, col = cust_pal)




