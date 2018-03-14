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
planet_crs <- "+proj=utm +zone=44 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#** vector ===============================================================================
cust_poly <- read_sf("data/Tummalapalle.kml") %>% 
  st_transform(planet_crs) %>% 
  st_zm()

tumm_mill_area <- read_sf("data/tumm_mill_area.kml") %>% 
  st_transform(planet_crs) %>% 
  st_zm()

tumm_big_tail_pond <- read_sf("data/tumm_big_tail_pond.kml") %>% 
  st_transform(planet_crs) %>% 
  st_zm()

#** dirs =================================================================================
tumm_dir <- "data/planet/tummalapalle"

tumm_orders_regex <- tumm_dir %>% 
  dir(recursive = TRUE) %>% 
  str_subset("zipped/") %>% 
  str_extract("\\d+") %>% 
  str_c(collapse = "|")

#* visualization =========================================================================
cust_pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(11)
# cust_pal <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(255)
cust_theme <- rasterTheme(region = brewer.pal(11, "Spectral"))

#* foos ==================================================================================
get_indices <- function(brick, ...){
  spectralIndices(brick,
                  blue = 1, green = 2, red = 3,
                  nir = 4, ...)
}

get_green_blue <- function(raster_brick){
  green_band <- raster_brick[[2]]
  blue_band <- raster_brick[[1]]
  
  green_band / blue_band
}

# unzip planet imagery ===================================================================
#* turamidih =============================================================================
dir(tumm_dir, full.names = TRUE, recursive = TRUE) %>% 
  str_subset("zipped/") %>% 
  str_subset(tumm_orders_regex) %>% 
  walk(~ unzip(.x, exdir = tumm_dir))

dates <- tumm_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_extract("\\d{8}") %>% 
  .[!str_detect(., "20170428")] %>% 
  unique()

dates_regex <- dates %>% 
  str_c(collapse = "|")

#* tumm_brick =======================================================================
tumm_bricks <- tumm_dir %>% 
  dir(full.names = TRUE, recursive = TRUE) %>% 
  map(find_planet_folders) %>% 
  compact() %>% 
  # str_subset(., "MS\\.tif$") %>% 
  map(prep_planet_images) %>% 
  map(crop, as(tumm_mill_area, "Spatial"))

tumm_bricks[[3]] <- NULL

nirs <- tumm_bricks %>% 
  map(~.x[[4]]) %>% 
  brick()
plot(nirs, col = cust_pal)

nirs[nirs < 0.4] <- NA
plot(nirs, col = cust_pal)

mapview(nirs)

mask_ndwi <- function(x, y, ...){x[x < y] <- NA; return(x)}

tumm_ndwi <- tumm_bricks %>% 
  map(get_indices, index = "NDWI") %>% 
  brick()

ndwi_quant_75 <- quantile(values(tumm_ndwi), 0.98, na.rm = TRUE)
# ndwi_quant_75 <- max(values(tumm_ndwi))

ndwi_masked <- raster::calc(tumm_ndwi, function(x, y){
  x[x < ndwi_quant_75] <- NA
  return(x)
  })

plot(ndwi_masked, col = cust_pal)

red_water <- tumm_bricks %>% 
  map(~.x[[3]]) %>% 
  brick() %>% 
  mask(ndwi_masked)

plot(red_water, col = cust_pal)

red_water %>% 
  walk(plot, col = cust_pal)

red_water <- tumm_bricks %>% 
  map(mask, ndwi_masked)

  quantile(values(tumm_ndvi), na.rm = TRUE)[["75%"]]
  # raster::calc(function(x){x[x < 0] <- NA; return(x)})

plot(tumm_ndwi, col = cust_pal)


tumm_ndvi <- tumm_bricks[[1]] %>% 
  get_indices(index = "NDVI")
  map(get_indices, index = "NDVI") %>% brick()
plot(tumm_ndvi, col = cust_pal)

quantile(values(tumm_ndvi), na.rm = TRUE)[["75%"]]

%>% 
  map(~ raster::calc(.x, function(x){x[x > 0] <- NA; return(x)})) %>% 
  brick()

animate(tumm_ndvi)

tumm_green_blue <- tumm_bricks %>% 
  map(get_green_blue) %>% 
  brick()
plot(tumm_green_blue, col = cust_pal)

tumm_bricks %>% walk(plot)

tumm_green_blue <- tumm_bricks %>% 
  map(get_green_blue) %>% 
  brick()
animate(tumm_green_blue)

tumm_reds <- tumm_bricks %>% 
  map(~.x[[3]]) %>% 
  brick()

plot(tumm_reds, col = brewer.pal(9, "Reds"))

par(mfrow = c(3, 4))
tumm_ndvi %>% 
  walk(~ image(.x, col = cust_pal))
par(mfrow = c(1, 1))



not_veg <- raster::calc(ndvi, function(x){x[x > 0] <- NA; return(x)})

# tumm_bricks <- tumm_dir %>%
#   dir(full.names = TRUE, recursive = TRUE) %>%
#   str_subset(., "MS\\.tif$") %>%
#   .[!str_detect(., "20170428")] %>%
#   map(brick) %>%
#   set_names(paste0("date_", str_extract(map(., names), dates_regex))) %>%
#   # map(crop, as(tumm_big_tail_pond, "Spatial"))
#   map(crop, as(tumm_mill_area, "Spatial"))



par(mfrow = c(3, 3))
tumm_bricks %>% 
  walk(~ plotRGB(.x, r = 3, g = 2, b = 1, stretch = "hist"))
par(mfrow = c(1, 1))

tumm_bricks %>% 
  # map(~.x[[1]])
  walk(~ plotRGB(.x, r = 4, g = 2, b = 1, stretch = "lin"))

MSAVI <- tumm_bricks %>% 
  map(get_indices, index = "MSAVI") %>% 
  brick()

image(MSAVI[[1]], col = cust_pal)

MSAVI2 <- tumm_bricks %>% 
  map(get_indices, index = "MSAVI2") %>% 
  brick()

mask_ndvi <- function(brick){
  ndvi_mask <- brick
  ndvi_mask[ndvi_mask > -0.3] <- NA
  mask(brick, ndvi_mask)
}

NDVI <- tumm_bricks %>% 
  map(get_indices, index = "NDVI") #%>%
  # map(mask_ndvi) %>% 
  # brick()
NDVI %>% 
  brick() %>% 
  animate(col = cust_pal)
image(NDVI[[1]], col = cust_pal)

plot_mask <- function(rgb, ndvi){
  plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "hist")
  plot(ndvi, add = TRUE)
}

tumm_bricks %>% 
  map(overlay, NDVI)

# walk2(tumm_bricks, NDVI + plot_mask)



animate(ndvi_mask,
        col = cust_pal, axes = FALSE, pause = 2, maxpixels = 100000)

plot(MSAVI2, col = cust_pal, axes = FALSE)

EVI <- tumm_bricks %>% 
  map(get_indices, index = "EVI") %>% 
  brick()

plot(EVI, col = cust_pal, axes = FALSE)

green_blue <- tumm_bricks %>% 
  map(get_green_blue) %>% 
  brick()
plot(green_blue, col = cust_pal, axes = FALSE)

reds <- tumm_bricks %>% 
  map(~.x[[3]]) %>% 
  brick()

plot(reds[[1]], col = cust_pal, axes = FALSE)


indicies <- tumm_bricks %>% 
  map(get_indices) %>% 
  brick()


ani.options(convert ="C:/Program Files/ImageMagick-7.0.7-Q16/magick.exe")

# dir.create("examples")
# setwd("examples")

# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width = 600, height = 600)

plot(indicies[[1]], col = cust_pal)
indicies%>% 
    walk(~ image(.x, col = cust_pal, axes = FALSE))

animate(indicies, col = cust_pal)

dev.off()

# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("magick *.png -delay 500 example_1.gif",
       show.output.on.console = TRUE)

# to not leave the directory with the single jpeg files
# I remove them.
# file.remove(list.files(pattern=".png"))
