library(raster)
library(rasterVis)
library(sf)
library(RStoolbox)
library(tidyverse)
library(lubridate)
library(mapview)
library(units)
library(maptools)
library(ggspectra)

# global =================================================================================
#* vars ==================================================================================
planet_crs <- "+proj=utm +zone=42 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#** vector ===============================================================================
cust_poly <- read_sf("data/qabul_khel.kml") %>% 
  st_transform(planet_crs) %>% 
  st_zm()

qabul_ponds <- read_sf("data/qabul_ponds.kml") %>% 
  st_transform(planet_crs) %>% 
  st_zm()

big_bbox <- cust_poly %>% 
  st_buffer(set_units(0.75, km))

#** dirs =================================================================================
qabul_dir <- "data/planet/qabul_khel"

qabul_orders_regex <- qabul_dir %>% 
  dir(recursive = TRUE) %>% 
  str_subset("zipped/") %>% 
  str_extract("\\d+") %>% 
  str_c(collapse = "|")

#* visualization =========================================================================
cust_pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(11)
red_pal <- colorRampPalette(brewer.pal(9, "Reds"))(11)
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
#* qabul khel ============================================================================
# dir(qabul_dir, full.names = TRUE, recursive = TRUE) %>% 
#   str_subset("zipped/") %>% 
#   str_subset(qabul_orders_regex) %>% 
#   walk(~ unzip(.x, exdir = qabul_dir))

dates <- qabul_dir %>% 
  dir(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("\\.tif$") %>% 
  str_extract("\\d{8}") %>% 
  unique()

dates_regex <- dates %>% 
  str_c(collapse = "|")

#* qabul_brick ======================================================================
qabul_bricks <- qabul_dir %>% 
  dir(full.names = TRUE, recursive = TRUE) %>% 
  str_subset(., "MS\\.tif$") %>% 
  map(brick) %>% 
  # set_names(paste0("date_", str_extract(map(., names), dates_regex))) %>% 
  # map(crop, as(cust_poly, "Spatial"))
  # map(crop, as(qabul_ponds, "Spatial"))
  map(crop, as(big_bbox, "Spatial"))


par(mfrow = c(2, 2))
qabul_bricks %>% 
  walk(~ plotRGB(.x, r = 3, g = 2, b = 1, stretch = "hist"))
par(mfrow = c(1, 1))

qabul_indices <- qabul_bricks %>% 
  map(get_indices)

qabul_indices %>% 
  walk(~ plot(.x, col = cust_pal, axes = FALSE))

ndvi <- qabul_bricks %>% 
  map(get_indices, index = "NDVI") %>% 
  brick()

animate(ndvi, col = cust_pal, axes = FALSE, legend = FALSE)

plot(ndvi, col = cust_pal, axes = FALSE)

MSAVI2 <- qabul_bricks %>% 
  map(get_indices, index = "MSAVI2") %>% 
  brick()

plot(MSAVI2, col = cust_pal, axes = FALSE)

green_blue <- qabul_bricks %>% 
  map(get_green_blue) %>% 
  brick()
plot(green_blue, col = cust_pal, axes = FALSE)

reds <- qabul_bricks %>% 
  map(~.x[[3]]) %>% 
  brick()

plot(reds, col = red_pal, axes = FALSE)


spectra_df <- raster::extra

plot_band_hist <- function(RasterLayer){
  histo <- hist(RasterLayer, breaks=30, plot = FALSE)
  data <- data.frame(counts = histo$counts, breaks = histo$mids)
  ggplot(data, aes(x = breaks, y = counts, color = counts)) +
    # geom_bar(stat = "identity",alpha = 0.8)+
    geom_line() +
    xlab("Pearson correlation")+ ylab("Frequency") +
    # scale_x_continuous(breaks = seq(-1,1,0.25),
                     # labels = seq(-1,1,0.25)) +
    ggspectra::scale_x_wl_continuous() +
    stat_wb_column(w.band = photobiologyWavebands::VIS_bands()) 
    # stat_color() +
    # scale_color_identity()
    # scale_fill_gradient(low="blue", high="red")
}

gridExtra::grid.arrange(
plot_band_hist(qabul_bricks[[1]]$X20180221_052052_1033_3B_AnalyticMS.1),
plot_band_hist(qabul_bricks[[1]]$X20180221_052052_1033_3B_AnalyticMS.2),
plot_band_hist(qabul_bricks[[1]]$X20180221_052052_1033_3B_AnalyticMS.3),
plot_band_hist(qabul_bricks[[1]]$X20180221_052052_1033_3B_AnalyticMS.4),
ncol = 2
)

grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are not transparent

plot(qabul_bricks[[1]]$X20180221_052052_1033_3B_AnalyticMS.4, col = grayscale_colors)


get_ndvi <- function(raster_obj, red_band_number, nir_band_number){
  red_band <- raster_obj[[red_band_number]]
  nir_band <- raster_obj[[nir_band_number]]
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  return(ndvi)
}
ndvi <- get_ndvi(qabul_bricks[[1]], 3, 1)

veg <- calc(ndvi, function(x){x[x < 0.4] <- NA; return(x)})

ndvi


# qabul_bricks <- 
test <- qabul_dir %>% 
  dir(full.names = TRUE, recursive = TRUE) %>% 
  map(find_planet_folders) %>% 
  compact() %>% 
  # str_subset(., "MS\\.tif$") %>% 
  map(prep_planet_images)
  # map(brick) 


find_planet_folders <- function(dir){
  dir %>%
    str_subset("xml$") %>% 
    dirname() 
}

prep_planet_images <- function(path){
  require(xml2)
  reflect_coefs <- path %>% 
    dir(recursive = TRUE, full.names = TRUE) %>% 
    str_subset("xml$") %>%
    read_xml() %>%
    xml_children() %>% 
    .[[5]] %>% 
    xml_children() %>% 
    xml_find_all("ps:bandSpecificMetadata") %>%
    xml_find_all("ps:reflectanceCoefficient") %>%
    xml_text() %>%
    as.numeric()
  
  brick <- path %>%
    dir(full.names = TRUE) %>% 
    str_subset("MS\\.tif$") %>%
    brick()

  brick[[1]] <- brick[[1]] * reflect_coefs[[1]]
  brick[[2]] <- brick[[2]] * reflect_coefs[[2]]
  brick[[3]] <- brick[[3]] * reflect_coefs[[3]]
  brick[[4]] <- brick[[4]] * reflect_coefs[[4]]
  
  return(brick)
}

test_ndvi <- get_indices(test[[1]], index = "NDVI")

test_ndvi %>% 
  crop(as(big_bbox, "Spatial")) %>% 
  image(col = cust_pal)

"data/planet/qabul_khel/20180312_054915_1049/20180312_054915_1049_3B_AnalyticMS_metadata.xml" %>%
  read_xml() %>%
  xml_children() %>% 
  .[[5]] %>% 
  xml_children() %>% 
  xml_find_all("ps:bandSpecificMetadata") %>%
  xml_find_all("ps:reflectanceCoefficient") %>%
  xml_text() %>%
  as.numeric()
  
str_remove("/\\d{8}_\\d{6}_\\d{4}_\\dB_AnalyticMS_metadata\\.xml$") %>% 
  dir()
  
  # %>% 
    # discard(~ identical(.x, character(0))) %>% 
    # .[[1]] 
  # %>%
    read_xml() %>%
    xml_children() %>%
    .[[5]] %>%
    xml_children() %>%
    xml_find_all("ps:bandSpecificMetadata") %>%
    xml_find_all("ps:reflectanceCoefficient") %>%
    xml_text() %>%
    as.numeric() #%>%
    # as.list() %>% 
    # `names<-`(c("blue", "green", "red", "nir"))
  
  # brick <- dir %>%
  #   str_subset(., "MS\\.tif$") %>%
  #   brick()
  # 
  # brick[[1]] <- brick[[1]] * reflect_coefs[[1]]
  # brick[[2]] <- brick[[2]] * reflect_coefs[[2]]
  # brick[[3]] <- brick[[3]] * reflect_coefs[[3]]
  # brick[[4]] <- brick[[4]] * reflect_coefs[[4]]
  # 
  # return(brick)
}

























