filter_scenes <- function(scenes, sf_object) {
  if(!st_geometry_type(sf_object) %in% c("MULTIPOINT","LINESTRING",
                                          "POLYGON", "MULTIPOLYGON")){
    stop('`sf_object` must be on of "MULTIPOINT","LINESTRING", "POLYGON",
         or "MULTIPOLYGON".')
  }
  scene_bbox <- sf_object %>%
    st_bbox() %>%
    as.list()
  
  scenes_filtered <- scenes %>% 
    dplyr::filter(min_lon >= scene_bbox$xmin, 
                  max_lon <= scene_bbox$xmax, 
                  min_lat >= scene_bbox$ymin, 
                  max_lat <= scene_bbox$ymax) %>% 
    dplyr::transmute(min_lon, max_lon, min_lat, max_lat, row = row_number())
    
  scene_extent <- raster::extent(c(scene_bbox$xmin,scene_bbox$xmax,
                                   scene_bbox$ymin, scene_bbox$ymax))
  
  exes0 <- lapply(split(scenes_filtered %>%
                          dplyr::select(min_lon, max_lon, min_lat, max_lat),
                        scenes_filtered$row),
                  function(x) do.call(rex, x))
}