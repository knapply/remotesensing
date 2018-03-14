prep_planet_images <- function(path){
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