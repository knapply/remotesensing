find_planet_folders <- function(dir){
  dir %>%
    str_subset("xml$") %>% 
    dirname() 
}
