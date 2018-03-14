library(getSpatialData)
library(raster)

ext <- extent(10.29048, 11.75558, 45.93350, 46.94617)
time_range <-  c("20170801", "20170830")
platform <- "Sentinel-2"

set_cophub_login(hub_user = "syknapptic")

products <- getSentinel_query(ext = ext, time_range = time_range, platform = platform)
