### cropping landfire rasters

cwpps <- st_read("data/raw_data/cwpp_boundaries/CA.shp") 
evt2016 <- rast("data/raw_data/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif")
evt_key <- read_csv("data/raw_data/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv") %>%
  select(c(value = VALUE, lc = EVT_PHYS))
st_crs(evt2016) == st_crs(cwpps) #FALSE
cwpps <- st_transform()
evt2016ca <- crop(evt2016, ext(cwpps)) 
writeRaster(evt2016ca, "data/int_data/landfire/evt2016ca.tif")
write.csv(evt_key, "data/int_data/landfire/evt2016ca.csv")

