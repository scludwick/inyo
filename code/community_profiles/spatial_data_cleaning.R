#### TRANSFORMING SPATIAL DATA

#rasters
conus <- rast("data/raw_data/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif")
exp <- rast("large_data/fire_risk_comm_CA/Exposure_CA.tif")

#shapefiles
cwpps <- st_read("data/raw_data/cwpp_boundaries/CA.shp") 
st_is_valid(cwpps, reason = T)
cwpps <- st_make_valid(cwpps) #fix incomplete polygon

ownership <- st_read("data/raw_data/California_Land_Ownership/ownership22_1.shp")
st_is_valid(ownership, reason = T)

# st_crs(cwpps) == st_crs(conus) #FALSE
# st_crs(cwpps) == st_crs(exp) #FALSE
# st_crs(cwpps) == st_crs(ownership) #TRUE
# st_crs(conus) == st_crs(exp) #TRUE

#transform polys to match raster crs
cwpps <- st_transform(cwpps, st_crs(conus))
ownership <- st_transform(ownership, st_crs(conus))

st_write(cwpps, "data/int_data/cwpps_transformed.shp", append = F)
st_write(ownership, "data/int_data/ownership_transformed.shp", append = F)


## crop rasters and keys for each year (right now just 2016)
conus <- rast("data/raw_data/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif")
evt_key <- read_csv("data/raw_data/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv") %>%
  select(c(value = VALUE, lc = EVT_PHYS))
st_crs(evt2016) == st_crs(cwpps) #TRUE
evt2016ca <- crop(evt2016, ext(cwpps)) 
writeRaster(evt2016ca, "data/int_data/landfire/evt2016ca.tif", overwrite = T)
write.csv(evt_key, "data/int_data/landfire/evt2016ca.csv")

## exposure raster is already cropped to CA
