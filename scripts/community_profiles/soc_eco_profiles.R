packages <- c('terra', 'sf', 'dplyr', 'readr', 'exactextractr', 'foreign', 'tidycensus')
lapply(packages, library, character.only=TRUE)

#VERY MESSY BIOPHYS VARS CODE

#exposure data
exp <- rast("large_data/fire_risk_comm_CA/Exposure_CA.tif")
#cwpp polygon shapefile
cwpps <- st_read("data/raw_data/cwpp_boundaries/CA.shp") 
#check and make valid geometry to address rings
st_is_valid(cwpps, reason = T)
cwpps <- st_make_valid(cwpps)
#match crs
cwpps <- st_transform(cwpps, crs = st_crs(exp))
#extract mean exposure per cwpp boundary
cwpps$m_exp <- exact_extract(exposure, cwpps$geometry, fun = 'mean')
hist(cwpps$m_exp) # not normally dist, need to transform
cwpp_exp <- cwpps %>%
  select(Name, Jur, m_exp, geometry)


#land cover type/conus
conus <- rast("data/raw_data/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif")
#conus key
conus_key <- read_csv("data/raw_data/LF2022_EVT_230_CONUS/CSV_Data/LF22_EVT_230.csv") %>%
  select(c(value = VALUE, lc = EVT_PHYS))
# dbf <- foreign::read.dbf("data/raw_data/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif.vat.dbf", as.is = T) %>%
#   select(value = Value, class = EVT_PHYS)
st_crs(conus) == st_crs(cwpps) #TRUE
conus <- crop(conus, ext(cwpps)) 
levels(conus) <- conus_key 

lc <- exact_extract(conus, cwpps, fun = function(df) {
  df %>%
    mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(Name, value) %>%
    summarize(freq = sum(frac_total))
}, summarize_df = TRUE, include_cols = 'Name', progress = FALSE)

lc <- lc %>%
  inner_join(conus_key, by = 'value') %>%
  group_by(Name) %>%
  arrange(desc(freq)) %>%
  slice_head(n = 3) %>%
  mutate(lc_percent = sprintf('%0.1f%%', 100*freq))

#land ownership
ownership <- st_read("data/raw_data/California_Land_Ownership/ownership22_1.shp")
st_crs(ownership) == st_crs(cwpps) #FALSE
ownership <- st_transform(ownership, crs = st_crs(cwpps))
#get area of each landowner type within cwpp boundaries
int2 <- st_intersection(cwpp_exp, ownership) %>%
  mutate(intersect_area = st_area(.))%>% # create new column with shape area of overlapping polys 
  select(Name, OWN_LEVEL, intersect_area) %>%
  group_by(Name, OWN_LEVEL) %>%
  summarize(owner_area = sum(intersect_area)) %>%
  st_drop_geometry()
  
cwpp_exp$jur_area <- st_area(cwpp_exp)

own_jur <- merge(int2, cwpp_exp, by = 'Name') %>%
  mutate(own_percent = round(((owner_area / jur_area)*100), 3)) %>%
  select(Name, OWN_LEVEL, Jur, m_exp, own_percent, geometry) %>%
  group_by(Name) %>%
  mutate(public_percent = sum(own_percent)) 

all_eco <- merge(own_jur, lc, by = 'Name')
eco_sf <- st_write(all_eco, "data/int_data/biophys_vars.shp")

#social


