packages <- c('terra', 'sf', 'dplyr', 'readr', 'exactextractr', 'foreign', 'tidycensus', 'tigris', 'findSVI')
lapply(packages, library, character.only=TRUE)

###############exposure data
exp <- rast("large_data/fire_risk_comm_CA/Exposure_CA.tif")
#cwpp polygon shapefile
cwpps <- st_read("data/raw_data/cwpp_boundaries/CA.shp") 
#check and make valid geometry to address rings
st_is_valid(cwpps, reason = T)
cwpps <- st_make_valid(cwpps)
#match crs to raster
cwpps <- st_transform(cwpps, crs = st_crs(exp))
#extract mean exposure per cwpp boundary
cwpps$mean_exp <- exact_extract(exp, cwpps$geometry, fun = 'mean')
cwpps <- cwpps %>%
  select(Name, Jur, mean_exp, geometry)

##############land cover type/conus
conus <- rast("data/raw_data/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif")
#conus key, keep just generalized lc type
conus_key <- read_csv("data/raw_data/LF2022_EVT_230_CONUS/CSV_Data/LF22_EVT_230.csv") %>%
  select(c(value = VALUE, lc = EVT_PHYS))
st_crs(conus) == st_crs(cwpps) #TRUE
conus <- crop(conus, ext(cwpps)) 
levels(conus) <- conus_key 
#summarize categorical raster -- get conus values and coverage fractions for conus pixel in each cwpp, then get fraction of each cwpp of each lc
lc <- exact_extract(conus, cwpps, fun = function(df) {
  df %>%
    mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(Name, value) %>% #include name so it does not get lost when summarize
    summarize(frac_lc = sum(frac_total)) 
 }, summarize_df = TRUE, include_cols = 'Name', progress = FALSE)


lc <- lc %>%
  inner_join(conus_key, by = 'value') %>%
  group_by(Name) %>%
  arrange(desc(frac_lc)) %>%
  mutate(lc_percent = sprintf('%0.1f%%', 100*frac_lc)) %>%
  select(-c(value, frac_lc))


############### land ownership
ownership <- st_read("data/raw_data/California_Land_Ownership/ownership22_1.shp")
st_crs(ownership) == st_crs(cwpps) #FALSE
ownership <- st_transform(ownership, crs = st_crs(cwpps))
# st_is_valid(ownership) #TRUE

#get area of each landowner type within cwpp boundaries
own <- st_intersection(cwpps, ownership) %>%
  mutate(intersect_area = st_area(.))%>% # create new column with shape area of intersecting polys 
  select(Name, OWN_LEVEL, intersect_area) %>%
  group_by(Name, OWN_LEVEL) %>%
  summarize(owner_area = sum(intersect_area)) %>%
  st_drop_geometry()

#get area of cwpp jur
cwpps$jur_area <- st_area(cwpps)

own_jur <- merge(own, cwpps, by = 'Name') %>%
  mutate(own_percent = round(((owner_area / jur_area)*100), 3)) %>%
  select(Name, OWN_LEVEL, Jur, mean_exp, own_percent, geometry) %>%
  group_by(Name) %>%
  mutate(public_percent = sum(own_percent)) 

all_eco <- merge(own_jur, lc, by = 'Name')
eco_sf <- st_write(all_eco, "data/int_data/vars/biophys_vars.shp", append = F)




