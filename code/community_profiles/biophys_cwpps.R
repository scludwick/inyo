packages <- c('terra', 'sf', 'dplyr', 'readr', 'exactextractr', 'foreign', 'tidycensus', 'tigris', 'findSVI', 'tidyr')
lapply(packages, library, character.only=TRUE)

#cwpp polygon shapefile of 182 CWPP boundaries
cwpps <- st_read("data/raw_data/cwpp_boundaries/CA.shp") 
st_is_valid(cwpps, reason = T)
cwpps <- st_make_valid(cwpps)
###############exposure data
exp <- rast("large_data/fire_risk_comm_CA/Exposure_CA.tif")
#check and make valid geometry to address rings
#match cwpp crs to raster
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
#summarize categorical raster -- get conus values and coverage fractions for conus pixel in each cwpp, then get fraction of each lc type by dividing coverage fraction by all cov fractions in poly, then sum per lc value https://www.dante-project.org/vignettes/exactextractr-cat
lc <- exact_extract(conus, cwpps, fun = function(df) {
  df %>%
    mutate(frac_perpoly = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(Name, value) %>% 
    summarize(frac_lc = sum(frac_perpoly)) %>%
    ungroup()
 }, summarize_df = TRUE, include_cols = 'Name', progress = FALSE)
# length(unique(lc$Name)) #182 


#join to key and get lc percent. dupe lc because this is coarse phys desc., so summarize those
lc_p <- lc %>%
  inner_join(conus_key, by = 'value') %>%
  group_by(Name, lc) %>%
  summarize(sum_lc = sum(frac_lc), na.rm = F) %>%
  arrange(desc(sum_lc)) %>%
  mutate(lc_percent = (100*sum_lc)) %>%
  ungroup() %>%
  select(-c(sum_lc)) %>%
  group_by(Name) %>%
  pivot_wider(names_from = lc, values_from = lc_percent) %>% #will want to reduce these later
  ungroup()
# length(unique(lc_p$Name)) #179

### NEED TO FIGURE OUT how to get missing LC for NE CA
missing <- lc[!lc$Name %in% lc_p$Name,]
missing # no lc data for these polys
miss_cw <- cwpps[!cwpps$Name %in% lc_p$Name,]
plot(conus) #missing a chunk where these are!!! whyyyyyyyy 
plot(miss_cw)
# no conus values for these polys 



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
# length(unique(own$Name)) #177 
# missing <- cwpps[!cwpps$Name %in% own$Name,]
# missing # all comm plans, prob all private

#get area of cwpp jur
cwpps$jur_area <- st_area(cwpps)

own_jur <- merge(own, cwpps, by = 'Name', all = T) %>%
  mutate(own_percent = round(((owner_area / jur_area)*100), 3)) %>%
  select(Name, OWN_LEVEL, own_percent, geometry) %>%
  group_by(Name) %>%
  mutate(public_percent = sum(own_percent)) %>%
  pivot_wider(names_from = OWN_LEVEL, values_from = own_percent)
### should i make NAs zero for those 5 plans? 


all_eco <- merge(own_jur, lc_p, by = 'Name', all = T)
eco_sf <- st_write(all_eco, "data/int_data/biophys_vars.shp", append = F)

eco <- st_read("data/int_data/biophys_vars.shp")

