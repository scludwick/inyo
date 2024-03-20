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
  select(Name, mean_exp) %>%
  st_drop_geometry()

##############land cover type/conus
conus <- rast("data/int_data/landfire/evt2016ca.tif")
conus_key <- read.csv("data/int_data/landfire/evt2016ca.csv")
#conus key, keep just generalized lc type
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
lc_sel <- lc %>%
  inner_join(conus_key, by = 'value') %>%
  group_by(Name, lc) %>%
  summarize(sum_lc = sum(frac_lc), na.rm = F) %>%
  arrange(desc(sum_lc)) %>%
  mutate(lc_percent = (100*sum_lc)) %>%
  ungroup() %>%
  select(-c(sum_lc)) %>%
  group_by(Name) %>%
  pivot_wider(names_from = lc, values_from = lc_percent) %>% 
  ungroup() %>%
  #trying it out condensing and reducing categories
  mutate(allDev = select(., starts_with("Develop")) %>% 
           rowSums(na.rm = T)) %>%
  select(c(`Name`, `Conifer`, `Shrubland`, `Hardwood`, `Grassland`, `allDev`))



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
length(unique(own$Name)) #177 plans only
missing <- cwpps[!cwpps$Name %in% own$Name,]
missing # all comm plans, prob all private??

#get area of cwpp jur
cwpps$jur_area <- st_area(cwpps)

own_jur <- merge(own, cwpps, by = 'Name', all = T) %>%
  mutate(own_percent = round(((owner_area / jur_area)*100), 3)) %>%
  select(Name, OWN_LEVEL, own_percent, geometry) %>%
  group_by(Name) %>%
  mutate(public_percent = sum(own_percent)) %>%
  pivot_wider(names_from = OWN_LEVEL, values_from = own_percent)

cwpp_phys <- merge(own_jur, lc_select, by = 'Name', all = T)
cwpp_phys <- merge(cwpp_phys, cwpps, by = 'Name', all = T)
cwpp_phys <- st_write(cwpp_phys, "data/int_data/cwpp_vars/cwpp_phys.shp", append = F)

# need per year !!!!!!!!!!!!!!! 
# add SVI percentiles


