packages <- c('terra', 'sf', 'dplyr', 'readr', 'exactextractr', 'foreign', 'tidycensus', 'tigris', 'findSVI', 'tidyr')
lapply(packages, library, character.only=TRUE)

# read in cwpp shapefile
cwpps <- st_read("data/int_data/cwpps_transformed.shp")
#get area of ownership per area of cwpp
cwpps$jur_area <- st_area(cwpps)

### exposure
# read in exposure rast
exp <- rast("large_data/fire_risk_comm_CA/Exposure_CA.tif")
# st_crs(exp) == st_crs(cwpps)
# get average exposure per CWPP boundary
cwpps$mean_exp <- exact_extract(exp, cwpps$geometry, fun = 'mean')


### land ownership
ownership <- st_read("data/int_data/ownership_transformed.shp")
# st_crs(ownership) == st_crs(cwpps) #TRUE
#get area of each landowner type within cwpp boundaries
own <- st_intersection(cwpps, ownership) %>%
  mutate(intersect_area = st_area(.))%>% # create new column with shape area of intersecting polys
  select(Name, OWN_LEVEL, intersect_area) %>%
  group_by(Name, OWN_LEVEL) %>%
  summarize(owner_area = sum(intersect_area)) %>% # get the sum of areas for all owner types
  rename(owner_type = OWN_LEVEL) %>%
  st_drop_geometry()
# length(unique(own$Name)) #177 plans only, perhaps all private? 
# cwpps[!cwpps$Name %in% own$Name,] #all comm plans, prob all private

# sum public owners, not tribal or nonprofit
own <- merge(cwpps, own, by = 'Name', all = T) %>%
  mutate(own_frac = owner_area / jur_area) %>%
  group_by(Name) %>%
  filter(!owner_type %in% c("Tribal", "Non Profit")) %>%
  mutate(public_own = sum(own_frac)) %>%
  select(Name, Jur, jur_area, mean_exp, public_own) %>%
  unique() %>% # to remove duplicate rows from when the owner_type was different
  st_drop_geometry()
# cwpps[!cwpps$Name %in% pub$Name,]
# dropped 1 CWPP, Fort Ross. Checked and it was all non profit, add back
cwpps <- merge(cwpps, own, all = T)

### ECO vars
ecoregion <- st_read("data/int_data/ecoregions.shp")
# classify each poly based on largest biome type
cwpps_ecoreg <- st_join(cwpps, ecoregion, join = st_intersects, largest = T) %>%
  mutate(ecoregion = as.factor(BIOME_NAME)) %>%
  select(c(Name, Jur, ecoregion, geometry)) %>%
  st_drop_geometry()
# # or get fraction of each
ecoreg <- st_intersection(cwpps, ecoregion) %>%
  mutate(intersect_area = st_area(.))%>% # create new column with shape area of intersecting polys
  group_by(Name, BIOME_NAME) %>%
  summarize(ecoreg_area = sum(intersect_area)) %>% # get the sum of areas for all biome types per cwpp
  rename(ecoregion = BIOME_NAME) %>%
  st_drop_geometry()

cwppseco <- merge(cwpps, ecoreg, all = T) %>%
  mutate(ecoreg_frac = as.numeric(ecoreg_area / jur_area)) %>%
  select(-ecoreg_area) %>%
  group_by(Name) %>%
  unique() %>%
  pivot_wider(names_from = as.character("ecoregion"), values_from = "ecoreg_frac") %>%
  rename(med_forest = "Mediterranean Forests, Woodlands & Scrub", conifer = "Temperate Conifer Forests", grass_shrub = "Temperate Grasslands, Savannas & Shrublands", deserts_shrub = "Deserts & Xeric Shrublands") %>%
  st_drop_geometry()
all <- merge(cwpps, cwpps_ecoreg)
all <- merge(all, cwppseco)

# also have lc data
#land cover type/conus
lc <- rast("data/int_data/landfire/evt2016ca.tif")
lc_key <- read_csv("data/int_data/landfire/evt2016ca.csv")
#conus key, keep just generalized lc type
# st_crs(conus) == st_crs(cwpps) #TRUE
#summarize categorical raster -- get conus values and coverage fractions for conus pixel in each cwpp, then get fraction of each lc type by dividing coverage fraction by all cov fractions in poly, then sum per lc value https://www.dante-project.org/vignettes/exactextractr-cat
lc <- exact_extract(lc, cwpps, fun = function(df) {
  df %>%
    mutate(frac_perpoly = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(Name, value) %>% 
    summarize(frac_lc = sum(frac_perpoly)) %>%
    ungroup()
}, summarize_df = TRUE, include_cols = 'Name', progress = FALSE)
# length(unique(lc$Name)) #182 

#join to key and get lc frac. dupe lc because this is coarse phys desc., so summarize those
all_lc <- lc %>%
  inner_join(lc_key, by = 'value') %>%
  group_by(Name, lc) %>%
  summarize(sum_lc = sum(frac_lc, na.rm = T)) %>%
  pivot_wider(names_from = lc, values_from = sum_lc) %>% 
  ungroup() %>%
  #combine all developed intensities
  mutate(Develop = rowSums(select(., starts_with("Develop")), na.rm = T),
         Conifer = rowSums(select(., starts_with("Conifer")), na.rm = T)) %>%
  select(Name, Conifer, Develop, Grassland, Hardwood, Shrubland)
alleco <- merge(all, all_lc)
saveRDS(alleco, "data/int_data/cwpps_nosvi.RDS")
cwpps <- readRDS("data/int_data/cwpps_nosvi.RDS")

#SVI for 2013 ** ADD OTHER YEARS**
#requires census api key, doesn't include geometry, and is only for 2012-2021
svi <- find_svi(year = 2013, state = 'CA', geography = "tract")
# SVI documentation: 
# Socioeconomic Status – RPL_THEME1
# Household Characteristics – RPL_THEME2
# Racial & Ethnic Minority Status – RPL_THEME3
# Housing Type & Transportation – RPL_THEME4
#get tract geom
tracts <- tracts("CA", cb = TRUE, year = 2013)
svi_census <- merge(tracts[c('GEOID', 'geometry')], svi, by = 'GEOID') 
##get census tracts in cwpp polys
st_crs(svi_census) == st_crs(cwpps)
st_is_valid(svi_census, reason = T) 
svi_census <- st_make_valid(svi_census)
svi_census <- st_transform(svi_census, crs = st_crs(cwpps))
#overallSVI
svi <- st_intersection(svi_census, cwpps) %>%
  group_by(Name) %>%
  summarize_at(vars(RPL_themes), ~mean(., na.rm=T)) %>%
  rename(svi2013 = RPL_themes) %>%
  st_drop_geometry()
#not sure if mean is the best way to handle percentiles ???
#get census tracts intersecting with cwpp polygons and average the theme values for each cwpp
# themes <- st_intersection(svi_census, cwpp_phys) %>%
#   group_by(Name) %>%
#   summarize(across(c(RPL_theme1, RPL_theme2, RPL_theme3, RPL_theme4), ~mean(., na.rm = TRUE))) %>%
#   ungroup() %>%
#   st_drop_geometry() 

all_vars <- merge(cwpps, svi, all = T)
st_write(all_vars, "data/int_data/covars/cwpp_all.shp", append = F) # makes variable names crazy, fix later
