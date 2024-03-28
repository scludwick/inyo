packages <- c('terra', 'sf', 'dplyr', 'readr', 'exactextractr', 'foreign', 'tidycensus', 'tigris', 'findSVI', 'tidyr')
lapply(packages, library, character.only=TRUE)

#cwpp polygon shapefile of 182 CWPP boundaries
cwpps <- st_read("data/int_data/cwpps_transformed.shp") 
############### risk data
exp <- rast("large_data/fire_risk_comm_CA/Exposure_CA.tif")
whp <- rast("large_data/fire_risk_comm_CA/WHP_CA.tif")
rps <- rast("large_data/fire_risk_comm_CA/RPS_CA.tif")
max(whp)
st_crs(exp) == st_crs(cwpps) #TRUE for all 
#extract mean exposure, risk to potential structures, and wildfire hazard potential per cwpp boundary
cwpps$mean_exp <- exact_extract(exp, cwpps$geometry, fun = 'mean')
cwpps$mean_whp <- exact_extract(whp, cwpps$geometry, fun = 'mean')
cwpps$mean_rps <- exact_extract(rps, cwpps$geometry, fun = 'mean')
cwpps <- cwpps %>%
  select(Name, mean_exp, mean_whp, mean_rps)

##############land cover type/conus
conus <- rast("data/int_data/landfire/evt2016ca.tif")
conus_key <- read.csv("data/int_data/landfire/evt2016ca.csv")
#conus key, keep just generalized lc type
# st_crs(conus) == st_crs(cwpps) #TRUE
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
  pivot_wider(names_from = lc, values_from = sum_lc) %>% 
  ungroup() %>%
  #trying it out condensing and reducing categories
  mutate(allDev = select(., starts_with("Develop")) %>% 
           rowSums(na.rm = T)) %>%
  select(c(`Name`, `Conifer`, `Shrubland`, `Hardwood`, `Grassland`, `allDev`))


############### land ownership
ownership <- st_read("data/int_data/ownership_transformed.shp")
# st_crs(ownership) == st_crs(cwpps) #TRUE

#get area of each landowner type within cwpp boundaries
own <- st_intersection(cwpps, ownership) %>%
  mutate(intersect_area = st_area(.))%>% # create new column with shape area of intersecting polys
  select(Name, OWN_LEVEL, intersect_area) %>%
  group_by(Name, OWN_LEVEL) %>%
  summarize(owner_area = sum(intersect_area)) %>%
  st_drop_geometry()
# length(unique(own$Name)) #177 plans only
# missing <- cwpps[!cwpps$Name %in% own$Name,]
# missing # all comm plans, prob all private??

#get area of ownership per area of cwpp
cwpps$jur_area <- st_area(cwpps)
own_jur <- merge(cwpps, own, by = 'Name', all = T) %>%
  mutate(own_frac = owner_area / jur_area) %>%
  select(Name, mean_exp, mean_rps, mean_whp, OWN_LEVEL, own_frac) %>%
  group_by(Name) %>%
  filter(!OWN_LEVEL %in% c("Tribal", "Non Profit")) %>%
  mutate(all_public = sum(own_frac)) %>% 
  select(Name, mean_exp, mean_rps, mean_whp, all_public) 

#merge 
cwpp_phys <- merge(own_jur, lc_sel, by = 'Name', all = T)
cwpp_phys <- st_write(cwpp_phys, "data/int_data/cwpp_vars/cwpp_phys.shp", append = F)


# need per year !!!!!!!!!!!!!!! 
# add SVI percentiles 

#SVI 
#requires census api key, doesn't include geometry
svi <- find_svi(year = 2015, state = 'CA', geography = "tract")
# SVI documentation: 
# Socioeconomic Status – RPL_THEME1
# Household Characteristics – RPL_THEME2
# Racial & Ethnic Minority Status – RPL_THEME3
# Housing Type & Transportation – RPL_THEME4
library(tigris) #get tract geom
tracts <- tracts("CA", cb = TRUE, year = 2015)
svi_census <- merge(tracts[c('GEOID', 'geometry')], svi, by = 'GEOID') 
##get census tracts in cwpp polys
st_crs(svi_census) == st_crs(cwpp_phys)
st_is_valid(svi_census, reason = T) 
svi_census <- st_make_valid(svi_census)
svi_census <- st_transform(svi_census, crs = st_crs(cwpp_phys))
#not sure if mean is the best way to handle percentiles ???
#get census tracts intersecting with cwpp polygons and average the theme values for each cwpp
themes <- st_intersection(svi_census, cwpp_phys) %>%
  group_by(Name) %>%
  summarize(across(c(RPL_theme1, RPL_theme2, RPL_theme3, RPL_theme4), ~mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  st_drop_geometry()

all_vars <- merge(cwpp_phys, themes, by = 'Name')
all_write <- st_write(all_vars, "data/int_data/cwpp_vars/all_vars.shp", append = F)
all_analysis <- all_vars[is.na(all_vars)] <- 0
all_analysis <- st_drop_geometry(all_vars) 





