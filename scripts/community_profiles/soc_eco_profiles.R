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
  mutate(intersect_area = st_area(.))%>% # create new column with shape area of intersecting polys 
  select(Name, OWN_LEVEL, intersect_area) %>%
  group_by(Name, OWN_LEVEL) %>%
  summarize(owner_area = sum(intersect_area)) %>%
  st_drop_geometry()

#get area of cwpp jur
cwpp_exp$jur_area <- st_area(cwpp_exp)

own_jur <- merge(int2, cwpp_exp, by = 'Name') %>%
  mutate(own_percent = round(((owner_area / jur_area)*100), 3)) %>%
  select(Name, OWN_LEVEL, Jur, m_exp, own_percent, geometry) %>%
  group_by(Name) %>%
  mutate(public_percent = sum(own_percent)) 

all_eco <- merge(own_jur, lc, by = 'Name')
eco_sf <- st_write(all_eco, "data/int_data/biophys_vars.shp")

#social
cales <- st_read("data/raw_data/calenviroscreen/CES4 Final Shapefile.shp")
st_crs(cales) == st_crs(cwpps)
st_is_valid(cales, reason = T)
cales <- st_make_valid(cales)
cales <- st_transform(cales, crs = st_crs(cwpps))

#does not account for area, assigns entire tract value to cwpp even if not all of tract is inside of cwpp jur
cales_cwpp <- st_intersection(cwpp_exp, cales) %>%
  select(Name, County, CIscore, CIscoreP) %>%
  group_by(Name) %>%
  summarize(avg_CI = mean(CIscore)) %>%
  st_drop_geometry()

eco_soc <- merge(all_eco, cales_cwpp, by = 'Name')

#census
#download acs data
census_api_key("77b4aae9e134b43d2edeb8aabb1ceb4713fe8a2a", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
v20 <- load_variables(2020, "acs5", cache = TRUE)

#these are not right 
acs_vars <- c(total_pop = 'B01003_001', 
              below_pov = 'B17001_002', 
              unemp = 'B23025_005', 
              med_inc = 'B19013_001', 
              mob_home = 'B25024_010', 
              # mult_hous = , 
              rent_units = 'B25011_026', 
              bad_eng = 'B06007_005', 
              no_hs = 'B15003_017')

ca_census <- get_acs(geography = "tract", year = 2020, state = 'CA',
                        variables = acs_vars, summary_var = "B01003_001", geometry = TRUE)
st_crs(ca_census) == st_crs(cwpp_exp)
st_is_valid(ca_census, reason = T) 
ca_census <- st_make_valid(ca_census)
ca_census <- st_transform(ca_census, crs = st_crs(cwpps))
#turn counts into rates
ca_census$rate <- (ca_census$estimate / ca_census$summary_est)
#give med_inc rate as value, since not a rate
ca_census$rate[ca_census$variable == 'med_inc'] <- ca_census$estimate[ca_census$variable == 'med_inc']
ca_census$census_area <- st_area(ca_census)

#only census tracts intersecting CWPPs, then multiply rate by % of census tract within cwpp boundary to get weighted rate of each variable
census_cwpp <- st_intersection(cwpp_exp, ca_census) %>%
  mutate(overlap_area = st_area(.)) %>%
  mutate(overlap_frac = (overlap_area / census_area)) %>%
  mutate(weighted_rate = overlap_frac *rate) 

# nans <- census_cwpp[which(is.nan(census_cwpp$weighted_rate)),]
#getting bc estimates are 0, maybe no one lives here? check later
#for now assign NA
census_cwpp$weighted_rate[is.nan(census_cwpp$weighted_rate)] <- NA
#average for CWPP 
cwpp_avgs <- census_cwpp %>%
  group_by(Name, variable) %>%
  summarize(cwpp_avg_pct = mean(weighted_rate, na.rm=T)) %>%
  st_drop_geometry()

cwpp_avg_soc <- pivot_wider(cwpp_avgs, names_from = variable, values_from = cwpp_avg_pct) 

#these values don't seem right
#i think what i should do is for county CWPPs, get census county value 
#and for others, ?
all <- merge(census_cwpp, all_eco, by = 'Name')
  
  

eco_cwpp <- st_read("data/int_data/biophys_vars.shp")
#CDC SVI
library(findSVI)
#requires census api key, doesn't include geometry
svi <- find_svi(year = 2020, state = 'CA', geography = "tract")
# SVI documentation: 
# Socioeconomic Status – RPL_THEME1
# Household Characteristics – RPL_THEME2
# Racial & Ethnic Minority Status – RPL_THEME3
# Housing Type & Transportation – RPL_THEME4
library(tigris) #get tract geom
tracts <- tracts("CA", cb = TRUE, year = 2020)
svi_census <- merge(tracts[c('GEOID', 'geometry')], svi, by = 'GEOID') 
##get census tracts in cwpp polys
st_crs(svi_census) == st_crs(eco_cwpp)
st_is_valid(svi_census, reason = T) 
svi_census <- st_make_valid(svi_census)
svi_census <- st_transform(svi_census, crs = st_crs(eco_cwpp))
svi_census_cwpp <- st_intersection(svi_census, eco_cwpp) %>%
  select('')
plot(svi_census_cwpp['RPL_theme1'])


sc <- svi_census_cwpp %>%
  select(Name == 'CA_SantaClara_CNTY')