# Community Colleges Programs -- County level FCAs
# packages
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(tidycensus)
library(osrm)
library(catchment)
library(RPostgreSQL)
library(reshape2)

######################
# VA COUNTIES GEOIDS
######################

engin_rel_lonlat <- read_csv("program_data/engin_rel_lonlat.csv")
engin_lonlat <- read_csv("program_data/engin_lonlat.csv")
comp_lonlat <- read_csv("program_data/comp_lonlat.csv")

# get US counties shapefile
counties <- st_as_sf(counties(state="VA"))

# lon and lat to geo-points
geopts_engin_rel <- engin_rel_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

geopts_engin <- engin_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

geopts_comp <- comp_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

# indeces of counties which contain a geopoint
inds_engin_rel <- st_within(geopts_engin_rel$geometry, counties$geometry, sparse=T)
inds_engin <- st_within(geopts_engin$geometry, counties$geometry, sparse=T)
inds_comp <- st_within(geopts_comp$geometry, counties$geometry, sparse=T)

counties_list_engin_rel <- c()
counties_list_engin <- c()
counties_list_comp <- c()

for (i in inds_engin_rel){
  if (identical(counties$NAME[i],character(0))){
    counties_list_engin_rel<- append(counties_list_engin_rel, NA)}
  else{
    counties_list_engin_rel<- append(counties_list_engin_rel, counties$GEOID[i])}
}

for (i in inds_engin){
  if (identical(counties$NAME[i],character(0))){
    counties_list_engin<- append(counties_list_engin, NA)}
  else{
    counties_list_engin<- append(counties_list_engin, counties$GEOID[i])}
}

for (i in inds_comp){
  if (identical(counties$NAME[i],character(0))){
    counties_list_comp<- append(counties_list_comp, NA)}
  else{
    counties_list_comp<- append(counties_list_comp, counties$GEOID[i])}
}

engin_rel_lonlat['geoid'] <- counties_list_engin_rel
engin_lonlat['geoid'] <- counties_list_engin
comp_lonlat['geoid'] <- counties_list_comp

##########################
# SUPPLY
##########################

# create new supply
supply_engin_rel <- data.frame(engin_rel_lonlat$longitude, engin_rel_lonlat$latitude,
                               engin_rel_lonlat$geoid, engin_rel_lonlat$tot_enrol, engin_rel_lonlat$...1)
colnames(supply_engin_rel) <- c("lon", "lat", "GEOID", "capacity", "id")

supply_engin <- data.frame(engin_lonlat$longitude, engin_lonlat$latitude,
                           engin_lonlat$geoid, engin_lonlat$tot_enrol, engin_lonlat$...1)
colnames(supply_engin) <- c("lon", "lat", "GEOID", "capacity", "id")

supply_comp <- data.frame(comp_lonlat$longitude, comp_lonlat$latitude,
                          comp_lonlat$geoid, comp_lonlat$tot_enrol, comp_lonlat$...1)
colnames(supply_comp) <- c("lon", "lat", "GEOID", "capacity", "id")

#########################
# DEMAND
#########################

# installed census api key
readRenviron("~/.Renviron/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# population under 15 years in NCR
va.ct <- get_acs(geography = "county",
                 year = 2019,
                 variables = c("B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010",
                               "B01001_011",  "B01001_012", "B01001_013",  "B01001_014",  "B01001_015",
                               "B01001_016", "B01001_017", "B01001_018", "B01001_019",  "B01001_020",
                               "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", # male 15 to 85+
                               "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034",
                               "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039",
                               "B01001_040", "B01001_041", "B01001_042", "B01001_043", "B01001_044",
                               "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"), # female 15 to 85+
                 state = c("VA"),
                 survey = "acs5",
                 output = "wide",
                 geometry = TRUE)

va.ct <- va.ct %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  pop_over15 = B01001_006E + B01001_007E + B01001_008E + B01001_009E + B01001_010E +
    B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E + B01001_016E +
    B01001_017E + B01001_018E + B01001_019E + B01001_020E + B01001_021E +  B01001_022E +
    B01001_023E +  B01001_024E + B01001_025E + B01001_030E +B01001_031E + B01001_032E +
    B01001_033E +  B01001_034E + B01001_035E + B01001_036E + B01001_037E + B01001_038E +
    B01001_039E + B01001_040E +  B01001_041E + B01001_042E + B01001_043E + B01001_044E +
    B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E,
  geometry = geometry
)

ct_centroid <- va.ct %>%
  st_centroid()

demand <- ct_centroid %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)',
                                  convert = TRUE)
demand <- as.data.frame(demand)

# drop where the block group coordinates are NaN
demand <- demand[is.na(demand$lon) == FALSE,]
demand <- demand[is.na(demand$lat) == FALSE,]

##################
# DRIVE TIMES
##################

supply_engin$id <- as.character(supply_engin$id)
supply_engin_rel$id <- as.character(supply_engin_rel$id)
supply_comp$id <- as.character(supply_comp$id)

# OSRM travel times from case study
options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "car")
traveltimes_engin <- osrmTable(
  src = demand[, c("GEOID", "lon", "lat")],
  dst = supply_engin[, c("id", "lon", "lat")]
)$duration

traveltimes_engin_rel <- osrmTable(
  src = demand[, c("GEOID", "lon", "lat")],
  dst = supply_engin_rel[, c("id", "lon", "lat")]
)$duration

traveltimes_comp <- osrmTable(
  src = demand[, c("GEOID", "lon", "lat")],
  dst = supply_comp[, c("id", "lon", "lat")]
)$duration

traveltimes_engin[is.na(traveltimes_engin)] <- 0
traveltimes_engin_rel[is.na(traveltimes_engin_rel)] <- 0
traveltimes_comp[is.na(traveltimes_comp)] <- 0

# drive times to 5 nearest STEM programs
# engineering

list_5_top_engin <- apply(traveltimes_engin, 1, function(x) sort(x, decreasing = F)[1:5])
# mean and median value
mean_low_five_engin <- apply(list_5_top_engin, 2, function(x) (mean(x)))
median_low_five_engin <- apply(list_5_top_engin, 2, function(x) (median(x)))

out_df_engin <- data.frame(geoid=names(mean_low_five_engin),
                           mean_drive_time_top5=mean_low_five_engin, row.names=NULL)
med_df_engin <- data.frame(geoid=names(median_low_five_engin),
                           median_drive_time_top5=median_low_five_engin, row.names=NULL)

out_df_engin <- left_join(out_df_engin, med_df_engin, by="geoid")

# engineering related

list_5_top_engin_rel <- apply(traveltimes_engin_rel, 1, function(x) sort(x, decreasing = F)[1:5])
# mean and median value
mean_low_five_engin_rel <- apply(list_5_top_engin_rel, 2, function(x) (mean(x)))
median_low_five_engin_rel <- apply(list_5_top_engin_rel, 2, function(x) (median(x)))

out_df_engin_rel <- data.frame(geoid=names(mean_low_five_engin_rel),
                           mean_drive_time_top5=mean_low_five_engin_rel, row.names=NULL)
med_df_engin_rel <- data.frame(geoid=names(median_low_five_engin_rel),
                           median_drive_time_top5=median_low_five_engin_rel, row.names=NULL)

out_df_engin_rel <- left_join(out_df_engin_rel, med_df_engin_rel, by="geoid")

# computer science

list_5_top_comp <- apply(traveltimes_comp, 1, function(x) sort(x, decreasing = F)[1:5])
# mean and median value
mean_low_five_comp <- apply(list_5_top_comp, 2, function(x) (mean(x)))
median_low_five_comp <- apply(list_5_top_comp, 2, function(x) (median(x)))

out_df_comp <- data.frame(geoid=names(mean_low_five_comp),
                           mean_drive_time_top5=mean_low_five_comp, row.names=NULL)
med_df_comp <- data.frame(geoid=names(median_low_five_comp),
                           median_drive_time_top5=median_low_five_comp, row.names=NULL)

out_df_comp <- left_join(out_df_comp, med_df_comp, by="geoid")

# format for database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
dbDisconnect(con)

counties_names <- geo_names %>% filter(region_type=="county")

out_df_engin <- left_join(out_df_engin, counties_names, by=c("geoid"))
out_df_engin_rel <- left_join(out_df_engin_rel, counties_names, by=c("geoid"))
out_df_comp <- left_join(out_df_comp, counties_names, by=c("geoid"))

out_df_engin["year"] <- 2019
out_df_engin_rel["year"] <- 2019
out_df_comp["year"] <- 2019

# long format
df_long_engin <- melt(out_df_engin,
                      id.vars=c("geoid", "region_type", "region_name", "year"),
                      variable.name="measure",
                      value.name="value"
)

df_long_engin_rel <- melt(out_df_engin_rel,
                      id.vars=c("geoid", "region_type", "region_name", "year"),
                      variable.name="measure",
                      value.name="value"
)

df_long_comp <- melt(out_df_comp,
                      id.vars=c("geoid", "region_type", "region_name", "year"),
                      variable.name="measure",
                      value.name="value"
)

df_long_engin['measure_type'] = ""
indx1 <- grepl('mean', df_long_engin$measure)
indx2 <- grepl('median', df_long_engin$measure)

df_long_engin$measure_type[indx1] <- 'mean'
df_long_engin$measure_type[indx2] <- 'median'

df_long_engin_rel['measure_type'] = ""
indx1 <- grepl('mean', df_long_engin_rel$measure)
indx2 <- grepl('median', df_long_engin_rel$measure)

df_long_engin_rel$measure_type[indx1] <- 'mean'
df_long_engin_rel$measure_type[indx2] <- 'median'

df_long_comp['measure_type'] = ""
indx1 <- grepl('mean', df_long_comp$measure)
indx2 <- grepl('median', df_long_comp$measure)

df_long_comp$measure_type[indx1] <- 'mean'
df_long_comp$measure_type[indx2] <- 'median'

# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_ct_osrm_2019_drive_times_nearest_engineering_program"),
             df_long_engin,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_ct_osrm_2019_drive_times_nearest_engineering_related_program"),
             df_long_engin_rel,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_ct_osrm_2019_drive_times_nearest_computer_science_program"),
             df_long_comp,  row.names = F)

# dbRemoveTable(con, c("dc_education_training", "va_ct_osrm_2019_drive_times_nearest_engineering_program"))
# dbRemoveTable(con, c("dc_education_training", "va_ct_osrm_2019_drive_times_nearest_engineering_related_program"))
# dbRemoveTable(con, c("dc_education_training", "va_ct_osrm_2019_drive_times_nearest_computer_science_program"))

dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_osrm_2019_drive_times_nearest_engineering_program
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_osrm_2019_drive_times_nearest_engineering_related_program
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_osrm_2019_drive_times_nearest_computer_science_program
                    OWNER TO data_commons")

dbDisconnect(con)

##################
# FCA
##################
step_weights_30 <- list(c(30, .22), c(20, .68), c(10, 1))
step_weights_60 <- list(c(60, .042), c(30, .377), c(20, .704), c(10, .962))

demand_engin <- demand
demand_engin_rel <- demand
demand_comp <- demand

# engineering programs
demand_engin['e2sfca'] <- catchment_ratio(
  demand_engin, supply_engin,
  cost=traveltimes_engin,
  step_weights_30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_engin['fca2s'] <- catchment_ratio(
  demand_engin, supply_engin,
  cost=traveltimes_engin,
  30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_engin['fca3s'] <- catchment_ratio(
  demand_engin, supply_engin,
  cost=traveltimes_engin,
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# engineering-related programs
demand_engin_rel['e2sfca'] <- catchment_ratio(
  demand_engin_rel, supply_engin_rel,
  cost=traveltimes_engin_rel,
  step_weights_30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_engin_rel['fca2s'] <- catchment_ratio(
  demand_engin_rel, supply_engin_rel,
  cost=traveltimes_engin_rel,
  30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_engin_rel['fca3s'] <- catchment_ratio(
  demand_engin_rel, supply_engin_rel,
  cost=traveltimes_engin_rel,
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# computer science programs
demand_comp['e2sfca'] <- catchment_ratio(
  demand_comp, supply_comp,
  cost=traveltimes_comp,
  step_weights_30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_comp['fca2s'] <- catchment_ratio(
  demand_comp, supply_comp,
  cost=traveltimes_comp,
  30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_comp['fca3s'] <- catchment_ratio(
  demand_comp, supply_comp,
  cost=traveltimes_comp,
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# DB formatting
# add total college capacity
inter_engin <- left_join(demand_engin, supply_engin[,c("GEOID", "capacity")], by = "GEOID")
inter_engin_rel <- left_join(demand_engin_rel, supply_engin_rel[,c("GEOID", "capacity")], by = "GEOID")
inter_comp <- left_join(demand_comp, supply_comp[,c("GEOID", "capacity")], by = "GEOID")

inter_engin$capacity <- as.numeric(as.character(inter_engin$capacity ))
inter_engin_rel$capacity <- as.numeric(as.character(inter_engin_rel$capacity ))
inter_comp$capacity <- as.numeric(as.character(inter_comp$capacity ))

# calculate the tot capacity by GEOID
df2_engin <- inter_engin %>%  group_by(GEOID) %>%
  summarise(capacity = sum(capacity)) %>%
  as.data.frame()

df2_engin_rel <- inter_engin_rel %>%  group_by(GEOID) %>%
  summarise(capacity = sum(capacity)) %>%
  as.data.frame()

df2_comp <- inter_comp %>%  group_by(GEOID) %>%
  summarise(capacity = sum(capacity)) %>%
  as.data.frame()

df3_engin <- left_join(demand_engin, df2_engin, by=("GEOID"))
df3_engin_rel <- left_join(demand_engin_rel, df2_engin_rel, by=("GEOID"))
df3_comp <- left_join(demand_comp, df2_comp, by=("GEOID"))

# add normalized fca capacities values
df3_engin$norm_fca3s <- (df3_engin$fca3s -
                           min(df3_engin$fca3s, na.rm=TRUE)) /
  (max(df3_engin$fca3s, na.rm=TRUE)
   -min(df3_engin$fca3s, na.rm=TRUE))* 100

df3_engin_rel$norm_fca3s <- (df3_engin_rel$fca3s -
                           min(df3_engin_rel$fca3s, na.rm=TRUE)) /
  (max(df3_engin_rel$fca3s, na.rm=TRUE)
   -min(df3_engin_rel$fca3s, na.rm=TRUE))* 100

df3_comp$norm_fca3s <- (df3_comp$fca3s -
                           min(df3_comp$fca3s, na.rm=TRUE)) /
  (max(df3_comp$fca3s, na.rm=TRUE)
   -min(df3_comp$fca3s, na.rm=TRUE))* 100

# add normalized fca capacities values
df3_engin$norm_fca2s <- (df3_engin$fca2s -
                           min(df3_engin$fca2s, na.rm=TRUE)) /
  (max(df3_engin$fca2s, na.rm=TRUE)
   -min(df3_engin$fca2s, na.rm=TRUE))* 100

df3_engin_rel$norm_fca2s <- (df3_engin_rel$fca2s -
                           min(df3_engin_rel$fca2s, na.rm=TRUE)) /
  (max(df3_engin_rel$fca2s, na.rm=TRUE)
   -min(df3_engin_rel$fca2s, na.rm=TRUE))* 100

df3_comp$norm_fca2s <- (df3_comp$fca2s -
                           min(df3_comp$fca2s, na.rm=TRUE)) /
  (max(df3_comp$fca2s, na.rm=TRUE)
   -min(df3_comp$fca2s, na.rm=TRUE))* 100

# add normalized fca capacities values
df3_engin$norm_efca2s <- (df3_engin$e2sfca -
                            min(df3_engin$e2sfca, na.rm=TRUE)) /
  (max(df3_engin$e2sfca, na.rm=TRUE)
   -min(df3_engin$e2sfca, na.rm=TRUE))* 100

df3_engin_rel$norm_efca2s <- (df3_engin_rel$e2sfca -
                            min(df3_engin_rel$e2sfca, na.rm=TRUE)) /
  (max(df3_engin_rel$e2sfca, na.rm=TRUE)
   -min(df3_engin_rel$e2sfca, na.rm=TRUE))* 100

df3_comp$norm_efca2s <- (df3_comp$e2sfca -
                            min(df3_comp$e2sfca, na.rm=TRUE)) /
  (max(df3_comp$e2sfca, na.rm=TRUE)
   -min(df3_comp$e2sfca, na.rm=TRUE))* 100

# select columns to keep
df3_engin <- df3_engin %>% select(c("GEOID", "e2sfca", "fca2s",
                                    "fca3s", "capacity", "norm_fca3s",
                                    "norm_fca2s", "norm_efca2s"))

df3_engin_rel <- df3_engin_rel %>% select(c("GEOID", "e2sfca", "fca2s",
                                    "fca3s", "capacity", "norm_fca3s",
                                    "norm_fca2s", "norm_efca2s"))

df3_comp <- df3_comp %>% select(c("GEOID", "e2sfca", "fca2s",
                                    "fca3s", "capacity", "norm_fca3s",
                                    "norm_fca2s", "norm_efca2s"))

# add region names
out_df_engin <- left_join(df3_engin, counties_names, by=c("GEOID"="geoid"))
out_df_engin_rel <- left_join(df3_engin, counties_names, by=c("GEOID"="geoid"))
out_df_comp <- left_join(df3_comp, counties_names, by=c("GEOID"="geoid"))

out_df_engin["year"] <- 2019
out_df_engin_rel["year"] <- 2019
out_df_comp["year"] <- 2019

names(out_df_engin)[1] <- "geoid"
names(out_df_engin_rel)[1] <- "geoid"
names(out_df_comp)[1] <- "geoid"

# long format
out_df_long_engin <- melt(out_df_engin,
                          id.vars=c("geoid", "region_type", "region_name", "year"),
                          variable.name="measure",
                          value.name="value"
)

out_df_long_engin_rel <- melt(out_df_engin_rel,
                          id.vars=c("geoid", "region_type", "region_name", "year"),
                          variable.name="measure",
                          value.name="value"
)

out_df_long_comp <- melt(out_df_comp,
                          id.vars=c("geoid", "region_type", "region_name", "year"),
                          variable.name="measure",
                          value.name="value"
)

out_df_long_engin['measure_type'] = ""
indx1 <- grepl('fca', out_df_long_engin$measure)
indx2 <- grepl('norm', out_df_long_engin$measure)
indx3 <- grepl('capacity', out_df_long_engin$measure)
out_df_long_engin$measure_type[indx1] <- 'index'
out_df_long_engin$measure_type[indx2] <- 'normalized index'
out_df_long_engin$measure_type[indx3] <- 'count'

out_df_long_engin_rel['measure_type'] = ""
indx1 <- grepl('fca', out_df_long_engin_rel$measure)
indx2 <- grepl('norm', out_df_long_engin_rel$measure)
indx3 <- grepl('capacity', out_df_long_engin_rel$measure)
out_df_long_engin_rel$measure_type[indx1] <- 'index'
out_df_long_engin_rel$measure_type[indx2] <- 'normalized index'
out_df_long_engin_rel$measure_type[indx3] <- 'count'

out_df_long_comp['measure_type'] = ""
indx1 <- grepl('fca', out_df_long_comp$measure)
indx2 <- grepl('norm', out_df_long_comp$measure)
indx3 <- grepl('capacity', out_df_long_comp$measure)
out_df_long_comp$measure_type[indx1] <- 'index'
out_df_long_comp$measure_type[indx2] <- 'normalized index'
out_df_long_comp$measure_type[indx3] <- 'count'

# replace NAs in capacity with 0
out_df_long_engin$value[is.na(out_df_long_engin$value)]<-0
out_df_long_engin_rel$value[is.na(out_df_long_engin_rel$value)]<-0
out_df_long_comp$value[is.na(out_df_long_comp$value)]<-0

# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_ct_nces_2020_community_college_engineering_related_fca"),
             out_df_long_engin_rel,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_ct_nces_2020_community_college_engineering_fca"),
             out_df_long_engin,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_ct_nces_2020_community_college_computer_sciences_fca"),
             out_df_long_comp,  row.names = F)

dbRemoveTable(con, c("dc_education_training", "va_ct_nces_2020_community_college_engineering_related_fca"))
dbRemoveTable(con, c("dc_education_training", "va_ct_nces_2020_community_college_engineering_fca"))
dbRemoveTable(con, c("dc_education_training", "va_ct_nces_2020_community_college_computer_sciences_fca"))

dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_nces_2020_community_college_engineering_related_fca
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_nces_2020_community_college_engineering_fca
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_nces_2020_community_college_computer_sciences_fca
                    OWNER TO data_commons")

dbDisconnect(con)


