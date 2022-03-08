# Tract level FCAs for STEM programs in comunity colleges

# packages
library(dplyr)
library(data.table)
library(RPostgreSQL)
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(matrixStats)
library(SpatialAcc)
library(tidygeocoder)
library(osrm)

# load in data
engin_rel_lonlat <- read_csv('/program_data/engin_rel_lonlat.csv')
comp_lonlat <- read_csv('/program_data/comp_lonlat.csv')
engin_lonlat <- read_csv('/program_data/engin_lonlat.csv')

########### GEOIDS ###########
# get census tract shapefile
tracts <- st_as_sf(tracts(state="VA"))

# lon and lat to geo-points
geopts_engin_rel <- engin_rel_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

geopts_engin <- engin_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

geopts_comp <- comp_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

# indeces of counties which contain a geopoint
inds_engin_rel <- st_within(geopts_engin_rel$geometry, tracts$geometry, sparse=T)
inds_engin <- st_within(geopts_engin$geometry, tracts$geometry, sparse=T)
inds_comp <- st_within(geopts_comp$geometry, tracts$geometry, sparse=T)

tract_list_engin_rel <- c()
tract_list_engin <- c()
tract_list_comp <- c()

for (i in inds_engin_rel){
  if (identical(tracts$NAME[i],character(0))){
    tract_list_engin_rel<- append(tract_list_engin_rel, NA)}
  else{
    tract_list_engin_rel<- append(tract_list_engin_rel, tracts$GEOID[i])}
}

for (i in inds_engin){
  if (identical(tracts$NAME[i],character(0))){
    tract_list_engin<- append(tract_list_engin, NA)}
  else{
    tract_list_engin<- append(tract_list_engin, tracts$GEOID[i])}
}

for (i in inds_comp){
  if (identical(tracts$NAME[i],character(0))){
    tract_list_comp<- append(tract_list_comp, NA)}
  else{
    tract_list_comp<- append(tract_list_comp, tracts$GEOID[i])}
}

engin_rel_lonlat['geoid'] <- tract_list_engin_rel
engin_lonlat['geoid'] <- tract_list_engin
comp_lonlat['geoid'] <- tract_list_comp

################ SUPPLY #################
# create new supply
supply_engin_rel <- data.frame(engin_rel_lonlat$longitude, engin_rel_lonlat$latitude,
                               engin_rel_lonlat$geoid, engin_rel_lonlat$tot_enrol)
colnames(supply_engin_rel) <- c("lon", "lat", "GEOID", "capacity")

supply_engin <- data.frame(engin_lonlat$longitude, engin_lonlat$latitude,
                           engin_lonlat$geoid, engin_lonlat$tot_enrol)
colnames(supply_engin) <- c("lon", "lat", "GEOID", "capacity")

supply_comp <- data.frame(comp_lonlat$longitude, comp_lonlat$latitude,
                          comp_lonlat$geoid, comp_lonlat$tot_enrol)
colnames(supply_comp) <- c("lon", "lat", "GEOID", "capacity")

write_csv(supply_engin_rel, '/program_data/engin_rel_tr_supply.csv')
write_csv(supply_engin, '/program_data/engin_tr_supply.csv')
write_csv(supply_comp, '/program_data/comp_tr_supply.csv')

###################### DEMAND
# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# tracts in VA
va.tracts <- get_acs(geography = "tract",
                       year = 2019,
                       variables = c(pop = "B23025_001"), # population over 16
                       state = "VA",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE)
ct_centroid <- va.tracts %>%
  st_centroid()

demand <- ct_centroid %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)',
                                  convert = TRUE)

demand <- as.data.frame(demand)
write_csv(demand, '/program_data/prog_tr_demand.csv')

######################################################################################
#################### DRIVE TIMES (one by one)
# read in supply data
supply <- read_csv("program_data/engin_rel_tr_supply.csv")
#supply <- read_csv("program_data/engin_tr_supply.csv")
#supply <- read_csv("program_data/comp_tr_supply.csv")
supply$GEOID <- as.character(supply$GEOID)

# options for OSRM
options(osrm.server = Sys.getenv("OSRM_SERVER"), osrm.profile = "car")

start.time <- Sys.time() # using this to see run-time
all_data <- matrix(, nrow = 0, ncol = nrow(supply))

# maximum number of requests that OSRM can handle at a time
max.size <- 100

# based on the max.size
n <- floor(max.size / nrow(supply))
chunks <- ceiling((nrow(demand))/n)

for (i in 1 : chunks)
{
  # if not at the final chunk
  if (i != chunks)
  {
    matrix <- osrmTable(src = demand[(1 + n * (i - 1)):(n * i), c("GEOID", "lon", "lat")],
                        dst = supply[, c("GEOID", "lon", "lat")])$durations
  }
  # if at final chunk, only go until final row
  else
  {
    #matrix <- osrmTable(src = demand[(1 + n * (i - 1)):nrow(demand), c("geoid_bg", "closest_property_lon", "closest_property_lat")],
    #                    dst = new_supply[, c("GEOID", "lon", "lat")])$durations

    matrix <- osrmTable(src = demand[(1 + n * (i - 1)):nrow(demand), c("GEOID", "lon", "lat")],
                        dst = supply[, c("GEOID", "lon", "lat")])$durations
  }
  # show percentage completion
  if (i == ceiling(chunks / 4)) {print( "25%" )}
  if (i == ceiling(chunks / 2)) {print( "50%" )}
  if (i == ceiling(3 * chunks / 4)) {print( "75%" )}
  all_data <- rbind(all_data, matrix)
}

# convert data to times dataframe with origin, dest,
# and cost columns (needed for floating catchment areas)

colnames(all_data) <- supply$GEOID
times <- as.data.frame(as.table(all_data))
colnames(times) <- c("origin", "dest", "cost")
write_csv(times, '/program_data/engin_rel_tr_drive_times.csv')

# ################# PLOT
# # read in access file
# access <- read.csv("/program_data/3sfca_engin_rel_tr.csv")
# #access <- read.csv("/program_data/3sfca_engin_tr.csv")
# #access <- read.csv("/program_data/3sfca_comp_tr.csv")
# access$GEOID <- as.character(access$GEOID)
#
# # Reproject
# va.tr.utm <- st_transform(va.tracts, crs = "+proj=longlat +datum=WGS84")
# va.tr.utm <- va.tr.utm[!st_is_empty(va.tr.utm), ]
#
# va_data <- left_join(va.tr.utm, access, by = "GEOID")
#
# tm_shape(va_data, unit = "mi") +
#   tm_polygons(col = 'X3sfca_capacity',
#               style = 'fisher',
#               palette = 'Blues',
#               border.alpha = 0,
#               title = 'Engineering Related
# (2 year Community College)',
#               breaks = c(0, 1.25, 2.5, 5, 10, 20)) +
#   tm_scale_bar(position = c('left', 'top')) +
#   tm_layout(main.title = '3-Step FCA',
#             main.title.size = 0.95, frame = F,
#             legend.outside = T, legend.outside.position = 'right')
###################################################
# GO TO PYTHON NOTEBOOK TO CALCULATE ACCESS SCORES
###################################################

#################### ADD TO DATABASE

# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

tables_dc_com <- dbGetQuery(con, "SELECT * FROM information_schema.tables
WHERE table_schema = 'dc_common'")

tracts <- dbGetQuery(con, "SELECT * FROM dc_common.va_tr_sdad_2021_virginia_tract_geoids")

dbDisconnect(con)

# read in access file
#fca <- read.csv("/program_data/3sfca_engin_rel_tr.csv")
#fca <- read.csv("/program_data/3sfca_engin_tr.csv")
fca <- read.csv("/program_data/3sfca_comp_tr.csv")
fca$GEOID <- as.character(fca$GEOID)

# read in supply for capacities
#supply <- read_csv("program_data/engin_rel_tr_supply.csv")
#supply <- read_csv("program_data/engin_tr_supply.csv")
supply <- read_csv("program_data/comp_tr_supply.csv")
supply$GEOID <- as.character(supply$GEOID)

# add total college capacity
inter_df <- left_join(fca, supply[,c("GEOID", "capacity")], by = "GEOID")

# calculate the tot capacity by GEOID
df2 <- inter_df %>%  group_by(GEOID) %>%
  dplyr::summarise(capacity = sum(capacity)) %>%
  as.data.frame()
df3 <- left_join(fca, df2, by=("GEOID"))

# add normalized fca capacities values
df3$norm_3sfca <- (df3$X3sfca_capacity -
                     min(df3$X3sfca_capacity, na.rm=TRUE)) /
  (max(df3$X3sfca_capacity, na.rm=TRUE)
   -min(df3$X3sfca_capacity, na.rm=TRUE))* 100

# add normalized fca capacities values
df3$norm_2sfca <- (df3$X2sfca30_capacity -
                     min(df3$X2sfca30_capacity, na.rm=TRUE)) /
  (max(df3$X2sfca30_capacity, na.rm=TRUE)
   -min(df3$X2sfca30_capacity, na.rm=TRUE))* 100

# add census tract/counties names
va_data <- left_join(df3, tracts, by=c("GEOID"= "geoid"))
#va_data <- left_join(df3, counties, by=c("GEOID"= "geoid"))

va_data <- va_data[, c(1, 7, 2, 3, 5,6,4)]

# rename columns
names(va_data)[1] <- 'geoid'
names(va_data)[3] <- '3sfca_capacity'
names(va_data)[4] <- '2sfca30_capacity'
names(va_data)[7] <- 'program_capacity'

va_data["region_type"] <- "tract"

# long format
va_data_long <- melt(va_data,
                     id.vars=c("geoid", "region_name", "region_type"),
                     variable.name="measure",
                     value.name="value"
)

va_data_long['measure_type'] = ""
indx1 <- grepl('capacity', va_data_long$measure)
indx2 <- grepl('norm', va_data_long$measure)
indx3 <- grepl('program', va_data_long$measure)
va_data_long$measure_type[indx1] <- 'index'
va_data_long$measure_type[indx2] <- 'normalized index'
va_data_long$measure_type[indx3] <- 'count'

va_data_long['measure_units'] <- NA
indx4 <- grepl('count', va_data_long$measure_type)
va_data_long$measure_units[indx4] <- 'number of graduates'

# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_tr_nces_2020_community_college_engineering_related_fca"),
             va_data_engin_rel,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_nces_2020_community_college_engineering_related_fca
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_tr_nces_2020_community_college_engineering_fca"),
             va_data_engin_rel,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_nces_2020_community_college_engineering_fca
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_tr_nces_2020_community_college_computer_sciences_fca"),
             va_data_comp,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_nces_2020_community_college_computer_sciences_fca
                OWNER TO data_commons")

va_data_engin <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_tr_nces_2020_community_college_engineering_fca")
va_data_engin_rel <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_tr_nces_2020_community_college_engineering_related_fca")
va_data_comp <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_tr_nces_2020_community_college_computer_sciences_fca")

dbRemoveTable(con, c("dc_education_training", "va_tr_nces_2020_community_college_engineering_related_fca"))
dbRemoveTable(con, c("dc_education_training", "va_tr_nces_2020_community_college_engineering_fca"))
dbRemoveTable(con, c("dc_education_training", "va_tr_nces_2020_community_college_computer_sciences_fca"))


dbDisconnect(con)

# remove extra columns
va_data_engin <- va_data_engin %>% select(-measure_units)
va_data_engin_rel <- va_data_engin_rel %>% select(-measure_units)
va_data_comp <- va_data_comp %>% select(-measure_units)

# multiply access scores by a 1000
va_data_comp$value[va_data_comp$measure == "2sfca30_capacity"] <-
  va_data_comp$value[va_data_comp$measure == "2sfca30_capacity"] *1000

va_data_comp$value[va_data_comp$measure == "3sfca_capacity"] <-
  va_data_comp$value[va_data_comp$measure == "3sfca_capacity"] *1000

va_data_engin$value[va_data_engin$measure == "2sfca30_capacity"] <-
  va_data_engin$value[va_data_engin$measure == "2sfca30_capacity"] *1000

va_data_engin$value[va_data_engin$measure == "3sfca_capacity"] <-
  va_data_engin$value[va_data_engin$measure == "3sfca_capacity"] *1000

va_data_engin_rel$value[va_data_engin_rel$measure == "2sfca30_capacity"] <-
  va_data_engin_rel$value[va_data_engin_rel$measure == "2sfca30_capacity"] *1000

va_data_engin_rel$value[va_data_engin_rel$measure == "3sfca_capacity"] <-
  va_data_engin_rel$value[va_data_engin_rel$measure == "3sfca_capacity"] *1000

# replace NAs in counts with 0s
va_data_comp <- within(va_data_comp, value[(is.na(value) & measure == 'program_capacity')] <- 0)
va_data_engin <- within(va_data_engin, value[(is.na(value) & measure == 'program_capacity')] <- 0)
va_data_engin_rel <- within(va_data_engin_rel, value[(is.na(value) & measure == 'program_capacity')] <- 0)

# add year
va_data_comp["year"] <- 2020
va_data_engin["year"] <- 2020
va_data_engin_rel["year"] <- 2020

# re-arrange columns
va_data_comp <- va_data_comp[, c(1, 3, 2, 7, 4, 5, 6)]
va_data_engin <- va_data_engin[, c(1, 3, 2, 7, 4, 5, 6)]
va_data_engin_rel <- va_data_engin_rel[, c(1, 3, 2, 7, 4, 5, 6)]
