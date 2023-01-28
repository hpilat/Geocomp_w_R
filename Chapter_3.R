library(sf)
library(terra)
library(dplyr)
library(spData)
library(tidyverse)

methods(class = "sf")
class(world)
dim(world)
colnames(world)
# st_drop_geometry function will keep only the attribute data of an sf object
world_df <- st_drop_geometry(world)
class(world_df)
ncol(world_df)

# dplyr
# subset rows with filter() and slice()
# subset columns with select()
# [] can subset both rows and columns

# subset rows by position
world[1:6, ]
# subset columns by position
world[, 1:3]
# subset rows and columns by position
world[1:6, 1:3]
# subset columns by name
world[, c("name_long", "pop")]
# subset by logical indices
world[, c(T, T, F, F, F, F, F, T, T, F, F)]
# subset by an index representing a non-existent column
world[, 888]

# create an object containing nations whose surface area is smaller than 10 000km^2
i_small <- world$area_km2 < 10000
summary(i_small)
small_countries <- world[i_small, ]

# shortcut to do the same thing:
small_countries <- world[world$area_km2 < 10000, ]
small_countries <- subset(world, area_km2 < 10000)

# select columns:
world1 <- dplyr::select(world, name_long, pop)
names(world1)

# select all columns between name_long and pop
world2 <- dplyr::select(world, name_long:pop)

# remove specific columns with the - operator
# remove all columns except subregion and area_km2
world3 <- dplyr::select(world, -subregion, -area_km2)

# subset and rename columns at the same time with new_name = old_name syntax
world4 <- dplyr::select(world, name_long, population = pop)

# extract a single column as a vector with pull()
pull(world, pop)
world$pop
world[["pop"]]

# select rows 1 to 6:
slice(world, 1:6)

# filter out countries with an area below a certain threshold:
world7 <- filter(world, area_km2 < 10000)
# filter out countries with a high average life expectancy
world7 <- filter(world, lifeExp > 82)
world7

# use the pipe to filter countries from Asia
world7 <- world %>% 
  filter(continent == "Asia") %>% 
  dplyr::select(name_long, continent) %>% 
  slice(1:5)

# aggregation: summarizing data with grouping variables
# Ex: calculate the number of people per continent based on country-level data
# (one row per country)
# find the sum of country poulations for each continent
world_agg1 <- aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)

# can do the same thing with sf
world_agg2 <- aggregate(world["pop"], list(world$continent), FUN = sum, na.rm = TRUE)
class(world_agg2)
nrow(world_agg2)

# can do the same thing with dplyr
world_agg <- world %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE))

# calculate the population, area, and number of countries in each continent
world_agg4 <- world %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE), 'area_sqkm' = sum(area_km2), n = n())
world_agg4

# calculate population density, arrange continents by the number of countries they contain,
# keep only the 3 most populous continents
world_agg5 <- world %>% 
  st_drop_geometry() %>% # drop the geometry for increased computing speed
  dplyr::select(pop, continent, area_km2) %>% # subset the columns of interest
  group_by(continent) %>% # group by continent and summarize
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n()) %>% 
  mutate(Density = round(Pop / Area)) %>% # calculate population density
  slice_max(Pop, n = 3) %>% # keep only the top 3 most populous continents
  arrange(desc(N)) #arrange in order of n countries
world_agg5  
  
# joining: combining tables
# combine data on coffee production with the world dataset
# both datasets must have a common column/key, in this case it's the name_long column
# use a left_join() to combine the tables, which will preserve the first dataset
world_coffee <- left_join(world, coffee_data)
class(world_coffee)

names(world_coffee)
plot(world_coffee["coffee_production_2017"])

# if the datasets don't have a key variable, can use the "by" argument
coffee_renamed <- rename(coffee_data, nm = name_long)
world_coffee2 <- left_join(world, coffee_renamed, by = c(name_long = "nm"))

# use an inner join to only keep the countries that have a matching key variable
world_coffee_inner <- inner_join(world, coffee_data)
nrow(world_coffee_inner)
# identify the rows that did not match
setdiff(coffee_data$name_long, world$name_long)
# Dem. Rep. of Congo is abbreviated and so it was missed
# confirme which country this should be using stringr package
(drc = stringr::str_subset(world$name_long, "Dem*.+Congo"))

# create a new version of coffee_data and update the name
coffee_data$name_long[grep("Congo,", coffee_data$name_long)] = drc
world_coffee_match <- inner_join(world, coffee_data)
nrow(world_coffee_match)

# join in the other direction
# start with the coffee dataset and add variables from the world dataset
coffee_world <- left_join(coffee_data, world)
class(coffee_world)

# calculate population density for each country
world_new <- world # so we don't overwrite our original data 
world_new$pop_dens <- world_new$pop / world_new$area_km2

# add a new column with mutate()
world %>% 
  mutate(pop_dens = pop / area_km2)

# transmute() will drop all columns other than the geometry column
world %>% 
  transmute(pop_dens = pop / area_km2)

# unite() will paste together existing columns
# combine the continent and region_un into one column
# define a separator and if the original columns should be removed
world_unite <- world %>% 
  tidyr::unite("con_reg", continent:region_un, sep = ":", remove = TRUE)

# separate() will separate one column with 2 variables into separate columns
world_separate <- world_unite %>% 
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")
# rename columns with tidy:
world %>% 
  rename(name = name_long)

# drop geometry for improved computation speed:
world_data <- world %>% st_drop_geometry()
class(world_data)

# create a raster dataset from scratch
elev <- rast(nrows = 6, ncols = 6,
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, 
             vals = 1:36)

# create another raster object
grain_order <- c("clay", "silt", "sand")
grain_char <- sample(grain_order, 36, replace = TRUE)
grain_fact <- factor(grain_char, levels = grain_order)
grain <- rast(nrows = 6, ncols = 6,
              xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, 
              vals = grain_fact)
# use levels() function to retrieve and add new or replace existing factor levels
levels(grain) <- data.frame(value = c(0, 1, 2), wetness = c("wet", "moist", "dry"))
levels(grain)

# raster subsetting
# get the value of the top left pixel in the elev raster object
# row 1, column 1
elev[1, 1]
# cell ID 1
elev[1]

# return a data frame with one row and two columns (one for each layer)
two_layers <- c(grain, elev)
two_layers[1]

# set the upper left cell of elev to 0
elev[1, 1] = 0
elev[] # empty brackets returns all values

# modify multiple cells
two_layers <-  c(grain, elev)
two_layers[1] <-  cbind(c(1), c(4))
two_layers[]

# summarizing raster objects
# use global() to calculate summary statistics
global(elev, sd)

# can visualize raster value statistics
hist(elev)
