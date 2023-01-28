class(world)
names(world)
# world$geom is a list that contains all the coordinates of the country polygons
plot(world)
summary(world["lifeExp"])

# return an object with only the first two rows and the first three columns 
  # of the world object

world_mini <- world[1:2, 1:3]
world_mini

# read_sf() function returns data as a tidyverse tibble
world_dfr <- st_read(system.file("shapes/world.shp", package = "spData"))
world_tbl <- read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)
class(world_tbl)

# plot function makes maps with sf
  # default is to create a multi-panel plot, one plot for each variable of the object
plot(world[3:6])
plot(world["pop"])

# add plots as layers to existing images by setting add = TRUE
world_asia <- world[world$continent == "Asia", ]
asia <- st_union(world_asia)
# plot the Asian continent over a map of the world
  # For add = TRUE to work, first plot can only have one facet
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# overlay circles, cex = circle diameter, to represent population by country
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop)/10000
# use st_centroid function to convert polygons to points
world_cents <- st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

# map India in the context of its surrounding countries
india <- world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "dark goldenrod", lwd = 3)
# lwd emphasizes India on the plot
plot(st_geometry(world_asia), add = TRUE)

lnd_point <- st_point(c(0.1, 51.5)) # sfg object
lnd_geom <- st_sfc(lnd_point, crs = "EPSG:4326")
lnd_attrib <- data.frame(
  name = "London", 
  temperature = 25, 
  date = as.Date("2017-06-21")
)
lnd_sf <- st_sf(lnd_attrib, geometry = lnd_geom) # sf object
lnd_sf
class(lnd_sf) # sf objects are usually both sf and dataframe classes

# create single points from numeric vectors:
st_point(c(5, 2)) # XY point, 2D
st_point(c(5, 2, 3)) # XYZ point, 3D
st_point(c(5, 2, 1), dim = "XYM") # XYM point, 2D + M = measurement accuracy
st_point(c(5, 2, 3, 1)) # XYZM point, 3D + M = measurement accuracy

# use matrices for multilinestrings, multipolygons, and geometry collections
# rbind function simplifies the creation of matrices
# multipoint
multipoint_matrix <- rbind(c(5,2), c(1,3), c(3,4), c(3,2))
st_multipoint(multipoint_matrix)
# linestring
linestring_matrix <- rbind(c(1,5), c(4,4), c(4,1), c(2,2), c(3,2))
st_linestring(linestring_matrix)

# use lists for the creation of multilinestrings, multipolygons, and geometry collections
# polygon:
polygon_list <- list(rbind(c(1,5), c(2,2), c(4,1), c(4,4), c(1,5)))
st_polygon(polygon_list)

# polygon with a hole:
polygon_border <- rbind(c(1,5), c(2, 2), c(4,1), c(4,4), c(1,5))
polygon_hole <- rbind(c(2,4), c(3,4), c(3,3), c(2,3), c(2,4))
polygon_with_hole_list <- list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

# sfc point
point1 <- st_point(c(5,2))
point2 <- st_point(c(1,3))
points_sfc <- st_sfc(point1, point2)
points_sfc

# default values for CRS is NA
st_crs(points_sfc)
# all geometries in sfc objects must have the same CRS
# specify the CRS
points_sfc_wgs <- st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs)

# create a single coordinate pair, v
v <- c(1,1)
v_sfg_sfh <- sfheaders::sfg_point(obj = v)
v_sfg_sfh

v_sfg_sf <- st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)

# create sfg objects from matrices and dataframes
# matrices
m <- matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)

# dataframes
df <- data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)

# build simple feature columns 
sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

# create sf objects
sfheaders::sf_point(obj = v)
sfheaders::sf_linestring(obj = m)
sfheaders::sf_polygon(obj = df)

#set the CRS
df_sf <- sfheaders::sf_polygon(obj = df)
st_crs(df_sf) = "EPSG:4326"

# S2 = spherical geometry = calculations done with consideration of the world as a sphere
# otherwise calculations are done on projections
# example of when S2 is turned off:

india_buffer_with_s2st_buffer(india, 1)
sf_use_s2(FALSE)
india_buffer_without_s2 <- st_buffer(india, 1)
plot(india_buffer_without_s2)

# turn S2 back on (general assumption is that it is on)
sf_use_s2(TRUE)

# Raster Data
# Example using data from Zion National Park:
raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
my_rast <- rast(raster_filepath)
class(my_rast)
my_rast

# map making
plot(my_rast)

# create a raster object in R by reading in a raster file
single_raster_file <- system.file("raster/srtm.tif", package = "spDataLarge")
single_rast <- rast(raster_filepath)

# create a raster from scratch
new_raster <- rast(nrows = 6, ncols = 6, 
                   xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, 
                   vals = 1:36) # values assigned to each cell

multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast <- rast(multi_raster_file)
multi_rast

# get the number of layers in the SpatRaster object
nlyr(multi_rast)

# select layers with terra::subset()
multi_rast3 <- subset(multi_rast, 3)
multi_rast4 <- subset(multi_rast, "landsat_4")

# combine several SpatRaster objects into one
multi_rast34 <- c(multi_rast3, multi_rast4)
 
# get the CRS of terra objects with crs()

# calculate the area of Luxembourg
Luxembourg <- world[world$name_long == "Luxembourg", ]
st_area(Luxembourg)
attributes(st_area(Luxembourg))
# divide the number by one million
st_area(Luxembourg)/1000000
# change the units to km^2
units::set_units(st_area(Luxembourg), km^2)

res(my_rast) # decimal degrees, but doesn't explicitly say this
# use the UTM projection
repr <- project(my_rast, "ESPG:26912")
res(repr)

summary(world$geom)
colnames(world)
