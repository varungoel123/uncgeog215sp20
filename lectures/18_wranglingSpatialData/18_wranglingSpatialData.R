## Examples from Lecture 18 - Wrangling Spatial Data
# March 25
## Varun Goel
## Geog 215: Introduction to Spatial Data Science

library(spDataLarge)
library(sf)
library(spData)
library(raster)
library(tidyverse)
library(tmap)
library(RColorBrewer)


# Slide 10: Spatial Sub-setting
# You have an sf object called nz
nz
# plot nz using tmap (similar to ggplot2 but better for mapping)
## You will learn more about it in lab
tm_shape(nz) + tm_polygons(col = "gray90", border.col = "black")

# Spatial subsetting (using one sf object)
canterbury = nz %>% filter(Name == "Canterbury")
tm_shape(canterbury) + tm_polygons(col = "gray90", border.col = "black")

#Spatial subsetting (2 sf objects)
## You have another object called nz_height
nz_height

## Plot nz_height
tm_shape(nz) + tm_polygons(col = "gray90", border.col = "black") +
  tm_shape(nz_height) + tm_dots(col = "red",size = 0.2, shape = 7)

# subsetting target point object using source polygon object
canterbury_height = nz_height[canterbury, ]

tm_shape(nz) + tm_polygons(col = "gray90", border.col = "black") +
  tm_shape(canterbury_height) + tm_dots(col = "red",size = 0.2, shape = 7)

## subsetting target polygon object using source point object
nz_states_w_mount <- nz[nz_height,]

tm_shape(nz) + tm_polygons(col = "gray90", border.col = "black") + 
  tm_shape(nz_states_w_mount) + tm_polygons(col = "red")


#### Topological Operations - Slide 11

# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1)))) # starting/ending points are the same 
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")
p <- st_as_sf(p) %>% mutate(num = 1:4)
## map the 3 sf objects
tm_shape(a) + tm_polygons(col = "gray90", border.col = "red") +
  tm_shape(p) + tm_dots(col = "black",size = 0.2, shape = 1) +
tm_text("num",auto.placement = T, size = 1, col = "red", just = "top") +
  tm_shape(l) + tm_lines(col="black")
# Perform topological operations - binary predictes

st_intersects(p, a)
st_intersects(p, a, sparse = FALSE)
st_disjoint(p, a, sparse = FALSE)[, 1]
st_within(p, a, sparse = FALSE)[, 1]
st_touches(p, a, sparse = FALSE)[, 1]
sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
lengths(sel) > 0
st_contains(a,p,sparse = FALSE)

## Spation Joins - slide 15
nz_height # 101 features, 2 fields
nz_height_join <- st_join(nz_height, nz) # 101 features, 9 fields
# by_default left_join, based on intersection, retains geometry of 1st object

nz #16 features, 6 fields

nz_join <- st_join(nz, nz_height) # 110 features, 9 fields
# by_default left_join, based on intersection
## Looks correct? What happened
## it creates a duplicate polygon feature for when there is more than 1 point that falls within a polygon
tm_shape(nz_join) + tm_polygons(col = "gray90", border.col = "red")

## What would make sense?
# maybe having elevation of largest peak
nz_join_highest_peak <- nz_join %>% group_by(Name) %>% 
  filter(elevation == max(elevation)) # not perfect, why are there missing v
## what is wrong?
tm_shape(nz_join_highest_peak) + 
  tm_polygons(col = "elevation", border.col = "red")

## Intuiton for Spatial Data averaging

# using base R function: aggregate
# average height
nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)
tm_shape(nz_avheight) + 
  tm_polygons(col = "elevation", border.col = "red")

nz_maxheight = aggregate(x = nz_height, by = nz, FUN = max)
tm_shape(nz_maxheight) + 
  tm_polygons(col = "elevation", border.col = "red")

## Using tidyverse verbs
nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>% 
  summarize(elevation = mean(elevation, na.rm = TRUE))

nz_maxheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>% 
  summarize(elevation = max(elevation, na.rm = TRUE))

## Maybe want number of mountains in each province?

## Calculating number of points in a polygon
nz_numpeaks = nz %>% 
  st_join(nz_height) %>% mutate(n = ifelse(is.na(elevation),0,1)) %>%
 group_by(Name) %>%
  summarize(num_peaks = sum(n))

tm_shape(nz_numpeaks) + 
  tm_polygons(col = "num_peaks", border.col = "red")
## Distance relations: slide 17

nz_heighest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_heighest, canterbury_centroid)


# what does this do
st_distance(nz,nz_height) %>% View
# what does this do
st_distance(nz_height,nz_height) %>% View

### Raster Spatial Operations slide 18

# spatial sub-setting
elev
plot(elev)

id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]
# the same as
raster::extract(elev, data.frame(x = 0.1, y = 0.1))

clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
plot(raster)
plot(clip, add = T)

tm_shape(elev) + tm_raster(palette = "Greens") + tm_shape(clip) + tm_raster()

elev[clip]
# we can also use extract
extract(elev, extent(clip)) 

## outputting subset as a raster
elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs
elev[1, 1:2, drop = FALSE] # spatial subsetting by row,column indices
elev[clip, drop = FALSE]
## subsetting using a raster mask
rmask = elev 
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

# spatial subsetting
elev[rmask, drop = FALSE]           # with [ operator
mask(elev, rmask)     # with mask()
overlay(elev, rmask, fun = "max")   # with overlay

# map algebra: slide 20

# local operations slide 21
raster1 <- matrix(sample(100, 36), ncol =6) %>% raster()
raster2 <- matrix(sample(100, 36), ncol =6) %>% raster()
add_raster <- raster1 + raster2

tm_shape(add_raster) + tm_raster(palette = "Greens")

# reclassification
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)
tm_shape(recl) + tm_raster(palette = "Greens")
## focal operations: slide 23

r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)
tm_shape(r_focal) + tm_raster(palette = "Greens")

## Zonal operations: slide 25
t1 <- tm_shape(elev) + tm_raster(palette = "Greens")
t2 <- tm_shape(grain) + tm_raster(palette = "Set2")

tmap_arrange(t1, t2)

z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()
z = zonal(elev, grain, fun = "sum") %>%
  as.data.frame()

## combining vector and raster
tm_shape(land$cover) + tm_raster(palette = "Greens")
tm_shape(World) + tm_polygons(col=NA)

world_centroid <- st_centroid(World)
tm_shape(world_centroid) + tm_dots()

raster::extract(land$trees, world_centroid)

