
#################################
## LOAD & PROCESS SPECIES DATA ##
#################################

# Source of species present: https://ricklobello.wordpress.com/2016/04/23/biodiversity-of-the-castner-range/
# Data: https://gapanalysis.usgs.gov/apps/species-data-download/

# Temporarily set working directory
setwd("C:/Users/clitt/OneDrive/Desktop/spp_hab")

# List all the zips I've downloaded
zips.ar.orig <- list.files("zipped_amphrept", pattern = ".zip", full.names = TRUE)
zips.m.orig <- list.files("zipped_mammals", pattern = ".zip", full.names = TRUE)

# Apply unzip function over vector of original zips; put into new folder
sapply(zips.ar.orig, unzip, exdir = "unzipped_amphrept_orig")
sapply(zips.m.orig, unzip, exdir = "unzipped_mammals_orig")

# Find the next level of zips (ie the zipped raster of habitat maps)
zips.ar.maps <- list.files("unzipped_amphrept_orig", pattern = ".zip", full.names = TRUE)
zips.m.maps <- list.files("unzipped_mammals_orig", pattern = ".zip", full.names = TRUE)

# Apply unzip function over vector of map zips; put into new folder
sapply(zips.ar.maps, unzip, exdir = "unzipped_amphrept_maps")
sapply(zips.m.maps, unzip, exdir = "unzipped_mammals_maps")

# List the tifs in each; $ to specify "end of line" so won't grab, eg, ".tif.var.dbf"
ar.tifs <- list.files("unzipped_amphrept_maps", pattern = ".tif$", full.names = TRUE)
m.tifs <- list.files("unzipped_mammals_maps", pattern = ".tif$", full.names = TRUE)

# Load all into raster stack.
# Same crs/res, but diff extents therefore specify quick=TRUE
ar <- stack(ar.tifs, quick = TRUE)
a <- raster(ar.tifs[[1]])
b <- raster(ar.tifs[[2]])

minValue(a) # 3
maxValue(a) # 3
minValue(b) # 3
maxValue(b) # 3
minValue(ar) # NA, but why not 3 ??
maxValue(ar) # NA, but why not 3 ??


## Load Castner Range
cr <- load_f(paste0(data.dir, "CastnerRange.shp"))


# https://gis.stackexchange.com/questions/217082/handling-multiple-extent-problem-to-create-raster-stack-in-r
# https://stackoverflow.com/questions/53440229/make-raster-stack-with-different-extent
# https://gis.stackexchange.com/questions/385850/mask-and-crop-a-raster-to-multiple-polygons-in-r

# Mask and cropping to extent of cr
c <- raster::mask(raster::crop(a, raster::extent(cr)), cr)
d <- mask(crop(b, extent(cr)), cr)
e <- mask(crop(b, cr), cr)

plot(c)
plot(d)
plot(e)

f <- mask(crop(ar, extent(cr), cr))


( rp <- do.call(stack,lapply(1:nrow(p), function(x) 
  raster::mask(crop(r, extent(p)), p[x,]) )) )
plot(rp)

?do.call

# This is for cropping same raster to multiple polygons, from last link above
( rp <- do.call(stack,lapply(1:nrow(p), function(x) 
  raster::mask(crop(r, extent(p)), p[x,]) )) )
plot(rp)

# Try it for cropping stacked raster to single polygon
( rp <- do.call(stack,lapply(1:nlayers(ar), function(x) 
  raster::mask(crop(ar[[x]], extent(cr)), cr) )) )
# 
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'x' in selecting a method for function 'mask': Failure during raster IO

# Try it for cropping stacked raster to single polygon
# Do.call says apply this function (stack) to a list (returned from lapply)
( rp <- do.call(stack,lapply(1:nlayers(ar), function(x)
  mask(crop(ar[[x]], extent(cr)), cr) )) )

# Or just do this all in a loop??

x <- stack()
for(i in 1:nlayers(ar)){
  m <- mask(crop(ar[[i]], extent(cr)), cr)
  x <- stack(x, m)
}
# 
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'x' in selecting a method for function 'mask': Failure during raster IO

filename <- system.file("external/test.grd", package="raster")
r <- raster(filename)
plot(r)
t <- flip(r, direction = 'x')
plot(t)

p <- as(sf::st_buffer(as(sampleRandom(r, 4, sp=TRUE), "sf"), 200), "Spatial")
plot(p)
plot(r)
plot(p, add = TRUE)

# # This is for cropping same raster to multiple polygons, from last link above
# ( rp <- do.call(stack,lapply(1:nrow(p), function(x) 
#   raster::mask(crop(r, extent(p)), p[x,]) )) )
# plot(rp)


