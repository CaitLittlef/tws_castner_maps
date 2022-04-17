
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

# These deletions are based on trial and error below for spp that have no correspondence w CR extent
m.tifs <- m.tifs[-2] 

##------------------------------------------------------------------
## Mask/crop rasters to CR extent.
# Raster have same crs/res but dif extents. Can load into stack with quick=TRUE.
# But mask/crop in a loop (e.g., stack[[i]] still doesn't seem to like that.
# So load each raster separately in the loop.
# Set all raster values equal to 1 for easy adding. Some are all valued valued at 3.

# Create empty stack
maps <- stack()
taxa.tifs <- ar.tifs ; taxa = "amphibian/reptile"
# taxa.tifs <- m.tifs ; taxa = "mammal"
for(i in 1:length(taxa.tifs)){ #Loop through each of the tifs
  r <- raster(taxa.tifs[i]) #Load tif
  m <- mask(crop(r, extent(cr)), cr) # crop does x/y extent; mask uses precise footprint
  m <- reclassify(m, cbind(0,10,1)) # reclassify from 0 to 10 as 1
  maps <- stack(maps, m)
  print(paste0(taxa, " number ", i, " is complete."))
}

plot(maps)
s <- sum(maps, na.rm = TRUE)
plot(s)




## -----------------------------------------------------------------
###########################
## REPRODUCIBLE EXAMPLE# ##
###########################

# Ref: https://gis.stackexchange.com/questions/385850/mask-and-crop-a-raster-to-multiple-polygons-in-r

# Create dummy rasters and polygon
filename <- system.file("external/test.grd", package="raster")
r <- raster(filename)
f <- flip(r, direction = 'x')
rf <- stack(r,f)
plot(r)

# Create polygons
p <- as(sf::st_buffer(as(sampleRandom(r, 4, sp=TRUE), "sf"), 500), "Spatial")
plot(p, add = TRUE)

# Loop through each raster in the stack of maps, crop it, mask it, then stick back in stack
s <- stack() # create empty stack for adding to
for(i in 1:nlayers(rf)){
# for(i in 1){
  r <- rf[[i]] # subset to the first raster in the stack of maps
  m <- raster::mask(raster::crop(r, extent(p)), p) # mask and crop per polygon
  s <- stack(s, m) # add newly croped/masked raster into the stack
}
# 
plot(s[[1]])
plot(s[[2]])
# ^ THIS WORKS
## -----------------------------------------------------------------




# Error when quick = TRUE
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'x' in selecting a method for function 'mask': Failure during raster IO


