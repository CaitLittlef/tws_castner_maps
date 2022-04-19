
#################################
## LOAD & PROCESS SPECIES DATA ##
#################################

# Source of species present: https://ricklobello.wordpress.com/2016/04/23/biodiversity-of-the-castner-range/
# Data: https://gapanalysis.usgs.gov/apps/species-data-download/
# Temp set local wd to avoid massive upload to Drive proj folder.
setwd("C:/Users/clitt/OneDrive/Desktop/spp_hab")


## ** UPDATE ** ##
# I RECALLED THAT GAP HAD ALREADY CREATED SPP RICHNESS OVERLAYS :/
# THEY'RE LOADED BELOW AND WHAT I USED FOR FINAL MAPS FOR CASTNER RANGE.
# BUT THEY DON'T HAVE INDIVIDAUL SPECIES PARSED, SO I CREATED SEPARATE MAPS
# FOR THE TWO IMPERILLED SPECIES:
# TX Lyresnake (rTXLYx; aka Chihuahuan Desert) & TX Horned Lizard (rTHLIx)
# rTXLYx_CONUS_HabMap_2001v1.tif
# rTHLIx_CONUS_HabMap_2001v1.tif

##------------------------------------------------------------------

# Create vector of downloaded zips' paths.
# Apply unzip over that vector. Put stuff in new folder. 
list.files("zipped_amphrept_orig",
           pattern = ".zip", full.names = TRUE) %>%
  sapply(unzip, exdir = "unzipped_amphrept_orig")

list.files("zipped_mammals_orig",
           pattern = ".zip", full.names = TRUE) %>%
  sapply(unzip, exdir = "unzipped_mammals_orig")


# Find next level of zips (ie zipped maps) and unzip.
list.files("unzipped_amphrept_orig",
           pattern = ".zip", full.names = TRUE) %>%
  sapply(unzip, exdir = "unzipped_amphrept_maps")

list.files("unzipped_mammals_orig",
           pattern = ".zip", full.names = TRUE) %>%
  sapply(unzip, exdir = "unzipped_mammals_maps")


# List tifs in each; $ = hard stop.
ar.tifs <- list.files("unzipped_amphrept_maps",
                      pattern = ".tif$", full.names = TRUE)

m.tifs <- list.files("unzipped_mammals_maps",
                     pattern = ".tif$", full.names = TRUE)


# Nix spp if mask/crop fails b/c of no extent overlap.
# But just leave in spp for which mask/crop works even if no habitat (adds zero)
nix <- c("unzipped_mammals_maps/mNPMOx_CONUS_HabMap_2001v1.tif")
m.tifs <- m.tifs[!m.tifs %in% nix] 



##------------------------------------------------------------------

## Mask/crop rasters to CR extent.
# NB Raster have same crs/res but dif extents. Can load into stack with quick=TRUE.
# But mask/crop in a loop (e.g., stack[[i]] still doesn't seem to like that.
# So load each raster separately in the loop -- either amph/rept or mamm
# Set all raster values equal to 1 for easy adding. Some are all valued valued at 3.

# Create empty stack
maps <- stack()
taxa.tifs <- ar.tifs ; taxa = "amph_rept"
taxa.tifs <- m.tifs ; taxa = "mammal"
for(i in 1:length(taxa.tifs)){ #Loop through each of the tifs
  r <- raster(taxa.tifs[i]) #Load tif
  m <- mask(crop(r, extent(cr)), cr) # crop does x/y extent; mask uses precise footprint
  m <- reclassify(m, cbind(0,10,1)) # reclassify from 0 to 10 as 1
  maps <- stack(maps, m)
  print(paste0(taxa, "#", i, " is complete."))
}

plot(maps)

# Add all rasters (now with val 1) together for total spp richness map.
s <- sum(maps, na.rm = TRUE)
plot(s)
minValue(s)
maxValue(s)

# Write raster
writeRaster(s, paste0(data.dir, taxa, "_richness_", today, ".tif"))




## Load and crop/mask GAP's ALREADY CREATED spp richness layers. UGH.
# d <- raster("G:/My Drive/1Data/spp/amphibian_richness_habitat30m.tif") ; taxa = "amph"
# d <- raster("G:/My Drive/1Data/spp/bird_richness_habitat30m.tif") ; taxa = "bird"
# d <- raster("G:/My Drive/1Data/spp/mammal_richness_habitat30m.tif") ; taxa = "mammal"
d <- raster("G:/My Drive/1Data/spp/reptile_richness_habitat30m.tif") ; taxa = "rept"
d <- mask(crop(d, extent(cr)), cr)
# Write raster
writeRaster(d, paste0(data.dir, "GAP_", taxa, "_richness_", today, ".tif"), overwrite = TRUE)


## Load and crop just the state threatened spp
snake <- raster("unzipped_amphrept_maps/rTXLYx_CONUS_HabMap_2001v1.tif")
snake <- mask(crop(snake, extent(cr)), cr)
lizard <- raster("unzipped_amphrept_maps/rTHLIx_CONUS_HabMap_2001v1.tif")
lizard <- mask(crop(lizard, extent(cr)), cr)

plot(snake)
plot(lizard)

writeRaster(snake, paste0(data.dir, "TXLyreSnake_", today, ".tif"), overwrite = TRUE)
writeRaster(lizard, paste0(data.dir, "TXHornedLiz_", today, ".tif"), overwrite = TRUE)


# Reset working dir
setwd("G:/My Drive/2TWS Castner Range Maps/analyses/tws_castner_maps/")





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

