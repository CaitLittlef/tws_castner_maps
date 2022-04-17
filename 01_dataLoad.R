
######################################
## DATA COLLECTION & PRE-PROCESSING ##
######################################

## Function to load features, set common crs, and fix any invalid geometries
# CRS based on what PADUS came in.
load_f <- function(f) {
  # proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  read_sf(f) %>%
    st_transform(proj.crs) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
}
# proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #PADUS


## Load Castner Range
cr <- load_f(paste0(data.dir, "CastnerRange.shp"))



## Load padus (can't do function above given errors related to invalid geometries/polygons w <4 pts)
# Can't do function above given errors related to invalid geometries/polygons w <4 pts.
# Rather than try to fix and enable, for example, dissolving/aggregation, just jump to rasterizing.
# For all sorts of experiments on fixing, see G:\My Drive\1Misc\misc code\invalid geometries etc.R
padus <- st_read("G:/My Drive/1Data/PADUS_2_1/PAD_US2_1.gdb",
                 layer="PADUS2_1Combined_Proclamation_Marine_Fee_Designation_Easement")
# Warning messages:
#   1: In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
#                        GDAL Message 1: organizePolygons() received a polygon with more than 100 parts. The processing may be really slow.  You can skip the processing by setting METHOD=SKIP, or only make it analyze counter-clock wise parts by setting METHOD=ONLY_CCW if you can assume that the outline of holes is counter-clock wise defined
#   2: In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
#                        GDAL Message 1: organizePolygons() received an unexpected geometry.  Either a polygon with interior rings, or a polygon with less than 4 points, or a non-Polygon geometry.  Return arguments as a collection.
object.size(padus)
gc()


## Filter to GAP 1, GAP 2, etc. N.b., filter() won't work prob b/c of geometry issues above.
# But this works for some reason.
gap12 <- padus[padus$GAP_Sts == "1" | padus$GAP_Sts == "2",]
gap12 <- gap12[gap12$Category == "Proclamation" | 
                 gap12$Category == "Designation" |
                 # gap12$Category == "Easement" |
                 gap12$Category == "Fee",]
object.size(gap12)


# Nix non-CONUS and maybe only select western
# AK Alaska
# FM Federated States of Micronesia
# GU Guam
# HI Hawaii
# MP Mariana Islands
# MH Marshall Islands
# PW Palau
# PR Puerto Rico
# UM U.S. Minor Outlying Islands
# VI United States Virgin Islands
# UNKF Unknown Federal
nix <- c("AK", "FM", "GU", "HI", "MP", "MH", "PW", "PR", "UM", "VI", "UNKF")
keeps <- c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "UT", "CO", "AZ", "NM", "ND", "SD", "NE", "KS", "OK", "TX")
# gap12 <- gap12[!gap12$State_Nm %in% nix,]
gap12 <- gap12[gap12$State_Nm %in% keeps,]
object.size(gap12)

# 
# ## Nix tiny scraps (I confirmed with selecting YNP and checking that SHAPE_Area is in sqm).
# sqm_in_sqkm <- 1000000
# sqm_in_10sqkm <- 10000000
# gap12 <- gap12[gap12$SHAPE_Area > sqm_in_sqkm,] ; object.size(gap12)



## Load land cover
# lc <- raster("G:/My Drive/1Data/National_Landcover_l48_eslf_v3_5_2018/l48_IVC_Existing_v3_5_2018.tif")
lc <- raster("G:/My Drive/1Data/NatureServe Ecosystems/NorthAmerican_Ecosystems_and_Macrogroups/NorthAmerica_IVC_Ecosystems_existing_NatureServe_v846.tif")

crs(lc)
# Extract attributes as dataframe
lu.lc <- as.data.frame(levels(lc)[[1]])
# lc@data@attributes[[1]]
(l <- levels(lc))

# Prior fail at deratifying (ie removing raster attribute table) with intention to load to GEE.
# Turns out values ARE the field I already want. And derafitying to EVT_cd does a few things:
# 1) weirdly drops many values
# 2) weirdly swaps some values
# 3) doesn't actually have all the values that are already in VALUE column.
# Below took 8 hrs, plus prob 20 min to save tif. 
# lc.new <- deratify(lc, "EVT_cd") 



# PA layer may still have weirdo multi-surface stuff; force multipolygons
class(gap12)
library(gdalUtilities)
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}
gap12 <- ensure_multipolygons(gap12)


# Also fix any invalid geometries
gap12 <- gap12 %>% st_make_valid() %>% st_buffer(dist = 0)

remove(padus)



