###########
## SETUP ##
###########

# start <- Sys.time()
# print(Sys.time() - start)

today <- paste0(mid(Sys.Date(),3,2),
                mid(Sys.Date(),6,2),
                mid(Sys.Date(),9,2))

#####################################
## Set working directory 
setwd("G:/My Drive/2TWS Castner Range Maps/analyses/tws_castner_maps/")
wd <- "G:/My Drive/2TWS Castner Range Maps/analyses/tws_castner_maps/"
data.dir <- "G:/My Drive/2TWS Castner Range Maps/data/"
out.dir <- "G:/My Drive/2TWS Castner Range Maps/output/"


#####################################
# Install packages if not already installed
required.packages <- c("plyr", "ggplot2", "gridExtra", "terra", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "partykit", "vcd", "maps", "mgcv", "tmap",
                       "MASS", "pROC", "ResourceSelection", "caret", "broom", "boot",
                       "dismo", "gbm", "usdm", "pscl", "randomForest", "pdp", "classInt", "plotmo",
                       "ggspatial", "lmtest",  "dynatopmodel", "spatialEco", "exactextractr", "fasterize",
                       "chemCal")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages, dependencies = "TRUE")
rm(required.packages, new.packages)

# Libraries
# library(plyr)
library(ggplot2)
library(gridExtra)
library(terra)
library(raster)
# library(sp)
library(sf)
library(rgdal)

# Please note that rgdal will be retired by the end of 2023,
# plan transition to sf/stars/terra functions using GDAL and PROJ
# at your earliest convenience.

library(dplyr)
library(tidyverse)
library(maptools)
library(rgeos)
library(partykit)
library(vcd)
library(maps)
library(mgcv)
library(tmap)
library(MASS)
library(pROC)
library(ResourceSelection)
library(caret)
library(broom)
library(boot)
library(dismo)
library(gbm)
library(usdm)
library(pscl)
library(randomForest)
library(pdp)
library(classInt)
library(plotmo)
library(ggspatial)
library(lmtest)
library(dynatopmodel)
library(spatialEco)
library(exactextractr)
library(RColorBrewer)
library(fasterize)
library(chemCal)




# rm(GCtorture)

#####################################
# Turn off scientific notation
options(scipen=999) 


#####################################
# Grab date for saving files
# currentDate <- Sys.Date()

today <- paste0(mid(Sys.Date(),3,2),
                mid(Sys.Date(),6,2),
                mid(Sys.Date(),9,2))


#####################################
# Functions

### COEFFICIENT OF VARIATION
CV <- function(x) {100*sd(x) / mean(x)}



############################################################################################
### MORANS I FUNCTION
Moran_tpha <-function(x)
{cbind(x$long, x$lat) %>%
    dist(.) %>%
    as.matrix(.) %>%
    .^(-1) -> temp
  diag(temp) <- 0
  Moran.I(x$tpha, temp)}


Moran_space <-function(x, y)
{cbind(x$long, x$lat) %>%
    dist(.) %>%
    as.matrix(.) %>%
    .^(-1) -> temp
  diag(temp) <- 0
  Moran.I(y, temp)}



############################################################################################
### Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


############################################################################################
# Text extraction
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}


############################################################################################
# Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# SE
stdErr <- function(x) {sd(x)/ sqrt(length(x))}


############################################################################################
# Wes palette
# install.packages("wesanderson")
# library(wesanderson)
# pal.d1 <- wes_palette("Darjeeling1")
# pal.d2 <- wes_palette("Darjeeling2")
# pal <- c("#000000", "#F98400",  "#046C9A", "#FF0000", "#00A08A", "#00A08A", "#F98400", "#FF0000", "#00A08A", "#5BBCD6","#F2AD00", "#F98400")
# pal colors are X, X, blue, X, teal, red, X, light blue, yellow, orange

#046C9A # blue
#F98400 # orange



# Color blind palette
# The palette with grey:
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# grey, orange, light blue, pine green, yellow, dark blue, red, pink

library(RColorBrewer)
# display.brewer.all(7)
# display.brewer.pal(7, "Set1")
# palette <- brewer.pal(7, "Set1")

# display.brewer.all(colorblindFriendly = TRUE)
# display.brewer.pal(8, "Dark2")
# display.brewer.pal(8, "RdYlBu")
# palette <- brewer.pal(8, "Dark2")
# palette <- brewer.pal(8, "Set2")
# palette <- brewer.pal(8, "RdYlBu")

