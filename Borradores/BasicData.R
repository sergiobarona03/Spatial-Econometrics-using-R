

library(terra)
library(sf)
library(ggplot2)

############################
## Explaining vector data ##
############################

# Create a longitude-latitude vector
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)

# Create spatvector object, defininga coordinate reference system
crdref <- "+proj=longlat +datum=WGS84"
pts <- vect(lonlat, crs = crdref)
class(pts)
# To check geometry
geom(pts)

# Add variable to Spatvector object
precip <- runif(nrow(lonlat), min = 0, max = 100)
df <- data.frame(ID = 1:nrow(lonlat), precip = precip)
ptv <- vect(lonlat, atts = df, crs = crdref)
geom(ptv)

# Creating a SpatVector of lines or polygons
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(id=1, part=1, lon, lat)

# Líneas
lns <- vect(lonlat, type = "lines", crs = crdref)

# Polígonos
pols <- vect(lonlat, type = "polygons", crs = crdref)

# Plot
plot(pols)
plot(pols, border='black', col= alpha("red", 0.3), lwd=3)
points(pts, col='red', pch=20, cex=3)


############################
## Explaining raster data ##
############################
# Terra package has functions for creating, reading, manipulating raster data
# A SpatRaster represents multi-layer raster data
r <- rast(ncol=10, nrow=10,
          xmin=-150,xmax=-80,
          ymin=20, ymax=60) # Parameters: ncol, nrow, spatial extent, and CRS

# Let's assign some values (assign a vector of random numbers)
values(r) <- runif(ncell(r))
values(r) <- 1:ncell(r) # or u could simply assign cell numbers

#Plot
plot(r)

# Plotting a multi-layer (n-layer = 3) object
r2 <- r*r
r3 <- sqrt(r)
s <- c(r, r2, r3)
plot(s)
