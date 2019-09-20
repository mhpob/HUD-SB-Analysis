library(gdistance); library(rgdal); library(raster)

midatl <- readOGR('c:/users/secor/desktop/gis products/chesapeake/midatlantic',
                  'matl_states_land')
midatl <- spTransform(midatl,
                      CRS('+proj=utm +zone=18 +datum=NAD83 +units=m'))

# Create nonsense raster file to clip shapefile
ras.back <- raster(extent(576332.1, 608029.5, 4504792.9, 4738441),
                   # resolution = 1/100, #5 arc-second grids = 720, 10 = 360,
                   # ncol = 6699, nrow = 7045,
                   resolution = c(100, 100),
                   vals = 1,
                   crs = proj4string(midatl))

ras.water <- mask(ras.back, midatl, inverse = T)

arrays <- read.csv('p:/obrien/biotelemetry/hudson sb/receiver arrays.csv',
                   stringsAsFactors = F)
row.names(arrays) <- arrays[, 'station']
arrays <- arrays[, c(3, 2)]
arrays <- SpatialPoints(arrays, CRS('+proj=longlat'))
arrays <- spTransform(arrays,
                      proj4string(midatl))
arrays <- arrays@coords

rm(ras.back, midatl)

trans <- transition(ras.water, transitionFunction = function(x){1}, 16)
geo <- geoCorrection(trans, type = 'c')
# Cost distance between receivers and river mouth (tip of the Battery in UTM)
distances <- costDistance(geo, as.matrix(arrays), c(582282.4, 4506542))/1000
distances <- round(distances, digits = 2)

test <- cbind.data.frame(row.names(distances), distances)
names(test) <- c('station', 'rkm')

write.csv(test, 'data and imports/receiverRKM.csv', row.names = F)
