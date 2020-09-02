library(tidyr); library(dplyr); library(sf)


# Read and manipulate flowline data ---
flowline <- st_read('data and imports/hudson_flowline.gpkg')

##  Merge different river sections into one.
flowline <- st_combine(flowline)
flowline <- st_line_merge(flowline)



# Read in and manipulate receiver locations ----
dets <- readRDS('data and imports/hud_detects.RDS')

recs <- dets %>%
  filter(grepl('West |Below|Between|Saug|Above', array)) %>%
  distinct(station, lat, long) %>%
  arrange(-lat) %>%
  st_as_sf(coords = c('long', 'lat'),
           crs = 4326,
           remove = F)




# Reproject spatial objects ----
flowline <- st_transform(flowline, 32618)
recs <- st_transform(recs, 32618)



# Break the line that makes up the flowline into points 1 m apart from each other ----
flowline_pts <- flowline %>%
  st_segmentize(1) %>%
  st_cast('MULTIPOINT')


recs <- st_nearest_points(flowline_pts,
                           recs) %>%
  st_as_sf(station = recs$station,
           lat = recs$lat,
           long = recs$long,
           error_m = st_length(.))

recs$rkm <- units::set_units(1, 'km')

# Cast flowline from MULTIPOINT into simplified LINESTRING ---
flowline_simp <- st_cast(flowline_pts, 'LINESTRING')

for(i in 1:nrow(recs)){
  # Split flowline in half according to location of receiver
  flowline_split <- flowline_simp %>%
    lwgeom::st_split(recs[i,]) %>%
    st_collection_extract('LINESTRING')

  # Find the length of the flowline between locations
  lengths <- flowline_split %>%
    st_length() %>%

    # convert to KM
    units::set_units(km) %>%
    # Choose the down-river section (flowlines are measured from up- to down-river)
    .[[2]]

  recs[i,]$rkm <- lengths
}


recs <- recs %>%
  data.frame() %>%
  select(-x)

write.csv(recs, 'data and imports/rkms.csv', row.names = F)
