library(httr); library(sf); library(data.table)

# Function to import flow lines ----
##  This code copied directly from here: https://ryanpeek.org/2017-11-05-mapping-with-sf-Part-2/
##  Which used code from here: https://gist.github.com/ldecicco-USGS/56262f3809f0807cb523d7105cb790a9

get_flowlines <- function(streamorder, mapRange){

  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',streamorder-1,'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',mapRange[3]," ",mapRange[1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',mapRange[4]," ",mapRange[2],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')

  destination = file.path(tempdir(),"nhdflowline_network.zip")
  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))

  filePath <- tempdir()
  print("unzipping...")
  unzip(destination, exdir = filePath)

  flowLines <- st_read(filePath, layer = 'nhdflowline_network')

  return(flowLines)
}

# Find bounding box of Hudson ----
lats <- c(40.52, 42.796)
longs <- c(-74.09, -73.63)



# Get flow lines ----
##  Second argument of function takes c(left, right, bottom, top)
##  Buffer this by 0.01 degrees
flowlines <- get_flowlines(1, c(longs[1] - 0.01, longs[2] + 0.01,
                                lats[1] - 0.01, lats[2] + 0.01))


## Select Hudson River
hudson <- flowlines[grepl('Hudson', flowlines$gnis_name),]


# Export ----
st_write(hudson, 'data and imports/hudson_flowline.gpkg')
