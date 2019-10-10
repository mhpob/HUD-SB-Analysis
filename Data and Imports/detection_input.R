sb <- read.csv('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a', stringsAsFactors = F)
sb <- dplyr::mutate(sb, TL = TL/10,
                    FL = FL/10,
                    Weight = Weight/1000,
                    Region = ifelse(Location == 'RM 59',
                                    'West Point-Newburgh',
                                    'Saugerties-Coxsackie'))
sb <- reshape2::melt(sb, id.vars = c('TagDate','Batch', 'Location', 'Region',
                                     'Gear', 'Sex', 'Stage', 'Transmitter'),
                     measure.vars = c('TL', 'FL', 'Weight'))
names(sb) <- tolower(names(sb))

cl <- parallel::makeCluster(parallel::detectCores() - 1)
hud_detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections',
                                   clust = cl, prog_bar = T,
                                   creation_date = '2016-01-01')
parallel::stopCluster(cl)

hud_detects <- dplyr::filter(hud_detects, transmitter %in%
                               paste0('A69-1303-', seq(11423, 11522, 1)),
                             date.utc >= lubridate::ymd('2016-04-20'))
hud_detects$date.floor <- lubridate::floor_date(hud_detects$date.local,
                                                unit = 'day')
hud_detects <- dplyr::left_join(sb, hud_detects)

# Assign arrays
array_greps <- list(
  'Above' = 'can( |$)|br$|buoy 2\\d\\d|LB 1[6-9]\\d',
  # Lower Coxsackie Island (~42.36 N)
  'Saugerties-Coxsackie' = '(d buoy |[rg])(9\\d|1\\d\\d)|lb 1[015]\\d',
  # Saugerties Lighthouse (~42.07 N)
  'Between' = 'boll|rogers|rgn|[ r][78]\\d$|lb ([abe7]|6\\d)', #74
  # Should be I-84 (41.52N), but using Whites Marina (~41.58 N)
  'West Point-Newburgh' = '^lighted buoy (27|[4-5]\\d)|king|LL# 3[78][90][1-8].',
  # Should be Bear Mtn Br (~41.32), but using above Verplanck (41.26)
  'Below' = 'croton|^lighted buoy *(7|1\\d|2[0-6])( |$)|nysta',
  'ME' = '^\\d\\d ',
  'MA' = paste0('barns|^b[bh]|buzz|ca\\d|ccc|(chat|hing)ham|ledge|beach|cutty|',
                'ellis|gurnet|town|hull|m[ao]nom|mar[bst]|comb|merri|musk|',
                'noman|orl|ph\\d|RI$|rocky|sandw|scit|shark|taun|vs|well|^ei|',
                '^pamet|truro|chs|sippi|mmk|sud|joppa'),
  'LI Sound' = 'east r|matti|thames',
  'NY Coast' = 'ltb|[ny] (ea|we)|e\\.c|junc|ique|stony|nywea|mb |moriches',
  'NJ Coast' = 'opt|garden|egg|barne|chapel|raritan|sandy hook',
  'DE Coast' = 'BOEM',
  'DE' = 'C&D|LL# [23]... |de ',
  'MD Coast' = '([at]|cs)-|inner|outer|middle|[iao][nms]\\d',
  'VA Coast' = 'scl|^wea|ncc|^cb(\\d| )|2c |^cb$',
  'Ches' = 'cbbt|^york|^b\\d|kent|cedar|ts\\d|tang|poco'
)

station_list <- lapply(array_greps,
                       grep, x = unique(hud_detects$station),
                       ignore.case = T, value = T)

# Test that GREP doesn't match multiple arrays
grep_check <- sapply(1:length(station_list),
                     function(n) intersect(station_list[[n]],
                                           unlist(station_list[-n])))
if(length(unlist(grep_check)) != 0){
  names(grep_check) <- names(station_list)
  print(unlist(grep_check))
  stop('Regex matches multiple stations!!')
}


# Designate arrays
hud_detects$array <- NA
for(i in seq(1:length(station_list))){
  hud_detects$array <- ifelse(hud_detects$station %in% station_list[[i]],
                              names(station_list)[i],
                              hud_detects$array)
}


# Missing array info test
if(dim(dplyr::filter(hud_detects, is.na(array)))[1] > 1){
  stop('UNID array found!')
}

saveRDS(hud_detects, file = 'hud_detects.RDS')

rm(sb, array_greps, station_list, grep_check, i)


## Visual array assignment check
# library(sf)
# library(mapview)
# library(dplyr)
#
# stations <- hud_detects %>%
#   filter(lat != 0) %>%
#   distinct(station, lat, long, .keep_all = T) %>%
#   st_as_sf(coords = c('long', 'lat'), crs = 4267)
#
# mapview(stations, zcol = 'array')
