sb <- read.csv('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a', stringsAsFactors = F)
sb <- dplyr::mutate(sb, TL = TL/10,
                    FL = FL/10,
                    Weight = Weight/1000,
                    Region = ifelse(Location == 'RM 59',
                                    'West Point-Newburgh',
                                    'Saugerties-Coxsackie'))
sb <- reshape2::melt(sb, id.vars = c('Date','Batch', 'Location', 'Region',
                                     'Gear', 'Sex', 'Stage', 'Transmitter'),
                     measure.vars = c('TL', 'FL', 'Weight'))


hud_detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections')
hud_detects <- dplyr::filter(hud_detects, transmitter %in%
                               paste0('A69-1303-', seq(11423, 11522, 1)),
                             date.utc >= lubridate::ymd('2016-04-20'))
hud_detects$date.floor <- lubridate::floor_date(hud_detects$date.local,
                                                unit = 'day')
hud_detects <- dplyr::left_join(sb, hud_detects,
                                by = c('Transmitter' = 'transmitter'))

# Assign arrays
array_greps <- list(
  'Above' = 'can( |$)|br$|buoy 2\\d\\d',
  'Saugerties-Coxsackie' = 'd buoy ([79]\\d|1\\d\\d)',
  'Between' = 'rogers|rgn|8\\d$',
  'West Point-Newburgh' = 'd buoy (27|[4-5]\\d)|king',
  'Below' = 'd buoy *(7|1\\d|2[0-6])( |$)',
  'ME' = '^\\d',
  'MA' = paste0('barn|^b[bh]|buzz|ca\\d|ccc|(chat|hing)ham|ledge|beach|cutty|',
                'ellis|gurnet|town|hull|m[ao]no|mar[bst]|mb|merri|mor|musk|',
                'noman|orl|ph\\d|RI$|rocky|sand|scit|shark|taun|vs|well'),
  'LI Sound' = 'east r|matti|thames',
  'NY Coast' = 'ltb|[ny] [ew]|e\\.c|junc|ique|stony',
  'NJ Coast' = 'opt',
  'MD Coast' = '([at]|cs)-|inner|outer|middle|[iao][nms]\\d',
  'DE Coast' = 'BOEM',
  'DE' = 'C&D|LL#',
  'Ches' = 'kent|cedar'
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
