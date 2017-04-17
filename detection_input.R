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

arrays <- read.csv('p:/obrien/biotelemetry/hudson sb/receiver arrays.csv',
                   stringsAsFactors = F)
arrays$array <- factor(arrays$array, ordered = T,
                  levels = c('Above', 'Saugerties-Coxsackie', 'Between',
                             'West Point-Newburgh', 'Below'))

hud_detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections')
hud_detects <- dplyr::filter(hud_detects, transmitter %in%
                               paste0('A69-1303-', seq(11423, 11522, 1)),
                             date.utc >= lubridate::ymd('2016-04-20'))
hud_detects$date.floor <- lubridate::floor_date(hud_detects$date.local,
                                                unit = 'day')
hud_detects <- dplyr::left_join(sb, hud_detects,
                                by = c('Transmitter' = 'transmitter'))
hud_detects <- dplyr::left_join(hud_detects, arrays[, c('station', 'array')],
                                by = 'station')

saveRDS(hud_detects, file = 'hud_detects.RDS')

rm(sb, arrays)
