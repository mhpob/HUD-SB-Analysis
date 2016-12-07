sb <- read.csv('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a', stringsAsFactors = F)
sb <- dplyr::mutate(sb, TL = TL/100,
                    FL = FL/100,
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

detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/hudson sb/receiver logs')
detects <- dplyr::filter(detects, transmitter %in%
                           paste0('A69-1303-', seq(11423, 11522, 1)),
                         date.utc >= lubridate::ymd('2016-04-20'))
detects$date.floor <- lubridate::floor_date(detects$date.local, unit = 'day')
detects <- dplyr::left_join(sb, detects, by = c('Transmitter' = 'transmitter'))
detects <- dplyr::left_join(detects, arrays[, c('station', 'array')], by = 'station')

rm(sb, arrays)