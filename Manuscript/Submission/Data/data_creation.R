## TL, Wt, sex, tagging region, cluster, last detection comparison/figures ----
library(data.table)

# Tagging data
sb <- fread('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a', col.names = tolower)

sb[, ':='(transmitter = sub('.*-' ,'', transmitter),
          tagging_date = as.Date(tagdate, format = '%m/%d/%Y'),
          sex = tolower(sex),
          total_length_cm = tl / 10,
          weight_kg = weight / 1000,
          tagging_region = ifelse(location == 'RM 59', 'lower', 'upper'))]

sb <- sb[, c('transmitter', 'tagging_date', 'sex', 'total_length_cm',
             'weight_kg', 'tagging_region')]


# Recategorization data
recat <- fread('manuscript/recategorized.csv')
recat <- recat[, .(transmitter = sub('.*-' ,'', transmitter),
                   categorized_2017 = tolower(cluster17),
                   predicted_2018 = tolower(pred18))]


joined <- recat[sb, on = 'transmitter']


# Last date recorded
detects <- setDT(readRDS('data and imports/hud_detects.rds'))

last <- detects[, .(last_record_utc = max(date.utc)),
                by = (transmitter = sub('.*-', '', transmitter))]


# Final data
joined <- joined[last, on = 'transmitter']
setcolorder(joined, c('transmitter', 'tagging_date', 'last_record_utc', 'sex',
                      'total_length_cm', 'weight_kg', 'tagging_region'))


# Export
fwrite(joined, 'manuscript/submission/data/tagging_clusters_finaldetection.csv')



## Spawning run time series ----
library(imputeTS); library(data.table)

# Detection data
all <- data.table(readRDS('data and imports/hud_detects.RDS'))


# Select spawning runs from transmitters that completed each year's run
hud <- all[date.local %between% c('2017-03-15', '2017-07-01') |
             date.local %between% c('2018-03-15', '2018-07-01')]
hud <- hud[array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                       'West Point-Newburgh', 'Below')]

hud <- hud[, ':='(year = year(date.local),
                  doy = yday(date.local),
                  transmitter = sub('.*-', '', transmitter))]

# Filter out transmitters that died
hud <- hud[!(grepl('11(480|507)', transmitter) & year == 2017) &
             !(grepl('11(424|479|494|500|505)', transmitter) & year == 2018)]



# Aggregate daily positions, label data type
agg.pos <- hud[, .(mean_latitude = mean(lat),
                   record_type = 'recorded'),
               by = c('transmitter', 'doy', 'year')]

# Create dummy doy for use later
agg.pos <- agg.pos[, doy_hold := doy]


setorder(agg.pos, transmitter, year, doy)



# Create table containing full year/transmitter/doy combinations
trans_yr_combn <- unique(hud, by = c('transmitter', 'year'))[, c('transmitter', 'year')]
doy_yr_combn <- expand.grid(doy = seq(min(hud$doy), max(hud$doy), 1), year = c(2017, 2018))
trans_doy_yr <- setDT(doy_yr_combn)[trans_yr_combn, on = 'year', allow.cartesian = T]

# Join table
agg.pos <- agg.pos[trans_doy_yr, on = c('transmitter', 'year', 'doy')]


# Concatenate with 41 deg. latitude, leaving two days on either side of each
#   transmitter's run to impute later
agg.pos <- agg.pos[, ':='(mean_latitude = fifelse(doy %inrange%
                                           (range(doy_hold, na.rm = T) + c(-2, 2)),
                                           mean_latitude, 41),
                          record_type = fifelse(doy %inrange%
                                                  (range(doy_hold, na.rm = T) + c(-2, 2)),
                                                record_type, 'concatenated')),
                   by = c('transmitter', 'year')]


# Impute leftover missing data
agg.pos <- agg.pos[, ':='(mean_latitude = na_ma(mean_latitude, k = 2),
                          record_type = fifelse(is.na(record_type), 'imputed', record_type)),
                   by = c('transmitter', 'year')]



# Make things pretty in preparation of export
agg.pos <- agg.pos[, ':='(date = as.Date(paste0(year, '-01-01')) + doy - 1,
                          day_of_year = doy,
                          doy = NULL,
                          doy_hold = NULL,
                          year = NULL)]

setcolorder(agg.pos, c('transmitter', 'date', 'day_of_year', 'mean_latitude', 'record_type'))


# Export
fwrite(agg.pos, 'manuscript/submission/data/spawning_run.csv')



## Kneebone plot ----
library(data.table)

# Detection data
detects <- data.table(readRDS('data and imports/hud_detects.rds'))

# Limit detections to before 2019
detects <- detects[date.local <= '2019-01-01']

# A bit of manipulation
detects <- detects[, ':='(transmitter = sub('.*-', '', transmitter),
                          date = as.Date(date.floor),
                          day_of_year = yday(date.local),
                          array = dplyr::case_when(grepl('Ab|Be|Saug|Newb', array) ~ 'Hudson',
                                                   grepl('N[JY]', array) ~ 'NYB',
                                                   array == 'Ches' ~ 'CH',
                                                   array == 'LI Sound' ~ 'LIS',
                                                   T ~ gsub(' .*', '', array)))]

recat <- fread('manuscript/recategorized.csv')
recat <- recat[, .(transmitter = sub('.*-' ,'', transmitter),
                   categorized_2017 = tolower(cluster17),
                   predicted_2018 = tolower(pred18))]

detects <- recat[detects, on = 'transmitter']

data <- unique(detects, by = c('categorized_2017', 'array', 'date'))

data <- data[, c('categorized_2017', 'date', 'day_of_year', 'array')]
setorder(data, date, categorized_2017, array)

# Export
fwrite(data, 'manuscript/submission/data/coastal_migration.csv')



## Cumulative frequency ----
library(data.table)

# Water quality data
usgs_data <- data.table(readRDS("data and imports/usgs_wq.rds"))

usgs_data <- usgs_data[, date := as.Date(datetime)]
usgs_data <- usgs_data[, year := year(date)]

usgs_data <- usgs_data[grepl('pough', site_name) & month(date) %in% 3:7]


# Detection data
dets <- data.table(readRDS('data and imports/hud_detects.RDS'))
dets <- dets[grepl('Above|Saug|Between|Newb|Below', array) &
               as.Date(date.floor) %between% c('2017-01-01', '2018-12-31') &
               month(date.local) %in% 3:7]
dets <- dets[, ':='(transmitter = sub('.*-', '', transmitter),
                    year = year(date.local))]


# Remove 11424 in 2018, as it's likely false
dets <- dets[!(grepl('11424', transmitter) & year == 2018)]


# Recategorization data
recat <- fread('manuscript/recategorized.csv')
recat <- recat[, .(transmitter = sub('.*-' ,'', transmitter),
                   categorized_2017 = tolower(cluster17),
                   predicted_2018 = tolower(pred18))]


# Join detection and recategorization
dets <- dets[recat[!is.na(categorized_2017)], on = 'transmitter']


# Find date of first entry
dets <- dets[, .(entry = min(date.local)),
             by = c('transmitter', 'year', 'categorized_2017')]
setorder(dets, categorized_2017, entry)


# Apply ECDF by year and 2017 cluster
cum_frac <- dets[, lapply(.SD, ecdf(entry)), by = c('year', 'categorized_2017')]


# Bind back in
cum_frac <- cbind(dets[, .(year, categorized_2017, entry, date = as.Date(entry))],
                  cum_frac[, .(cumulative_fraction = entry)])


# Join water quality data
data <- usgs_data[, .(water_temperature_c = mwt,
                      discharge_m3_s = mdisch,
                      date)][cum_frac, on = 'date']


# Prep for export
data <- data[, .(categorized_2017, year, entry_datetime = entry, cumulative_fraction,
                 water_temperature_c, discharge_m3_s)]


# Export
fwrite(data, 'manuscript/submission/data/hudson_spawning_entry.csv')
