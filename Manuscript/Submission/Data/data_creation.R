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








