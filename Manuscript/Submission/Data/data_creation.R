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
