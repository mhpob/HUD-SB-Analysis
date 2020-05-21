library(TelemetryR); library(ggplot2); library(data.table)
detects <- data.table(readRDS('data and imports/hud_detects.RDS'))
setnames(detects, 'region', 'tagging.region')
detects <- detects[, tagging.region := ifelse(grepl('West', tagging.region),
                                              'Lower', 'Upper')]

recats <- fread('manuscript/recategorized.csv')
recats <- recats[, ':='(cluster17 = ifelse(cluster17 == 1, 'Upper','Lower'),
                        pred18 = ifelse(pred18 == 1, 'Upper','Lower'))]

detects <- recats[, -'region'][detects, on = 'transmitter']


# Overall loss
loss_overall <- data.table(trans_loss(detects, 'date.local', 'transmitter'))
loss_overall <- loss_overall[date <= as.POSIXct('2018-12-31 23:59:59')]

# This is loss per second. Multiply slope by number of seconds to get desired loss
#   per unit time. coef(m_all)[2] * 60 * 60 * 24 for per day, e.g.
m_all <- lm(data = loss_overall,
            log(remaining) ~ date)

# Per year
yr_mod <- list(
  '2016' = lm(data = loss_overall[date >= '2016-05-01' & date < '2016-07-01'],
              log(remaining) ~ date),
  '2017' = lm(data = loss_overall[date >= '2017-05-01' & date < '2017-07-01'],
              log(remaining) ~ date),
  '2018' = lm(data = loss_overall[date >= '2018-05-01' & date < '2018-07-01'],
              log(remaining) ~ date)
)

# seasonal losses
seasonal_mod <- list(
  spr = list(
    '2016' = lm(data = loss_overall[date >= '2016-05-01' & date < '2016-07-01'],
                log(remaining) ~ date),
    '2017' = lm(data = loss_overall[date >= '2017-05-01' & date < '2017-07-01'],
                log(remaining) ~ date),
    '2018' = lm(data = loss_overall[date >= '2018-05-01' & date < '2018-07-01'],
                log(remaining) ~ date)
  ),
  fall = list(
    '2016' = lm(data = loss_overall[date >= '2016-09-01' & date <= '2016-12-01'],
                log(remaining) ~ date),
    '2017' = lm(data = loss_overall[date >= '2017-09-01' & date <= '2017-12-01'],
                log(remaining) ~ date),
    '2018' = lm(data = loss_overall[date >= '2018-09-01' & date <= '2018-12-01'],
                log(remaining) ~ date)
  )
)

# Daily rate by season
lapply(seasonal_mod, function(x){
  lapply(x, function(x) coef(x)[2] * 60 * 60 * 24)
})



# By 2017 cluster
loss_clust <- detects[date.floor <= '2018-12-31'][
  , trans_loss(.SD, 'date.local', 'transmitter'),
  by = cluster17]

loss_clust[yday(date) >= yday('2017-05-01') & yday(date) <= yday('2017-07-01')][
  , .(loss_rate = lm(log(remaining) ~ date, data = .SD)$coef[2] *
                 60 * 60 * 24),
           by = .(year(date), cluster17)][year != 2016 & !is.na(cluster17)]

