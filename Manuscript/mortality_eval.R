library(TelemetryR); library(ggplot2); library(data.table)
detects <- data.table(readRDS('data and imports/hud_detects.RDS'))


# Overall loss
loss_overall <- data.table(trans_loss(detects, 'date.local', 'transmitter'))
loss_overall <- loss_overall[date <= as.POSIXct('2018-12-31 23:59:59')]

# Convert to date to have loss per day rather than per second
m_all <- lm(data = loss_overall,
            log(remaining) ~ as.Date(date))


# seasonal losses
seasonal_mod <- list(
  spr = list(
    '2016' = lm(data = loss_overall[date >= '2016-05-01' & date <= '2016-07-01'],
                log(remaining) ~ as.Date(date)),
    '2017' = lm(data = loss_overall[date >= '2017-05-01' & date <= '2017-07-01'],
                log(remaining) ~ as.Date(date)),
    '2018' = lm(data = loss_overall[date >= '2018-05-01' & date <= '2018-07-01'],
                log(remaining) ~ as.Date(date))
  ),
  fall = list(
    '2016' = lm(data = loss_overall[date >= '2016-09-01' & date <= '2016-12-01'],
                log(remaining) ~ as.Date(date)),
    '2017' = lm(data = loss_overall[date >= '2017-09-01' & date <= '2017-12-01'],
                log(remaining) ~ as.Date(date)),
    '2018' = lm(data = loss_overall[date >= '2018-09-01' & date <= '2018-12-01'],
                log(remaining) ~ as.Date(date))
  )
)
