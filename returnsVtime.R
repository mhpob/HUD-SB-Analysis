library(lubridate); library(ggplot2); library(dplyr)

source('detection_input.R')

# Proportion v week
agg_detects <- filter(detects, !is.na(date.local)) %>%
  mutate(week = ceiling_date(date.local, unit = 'week')) %>%
  group_by(week, Region, array) %>%
  distinct(trans.num) %>%
  summarize(count = n())

ggplot() + geom_step(data = agg_detects,
                     aes(x = week, y = count/50, color = Region),
                     lwd = 1) +
  scale_x_datetime(limits = c(ymd_hms('2016-04-19 00:00:00'),
                              ymd_hms('2016-07-05 00:00:00'))) +
  facet_wrap(~ array, ncol = 1) +
  theme_bw() +
  theme(legend.position = c(0.79, 0.94), axis.title.x = element_blank()) +
  labs(y = 'Proportion detected', color = 'Tagging region')

# River km v Date per individual
rec_rkm <- read.csv('p:/obrien/biotelemetry/hudson sb/receiverrkm.csv')
detects <- left_join(detects, rec_rkm, by = 'station')

agg_rkm <- filter(detects, !is.na(date.local)) %>%
  mutate(day = ceiling_date(date.local, unit = 'day'))

ggplot() + geom_raster(data = agg_rkm, aes(x = day, y = trans.num,
                                           fill = rkm)) +
  scale_fill_gradient(low = 'blue', high = 'orange') +
  scale_x_datetime(limits = c(ymd_hms('2016-04-19 00:00:00'),
                              ymd_hms('2016-06-19 00:00:00'))) +
  facet_wrap(~ Region, ncol = 1, scales = 'free_y')  +
  theme_bw() +
  theme(legend.position = c(0.95, 0.2), axis.title.x = element_blank()) +
  labs(y = 'Fish (Transmitter ID)', fill = 'River km')
