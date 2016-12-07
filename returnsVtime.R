library(lubridate); library(dplyr)

source('detection_input.R')

agg_detects <- filter(detects, !is.na(date.local)) %>%
  mutate(week = ceiling_date(date.local, unit = 'week')) %>%
  group_by(week, Region, array) %>%
  distinct(trans.num) %>%
  summarize(count = n())

library(ggplot2)
ggplot() + geom_step(data = agg_detects,
                     aes(x = week, y = count/50, color = Region),
                     lwd = 1) +
  scale_x_datetime(limits = c(ymd_hms('2016-04-19 00:00:00'),
                              ymd_hms('2016-07-05 00:00:00'))) +
  facet_wrap(~ array, ncol = 1) +
  theme_bw() +
  theme(legend.position = c(0.79, 0.94), axis.title.x = element_blank()) +
  labs(y = 'Proportion detected', color = 'Tagging region')

