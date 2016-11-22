library(TelemetryR); library(ggplot2); library(lubridate); library(reshape2);
library(dplyr)

## Music
#  Detection Data
sb <- read.csv('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a', stringsAsFactors = F)
sb <- mutate(sb, TL = TL/100,
             FL = FL/100,
             Weight = Weight/1000)
sb <- melt(sb, id.vars = c('Date','Batch', 'Location', 'Gear', 'Sex', 'Stage',
                           'Transmitter'),
           measure.vars = c('TL', 'FL', 'Weight'))

detects <- vemsort('p:/obrien/biotelemetry/hudson sb/receiver logs')
detects <- filter(detects, transmitter %in%
                    paste0('A69-1303-', seq(11423, 11522, 1)))
detects$date.floor <- floor_date(detects$date.local, unit = 'day')
detects <- left_join(sb, detects, by = c('Transmitter' = 'transmitter'))

#  Plotting
plot.data <- detects %>%
  filter(variable == 'TL', !is.na(date.floor)) %>%
  distinct(date.floor, Transmitter, .keep_all = T) %>%
  select(date.floor, Transmitter, value, Sex) %>%
  arrange(value, Transmitter)

music <- ggplot() + geom_raster(data = plot.data,
                                aes(x = date.floor, y = value, fill = Sex)) +
  labs(x = 'Date', y = 'Length (cm)') +
  xlim(c(ymd_hms('2016-05-19 00:00:00'), ymd_hms('2016-06-16 00:00:00'))) +
  theme(legend.position = c(0.9, 0.85))


## VR2AR Data
vr2ar <- read.csv('p:/obrien/biotelemetry/hudson sb/hudson_vr2ar_events.csv',
                  stringsAsFactors = F)
vr2ar <- filter(vr2ar, Description == 'Average temperature') %>%
  mutate(Date.Time = ymd_hms(Date.Time),
         temp = as.numeric(Data),
         Region = 'Lower') %>%
  group_by(Date.Time, Region) %>%
  summarize(avg.temp = mean(temp, na.rm = T))

## Albany USGS Temperature
alb <- read.csv('p:/obrien/biotelemetry/hudson sb/albanytemp.csv')
alb <- mutate(alb, date.time = ymd_hms(date.time),
         Date.Time = ceiling_date(date.time, unit = 'hours'),
         Region = 'Upper') %>%
  group_by(Date.Time, Region) %>%
  summarize(avg.temp = mean(value, na.rm = T))

temp.data <- rbind(vr2ar, alb)

temperature.plot <- ggplot() +
  geom_line(data = temp.data, aes(x = Date.Time, y = avg.temp, color = Region)) +
  labs(y = 'Temp') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  lims(x = c(ymd_hms('2016-05-19 00:00:00'), ymd_hms('2016-06-16 00:00:00')),
       y = c(14,25)) +
  theme(legend.position = 'none')

library(gridExtra)
grid.arrange(temperature.plot, music, heights = c(1,3))
