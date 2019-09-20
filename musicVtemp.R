library(ggplot2); library(lubridate); library(dplyr)

## Music
#  Detection Data
detects <- readRDS('data and imports/hud_detects.RDS')

#  Plotting
plot.data <- detects %>%
  filter(variable == 'TL', !is.na(date.floor)) %>%
  distinct(date.floor, transmitter, .keep_all = T) %>%
  arrange(value, transmitter) %>%
  mutate(trans.f = factor(transmitter, levels = unique(transmitter)))

hud.cols <- colorRampPalette(c('red', 'pink'))(5)
mab.cols <- colorRampPalette(c('blue', 'violet'))(4)
ne.cols <- colorRampPalette(c('yellow', 'orange4'))(3)

cols <- c('Above' = hud.cols[1], 'Saugerties-Coxsackie' = hud.cols[2],
          'Between' = hud.cols[3], 'West Point-Newburgh' = hud.cols[4],
          'Below' = hud.cols[5],
          'NJ Coast' = mab.cols[1], 'DE Coast' = mab.cols[2], 'DE' = mab.cols[3],
          'MD Coast' = mab.cols[4],
          'ME' = ne.cols[1], 'MA' = ne.cols[2],
          'LI Sound' = ne.cols[3], 'NY Coast' = ne.cols[3],
          'Ches' = 'green')

music <- ggplot() + geom_raster(data = plot.data,
                                aes(x = date.floor, y = trans.f, fill = array)) +
  labs(x = 'Date') +
  scale_fill_manual(values = cols, breaks =
            c('Above', 'Saugerties-Coxsackie', 'Between', 'West Point-Newburgh',
              'Below', 'ME', 'MA', 'LI Sound','NY Coast', 'NJ Coast', 'DE Coast',
              'DE',
              'MD Coast', 'Ches')) +
  facet_wrap(~ region, ncol = 1, scales = 'free_y') +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  xlim(c(ymd_hms('2016-05-19 00:00:00'), ymd_hms('2017-12-20 00:00:00')))
  # theme(legend.position = c(0.9, 0.85))


## VR2AR Data
vr2ar <- list.files('p:/obrien/biotelemetry/hudson sb/receiver events',
                    full.names = T)
vr2ar <- lapply(vr2ar, read.csv, stringsAsFactors = F)
vr2ar <- do.call(rbind, vr2ar)

data.trim <- filter(vr2ar, Description == 'Minimum seawater depth') %>%
  mutate(Data = as.numeric(Data)) %>%
  filter(Data <= 1) %>%
  select(Date.Time, Receiver)

vr2ar <- anti_join(vr2ar, data.trim) %>%
  filter(Description == 'Average temperature')%>%
  mutate(Date.Time = ymd_hms(Date.Time),
         temp = as.numeric(Data),
         region = 'Lower') %>%
  group_by(Date.Time, region) %>%
  summarize(avg.temp = mean(temp, na.rm = T))

## Albany USGS Temperature
alb <- read.csv('p:/obrien/biotelemetry/hudson sb/albanytemp.csv')
alb <- mutate(alb, date.time = ymd_hms(date.time),
         Date.Time = ceiling_date(date.time, unit = 'hours'),
         region = 'Upper') %>%
  group_by(Date.Time, region) %>%
  summarize(avg.temp = mean(value, na.rm = T))

temp.data <- rbind(vr2ar, alb)

temperature.plot <- ggplot() +
  geom_line(data = temp.data, aes(x = Date.Time, y = avg.temp, color = region)) +
  labs(y = 'Temp') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  lims(x = c(ymd_hms('2016-05-19 00:00:00'), ymd_hms('2016-06-16 00:00:00')),
       y = c(14,25)) +
  theme(legend.position = 'none')

library(gridExtra)
grid.arrange(temperature.plot, music, heights = c(1,3))
