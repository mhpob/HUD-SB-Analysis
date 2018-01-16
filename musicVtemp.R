library(ggplot2); library(lubridate); library(dplyr)

## Music
#  Detection Data
detects <- readRDS('hud_detects.RDS')

#  Plotting
plot.data <- detects %>%
  filter(variable == 'TL', !is.na(date.floor)) %>%
  distinct(date.floor, Transmitter, .keep_all = T) %>%
  select(date.floor, Transmitter, value, Sex, array) %>%
  arrange(value, Transmitter) %>%
  mutate(trans.f = factor(Transmitter, levels = unique(Transmitter)))

hud.cols <- colorRampPalette(c('red4', 'darksalmon'))(5)
mab.cols <- colorRampPalette(c('blue', 'violet'))(3)
ne.cols <- colorRampPalette(c('orange', 'orangered'))(3)

cols <- c('Above' = hud.cols[1], 'Saugerties-Coxsackie' = hud.cols[2],
          'Between' = hud.cols[3], 'West Point-Newburgh' = hud.cols[4],
          'Below' = hud.cols[5],
          'NJ Coast' = mab.cols[1], 'DE Coast' = mab.cols[2],
          'MD Coast' = mab.cols[3],
          'ME' = ne.cols[1], 'MA' = ne.cols[2],
          'Long Isl' = ne.cols[3],
          'Ches' = 'green')

music <- ggplot() + geom_raster(data = plot.data,
                                aes(x = date.floor, y = trans.f, fill = array)) +
  labs(x = 'Date', y = 'Length (cm) ->') +
  scale_fill_manual(values = cols, breaks =
            c('Above', 'Saugerties-Coxsackie', 'Between', 'West Point-Newburgh',
              'Below', 'ME', 'MA', 'Long Isl', 'NJ Coast', 'DE Coast',
              'MD Coast', 'Ches')) +
  theme(axis.text.y = element_blank())
  # xlim(c(ymd_hms('2016-05-19 00:00:00'), ymd_hms('2016-07-01 00:00:00'))) +
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
