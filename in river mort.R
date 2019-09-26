library(lubridate); library(dplyr)

all <- readRDS('data and imports/hud_detects.RDS') %>%
  left_join(readRDS('data and imports/recat_spawning_region.rds'))

last.dets <- all %>%
  distinct(transmitter, date.local, .keep_all = T) %>%
  group_by(transmitter) %>%
  arrange(desc(date.local)) %>%
  slice(2) %>%
  select(transmitter, date.local, array, station) %>%
  left_join(.,
    all %>%
      filter(variable == 'TL') %>%
      group_by(transmitter) %>%
      arrange(desc(date.local)) %>%
      slice(1) %>%
      select(transmitter, fdate.local = date.local, farray = array,
             fstation = station, lat, long, sex, variable, value, region,
             recat_region)
  )


write.csv(last.dets, 'data and imports/fish fates2.csv')

plot_pts <- all %>%
  filter(grepl('Ab|Be|Saug|Newb', array),
         year(date.local) == 2016) %>%
  distinct(transmitter, date.local, .keep_all = T)

library(ggplot2)
ggplot() + geom_tile(data = plot_pts, aes(x = yday(date.local), y = transmitter,
                                          fill = array))



all %>%
  filter(grepl('Ab|Be|Saug|Newb', array),
         year(date.local) == 2016,
         month(date.local)) %>%
  distinct(transmitter, date.local, .keep_all = T) %>%
  arrange(desc(date.local)) %>%
  slice(5)


all %>%
  filter(date.local >= '2018-03-15',
         date.local <= '2018-07-01',
         array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                      'West Point-Newburgh', 'Below')) %>%
  distinct(transmitter) %>%
  tally()
