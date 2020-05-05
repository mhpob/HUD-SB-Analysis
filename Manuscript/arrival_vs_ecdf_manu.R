# Based off of arrival_vs_ecdf.R, used in HRF-directed analysis
library(ggplot2); library(lubridate); library(dplyr)

## Import temperature data ----
# USGS stations
usgs_data <- readRDS("data and imports/usgs_wq.rds") %>%
  mutate(date.local = ymd(datetime, tz= 'America/New_York'),
         year = year(date.local),
         # Make a dummy date with the same year to align dates when plotting
         dummy.date = date.local) %>%
  filter(month(date.local) %in% 3:7)

# Assign the year 2017 to dummy date
year(usgs_data$dummy.date) <- 2017

## Create ECDF plot of detection returns, per array ----
## Import detections ----
dets <- readRDS('data and imports/hud_detects.RDS') %>%
  filter(grepl('Above|Saug|Between|Newb|Below', array),
         date.local >= '2017-01-01',
         month(date.local) %in% 3:7) %>%
  mutate(year = year(date.local),
         date.floor = floor_date(date.local, 'day'))


det_ecdf <- dets %>%
  group_by(array, transmitter, year,sex) %>%
  summarize(min = min(date.local)) %>%
  # Make a dummy date with the same year to align dates when plotting
  mutate(dummy.date = min)

# Assign the year 2017 to dummy date
year(det_ecdf$dummy.date) <- 2017


# Find date at 50%
d50_f <- split(det_ecdf, det_ecdf$year)
d50_f <- lapply(d50_f, function(x) split(x, x$array))
d50_f <- lapply(d50_f, function(x) lapply(x, function(y) ecdf(y$min)))

d50 <- expand.grid(year = c(2017, 2018),
                   array = c('West Point-Newburgh', 'Saugerties-Coxsackie'))
d50 <- cbind(d50,
             d50 = .POSIXct(apply(d50, 1,
                                  function(x) summary(d50_f[[x[1]]][[x[2]]])[3]))) %>%
  mutate(date.floor = floor_date(d50, 'day'))

for(i in 1:nrow(det_ecdf)){
  det_ecdf[i, 'frac'] <-  d50_f[[as.character(det_ecdf[i, 3])
                                 ]][[as.character(det_ecdf[i, 1])
                                     ]](det_ecdf[i, 5])
}

arrival <-
  ggplot(data = det_ecdf[det_ecdf$array %in%
                         c('West Point-Newburgh', 'Saugerties-Coxsackie'),]) +
  geom_step(aes(x = dummy.date, group = array), color = 'gray',
            stat = 'ecdf', pad = F) +
  geom_step(aes(x = dummy.date, color = array, linetype = sex),
            stat = 'ecdf', pad = F,
            size = 2) +

  xlim(as.POSIXct('2017-04-01'), as.POSIXct('2017-06-10')) +
  ylab('Cumulative fraction detected') +
  facet_wrap(~ year) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 11),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

swt <-
  ggplot(data = usgs_data[grepl('pough', usgs_data$site_name) &
                          usgs_data$year %in% 2017:2018,]) +
  geom_line(aes(x = dummy.date, mwt), size = 1) +
  # geom_point(data = d50, aes(x = d50)) +
  xlim(as.POSIXct('2017-04-01'), as.POSIXct('2017-06-10')) +
  ylab('Water temperature (°C)') +
  facet_wrap(~ year) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 11),
          panel.grid.minor.x = element_blank())


disch <-
  ggplot(data = usgs_data[grepl('pough', usgs_data$site_name) &
                          usgs_data$year %in% 2017:2018,]) +
  geom_line(aes(x = dummy.date, y = mdisch / 1000), size = 1) +
  xlim(as.POSIXct('2017-04-01'), as.POSIXct('2017-06-10')) +
  labs(x = NULL, y = bquote("Discharge"~("1000"~m^3/s))) +
  facet_wrap(~ year) +
    theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 11),
        panel.grid.minor = element_blank())

library(patchwork)
combined <- arrival / (swt / disch)

ggsave("manuscript/ecdf_sex.tif", combined,
       width = 7.5, height = 7.5, units = 'in',
       device = 'tiff', compression = 'lzw')
