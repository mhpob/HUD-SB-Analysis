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
         date.local <= '2018-12-31',
         month(date.local) %in% 3:7) %>%
  mutate(year = year(date.local),
         date.floor = floor_date(date.local, 'day'))

recats <- read.csv('manuscript/recategorized.csv')
recats <- recats %>%
  mutate(cluster17 = factor(cluster17, levels = c('Upper', 'Lower'), ordered = T),
         pred18 = factor(pred18, levels = c('Upper', 'Lower'), ordered = T))
dets <- left_join(dets, recats, by = 'transmitter')

det_ecdf <- dets %>%
  group_by(cluster17, transmitter, year,sex) %>%
  summarize(min = min(date.local)) %>%
  # Make a dummy date with the same year to align dates when plotting
  mutate(dummy.date = min) %>%
  # Remove 11424 in 2018, as it's likely false
  filter(!(grepl('11424', transmitter) & year == 2018))

# Assign the year 2017 to dummy date
year(det_ecdf$dummy.date) <- 2017


# Find date at 50%
d50_f <- split(det_ecdf, det_ecdf$year)
d50_f <- lapply(d50_f, function(x) split(x, x$cluster17))
d50_f <- lapply(d50_f, function(x) lapply(x, function(y) ecdf(y$min)))

d50 <- expand.grid(year = c(2017, 2018),
                   cluster17 = c('Lower', 'Upper'))
d50 <- cbind(d50,
             d50 = .POSIXct(apply(d50, 1,
                                  function(x) summary(d50_f[[x[1]]][[x[2]]])[3]))) %>%
  mutate(date.floor = floor_date(d50, 'day'))
usgs_data %>%
  filter(date(date.local) %in% date(d50$date.floor),
         grepl('pough', site_name))

for(i in 1:nrow(det_ecdf)){
  det_ecdf[i, 'frac'] <-  d50_f[[as.character(data.frame(det_ecdf)[i, 3])
                                 ]][[as.character(data.frame(det_ecdf)[i, 1])
                                     ]](det_ecdf$min[i])
}

arrival <-
  ggplot(data = filter(det_ecdf, !is.na(cluster17))) +
  geom_step(aes(x = dummy.date, color = cluster17),
            stat = 'ecdf', pad = F,
            size = 2) +
  scale_color_discrete()+
  xlim(as.POSIXct('2017-04-01'), as.POSIXct('2017-06-01')) +
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
  xlim(as.POSIXct('2017-04-01'), as.POSIXct('2017-06-01')) +
  ylim(0, 25) +
  ylab('Water temperature (°C)') +
  facet_wrap(~ year) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 11),
          panel.grid.minor = element_blank())


disch <-
  ggplot(data = usgs_data[grepl('pough', usgs_data$site_name) &
                          usgs_data$year %in% 2017:2018,]) +
  geom_line(aes(x = dummy.date, y = mdisch / 1000), size = 1) +
  xlim(as.POSIXct('2017-04-01'), as.POSIXct('2017-06-01')) +
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

ggsave("manuscript/ecdf_2017cluster_tzb.tif", combined,
       width = 7.5, height = 7.5, units = 'in',
       device = 'tiff', compression = 'lzw')



### Find times and corresponding values ----
plot(frac ~ min, data = det_ecdf,
     subset = (cluster17 == 'Lower' & year == 2018))

hold <- locator()
hold$x <- .POSIXct(hold$x)
usgs_data %>%
  filter(date(date.local) %in% date(hold$x),
         grepl('pough', site_name)) %>%
  select(date.local, min_wt, mwt, max_wt)
