
# One migration track highlight ----
# Use agg.pos.imp from perfishTS.R
source('data and imports/perfishTS.R')
# library(ggplot2); library(dplyr)
k <- filter(agg.pos.imp, grepl('11490', transmitter)) %>%
  mutate(avg.imp = ifelse(!is.na(lat.avg), NA, avg.imp))

# Using a dummy DOY as work-around for month labels
agg.pos.imp$dummydoy <- (agg.pos.imp$doy - 1) + as.Date('2017-01-01')

ggplot() +
  annotate('rect', xmin = as.Date('2017-04-01'),
           xmax = as.Date('2017-07-01'),
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = as.Date('2017-04-01'),
           xmax = as.Date('2017-07-01'),
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  ylim(c(40.8, 42.75)) +
  geom_line(data = k,
            aes(x = dummydoy, y = avg.imp, lty = year), col = 'black', lwd = 1.5) +
  geom_line(data = filter(agg.pos.imp, grepl('11490', transmitter)),
            aes(x = dummydoy, y = lat.avg, lty = year), col = 'red', lwd = 1.5) +
  labs(x = NULL, y = 'Latitude', lty = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.background = element_blank(),
        legend.position = c(0.87, 0.89))


# Centroid plots of both years ----
# Use data from TSclustering.R
# library(ggplot2); library(dplyr)

cents17 <- data.frame(cent = rep(paste('Centroid', 1:2, sep = ' '),
                                 each = 86),
                      value = c(c2_17@centroids[[1]], c2_17@centroids[[2]]),
                      date = rep(unique(agg.pad.imp$doy), times = 2)) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1),
         year = '2017')
cents18 <- data.frame(cent = rep(paste('Centroid', 1:2, sep = ' '),
                               each = 86),
                    value = c(k2_18@centroids[[2]], k2_18@centroids[[1]]),
                    date = rep(unique(agg.pad.imp$doy), times = 2)) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1),
         year = '2018')

cents <- rbind(cents17, cents18)


mindate <- as.Date('2017-01-01') + (min(agg.pad.imp$doy) - 1)
maxdate <- as.Date('2017-01-01') + (max(agg.pad.imp$doy) - 1)

ggplot(cents) +
  facet_wrap(~cent) +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  ylim(c(40.8,42.75)) +
  geom_line(aes(x = date, y = value, lty = year), lwd = 1.5) +
  scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
  labs(x = NULL, y = 'Latitude', lty = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18))

# Add 2018 tracks to centroid 2 ----
# Use data from above section
TS <- data.frame(TS = do.call(c, k2_18@datalist),
                 trans = rep(names(k2_18@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$doy), times = 40),
                 cent = paste0('Centroid ',
                               rep(k2_18@cluster, each = 86))) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1),
         cent = ifelse(grepl('1', cent), 'Centroid 2', 'Centroid 1'))

ggplot(cents18) +
  facet_wrap(~cent) +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  ylim(c(40.8,42.75)) +
  geom_line(data = filter(TS, cent == 'Centroid 1'),
            aes(x = date, y = TS, group = trans),
            color = 'red', lwd = 1) +
  geom_line(aes(x = date, y = value), lwd = 1.5) +
  scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
  labs(x = NULL, y = 'Latitude', lty = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18))

# 2018 fish on 2017 centroids ----
# Use data from above section
c217_fish <- unique(as.character(filter(TS, cent == 'Centroid 1')$trans))
TS17 <- data.frame(TS = do.call(c, c2_17@datalist),
                 trans = rep(names(c2_17@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$doy), times = 66),
                 cent = paste0('Centroid ',
                               rep(c2_17@cluster, each = 86))) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1))

ggplot(cents17) +
  facet_wrap(~cent) +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  ylim(c(40.8,42.75)) +
  geom_line(data = filter(TS17, trans %in% c217_fish),
            aes(x = date, y = TS, group = trans),
            color = 'red', lwd = 1) +
  geom_line(aes(x = date, y = value), lwd = 1.5) +
  scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
  labs(x = NULL, y = 'Latitude', lty = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18))


# Sex highlighting ----
# Use agg.pad.imp from perfishTS.R
# library(ggplot2); library(dplyr)
c2 <- readRDS('data and imports/cluster data/c2.rda')
c2_17 <- c2[[1]][[1]][[1]]
c2_18 <- c2[[2]][[1]][[1]]

cents17 <- data.frame(cent = rep(paste('Centroid', 1:2, sep = ' '),
                                 each = 86),
                      value = c(c2_17@centroids[[1]], c2_17@centroids[[2]]),
                      date = rep(unique(agg.pad.imp$doy), times = 2)) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1),
         year = '2017')
cents18 <- data.frame(cent = rep(paste('Centroid', 1:2, sep = ' '),
                                 each = 86),
                      value = c(c2_18@centroids[[2]], c2_18@centroids[[1]]),
                      date = rep(unique(agg.pad.imp$doy), times = 2)) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1),
         year = '2018')

cents <- rbind(cents17, cents18)

TS17 <- data.frame(TS = do.call(c, c2_17@datalist),
                   trans = rep(names(c2_17@datalist), each = 86),
                   date = rep(unique(agg.pad.imp$doy), times = 66),
                   cent = paste0('Centroid ',
                                 rep(c2_17@cluster, each = 86))) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1)) %>%
  left_join(filter(distinct(ungroup(agg.pad.imp), transmitter, sex),
                   !is.na(sex)),
            by = c('trans' = 'transmitter'))

TS18 <- data.frame(TS = do.call(c, c2_18@datalist),
                 trans = rep(names(c2_18@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$doy), times = 40),
                 cent = paste0('Centroid ',
                               rep(c2_18@cluster, each = 86))) %>%
  mutate(date = as.Date('2017-01-01') + (date - 1),
         cent = ifelse(grepl('1', cent), 'Centroid 2', 'Centroid 1')) %>%
  left_join(filter(distinct(ungroup(agg.pad.imp), transmitter, sex),
                   !is.na(sex)),
            by = c('trans' = 'transmitter'))


# Using a dummy DOY as work-around for month labels
agg.pad.imp$dummydoy <- (agg.pad.imp$doy - 1) + as.Date('2017-01-01')
mindate <- as.Date('2017-01-01') + (min(agg.pad.imp$doy) - 1)
maxdate <- as.Date('2017-01-01') + (max(agg.pad.imp$doy) - 1)

ggplot(cents17) +
  facet_wrap(~cent) +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect',
           xmin = mindate,
           xmax = maxdate,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  ylim(c(40.8,42.75)) +
  geom_line(data = TS17,
            aes(x = date, y = TS, group = trans, color = sex), lwd = 1) +
  geom_line(aes(x = date, y = value), lwd = 1.5) +
  scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
  scale_color_manual(values = c('red', 'blue')) +
  labs(x = NULL, y = 'Latitude', color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = c(0.9, 0.9))

# Map of the mid-Atlantic ----
# library(ggplot2); library(sf)

midatl <- st_read('c:/users/secor/desktop/gis products/natural earth/50m coastline') %>%
  st_crop(xmin = -76.5, ymin = 36, xmax = -66, ymax = 45)
stlines <- st_read('c:/users/secor/desktop/gis products/natural earth/50m statelines') %>%
  st_crop(xmin = -76.5, ymin = 36, xmax = -66, ymax = 45)


ggplot() +
  geom_sf(data = midatl, fill = 'darkgray') +
  geom_sf(data = stlines) +
  coord_sf(xlim = c(-76.2, -67), ylim = c(36.2, 44.8), expand = F) +
  theme_bw()

# Map of whole Hudson ----
# library(ggplot2); library(dplyr); library(sf)

midatl <- st_read(file.path('p:/obrien/biotelemetry/past sb/past-analysis',
                            'manuscript/plos one/atlcoast.gpkg'))

plot_points <- read.csv('p:/obrien/biotelemetry/hudson sb/hudsonpoints.csv')

ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_sf(data = midatl) +
  coord_sf(xlim = c(-74.1, -73.6), ylim = c(40.59, 42.75)) +
  annotate('rect', xmin = -73.95, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, color = 'black', fill = NA) +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, color = 'black', fill = NA) +
  geom_point(data = filter(plot_points, Type == 'Receiver'),
             aes(x = Long, y = Lat), size = 3.5, pch = 1) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text = element_blank())
