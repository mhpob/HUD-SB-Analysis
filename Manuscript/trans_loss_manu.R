library(TelemetryR); library(ggplot2); library(data.table)
detects <- data.table(readRDS('data and imports/hud_detects.RDS'))
detects <- detects[, ':='(region16 = region,
                          region = ifelse(grepl('West', region),
                                          'Lower', 'Upper'))]


# Overall loss
loss_overall <- trans_loss(detects, 'date.utc', 'transmitter')
loss_overall$date <- lubridate::floor_date(loss_overall$date)
loss_plot <- ggplot() +
  geom_line(data = loss_overall, aes(x = date, y = remaining), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining') +
  scale_x_datetime(date_breaks = '6 month',
                   date_labels = '%b %Y',
                   limits = c(as.POSIXct('2016-05-01 00:00:00'),
                              as.POSIXct('2019-04-20 00:00:00'))) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.grid.minor.y = element_blank())

k <- detects[, date := lubridate::floor_date(date.utc, 'day')]
k <- k[, .(m_lat = median(lat, na.rm = T)), by = 'date']
k <- k[data.table(loss_overall), on = 'date']

color_loss <- ggplot(data = k) +
  geom_rect(aes(xmin = date, xmax = date + (60*60*24)-1,
                ymin = 0, ymax = remaining, fill = m_lat)) +
  geom_line(aes(x = date, y = remaining), lwd = 1.5, color = 'gray40') +
  labs(x = NULL, y = 'Fish Remaining', fill = NULL) +
  scale_x_datetime(date_breaks = '4 month',
                   date_labels = '%b %Y',
                   limits = c(as.POSIXct('2016-05-01 00:00:00'),
                              as.POSIXct('2018-12-31 00:00:00'))) +
  scale_fill_viridis_c( na.value = 'transparent') +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_colorbar(
    barheight = unit(1,"npc") - unit(2, 'line'),
    barwidth = unit(2, 'line')
  ))


ggsave('manuscript/loss_overall.eps', loss_plot)
ggsave('manuscript/loss_overall_colored.eps', color_loss)

# Loss coded by tagging-location-based classification
loss_tag <- detects[, trans_loss(.SD, 'date.local', 'transmitter'), by = region]
loss_tag <- loss_tag[, cl_type := 'tagging']

# Loss coded by cluster-based classification
# Load saved recategorization from manuscript/recategorize.R.
recats <- fread('manuscript/recategorized.csv')
recats <- recats[, ':='(cluster17 = ifelse(cluster17 == 1, 'Upper','Lower'),
                        pred18 = ifelse(pred18 == 1, 'Upper','Lower'))]

loss_clust <- recats[, -'region'][detects, on = 'transmitter']
loss_clust <- loss_clust[date.local >= '2017-04-01']
loss_clust <- loss_clust[,trans_loss(.SD, 'date.local', 'transmitter'),
                         by = cluster17]
loss_clust <- loss_clust[, cl_type := '2017cluster']
loss_clust[is.na(cluster17), cluster17 := 'Unclassified']
loss_clust[, cluster17 := factor(cluster17, levels = c('Upper', 'Lower', 'Unclassified'), ordered = T)]

loss_clust <- loss_clust[, .(region = cluster17, date, remaining, cl_type)]



ggplot(data = rbind(loss_tag, loss_clust)) +
  geom_line(aes(x = date, y = remaining,
                                 color = region, lty = cl_type), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining', lty = 'Classification') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(as.POSIXct('2016-05-01 00:00:00'),
                              as.POSIXct('2018-12-31 00:00:00'))) +
  theme_bw() +
  theme(legend.position = c(0.3, 0.3))



ggplot() +
  geom_line(data = loss_clust,
            aes(x = date, y = remaining,
                color = region), lwd = 1.5) +
  # geom_line(data = loss_overall, aes(x = date, y = remaining), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining', color = '2017 Classification') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(as.POSIXct('2016-05-01 00:00:00'),
                              as.POSIXct('2018-12-31 00:00:00'))) +
  scale_color_viridis_d(begin = 1, end = 0.3) +
  theme_bw()

k <-loss_clust[, ':='(doy = yday(date),
                      year = year(date))][
                        year != 2019
                      ]


ggplot() +
  geom_line(data = loss_clust[, ':='(doy = yday(date),
                                     year = year(date))][year != 2019],
            aes(x = doy, y = remaining, color = region), lwd = 1.5) +
  facet_wrap(~ year, ncol = 1, strip.position = 'right') +
  labs(x = NULL, y = 'Fish Remaining', color = '2017 Classification') +
  coord_cartesian(xlim = c(121, 181)) +
  scale_x_continuous(breaks = c(121, 152, 181),
                     labels  = c('1 May', '1 June', '1 July')) +
  scale_color_viridis_d(begin = 1, end = 0.3) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

## Map inset
library(sf)
rivers <- read_sf('p:/obrien/natural earth/ne_50m_rivers/ne_50m_rivers_lake_centerlines.shp')
states <- read_sf('p:/obrien/natural earth/ne_50m_states/ne_50m_admin_1_states_provinces.shp')
ggplot() +
  geom_sf(data = states, color = 'black', fill = 'gray') +
  geom_sf(data = rivers, color = 'lightgray') +
  coord_sf(xlim = c(-77.5, -68), ylim = c(36.54, 44.5), expand = F) +
  annotate('text', label = '>28"', x = -69, y = 37) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())



## Survival ----
library(survival)
library(ggplot2); library(data.table)
detects <- data.table(readRDS('data and imports/hud_detects.RDS'))
setnames(detects, 'region', 'tagging.region')
detects <- detects[, tagging.region := ifelse(grepl('West', tagging.region),
                                              'Lower', 'Upper')]
max_det <- detects[, .(max_date = max(date.local)),
                   by = c('transmitter', 'tagging.region')]

recats <- fread('manuscript/recategorized.csv')
max_det <- recats[, -'region'][max_det, on = 'transmitter']


# code surviving with max date and 0, everything else with 1

surv_overall <- max_det[max_date > '2017-05-01']
surv_overall[, ':='(max_c17 = ifelse(max_date >= '2019-01-01',
                                  (as.numeric(as.POSIXct('2018-12-31 23:59:59')) -
                                     as.numeric(as.POSIXct('2017-05-01'))) /
                                    (60 * 60 * 24),
                                  (as.numeric(max_date) -
                                     as.numeric(as.POSIXct('2017-05-01'))) /
                                    (60 * 60 * 24)),
                 status = ifelse(max_date >= '2019-01-01', 0, 1))]


sall_mod <- survfit(Surv(max_c17, status) ~  cluster17, data = surv_overall)
survdiff(Surv(max_c17, status) ~  cluster17, data = surv_overall, rho = 1)
plot(sall_mod, conf.int = T, col = c(1, 2))



surv_17 <- max_det[max_date >= '2017-05-01' & !is.na(cluster17)]
surv_17[, ':='(max_c17 = ifelse(max_date >= '2018-01-01',
                            (as.numeric(as.POSIXct('2017-12-31 23:59:59')) -
                               as.numeric(as.POSIXct('2017-05-01'))) /
                              (60 * 60 * 24),
                            (as.numeric(max_date) -
                               as.numeric(as.POSIXct('2017-05-01'))) /
                              (60 * 60 * 24)),
           status = ifelse(max_date >= '2018-01-01', 0, 1))]


s17_mod <- survfit(Surv(max_c17, status) ~  cluster17, data = surv_17)
summary(s17_mod)
survdiff(Surv(max_c17, status) ~  cluster17, data = surv_17, rho = 1)
plot(s17_mod, conf.int = T, col = c(1, 2))



surv_18 <- max_det[max_date >= '2018-05-01']
surv_18[, ':='(max_c17 = ifelse(max_date >= '2019-01-01',
                            (as.numeric(as.POSIXct('2018-12-31 23:59:59')) -
                               as.numeric(as.POSIXct('2018-05-01'))) /
                              (60 * 60 * 24),
                            (as.numeric(max_date) -
                               as.numeric(as.POSIXct('2018-05-01'))) /
                              (60 * 60 * 24)),
           status = ifelse(max_date >= '2019-01-01', 0, 1))]


s18_mod <- survfit(Surv(max_c17, status) ~  cluster17, data = surv_18)
summary(s18_mod)
survdiff(Surv(max_c17, status) ~  cluster17, data = surv_18, rho = 1)
plot(s18_mod, conf.int = T, col = c(1, 2))




