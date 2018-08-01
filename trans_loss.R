library(TelemetryR); library(ggplot2); library(lubridate); library(dplyr)
detects <- readRDS('hud_detects.RDS')
# Overall loss
loss_overall <- trans_loss(detects, 'date.local', 'transmitter')
ggplot() +
  geom_line(data = loss_overall, aes(x = date, y = remaining), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(ymd_hms('2016-05-01 00:00:00'),
                              ymd_hms('2018-05-01 00:00:00'))) +
  theme_bw() +
  theme(legend.position = c(0.3, 0.3))

# Loss coded by tagging-location-based classification
loss_tag <- detects %>%
  split(., .$region) %>%
  lapply(trans_loss, 'date.local', 'transmitter') %>%
  bind_rows(.id = 'region') %>%
  mutate(cl_type = 'tagging')

# Loss coded by cluster-based classification
# "temp" is from TSclustering.R. Need to change this name.
loss_clust <- detects %>%
  left_join(temp) %>%
  filter(date.local >= '2017-04-01') %>%
  split(., .$cluster) %>%
  lapply(trans_loss, 'date.local', 'transmitter') %>%
  bind_rows(.id = 'region') %>%
  mutate(cl_type = 'cluster') %>%
  rbind(loss_tag)

ggplot() +
  geom_line(data = loss_tag, aes(x = date, y = remaining,
                                 color = region, lty = cl_type), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining', lty = 'Classification') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(ymd_hms('2016-05-01 00:00:00'),
                              ymd_hms('2018-04-10 00:00:00'))) +
  theme_bw() +
  theme(legend.position = c(0.3, 0.3))


# Same thing, but select only the 66 transmitters used in the cluster analysis
# Loss coded by tagging-location-based classification
loss_tag <- detects %>%
  filter(transmitter %in% temp$transmitter) %>%
  split(., .$region) %>%
  lapply(trans_loss, 'date.local', 'transmitter') %>%
  bind_rows(.id = 'region') %>%
  mutate(cl_type = 'tagging')

# Loss coded by cluster-based classification
loss_clust <- detects %>%
  left_join(temp) %>%
  filter(date.local >= '2017-04-01') %>%
  split(., .$cluster) %>%
  lapply(trans_loss, 'date.local', 'transmitter') %>%
  bind_rows(.id = 'region') %>%
  mutate(cl_type = 'cluster') %>%
  rbind(loss_tag) %>%
  filter(date >= '2017-04-01')


ggplot() +
  geom_line(data = loss_clust, aes(x = date, y = remaining,
                                   color = region, lty = cl_type), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining', lty = 'Classification') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(ymd_hms('2017-04-01 00:00:00'),
                              ymd_hms('2018-04-10 00:00:00'))) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.8))