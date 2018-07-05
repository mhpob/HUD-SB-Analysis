library(ggplot2); library(dplyr)

detects <- readRDS('hud_detects.RDS')

data_lab <- c(
  'TL' = 'Total Length (cm)',
  'FL' = 'Fork Length (cm)',
  'Weight' = 'Weight (kg)'
)

plot_data <- distinct(detects, Transmitter, variable, .keep_all = T) %>%
  filter(variable %in% c('TL', 'Weight'))

ggplot() + geom_boxplot(data = plot_data,
                        aes(x = Region, y = value, fill = Sex)) +
  facet_wrap(~ variable, scales = 'free',
             labeller = labeller(variable = data_lab)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  labs(x = 'Spawning Region', y = 'Value') +
  theme_bw() +
  theme(legend.position = c(0.9, 0.9))

# Summary Data
summary <- detects %>%
  group_by(Region, variable) %>%
  summarize(min = min(value, na.rm = T),
            mean = mean(value, na.rm = T),
            max = max(value, na.rm = T))

didnt_leave <- detects %>%
  group_by(Transmitter, Region) %>%
  distinct(array) %>%
  filter(array == 'Below') %>%
  group_by(Region) %>%
  summarize(n = 50-n())

sex <- detects %>%
  distinct(Transmitter, .keep_all = T) %>%
  group_by(Region, Sex) %>%
  summarize(n = n())

WN2SC <- detects %>%
  filter(grepl('West', Region),
         grepl('Sau', array)) %>%
  distinct(Transmitter)

# Transmitter loss
library(TelemetryR); library(lubridate)
# Overall loss
loss_overall <- trans_loss(detects, 'date.local', 'Transmitter')

# Loss coded by tagging-location-based classification
loss_tag <- detects %>%
  split(., .$Region) %>%
  lapply(trans_loss, 'date.local', 'Transmitter') %>%
  bind_rows(.id = 'Region') %>%
  mutate(cl_type = 'Tagging')

# Loss coded by cluster-based classification
# "temp" is from TSclustering.R. Need to change this name.
loss_clust <- detects %>%
  left_join(temp) %>%
  filter(date.local >= '2017-04-01') %>%
  split(., .$cluster) %>%
  lapply(trans_loss, 'date.local', 'Transmitter') %>%
  bind_rows(.id = 'Region') %>%
  mutate(cl_type = 'Cluster') %>%
  rbind(loss_tag)

ggplot() +
  geom_line(data = loss_clust, aes(x = date, y = remaining,
                                   color = Region, lty = cl_type), lwd = 1.5) +
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
  filter(Transmitter %in% temp$Transmitter) %>%
  split(., .$Region) %>%
  lapply(trans_loss, 'date.local', 'Transmitter') %>%
  bind_rows(.id = 'Region') %>%
  mutate(cl_type = 'Tagging')

# Loss coded by cluster-based classification
loss_clust <- detects %>%
  left_join(temp) %>%
  filter(date.local >= '2017-04-01') %>%
  split(., .$cluster) %>%
  lapply(trans_loss, 'date.local', 'Transmitter') %>%
  bind_rows(.id = 'Region') %>%
  mutate(cl_type = 'Cluster') %>%
  rbind(loss_tag)


ggplot() +
  geom_line(data = loss_clust, aes(x = date, y = remaining,
                                   color = Region, lty = cl_type), lwd = 1.5) +
  labs(x = NULL, y = 'Fish Remaining', lty = 'Classification') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(ymd_hms('2017-04-01 00:00:00'),
                              ymd_hms('2018-04-10 00:00:00'))) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.8))
