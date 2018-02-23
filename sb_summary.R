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
loss <- trans_loss(detects, 'date.local', 'Transmitter')

ggplot() + geom_line(data = loss, aes(x = date, y = remaining)) +
  labs(x = NULL, y = 'Fish Remaining') +
  scale_x_datetime(date_breaks = '3 month',
                   date_labels = '%b %Y',
                   limits = c(ymd_hms('2016-05-01 00:00:00'),
                              ymd_hms('2017-08-31 00:00:00'))) +
  theme_bw()

