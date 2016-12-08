library(ggplot2); library(dplyr)

source('detection_input.R')

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
  theme_bw()

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

