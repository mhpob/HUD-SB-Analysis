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
