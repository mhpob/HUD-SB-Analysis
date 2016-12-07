library(ggplot2)

source('detection_input.R')

data_lab <- c(
  'TL' = 'Total Length (cm)',
  'FL' = 'Fork Length (cm)',
  'Weight' = 'Weight (kg)'
)

ggplot() + geom_boxplot(data = filter(sb, variable %in% c('TL', 'Weight')),
                        aes(x = Region, y = value, fill = Sex)) +
  facet_wrap(~ variable, scales = 'free',
             labeller = labeller(variable = data_lab)) +
  labs(x = 'Spawning Region', y = 'Value') +
  theme_bw()
