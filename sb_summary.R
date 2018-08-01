library(ggplot2); library(dplyr)

detects <- readRDS('hud_detects.RDS')

data_lab <- c(
  'TL' = 'Total Length (cm)',
  'FL' = 'Fork Length (cm)',
  'Weight' = 'Weight (kg)'
)

plot_data <- distinct(detects, transmitter, variable, .keep_all = T) %>%
  filter(variable %in% c('TL', 'Weight'))

ggplot() + geom_boxplot(data = plot_data,
                        aes(x = region, y = value, fill = sex)) +
  facet_wrap(~ variable, scales = 'free',
             labeller = labeller(variable = data_lab)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  labs(x = 'Spawning region', y = 'Value') +
  theme_bw() +
  theme(legend.position = c(0.9, 0.9))

# Summary Data
summary <- detects %>%
  group_by(region, variable) %>%
  summarize(min = min(value, na.rm = T),
            mean = mean(value, na.rm = T),
            max = max(value, na.rm = T))

didnt_leave <- detects %>%
  group_by(transmitter, region) %>%
  distinct(array) %>%
  filter(array == 'Below') %>%
  group_by(region) %>%
  summarize(n = 50-n())

sex <- detects %>%
  distinct(transmitter, .keep_all = T) %>%
  group_by(region, sex) %>%
  summarize(n = n())

WN2SC <- detects %>%
  filter(grepl('West', region),
         grepl('Sau', array)) %>%
  distinct(transmitter)
