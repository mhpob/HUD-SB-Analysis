library(TelemetryR); library(ggplot2); library(lubridate); library(reshape2);
library(dplyr)

sb <- read.csv('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a', stringsAsFactors = F)
sb <- mutate(sb, TL = TL/100,
             FL = FL/100,
             Weight = Weight/1000,
             Region = ifelse(Location == 'RM 59', 'Lower', 'Upper'))
sb <- melt(sb, id.vars = c('Date','Batch', 'Location', 'Region', 'Gear', 'Sex',
                           'Stage', 'Transmitter'),
           measure.vars = c('TL', 'FL', 'Weight'))

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

## Join detections
detects <- vemsort('p:/obrien/biotelemetry/hudson sb/receiver logs')
detects <- filter(detects, transmitter %in%
                    paste0('A69-1303-', seq(11423, 11522, 1)),
                  date.utc >= ymd('2016-04-20'))
detects$date.floor <- floor_date(detects$date.local, unit = 'day')
detects <- left_join(sb, detects, by = c('Transmitter' = 'transmitter'))
