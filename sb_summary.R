library(reshape2); library(dplyr)

sb <- read.csv('p:/obrien/biotelemetry/hudson sb/sb sonic tags 2016.csv',
               na.strings = 'n/a')
sb <- mutate(sb, TL = TL/100,
             FL = FL/100,
             Weight = Weight/1000)
sb <- melt(sb, id.vars = c('Date','Batch', 'Location', 'Gear', 'Sex', 'Stage',
                           'Transmitter'),
           measure.vars = c('TL', 'FL', 'Weight'))


library(ggplot2)
data_lab <- c(
  'TL' = 'Total Length (cm)',
  'FL' = 'Fork Length (cm)',
  'Weight' = 'Weight (kg)'
)

ggplot() + geom_boxplot(data = sb, aes(x = Sex, y = value)) +
  facet_wrap(~ variable, scales = 'free',
             labeller = labeller(variable = data_lab)) +
  labs(y = 'Value')

